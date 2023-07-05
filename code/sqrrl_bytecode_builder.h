
struct Stack_Entry {
    u32 size;
    u32 align;
};

struct Bytecode_Builder {
    Memory_Arena arena;
    
    array(string_id)* function_names;
    array(Bytecode_Instruction*)* labels;
    array(Stack_Entry)* stack;
    
    map(string_id, Bytecode_Operand)* locals;
    map(string_id, Bytecode_Operand)* globals;
    
    Bytecode_Function* curr_function;
    
    Data_Packer* data_packer;
    Interp* interp;
    
    Bytecode bytecode;
    
    u32 next_type_index;
    u32 next_register_index;
};

Bytecode_Type
to_bytecode_type(Type* type) {
    switch (type->kind) {
        case TypeKind_Basic: {
            
            switch (type->Basic.kind) {
                case Basic_bool:
                case Basic_s8:
                case Basic_u8:
                case Basic_s16:
                case Basic_u16:
                case Basic_s32:
                case Basic_u32:
                case Basic_int:
                case Basic_uint: return BytecodeType_i32;
                
                case Basic_s64: 
                case Basic_u64: 
                case Basic_smm: 
                case Basic_umm: 
                case Basic_string:
                case Basic_cstring:  return BytecodeType_i64;
                
                case Basic_f32: return BytecodeType_f32;
                case Basic_f64: return BytecodeType_i64;
                
                default: unimplemented; break;
            }
        } break;
        
        case TypeKind_Struct:
        case TypeKind_Union: // TODO(Alexander): should structs/ unions be a different type than 64-bit int?
        case TypeKind_Type:
        case TypeKind_Pointer: {
            return BytecodeType_i64;
        } break;
        
        case TypeKind_Enum: {
            return to_bytecode_type(type->Enum.type);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return BytecodeType_i32;
}

Bytecode_Function*
begin_bytecode_function(Bytecode_Builder* bc, Type* type) {
    assert(type && type->kind == TypeKind_Function && "not a function type");
    
    int ret_count = is_valid_type(type->Function.return_type) ? 1 : 0;
    int arg_count = (int) array_count(type->Function.arg_types);
    int size = sizeof(Bytecode_Function) + ret_count + arg_count;
    
    Bytecode_Function* func = (Bytecode_Function*) arena_push_size(&bc->arena, size, 
                                                                   alignof(Bytecode_Function));
    func->type_index = bc->next_type_index++;
    func->ret_count = ret_count;
    func->arg_count = arg_count;
    
    Bytecode_Type* curr_type = (Bytecode_Type*) (func + 1);
    if (ret_count > 0) {
        assert(ret_count == 1 && "TODO: multiple arguments");
        *curr_type++ = to_bytecode_type(type->Function.return_type);
    }
    
    for (int i = 0; i < arg_count; i++) {
        *curr_type++ = to_bytecode_type(type->Function.arg_types[i]);
    }
    
    array_push(bc->bytecode.functions, func);
    array_push(bc->function_names, type->Function.ident);
    
    bc->curr_function = func;
    return func;
}

void
end_bytecode_function(Bytecode_Builder* bc) {
    bc->curr_function = 0;
}

#define S1(x) #x
#define S2(x) S1(x)
#define add_insn(bc, opcode) add_bytecode_insn(bc, opcode, \
BytecodeInstructionKind_Base, \
sizeof(Bytecode_Instruction), \
alignof(Bytecode_Instruction), \
__FILE__ ":" S2(__LINE__));

#define add_insn_t(bc, opcode, T) (Bytecode_##T*) add_bytecode_insn(bc, opcode, \
BytecodeInstructionKind_##T, \
sizeof(Bytecode_##T), \
alignof(Bytecode_##T), \
__FILE__ ":" S2(__LINE__));

Bytecode_Instruction*
add_bytecode_insn(Bytecode_Builder* bc, 
                  Bytecode_Operator opcode, 
                  Bytecode_Instruction_Kind kind, 
                  umm size, umm align, cstring loc) {
    
    assert(bc->curr_function && "cannot add instruction outside function scope");
    
    // TODO(Alexander): when we run out of memory we need to make sure we have pointer to next instruction
    Bytecode_Instruction* insn = (Bytecode_Instruction*) arena_push_size(&bc->arena, size, align);
    insn->opcode = opcode;
    insn->kind = kind;
    insn->comment = loc;
    
    return insn;
}

void
add_store_insn(Bytecode_Builder* bc, Bytecode_Operand dest, Bytecode_Operand src) {
    assert(src.type == dest.type);
    Bytecode_Binary* result = add_insn_t(bc, BC_STORE, Binary);
    result->first = dest;
    result->second = src;
}

#define push_bytecode_stack_t(bc, T) push_bytecode_stack(bc, (u32) sizeof(T), (u32) alignof(T));

inline Bytecode_Operand
push_bytecode_register(Bytecode_Builder* bc, Bytecode_Type type) {
    Bytecode_Operand result = {};
    result.kind = BytecodeOperand_register;
    result.type = type;
    result.register_index = bc->next_register_index++;
    return result;
}

inline Bytecode_Operand
push_bytecode_stack(Bytecode_Builder* bc, u32 size, u32 align) {
    Bytecode_Operand result = {};
    result.kind = BytecodeOperand_stack;
    result.type = BytecodeType_i64;
    result.stack_index = (u32) array_count(bc->stack);
    Stack_Entry stk = { size, align };
    array_push(bc->stack, stk);
    return result;
}

inline Bytecode_Operand
push_bytecode_memory(Bytecode_Builder* bc, Bytecode_Memory_Kind kind, smm size, smm align, void* init=0) {
    Memory_Arena* arena = (kind == BytecodeMemory_read_only ? 
                           &bc->data_packer->rdata_arena : 
                           &bc->data_packer->data_arena);
    void* data = arena_push_size(arena, size, align);
    u32 offset = (s32) arena_relative_pointer(arena, data);
    if (init) {
        memcpy(data, init, size);
    }
    
    Bytecode_Operand result = {};
    result.kind = BytecodeOperand_memory;
    result.type = BytecodeType_i64;
    result.memory_offset = offset;
    result.memory_kind = kind;
    return result;
}

void string_bc_dump_bytecode_insn(String_Builder* sb, Bytecode_Instruction* insn);
void string_bc_dump_bytecode(String_Builder* sb, Bytecode_Function* func, Type* type=0);
void dump_bytecode(Compilation_Unit* cu);