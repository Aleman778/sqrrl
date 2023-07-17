
struct Bytecode_Builder {
    Memory_Arena arena;
    
    array(Bytecode_Instruction*)* labels;
    array(Bytecode_Operand*)* register_stack;
    
    map(string_id, Bytecode_Operand)* locals;
    map(string_id, Bytecode_Operand)* globals;
    
    Bytecode_Function* curr_function;
    Bytecode_Instruction* curr_insn;
    
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


Bytecode_Function* begin_bytecode_function(Bytecode_Builder* bc, Type* type);
void end_bytecode_function(Bytecode_Builder* bc);

Bytecode_Instruction* add_bytecode_insn(Bytecode_Builder* bc, 
                                        Bytecode_Operator opcode, 
                                        Bytecode_Instruction_Kind kind, 
                                        umm size, umm align, cstring loc);

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

inline void
add_mov_insn(Bytecode_Builder* bc, Type* type, Bytecode_Operand dest, Bytecode_Operand src) {
    Bytecode_Operator opcode = BC_MOV;
    Bytecode_Type bc_type = to_bytecode_type(type);
    switch (type->size) {
        case 1: opcode = BC_MOV_8; break;
        case 2: opcode = BC_MOV_16; break;
        case 4: { 
            opcode = (bc_type == BytecodeType_i64 || 
                      bc_type == BytecodeType_f64) ? BC_MOV_32 : BC_MOV; 
        } break;
    }
    Bytecode_Binary* result = add_insn_t(bc, opcode, Binary);
    result->type = bc_type;
    result->first = dest;
    result->second = src;
}

#define push_bytecode_stack_t(bc, T) push_bytecode_stack(bc, (u32) sizeof(T), (u32) alignof(T));

inline Bytecode_Operand
add_bytecode_register(Bytecode_Builder* bc) {
    assert(bc->curr_function);
    Bytecode_Operand result = {};
    result.kind = BytecodeOperand_register;
    result.register_index = (u32) array_count(bc->curr_function->register_lifetimes);
    array_push(bc->curr_function->register_lifetimes, bc->curr_function->insn_count);
    return result;
}

inline void
drop_bytecode_register(Bytecode_Builder* bc, u32 register_index) {
    assert(bc->curr_function);
    assert(register_index < array_count(bc->curr_function->register_lifetimes) && "unknown register");
    bc->curr_function->register_lifetimes[register_index] = bc->curr_function->insn_count;
}

inline Bytecode_Operand
push_bytecode_stack(Bytecode_Builder* bc, u32 size, u32 align) {
    assert(bc->curr_function && "need to start a new function first");
    Bytecode_Operand result = {};
    result.kind = BytecodeOperand_stack;
    result.stack_index = (u32) array_count(bc->curr_function->stack);
    Stack_Entry stk = { size, align };
    array_push(bc->curr_function->stack, stk);
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
    result.memory_offset = offset;
    result.memory_kind = kind;
    return result;
}

void string_bc_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn);
void string_bc_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Function* func, Type* type=0);
void dump_bytecode(Bytecode* bc);