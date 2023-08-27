
struct Bytecode_Builder {
    Memory_Arena arena;
    
    array(Bytecode_Operand*)* register_stack;
    
    map(string_id, Bytecode_Operand)* locals;
    map(string_id, Bytecode_Operand)* globals;
    
    Bytecode_Function* curr_function;
    Bytecode_Instruction* curr_insn;
    
    Data_Packer* data_packer;
    Interp* interp;
    
    Bytecode bytecode;
    
    u32 block_depth;
    
    u32 next_type_index;
    u32 next_register_index;
    
    Bytecode_Type pointer_type;
    bool use_absolute_memory;
};

Bytecode_Type
bc_pointer_type(Bytecode_Builder* bc) {
    return {};
    //return (bc->pointer_type != BytecodeType_void) ? bc->pointer_type : BytecodeType_i64;
}

Bytecode_Flags
to_bytecode_type_flags(Bytecode_Builder* bc, Type* type) {
    Bytecode_Flags result = {};
    
    switch (type->kind) {
        case TypeKind_Basic: {
            
            switch (type->Basic.kind) {
                case Basic_s8:  return BC_FLAG_8BIT  | BC_FLAG_SIGNED;
                case Basic_s16: return BC_FLAG_16BIT | BC_FLAG_SIGNED;
                case Basic_s32: return BC_FLAG_32BIT | BC_FLAG_SIGNED;
                case Basic_s64: return BC_FLAG_64BIT | BC_FLAG_SIGNED;
                
                case Basic_bool:
                case Basic_u8:  return BC_FLAG_8BIT;
                case Basic_u16: return BC_FLAG_16BIT;
                case Basic_u32: return BC_FLAG_32BIT;
                case Basic_u64: return BC_FLAG_64BIT;
                
                //case Basic_f32: return BytecodeType_f32;
                //case Basic_f64: return BytecodeType_f64;
                
                default: unimplemented;
            }
        } break;
    }
    
    
    return result;
}

Bytecode_Type
to_bytecode_type(Bytecode_Builder* bc, Type* type) {
    return {};
    
#if 0
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
                case Basic_uint: 
                
                case Basic_s64: 
                case Basic_u64: 
                case Basic_smm: 
                case Basic_umm: return BytecodeType_i64;
                
                case Basic_string:
                case Basic_cstring: {
                    return (bc->pointer_type != BytecodeType_void) ? bc->pointer_type : BytecodeType_i64;
                } break;
                
                case Basic_f32: return BytecodeType_f32;
                case Basic_f64: return BytecodeType_f64;
                
                default: unimplemented; break;
            }
        } break;
        
        case TypeKind_Struct:
        case TypeKind_Union: // TODO(Alexander): should structs/ unions be a different type than 64-bit int?
        case TypeKind_Type:
        case TypeKind_Array:
        case TypeKind_Function:
        case TypeKind_Pointer: {
            return (bc->pointer_type != BytecodeType_void) ? bc->pointer_type : BytecodeType_i64;
        } break;
        
        case TypeKind_Enum: {
            return to_bytecode_type(bc, type->Enum.type);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return BytecodeType_i32;
#endif
}

Bytecode_Function* add_bytecode_function(Bytecode_Builder* bc, Type* type);

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


#define instruction(bc, opcode, register_result, register_src0, register_src1) \
_instruction(bc, opcode, register_result, register_src0, register_src1, __FILE__ ":" S2(__LINE__))

#define const_instruction(bc, opcode, register_result, constant) \
add_const_instruction(bc, opcode, register_result, constant, __FILE__ ":" S2(__LINE__))

#define store_instruction(bc, type, dest, src) \
add_store_instruction(bc, type, dest, src, __FILE__ ":" S2(__LINE__))


inline Bytecode_Binary*
_instruction(Bytecode_Builder* bc, Bytecode_Operator opcode, 
             Bytecode_Operand result, Bytecode_Operand first, Bytecode_Operand second,
             cstring comment) {
    
    Bytecode_Binary* insn = add_insn_t(bc, opcode, Binary);
    insn->type = BytecodeType_i32;
    insn->res_index = result.register_index;
    insn->arg0_index = first.register_index;
    insn->arg1_index = second.register_index;
    insn->comment = comment;
    return insn;
}


inline void
add_const_instruction(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                      Bytecode_Operand result, s64 constant, cstring comment) {
    
    Bytecode_Binary* insn = add_insn_t(bc, opcode, Binary);
    insn->type = BytecodeType_i32;
    insn->res_index = result.register_index;
    insn->const_i64 = constant;
    insn->comment = comment;
}

inline void
add_store_instruction(Bytecode_Builder* bc, Type* type, Bytecode_Operand dest, Bytecode_Operand src, cstring comment) {
    Bytecode_Type bc_type = to_bytecode_type(bc, type);
    Bytecode_Binary* result = add_insn_t(bc, BC_STORE, Binary);
    result->type = bc_type;
    result->res_index = -1;
    result->arg0_index = dest.register_index;
    result->arg1_index = src.register_index;
    result->comment = comment;
}

inline Bytecode_Operand
add_bytecode_register(Bytecode_Builder* bc, Type* type=0) {
    assert(bc->curr_function);
    
    Bytecode_Operand result = {};
    result.kind = BytecodeOperand_register;
    result.register_index = bc->curr_function->register_count++;
    
    Bytecode_Flags reg_type = 0;
    if (type) {
        reg_type = to_bytecode_type_flags(bc, type);
    }
    array_push(bc->curr_function->register_types, reg_type);
    
    return result;
}

#define push_bytecode_stack_t(bc, T) \
push_bytecode_stack(bc, (u32) sizeof(T), (u32) alignof(T))

inline Bytecode_Operand
push_bytecode_stack(Bytecode_Builder* bc, u32 size, u32 align) {
    assert(bc->curr_function && "need to start a new function first");
    
    Bytecode_Alloca* alloca = add_insn_t(bc, BC_ALLOCA, Alloca);
    alloca->dest = add_bytecode_register(bc);
    alloca->size = size;
    alloca->align = align;
    
    //Stack_Entry stk = { size, align };
    //array_push(bc->curr_function->stack, stk);
    return alloca->dest;
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
    if (bc->use_absolute_memory) {
        result.memory_absolute = data;
    } else {
        result.memory_offset = offset;
        result.memory_kind = kind;
    }
    return result;
}

inline void
begin_block(Bytecode_Builder* bc, Bytecode_Operator opcode=BC_BLOCK) {
    add_insn(bc, opcode);
    bc->curr_function->block_count++;
    bc->block_depth++;
}

inline void
end_block(Bytecode_Builder* bc) {
    assert(bc->block_depth > 0);
    add_insn(bc, BC_END);
    bc->block_depth--;
}

void string_bc_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn);
void string_bc_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Function* func, Type* type=0);
void dump_bytecode(Bytecode* bc);