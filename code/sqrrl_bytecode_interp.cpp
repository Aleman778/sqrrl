
struct Bc_Interp_Scope {
    map(u32, Bc_Value)* registers; // TODO(Alexander): u32 or Bc_Register
};

struct Bc_Interp {
    array(Bc_Interp_Scope)* scopes;
    Bc_Label_To_Value_Table* declarations;
    
    Bc_Basic_Block* curr_block;
    umm curr_block_insn;
    
    Memory_Arena stack;
    smm base_pointer;
};

inline void
bc_interp_store_register(Bc_Interp* interp, Bc_Register reg, Bc_Value value) {
    assert(array_count(interp->scopes) > 0);
    Bc_Interp_Scope* scope = &array_last(interp->scopes);
    //pln("store: %, %", f_u32(reg.index), f_s64(value.signed_int));
    map_put(scope->registers, reg.index, value);
}

inline Bc_Value
bc_interp_load_register(Bc_Interp* interp, Bc_Register reg) {
    assert(array_count(interp->scopes) > 0);
    Bc_Interp_Scope* scope = &array_last(interp->scopes);
    smm index = map_key_index(scope->registers, reg.index);
    if (index != -1) {
        return scope->registers[index].value;
    } else {
        return map_get(interp->declarations, reg);
    }
}

Type*
bc_type_to_type(Bc_Type type) {
    Type* result = 0;
    
    switch (type.kind) {
        case BcTypeKind_s1:  result = &global_primitive_types[PrimitiveTypeKind_s8]; break;
        case BcTypeKind_s8:  result = &global_primitive_types[PrimitiveTypeKind_s8]; break;
        case BcTypeKind_s16: result = &global_primitive_types[PrimitiveTypeKind_s16]; break;
        case BcTypeKind_s32: result = &global_primitive_types[PrimitiveTypeKind_s32]; break;
        case BcTypeKind_s64: result = &global_primitive_types[PrimitiveTypeKind_s64]; break;
        case BcTypeKind_u8:  result = &global_primitive_types[PrimitiveTypeKind_u8]; break;
        case BcTypeKind_u16: result = &global_primitive_types[PrimitiveTypeKind_u16]; break;
        case BcTypeKind_u32: result = &global_primitive_types[PrimitiveTypeKind_u32]; break;
        case BcTypeKind_u64: result = &global_primitive_types[PrimitiveTypeKind_u64]; break;
        case BcTypeKind_f32: result = &global_primitive_types[PrimitiveTypeKind_f32]; break;
        case BcTypeKind_f64: result = &global_primitive_types[PrimitiveTypeKind_f64]; break;
        default: assert(0 && "invalid type"); break;
    }
    
    return result;
}

inline void*
bc_interp_alloc_register(Bc_Interp* interp, Bc_Register reg, Type* type, Value value = {}) {
    assert(type->cached_size > 0 && "bad size");
    assert(type->cached_align > 0 && "bad align");
    
    void* data = arena_push_size(&interp->stack, type->cached_size, type->cached_align);
    
    Bc_Value result;
    result.data = data; // TODO(Alexander): we should store stack offsets
    bc_interp_store_register(interp, reg, result);
    return data;
}

inline void
bc_interp_store_value(Bc_Interp* interp, Bc_Type type, void* data, Bc_Value value) {
    switch (type.kind) {
        case BcTypeKind_s1:   *((s8*) data) = (s8)  value.signed_int; break;
        case BcTypeKind_s8:   *((s8*) data) = (s8)  value.signed_int; break;
        case BcTypeKind_s16: *((s16*) data) = (s16) value.signed_int; break;
        case BcTypeKind_s32: *((s32*) data) = (s32) value.signed_int; break;
        case BcTypeKind_s64: *((s64*) data) = (s64) value.signed_int; break;
        case BcTypeKind_u8:   *((u8*) data) = (u8)  value.unsigned_int; break;
        case BcTypeKind_u16: *((u16*) data) = (u16) value.unsigned_int; break;
        case BcTypeKind_u32: *((u32*) data) = (u32) value.unsigned_int; break;
        case BcTypeKind_u64: *((u64*) data) = (u64) value.unsigned_int; break;
        case BcTypeKind_f32: *((f32*) data) = (f32) value.floating; break;
        case BcTypeKind_f64: *((f64*) data) = (f64) value.floating; break;
        default: assert(0 && "invalid type"); break;
    }
}

inline Bc_Value
bc_interp_load_value(Bc_Interp* interp, Bc_Type type, void* data) {
    Bc_Value result;
    switch (type.kind) {
        case BcTypeKind_s1:  result.signed_int   =  *((s8*) data); break;
        case BcTypeKind_s8:  result.signed_int   =  *((s8*) data); break;
        case BcTypeKind_s16: result.signed_int   = *((s16*) data); break;
        case BcTypeKind_s32: result.signed_int   = *((s32*) data); break;
        case BcTypeKind_s64: result.signed_int   = *((s64*) data); break;
        case BcTypeKind_u8:  result.unsigned_int =  *((u8*) data); break;
        case BcTypeKind_u16: result.unsigned_int = *((u16*) data); break;
        case BcTypeKind_u32: result.unsigned_int = *((u32*) data); break;
        case BcTypeKind_u64: result.unsigned_int = *((u64*) data); break;
        case BcTypeKind_f32: result.floating     = *((f32*) data); break;
        case BcTypeKind_f64: result.floating     = *((f64*) data); break;
        default: result.signed_int = 0; break;
    }
    
    return result;
}

inline Bc_Value
bc_interp_operand_value(Bc_Interp* interp, Bc_Operand* operand) {
    Bc_Value result = {};
    
    switch (operand->kind) {
        case BcOperand_Register: {
            result = bc_interp_load_register(interp, operand->Register);
        } break;
        
        case BcOperand_Value: {
            result.signed_int = operand->Value.signed_int;
        } break;
        
        case BcOperand_Basic_Block: {
            result.basic_block = operand->Basic_Block;
        } break;
        
        default: {
            assert(0 && "operand is not a value");
        } break;
    }
    
    return result;
}

void
bc_interp_instruction(Bc_Interp* interp, Bc_Instruction* bc) {
    
    switch (bc->opcode) {
        case Bytecode_noop: break;
        
        case Bytecode_stack_alloc: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_None);
            
            Type* type = bc_type_to_type(bc->src0.type);
            
            bc_interp_alloc_register(interp, bc->dest.Register, type);
        } break;
        
        case Bytecode_store: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind != BcOperand_None);
            assert(bc->src1.kind == BcOperand_None);
            
            Bc_Value dest = bc_interp_operand_value(interp, &bc->dest);
            Bc_Value src = bc_interp_operand_value(interp, &bc->src0);
            bc_interp_store_value(interp, bc->src0.type, dest.data, src);
        } break;
        
        case Bytecode_load: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_Register);
            
            Type* type = bc_type_to_type(bc->src0.type);
            Bc_Value src = bc_interp_operand_value(interp, &bc->src1);
            Bc_Value value = bc_interp_load_value(interp, bc->src0.type, src.data);
            bc_interp_store_register(interp, bc->dest.Register, value);
        } break;
        
        case Bytecode_neg: {
            Bc_Value first = bc_interp_operand_value(interp, &bc->src0);
            Bc_Value result;
            result.signed_int = -first.signed_int;
            bc_interp_store_register(interp, bc->dest.Register, result); \
        } break;
        
#define BINARY_CASE(opcode, binary_operator) \
case opcode: { \
Bc_Value first = bc_interp_operand_value(interp, &bc->src0); \
Bc_Value second = bc_interp_operand_value(interp, &bc->src1); \
        \
Bc_Value result; \
result.signed_int = first.signed_int binary_operator second.signed_int; \
        \
bc_interp_store_register(interp, bc->dest.Register, result); \
} break;
        
        BINARY_CASE(Bytecode_add, +);
        BINARY_CASE(Bytecode_sub, -);
        BINARY_CASE(Bytecode_div, /);
        BINARY_CASE(Bytecode_mul, *);
        BINARY_CASE(Bytecode_land, &&);
        BINARY_CASE(Bytecode_lor, ||);
        BINARY_CASE(Bytecode_cmpeq, ==);
        BINARY_CASE(Bytecode_cmpneq, !=);
        BINARY_CASE(Bytecode_cmple, <=);
        BINARY_CASE(Bytecode_cmplt, <);
        BINARY_CASE(Bytecode_cmpge, >=);
        BINARY_CASE(Bytecode_cmpgt, >);
#undef BINARY_CASE
        
        case Bytecode_branch: {
            Bc_Value branch;
            if (bc->dest.type.kind == BcTypeKind_s1) {
                // Conditional branch
                Bc_Value cond = bc_interp_operand_value(interp, &bc->dest);
                Bc_Operand* src = (cond.signed_int > 0) ? &bc->src0 : &bc->src1;
                branch = bc_interp_operand_value(interp, src);
            } else {
                // Unconditional branch
                branch = bc_interp_operand_value(interp, &bc->dest);
            }
            
            interp->curr_block = branch.basic_block;
            interp->curr_block_insn = 0;
        } break;
        
        case Bytecode_ret: {
            Bc_Value value = bc_interp_operand_value(interp, &bc->src0);
            pln("%", f_s64(value.signed_int));
        } break;
        
        default: {
            unimplemented;
        } break;
    }
}

void
bc_interp_function(Bc_Interp* interp, string_id ident) {
    // Push scope
    Bc_Interp_Scope new_scope = {};
    array_push(interp->scopes, new_scope);
    
    while (interp->curr_block) {
        while (interp->curr_block_insn++ < interp->curr_block->count) {
            bc_interp_instruction(interp, interp->curr_block->first + interp->curr_block_insn);
        }
        interp->curr_block = interp->curr_block->next;
        interp->curr_block_insn = 0;
    }
    
    // Pop scope
    array_pop(interp->scopes);
}