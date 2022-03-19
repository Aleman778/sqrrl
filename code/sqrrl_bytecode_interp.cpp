

Type*
bc_type_to_type(Bc_Type type) {
    Type* result = 0;
    
    switch (type.kind) {
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
    }
    
    assert(result && "invalid bc type");
    
    return result;
}

inline void*
interp_bc_alloc_register(Interp* interp, Bc_Register reg, Type* type, Value value = {}) {
    void* data = interp_push_value(interp, type, value);
    interp_push_entity_to_current_scope(interp, reg.index, data, type);
    return data;
}

inline void
interp_bc_save_register(Interp* interp, Bc_Register reg, Value value, Type* type = 0) {
    Interp_Entity entity = interp_load_entity_from_current_scope(interp, reg.index);
    if (entity.is_valid) {
        interp_save_value(interp, entity.type, entity.data, value);
    } else {
        assert(type);
        interp_bc_alloc_register(interp, reg, type, value);
    }
    
}

inline Value
interp_bc_load_register(Interp* interp, Bc_Register reg) {
    Interp_Value result = interp_load_value(interp, reg.index);
    return result.value;
}

inline Value
interp_bc_operand(Interp* interp, Bc_Operand* operand) {
    Value result = {};
    
    switch (operand->kind) {
        case BcOperand_Register: {
            result = interp_bc_load_register(interp, operand->Register);
        } break;
        
        case BcOperand_Const: {
            result = operand->Const.value;
        } break;
    }
    
    assert(result.type != Value_void);
    return result;
}

void
interp_bc_instruction(Interp* interp, Bc_Instruction* bc) {
    
    switch (bc->opcode) {
        case Bytecode_stack_alloc: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_None);
            
            Type* type = bc_type_to_type(bc->src0.type);
            
            interp_bc_alloc_register(interp, bc->dest.Register, type);
        } break;
        
        case Bytecode_store: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind != BcOperand_None);
            assert(bc->src1.kind == BcOperand_None);
            
            
            Value new_value = interp_bc_operand(interp, &bc->src0);
            interp_bc_save_register(interp, bc->dest.Register, new_value);
        } break;
        
        case Bytecode_load: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_Register);
            
            Type* type = bc_type_to_type(bc->src0.type);
            Value value = interp_bc_load_register(interp, bc->src1.Register);
            interp_bc_save_register(interp, bc->dest.Register, value, type);
        } break;
        
        case Bytecode_add: {
            Value lhs = interp_bc_operand(interp, &bc->src0);
            Value rhs = interp_bc_operand(interp, &bc->src1);
            
            Value result;
            result.type = lhs.type;
            result.signed_int = lhs.signed_int + rhs.signed_int;
            
            Type* type = bc_type_to_type(bc->src0.type);
            interp_bc_save_register(interp, bc->dest.Register, result, type);
        } break;
        
        case Bytecode_ret: {
            Value value = interp_bc_load_register(interp, bc->src0.Register);
            print_value(&value);
        } break;
    }
}

void
interp_bc_basic_block(Interp* interp, Bc_Basic_Block* block) {
    
    for (umm index = 0; index < block->count; index++) {
        interp_bc_instruction(interp, block->first + index);
    }
    
}