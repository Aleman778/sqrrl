
struct Bc_Interp_Scope {
    map(u32, Value_Data)* registers; // TODO(Alexander): u32 or Bc_Register
    Bc_Basic_Block* curr_block;
    umm curr_block_insn;
    u32 return_register;
};

struct Bc_Interp {
    array(Bc_Interp_Scope)* scopes;
    Bc_Label_To_Value_Table* declarations;
    
    Memory_Arena stack;
    smm base_pointer;
    Value_Data return_value;
};

inline void
bc_interp_store_register(Bc_Interp* interp, u32 reg, Value_Data value) {
    assert(array_count(interp->scopes) > 0);
    Bc_Interp_Scope* scope = &array_last(interp->scopes);
    map_put(scope->registers, reg, value);
}

inline Value_Data
bc_interp_load_register(Bc_Interp* interp, Bc_Register reg) {
    assert(array_count(interp->scopes) > 0);
    Bc_Interp_Scope* scope = &array_last(interp->scopes);
    smm index = map_get_index(scope->registers, reg.index);
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
    
    Value_Data result;
    result.data = data; // TODO(Alexander): we should store stack offsets
    bc_interp_store_register(interp, reg.index, result);
    return data;
}

inline void
bc_interp_store_value(Bc_Interp* interp, Bc_Type type, void* data, Value_Data value) {
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

inline Value_Data
bc_interp_load_value(Bc_Interp* interp, Bc_Type type, void* data) {
    Value_Data result;
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

inline Value_Data
bc_interp_operand_value(Bc_Interp* interp, Bc_Operand* operand) {
    Value_Data result = {};
    
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
bc_interp_function_call(Bc_Interp* interp, string_id ident, u32 return_register = 0) {
    Bc_Register label = { ident, 0 };
    Bc_Basic_Block* target_block = map_get(interp->declarations, label).basic_block;
    assert(target_block && target_block->label.ident != Kw_invalid);
    
    if (array_count(interp->scopes)) {
        Bc_Interp_Scope* scope = &array_last(interp->scopes);
        scope->return_register = return_register;
    }
    
    // Push new scope and with the arguments on it
    Bc_Interp_Scope new_scope = {};
    new_scope.curr_block = target_block;
    new_scope.curr_block_insn = 0;
    array_push(interp->scopes, new_scope);
}

void
bc_interp_instruction(Bc_Interp* interp, Bc_Instruction* bc) {
    
    switch (bc->opcode) {
        case Bytecode_noop:
        case Bytecode_label: break;
        
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
            
            Value_Data dest = bc_interp_operand_value(interp, &bc->dest);
            Value_Data src = bc_interp_operand_value(interp, &bc->src0);
            bc_interp_store_value(interp, bc->src0.type, dest.data, src);
        } break;
        
        case Bytecode_load: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_Register);
            
            Type* type = bc_type_to_type(bc->src0.type);
            Value_Data src = bc_interp_operand_value(interp, &bc->src1);
            Value_Data value = bc_interp_load_value(interp, bc->src0.type, src.data);
            bc_interp_store_register(interp, bc->dest.Register.index, value);
        } break;
        
        case Bytecode_assign: {
            Value_Data result = bc_interp_operand_value(interp, &bc->src0);
            bc_interp_store_register(interp, bc->dest.Register.index, result); \
        } break;
        
        case Bytecode_neg: {
            Value_Data first = bc_interp_operand_value(interp, &bc->src0);
            Value_Data result;
            result.signed_int = -first.signed_int;
            bc_interp_store_register(interp, bc->dest.Register.index, result); \
        } break;
        
        case Bytecode_not: {
            Value_Data first = bc_interp_operand_value(interp, &bc->src0);
            Value_Data result;
            result.boolean = !first.boolean;
            bc_interp_store_register(interp, bc->dest.Register.index, result); \
        } break;
        
        
#define BINARY_CASE(opcode, binary_operator) \
case opcode: { \
Value_Data first = bc_interp_operand_value(interp, &bc->src0); \
Value_Data second = bc_interp_operand_value(interp, &bc->src1); \
        \
Value_Data result; \
result.signed_int = first.signed_int binary_operator second.signed_int; \
        \
bc_interp_store_register(interp, bc->dest.Register.index, result); \
} break;
        
        BINARY_CASE(Bytecode_add, +);
        BINARY_CASE(Bytecode_sub, -);
        BINARY_CASE(Bytecode_div, /);
        BINARY_CASE(Bytecode_mul, *);
        BINARY_CASE(Bytecode_and, &&);
        BINARY_CASE(Bytecode_or, ||);
        BINARY_CASE(Bytecode_cmpeq, ==);
        BINARY_CASE(Bytecode_cmpneq, !=);
        BINARY_CASE(Bytecode_cmple, <=);
        BINARY_CASE(Bytecode_cmplt, <);
        BINARY_CASE(Bytecode_cmpge, >=);
        BINARY_CASE(Bytecode_cmpgt, >);
#undef BINARY_CASE
        
        case Bytecode_branch: {
            Value_Data branch;
            if (bc->dest.type.kind == BcTypeKind_s1) {
                // Conditional branch
                Value_Data cond = bc_interp_operand_value(interp, &bc->dest);
                Bc_Operand* src = (cond.signed_int > 0) ? &bc->src0 : &bc->src1;
                branch = bc_interp_operand_value(interp, src);
            } else {
                // Unconditional branch
                branch = bc_interp_operand_value(interp, &bc->dest);
            }
            
            Bc_Interp_Scope* scope = &array_last(interp->scopes);
            scope->curr_block = branch.basic_block;
            scope->curr_block_insn = 0;
        } break;
        
        // TODO(Alexander): do we need this we should truncate when storing the value instead
        case Bytecode_truncate: {
            Value_Data result = bc_interp_operand_value(interp, &bc->src0);
            bc_interp_store_register(interp, bc->dest.Register.index, result);
        } break;
        
        case Bytecode_sign_extend: {
            Value_Data value = bc_interp_operand_value(interp, &bc->src0);
            Value_Data result = value;
            
            bool high_bit_set = (value.unsigned_int & 
                                 (1ll << (u64) bc_type_to_bitsize(bc->src0.type.kind))) > 0;
            u64 mask = U64_MAX << (u64) bc_type_to_bitsize(bc->src1.type.kind);
            if (high_bit_set) {
                result.unsigned_int = result.unsigned_int | mask;
            } else {
                result.unsigned_int = result.unsigned_int & ~mask;
            }
            
            bc_interp_store_register(interp, bc->dest.Register.index, result);
        } break;
        
        case Bytecode_zero_extend: {
            Value_Data value = bc_interp_operand_value(interp, &bc->src0);
            Value_Data result = value;
            
            u64 mask = U64_MAX << (u64) bc_type_to_bitsize(bc->src1.type.kind);
            result.unsigned_int = result.unsigned_int & ~mask;
            
            bc_interp_store_register(interp, bc->dest.Register.index, result);
        } break;
        
        case Bytecode_cast_fp_to_sint: {
            unimplemented;
        } break;
        
        case Bytecode_cast_fp_to_uint: {
            unimplemented;
        } break;
        
        case Bytecode_cast_sint_to_fp: {
            unimplemented;
        } break;
        
        case Bytecode_cast_uint_to_fp: {
            unimplemented;
        } break;
        
        case Bytecode_fp_extend: {
            unimplemented;
        } break;
        
        case Bytecode_fp_truncate: {
            unimplemented;
        } break;
        
        case Bytecode_call: {
            bc_interp_function_call(interp, bc->src0.Register.ident, bc->dest.Register.index);
            
            Bc_Interp_Scope* scope = &array_last(interp->scopes);
            array(u32)* arg_registers = scope->curr_block->args;
            for_array(bc->src1.Argument_List, arg, arg_index) {
                Value_Data arg_data = bc_interp_operand_value(interp, arg);
                bc_interp_store_register(interp, arg_registers[arg_index], arg_data);
            }
            
        } break;
        
        case Bytecode_ret: {
            Value_Data value = bc_interp_operand_value(interp, &bc->src0);
            switch (bc->src0.type.kind) {
                case BcTypeKind_s1:  value.unsigned_int = (u64) 1 & value.unsigned_int; break;
                case BcTypeKind_s8:  value.signed_int = (s8) value.signed_int; break;
                case BcTypeKind_s16: value.signed_int = (s16) value.signed_int; break;
                case BcTypeKind_s32: value.signed_int = (s32) value.signed_int; break;
                case BcTypeKind_u8:  value.unsigned_int = (u8) value.unsigned_int; break;
                case BcTypeKind_u16: value.unsigned_int = (u16) value.unsigned_int; break;
                case BcTypeKind_u32: value.unsigned_int = (u32) value.unsigned_int; break;
            }
            
            // Pop scope
            array_pop(interp->scopes);
            
            // Store value in specific return register, specified by the caller instruction
            if (array_count(interp->scopes) == 0) {
                interp->return_value = value;
            } else  {
                Bc_Interp_Scope* scope = &array_last(interp->scopes);
                if (scope->return_register != 0) {
                    bc_interp_store_register(interp, scope->return_register, value);
                    scope->return_register = 0;
                }
            }
        } break;
        
        default: {
            unimplemented;
        } break;
    }
}

Value_Data
bc_interp_bytecode(Bc_Interp* interp, string_id entry_point = Sym_main) {
    bc_interp_function_call(interp, entry_point);
    
    for (;;) {
        if (array_count(interp->scopes) == 0) {
            break;
        }
        
        Bc_Interp_Scope* scope = &array_last(interp->scopes);
        if (!scope->curr_block) {
            array_pop(interp->scopes);
            continue;
        }
        
        if (scope->curr_block_insn >= scope->curr_block->count) {
            scope->curr_block = scope->curr_block->next;
            scope->curr_block_insn = 0;
            continue;
        }
        
        Bc_Instruction* insn = scope->curr_block->first + scope->curr_block_insn++;
        bc_interp_instruction(interp, insn);
    }
    
    return interp->return_value;
}