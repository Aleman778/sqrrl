
inline void
bc_interp_store_register(Bc_Interp* interp, Bc_Register reg, Value_Data value) {
    assert(array_count(interp->scopes) > 0);
    Bc_Interp_Scope* scope = &array_last(interp->scopes);
    map_put(scope->registers, reg, value);
}

inline Value_Data
bc_interp_load_register(Bc_Interp* interp, Bc_Register reg) {
    assert(array_count(interp->scopes) > 0);
    Bc_Interp_Scope* scope = &array_last(interp->scopes);
    smm index = map_get_index(scope->registers, reg);
    if (index != -1) {
        return scope->registers[index].value;
    } else {
        // Try global scope which is the first one
        scope = array_first(interp->scopes);
        index = map_get_index(scope->registers, reg);
        if (index != -1) {
            return scope->registers[index].value;
        } else {
            Bc_Decl* decl = &map_get(interp->declarations, reg);
            assert(decl && decl->kind == BcDecl_Data);
            return decl->Data.value;
        }
    }
}

inline void*
bc_stack_allocate_type(Bc_Interp* interp, Bc_Register reg, Bc_Type type) {
    s32 size = bc_type_to_size(type);
    s32 align = bc_type_to_align(type);
    
    void* data = arena_push_size(&interp->stack, size, align);
    Value_Data value;
    value.data = data;
    bc_interp_store_register(interp, reg, value);
    return data;
}

inline Value_Data
bc_interp_operand_value(Bc_Interp* interp, Bc_Operand operand, Bc_Type value_type) {
    Value_Data result = {};
    
    switch (operand.kind) {
        case BcOperand_Memory:
        case BcOperand_Stack: {
            Value_Data reg = bc_interp_load_register(interp, operand.Register);
            result = value_load_from_memory(value_type, reg.data).data;
        } break;
        
        case BcOperand_Register: {
            result = bc_interp_load_register(interp, operand.Register);
        } break;
        
        case BcOperand_Int: {
            result.signed_int = operand.Signed_Int;
        } break;
        
        case BcOperand_Float: {
            result.floating = operand.Float;
        } break;
        
        case BcOperand_Label: {
            Bc_Decl decl = map_get(interp->declarations, operand.Label);
            result = decl.Data.value;
        } break;
        
        default: {
            assert(0 && "operand is not a value");
        } break;
    }
    
    return result;
}

Bc_Basic_Block*
bc_interp_function_call(Bc_Interp* interp, string_id ident, Bc_Register return_register={}) {
    Bc_Label label = { ident, 0 }; // TODO(Alexander): might not always be 0
    Bc_Decl* decl = &map_get(interp->declarations, label);
    assert(decl && decl->kind == BcDecl_Procedure);
    
    Bc_Basic_Block* target_block = get_bc_basic_block(interp->code, decl->first_byte_offset);
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
    
    return target_block;
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
            
            bc_stack_allocate_type(interp, bc->dest.Register, bc->src0.Type);
        } break;
        
        case Bytecode_memory_alloc: {
            assert(bc->dest.kind == BcOperand_Stack);
            assert(bc->src0.kind == BcOperand_Type);
            assert(is_bc_operand_value(bc->src1.kind));
            
            // TODO(Alexander): should allocate on stack here too?
            void* data = bc_stack_allocate_type(interp, bc->dest.Register, bc->src0.Type);
            value_store_in_memory(bc->src0.Type, data, bc->src1.Const);
        } break;
        
        case Bytecode_copy: {
            Value_Data src = bc_interp_operand_value(interp, bc->src0, bc->dest_type);
            if (bc->dest.kind == BcOperand_Memory || bc->dest.kind == BcOperand_Stack) {
                Value_Data dest = bc_interp_load_register(interp, bc->dest.Register);
                value_store_in_memory(bc->dest_type, dest.data, src);
            } else {
                bc_interp_store_register(interp, bc->dest.Register, src);
            }
        } break;
        
        case Bytecode_copy_from_ref: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Stack || bc->src0.kind == BcOperand_Memory);
            assert(bc->src1.kind == BcOperand_None);
            
            Value_Data src = bc_interp_load_register(interp, bc->src0.Register);
            bc_interp_store_register(interp, bc->dest.Register, src);
        } break;
        
        case Bytecode_copy_from_deref: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Stack || bc->src0.kind == BcOperand_Memory);
            assert(bc->src1.kind == BcOperand_None);
            
            Value_Data src = bc_interp_operand_value(interp, bc->src0, bc->dest_type);
            bc_interp_store_register(interp, bc->dest.Register, src);
        } break;
        
        case Bytecode_copy_to_deref: {
            assert(bc->dest.kind == BcOperand_Stack || bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind != BcOperand_None);
            assert(bc->src1.kind == BcOperand_None);
            
            Bc_Type ptr_type = bc->dest_type;
            //ptr_type.ptr_depth++;
            
            Value_Data dest_reg = bc_interp_operand_value(interp, bc->dest, ptr_type);
            Value_Data src = bc_interp_operand_value(interp, bc->src0, bc->dest_type);
            value_store_in_memory(bc->dest_type, dest_reg.data, src);
        } break;
        
        case Bytecode_neg: {
            Value_Data first = bc_interp_operand_value(interp, bc->src0, bc->dest_type);
            Value_Data result;
            result.signed_int = -first.signed_int;
            bc_interp_store_register(interp, bc->dest.Register, result); \
        } break;
        
        case Bytecode_not: {
            Value_Data first = bc_interp_operand_value(interp, bc->src0, bc->dest_type);
            Value_Data result;
            result.boolean = !first.boolean;
            bc_interp_store_register(interp, bc->dest.Register, result); \
        } break;
        
        //pln("% = % % %", f_s64(result.signed_int), f_s64(first.signed_int), f_cstring(#binary_operator), f_s64(second.signed_int));
        
#define BINARY_CASE(opcode, binary_operator) \
case opcode: { \
Value_Data first = bc_interp_operand_value(interp, bc->src0, bc->dest_type); \
Value_Data second = bc_interp_operand_value(interp, bc->src1, bc->dest_type); \
        \
Value_Data result; \
result.signed_int = first.signed_int binary_operator second.signed_int; \
bc_interp_store_register(interp, bc->dest.Register, result); \
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
        
        
        case Bytecode_goto: {
            Bc_Label target_label = bc->dest.Label;
            Bc_Decl* target = &map_get(interp->declarations, target_label);
            assert(target && target->kind == BcDecl_Basic_Block);
            
            Bc_Interp_Scope* scope = &array_last(interp->scopes);
            scope->curr_block = get_bc_basic_block(interp->code, target->first_byte_offset);
            scope->curr_block_insn = 0;
        } break;
        
        case Bytecode_branch: {
            Value_Data cond = bc_interp_operand_value(interp, bc->dest, t_bool);
            Bc_Operand* src = (cond.signed_int > 0) ? &bc->src0 : &bc->src1;
            Bc_Label target_label = src->Label;
            
            Bc_Decl* target = &map_get(interp->declarations, target_label);
            assert(target && target->kind == BcDecl_Basic_Block);
            
            Bc_Interp_Scope* scope = &array_last(interp->scopes);
            scope->curr_block = get_bc_basic_block(interp->code, target->first_byte_offset);
            scope->curr_block_insn = 0;
        } break;
        
        case Bytecode_truncate: {
            Value_Data src = bc_interp_operand_value(interp, bc->src0, bc->dest_type);
            Value_Data result = value_cast_from_same_type(src, bc->dest_type->Basic.kind).data;
            bc_interp_store_register(interp, bc->dest.Register, result);
        } break;
        
        case Bytecode_sign_extend: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind != BcOperand_None);
            assert(bc->src1.kind == BcOperand_Type);
            
            Bc_Type src_type = bc->src1.Type;
            Value_Data value = bc_interp_operand_value(interp, bc->src0, src_type);
            Value_Data result = value;
            
            u64 dest_bits = bc_type_to_bitsize(bc->dest_type);
            if (dest_bits < 64) {
                u64 highest_bit_mask = (1ll << (u64) (bc_type_to_bitsize(bc->src1.Type) - 1));
                bool high_bit_set = (value.unsigned_int & highest_bit_mask) > 0;
                u64 mask = U64_MAX << dest_bits;
                if (high_bit_set) {
                    result.unsigned_int = result.unsigned_int | mask;
                } else {
                    result.unsigned_int = result.unsigned_int & ~mask;
                }
            }
            
            bc_interp_store_register(interp, bc->dest.Register, result);
        } break;
        
        case Bytecode_zero_extend: {
            Value_Data value = bc_interp_operand_value(interp, bc->src0, bc->dest_type);
            Value_Data result = value;
            
            u64 dest_bits = bc_type_to_bitsize(bc->dest_type);
            if (dest_bits < 64) {
                u64 mask = U64_MAX << dest_bits;
                result.unsigned_int = result.unsigned_int & ~mask;
            }
            
            bc_interp_store_register(interp, bc->dest.Register, result);
        } break;
        
        case Bytecode_float_to_sint: {
            unimplemented;
        } break;
        
        case Bytecode_float_to_uint: {
            unimplemented;
        } break;
        
        case Bytecode_sint_to_float: {
            unimplemented;
        } break;
        
        case Bytecode_uint_to_float: {
            unimplemented;
        } break;
        
        case Bytecode_float_extend: {
            unimplemented;
        } break;
        
        case Bytecode_float_truncate: {
            unimplemented;
        } break;
        
        case Bytecode_call: {
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_Argument_List);
            assert(bc->src0.Type->kind == TypeKind_Function);
            
            Type* type = bc->src0.Type;
            Type_Function* t_func = &type->Function;
            
            if (type->Function.interp_intrinsic) {
                Interp intrinsic_interp = {};
                array(Interp_Value)* variadic_args = 0;
                
                for_array(bc->src1.Argument_List, arg, arg_index) {
                    
                    Value_Data arg_value = bc_interp_operand_value(interp, arg->src, arg->type);
                    
                    if (arg_index < array_count(t_func->arg_idents)) {
                        // Normal argument
                        string_id ident = t_func->arg_idents[arg_index];
                        Type* arg_type = t_func->arg_types[arg_index];
                        
                        if (arg_type) {
                            void* data = interp_push_value(&intrinsic_interp, arg_type, arg_value);
                            interp_push_entity_to_current_scope(&intrinsic_interp, ident, data, arg_type);
                        } else {
                            assert(0 && "argument type was not found");
                        }
                        
                    } else {
                        // Variadic argument
                        Interp_Value interp_value = {};
                        interp_value.value.data = arg_value;
                        
                        Type* arg_type = arg->type;
                        if (arg_type) {
                            interp_value.type = *arg_type;
                            array_push(variadic_args, interp_value);
                        } else {
                            assert(0 && "argument type was not found");
                        }
                    }
                }
                
                type->Function.interp_intrinsic(&intrinsic_interp, variadic_args);
                
            } else {
                Bc_Basic_Block* target_block =
                    bc_interp_function_call(interp, type->Function.ident, bc->dest.Register);
                
                Bc_Instruction* label_insn = get_first_bc_instruction(target_block);
                assert(label_insn->src1.kind == BcOperand_Argument_List);
                array(Bc_Argument)* formal_args = label_insn->src1.Argument_List;
                
                Bc_Interp_Scope* scope = &array_last(interp->scopes);
                for_array(bc->src1.Argument_List, arg, arg_index) {
                    Value_Data arg_data = bc_interp_operand_value(interp, arg->src, arg->type);
                    
                    Bc_Argument* formal_arg = formal_args + arg_index;
                    Bc_Register reg = formal_arg->src.Register;
                    bc_interp_store_register(interp, reg, arg_data);
                }
            }
            
        } break;
        
        case Bytecode_ret: {
            Value_Data value = {};
            if (bc->dest.kind != BcOperand_None) {
                value = bc_interp_operand_value(interp, bc->dest, bc->dest_type);
                value = value_load_from_memory(bc->dest_type, &value.signed_int).data;
                
#if 0 // TODO(Alexander): don't think we need this!
                switch (bc->dest_type.kind) {
                    case BcType_s1:  value.unsigned_int = (u64) 1 & value.unsigned_int; break;
                    case BcType_s8:  value.signed_int = (s8) value.signed_int; break;
                    case BcType_s16: value.signed_int = (s16) value.signed_int; break;
                    case BcType_s32: value.signed_int = (s32) value.signed_int; break;
                    case BcType_u8:  value.unsigned_int = (u8) value.unsigned_int; break;
                    case BcType_u16: value.unsigned_int = (u16) value.unsigned_int; break;
                    case BcType_u32: value.unsigned_int = (u32) value.unsigned_int; break;
                }
#endif
            }
            
            // Pop scope
            array_pop(interp->scopes);
            
            if (array_count(interp->scopes) == 0) {
                interp->return_value = value;
            } else  {
                Bc_Interp_Scope* scope = &array_last(interp->scopes);
                //assert(scope->return_register);
                if (scope->return_register != 0) {
                    bc_interp_store_register(interp, scope->return_register, value);
                    scope->return_register = 0;
                }
            }
        } break;
        
        default: {
            //unimplemented;
        } break;
    }
}

Value_Data
bc_interp_bytecode(Bc_Interp* interp, string_id entry_point) {
    // TODO(Alexander): with data declarations do we still need this?
#if 0
    // First analyse the globals
    bc_interp_function_call(interp, Kw_global);
    
    for (;;) {
        Bc_Interp_Scope* scope = &array_last(interp->scopes);
        if (!scope->curr_block) {
            break;
        }
        
        if (scope->curr_block_insn >= scope->curr_block->count) {
            scope->curr_block = scope->curr_block->next;
            scope->curr_block_insn = 0;
            continue;
        }
        
        Bc_Instruction* insn = scope->curr_block->first + scope->curr_block_insn++;
        bc_interp_instruction(interp, insn);
    }
#endif
    
    // Run the entry point
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
        
        if (scope->curr_block_insn >= scope->curr_block->instruction_count) {
            scope->curr_block = get_bc_basic_block(interp->code, scope->curr_block->next_byte_offset);
            scope->curr_block_insn = 0;
            continue;
        }
        
        Bc_Instruction* insn = get_first_bc_instruction(scope->curr_block) + scope->curr_block_insn++;
        bc_interp_instruction(interp, insn);
    }
    
    return interp->return_value;
}