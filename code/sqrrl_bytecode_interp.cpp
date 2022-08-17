
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
bc_interp_alloc_register(Bc_Interp* interp, Bc_Register reg, Bc_Type type) {
    s32 size = bc_type_to_size(type);
    s32 align = bc_type_to_align(type);
    
    void* data = arena_push_size(&interp->stack, size, align);
    Value_Data value;
    value.data = data;
    bc_interp_store_register(interp, reg, value);
    return data;
}

void
bc_interp_store_value(Bc_Interp* interp, Bc_Type type, void* dest, Value_Data src) {
    if (type.ptr_depth > 0) {
        *((umm*) dest) = (umm) src.data;
        return;
    }
    
    switch (type.kind) {
        case BcType_s1:
        case BcType_s8:   *((s8*) dest) = (s8)  src.signed_int; break;
        case BcType_s16: *((s16*) dest) = (s16) src.signed_int; break;
        case BcType_s32: *((s32*) dest) = (s32) src.signed_int; break;
        case BcType_s64: *((s64*) dest) = (s64) src.signed_int; break;
        case BcType_u8:   *((u8*) dest) = (u8)  src.unsigned_int; break;
        case BcType_u16: *((u16*) dest) = (u16) src.unsigned_int; break;
        case BcType_u32: *((u32*) dest) = (u32) src.unsigned_int; break;
        case BcType_u64: *((u64*) dest) = (u64) src.unsigned_int; break;
        case BcType_f32: *((f32*) dest) = (f32) src.floating; break;
        case BcType_f64: *((f64*) dest) = (f64) src.floating; break;
        
        case BcType_Aggregate: {
            assert(type.aggregate);
            
            switch (type.aggregate->kind) {
                case Type_String: {
                    *((string*) dest) = src.str;
                } break;
                
                default: {
                    unimplemented;
                } break;
            }
            
        } break;
        
        default: assert(0 && "invalid type"); break;
    }
}

Value_Data
bc_interp_load_value(Bc_Interp* interp, Bc_Type type, void* data) {
    Value_Data result;
    if (type.ptr_depth > 0) {
        result.unsigned_int = *((umm*) data);
        return result;
    }
    
    switch (type.kind) {
        case BcType_s1:  result.signed_int   =  *((s8*) data); break;
        case BcType_s8:  result.signed_int   =  *((s8*) data); break;
        case BcType_s16: result.signed_int   = *((s16*) data); break;
        case BcType_s32: result.signed_int   = *((s32*) data); break;
        case BcType_s64: result.signed_int   = *((s64*) data); break;
        case BcType_u8:  result.unsigned_int =  *((u8*) data); break;
        case BcType_u16: result.unsigned_int = *((u16*) data); break;
        case BcType_u32: result.unsigned_int = *((u32*) data); break;
        case BcType_u64: result.unsigned_int = *((u64*) data); break;
        case BcType_f32: result.floating     = *((f32*) data); break;
        case BcType_f64: result.floating     = *((f64*) data); break;
        default: result.signed_int = 0; break;
    }
    
    return result;
}

inline Value_Data
bc_interp_operand_value(Bc_Interp* interp, Bc_Operand* operand) {
    Value_Data result = {};
    
    switch (operand->kind) {
        case BcOperand_Register:
        case BcOperand_Memory:
        case BcOperand_Stack: {
            result = bc_interp_load_register(interp, operand->Register);
        } break;
        
        case BcOperand_Int: {
            result.signed_int = operand->Signed_Int;
        } break;
        
        case BcOperand_Float: {
            result.floating = operand->Float;
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

// TODO(Alexander): this is very silly to go from Type* -> Bc_Type -> Type*
Type*
bc_type_to_type(Bc_Type type) {
    Type* result = 0;
    
    switch (type.kind) {
        case BcType_s1:  result = &global_primitive_types[PrimitiveType_s8]; break;
        case BcType_s8:  result = &global_primitive_types[PrimitiveType_s8]; break;
        case BcType_s16: result = &global_primitive_types[PrimitiveType_s16]; break;
        case BcType_s32: result = &global_primitive_types[PrimitiveType_s32]; break;
        case BcType_s64: result = &global_primitive_types[PrimitiveType_s64]; break;
        case BcType_u8:  result = &global_primitive_types[PrimitiveType_u8]; break;
        case BcType_u16: result = &global_primitive_types[PrimitiveType_u16]; break;
        case BcType_u32: result = &global_primitive_types[PrimitiveType_u32]; break;
        case BcType_u64: result = &global_primitive_types[PrimitiveType_u64]; break;
        case BcType_f32: result = &global_primitive_types[PrimitiveType_f32]; break;
        case BcType_f64: result = &global_primitive_types[PrimitiveType_f64]; break;
        case BcType_Aggregate: result = type.aggregate; break;
        default: assert(0 && "bug: provided type is not valid"); break;
    }
    
    return result;
}

void
bc_interp_instruction(Bc_Interp* interp, Bc_Instruction* bc) {
    
    switch (bc->opcode) {
        case Bytecode_noop:
        case Bytecode_label: break;
        
        case Bytecode_stack_alloc: {
            assert(bc->dest.kind == BcOperand_Stack);
            assert(bc->src0.kind == BcOperand_Type);
            assert(bc->src1.kind == BcOperand_None);
            
            bc_interp_alloc_register(interp, bc->dest.Register, bc->src0.Type);
        } break;
        
        case Bytecode_memory_alloc: {
            assert(bc->dest.kind == BcOperand_Stack);
            assert(bc->src0.kind == BcOperand_Type);
            assert(is_bc_operand_value(bc->src1.kind));
            
            void* data = bc_interp_alloc_register(interp, bc->dest.Register, bc->src0.Type);
            bc_interp_store_value(interp, bc->src0.Type, data, bc->src1.Const);
        } break;
        
        case Bytecode_copy: {
            Value_Data result = bc_interp_operand_value(interp, &bc->src0);
            bc_interp_store_register(interp, bc->dest.Register, result); \
        } break;
        
        case Bytecode_copy_from_ref: {
            unimplemented;
        } break;
        
        case Bytecode_copy_from_deref: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind == BcOperand_Stack || bc->src0.kind == BcOperand_Memory);
            assert(bc->src1.kind == BcOperand_None);
            
            Value_Data src = bc_interp_operand_value(interp, &bc->src0);
            Value_Data value = bc_interp_load_value(interp, bc->dest_type, src.data);
            bc_interp_store_register(interp, bc->dest.Register, value);
        } break;
        
        case Bytecode_copy_to_deref: {
            assert(bc->dest.kind == BcOperand_Stack || bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind != BcOperand_None);
            assert(bc->src1.kind == BcOperand_None);
            
            Value_Data dest = bc_interp_operand_value(interp, &bc->dest);
            Value_Data src = bc_interp_operand_value(interp, &bc->src0);
            bc_interp_store_value(interp, bc->dest_type, dest.data, src);
        } break;
        
        case Bytecode_neg: {
            Value_Data first = bc_interp_operand_value(interp, &bc->src0);
            Value_Data result;
            result.signed_int = -first.signed_int;
            bc_interp_store_register(interp, bc->dest.Register, result); \
        } break;
        
        case Bytecode_not: {
            Value_Data first = bc_interp_operand_value(interp, &bc->src0);
            Value_Data result;
            result.boolean = !first.boolean;
            bc_interp_store_register(interp, bc->dest.Register, result); \
        } break;
        
        //pln("% = % % %", f_s64(result.signed_int), f_s64(first.signed_int), f_cstring(#binary_operator), f_s64(second.signed_int));
        
#define BINARY_CASE(opcode, binary_operator) \
case opcode: { \
Value_Data first = bc_interp_operand_value(interp, &bc->src0); \
Value_Data second = bc_interp_operand_value(interp, &bc->src1); \
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
            Value_Data cond = bc_interp_operand_value(interp, &bc->dest);
            Bc_Operand* src = (cond.signed_int > 0) ? &bc->src0 : &bc->src1;
            Bc_Label target_label = src->Label;
            
            Bc_Decl* target = &map_get(interp->declarations, target_label);
            assert(target && target->kind == BcDecl_Basic_Block);
            
            Bc_Interp_Scope* scope = &array_last(interp->scopes);
            scope->curr_block = get_bc_basic_block(interp->code, target->first_byte_offset);
            scope->curr_block_insn = 0;
        } break;
        
        case Bytecode_truncate: {
            // NOTE(Alexander): we need to store and load the value to truncate the Value_Data
            Value_Data src = bc_interp_operand_value(interp, &bc->src0);
            Value_Data result = bc_interp_load_value(interp, bc->dest_type, &src.signed_int);
            bc_interp_store_register(interp, bc->dest.Register, result);
        } break;
        
        case Bytecode_sign_extend: {
            assert(bc->dest.kind == BcOperand_Register);
            assert(bc->src0.kind != BcOperand_None);
            assert(bc->src1.kind == BcOperand_Type);
            
            Value_Data value = bc_interp_operand_value(interp, &bc->src0);
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
            Value_Data value = bc_interp_operand_value(interp, &bc->src0);
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
            assert(bc->src0.Type.kind == BcType_Aggregate);
            
            
            Type* type = bc->src0.Type.aggregate;
            assert(type->kind == Type_Function);
            
            Type_Table* arg_types = &type->Function.arguments;
            if (type->Function.interp_intrinsic) {
                Interp intrinsic_interp = {};
                array(Interp_Value)* variadic_args = 0;
                
                for_array(bc->src1.Argument_List, arg, arg_index) {
                    
                    Value_Data arg_value = bc_interp_operand_value(interp, &arg->src);
                    
                    if (arg_index < array_count(arg_types->idents)) {
                        // Normal argument
                        string_id ident = arg_types->idents[arg_index];
                        Type* arg_type = map_get(arg_types->ident_to_type, ident);
                        
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
                        
                        Type* arg_type = bc_type_to_type(arg->type); // Improve our type system
                        if (arg_type) {
                            interp_value.type = *arg_type;
                            array_push(variadic_args, interp_value);
                        } else {
                            assert(0 && "argument type was not found");
                        }
                    }
                }
                
                //type->Function.interp_intrinsic(&intrinsic_interp, variadic_args);
                
            } else {
                Bc_Basic_Block* target_block =
                    bc_interp_function_call(interp, type->Function.ident, bc->dest.Register);
                
                Bc_Instruction* label_insn = get_first_bc_instruction(target_block);
                assert(label_insn->src1.kind == BcOperand_Argument_List);
                array(Bc_Argument*) formal_args = label_insn->src1.Argument_List;
                
                Bc_Interp_Scope* scope = &array_last(interp->scopes);
                for_array(bc->src1.Argument_List, arg, arg_index) {
                    Bc_Argument* formal_arg = formal_args + arg_index;
                    
                    if (arg_index < array_count(arg_types->idents)) {
                        string_id ident = arg_types->idents[arg_index];
                        Type* arg_type = map_get(arg_types->ident_to_type, ident);
                        
                        Value_Data arg_data = bc_interp_operand_value(interp, &arg->src);
                        Bc_Register reg = formal_arg->src.Register;
                        bc_interp_store_register(interp, reg, arg_data);
                    } else {
                        // Variadic argument
                        unimplemented;
                    }
                }
            }
            
        } break;
        
        case Bytecode_ret: {
            Value_Data value = {};
            if (bc->dest.kind == BcOperand_Register) {
                value = bc_interp_operand_value(interp, &bc->dest);
                switch (bc->dest_type.kind) {
                    case BcType_s1:  value.unsigned_int = (u64) 1 & value.unsigned_int; break;
                    case BcType_s8:  value.signed_int = (s8) value.signed_int; break;
                    case BcType_s16: value.signed_int = (s16) value.signed_int; break;
                    case BcType_s32: value.signed_int = (s32) value.signed_int; break;
                    case BcType_u8:  value.unsigned_int = (u8) value.unsigned_int; break;
                    case BcType_u16: value.unsigned_int = (u16) value.unsigned_int; break;
                    case BcType_u32: value.unsigned_int = (u32) value.unsigned_int; break;
                }
            }
            
            // Pop scope
            array_pop(interp->scopes);
            
            if (array_count(interp->scopes) == 0) {
                interp->return_value = value;
            } else  {
                Bc_Interp_Scope* scope = &array_last(interp->scopes);
                assert(scope->return_register);
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