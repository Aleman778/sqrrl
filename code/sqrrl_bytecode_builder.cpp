
Bc_Local
emit_reference_expression2(Bytecode_Builder* bc, Ast* expr) {
    
    Bc_Local result = {};
    
    if (expr->kind == Ast_Ident) {
        string_id ident = ast_unwrap_ident(expr);
        result = map_get(bc->locals, ident);
    }
    
    
    if (result.ptr != -1 && result.val != -1) {
        result.ptr = emit_reference_expression(bc, expr);
    }
    
    return result;
}


int
emit_reference_expression(Bytecode_Builder* bc, Ast* expr) {
    int result = -1;
    
    switch (expr->kind) {
        case Ast_Ident: {
            Type* type = expr->type;
            string_id ident = ast_unwrap_ident(expr);
            smm local_index = map_get_index(bc->locals, ident); 
            if (local_index != -1) {
                Bc_Local local = bc->locals[local_index].value;
                if (local.ptr == -1) { 
                    local.ptr = bc_local(bc, expr->type);
                    bc_store(bc, local.ptr, local.val);
                    bc->locals[local_index].value = local;
                }
                result = local.ptr;
                
            } else if (type->kind == TypeKind_Function && type->Function.unit) {
                Bytecode_Function* func = type->Function.unit->bytecode_function;
                result = add_bytecode_register(bc, t_void_ptr);
                bc_function(bc, result, func->type_index);
                
            } else {
                smm global_index = map_get_index(bc->globals, ident);
                if (global_index != -1) {
                    int global_id = bc->globals[global_index].value;
                    result = add_bytecode_register(bc, t_void_ptr);
                    bc_global(bc, result, global_id);
                    
                } else {
                    verify_not_reached();
                }
            }
        } break;
        
        case Ast_Unary_Expr: {
            switch (expr->Unary_Expr.op) {
                case Op_Dereference: {
                    result = emit_value_fetch_expression(bc, expr->Unary_Expr.first);
                } break;
                
                case Op_Pre_Increment:
                case Op_Pre_Decrement: {
                    unimplemented;
                    //result = emit_reference_expression(bc, expr);
                    //bc_load(bc, _result, src);
                    
                    
                } break;
                
                default: verify_not_reached();
            }
        } break;
        
        case Ast_Cast_Expr: {
            Type* t_dest = normalize_type_for_casting(expr->type);
            Type* t_src = normalize_type_for_casting(expr->Cast_Expr.expr->type);
            
            if (t_dest->kind == TypeKind_Array && t_src->kind == TypeKind_Array) {
                result = bc_local(bc, t_dest);
                emit_array_type_cast(bc, t_dest, t_src, expr->Cast_Expr.expr, result);
            } else {
                unimplemented;
                //result = add_bytecode_register(bc, expr->type);
                //emit_type_cast(bc, expr, result);
            }
        } break;
        
        case Ast_Paren_Expr: {
            result = emit_reference_expression(bc, expr->Paren_Expr.expr);
        } break;
        
        case Ast_Field_Expr: {
            Type* type = expr->Field_Expr.var->type;
            result = emit_reference_expression(bc, expr->Field_Expr.var);
            string_id ident = ast_unwrap_ident(expr->Field_Expr.field);
            
            if (type->kind == TypeKind_Pointer) {
                type = type->Pointer;
                int tmp = add_bytecode_register(bc);
                result = bc_load(bc, type, tmp, result);
            }
            
            switch (type->kind) {
                case TypeKind_Struct:
                case TypeKind_Union: {
                    Struct_Field_Info field = get_field_info(&type->Struct_Like, ident);
                    
                    if (field.offset != 0) {
                        int tmp = add_bytecode_register(bc, t_void_ptr);
                        result = bc_field_access(bc, tmp, result, (s32) field.offset);
                    }
                } break;
                
                case TypeKind_Array:
                case TypeKind_Basic: {
                    assert((type->kind == TypeKind_Array ||
                            type->Basic.kind == Basic_string) && "unsupported type");
                    
                    if (ident == Sym_data) {
                        if (!(type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace)) {
                            int tmp = add_bytecode_register(bc, type);
                            result = bc_field_access(bc, tmp, result, 0);
                        } else {
                            //bc->curr_function->register_types[result].flags |= BC_FLAG_RVALUE;
                        }
                        
                    } else if (ident == Sym_count) {
                        int tmp = add_bytecode_register(bc, type);
                        result = bc_field_access(bc, tmp, result, 8);
                    } else {
                        unimplemented;
                    }
                } break;
                
                default: unimplemented;
            }
        } break;
        
        case Ast_Index_Expr: {
            Type* array_type = expr->Index_Expr.array->type;
            
            Type* type;
            if (array_type->kind == TypeKind_Array) {
                type = array_type->Array.type;
                
            } else if (array_type->kind == TypeKind_Pointer) {
                type = array_type->Pointer;
                
            } else if (array_type->kind == TypeKind_Basic &&
                       array_type->Basic.flags & BasicFlag_String) {
                type = t_u8;
                
            } else {
                type = 0;
                unimplemented;
            }
            
            int array_ptr = emit_reference_expression(bc, expr->Index_Expr.array);
            if ((array_type->kind == TypeKind_Array &&
                 array_type->Array.kind != ArrayKind_Fixed_Inplace) ||
                array_type->kind == TypeKind_Pointer ||
                array_type == t_string ||
                array_type == t_cstring) {
                int tmp = add_bytecode_register(bc);
                array_ptr = bc_load(bc, array_type, tmp, array_ptr);
            }
            
            int array_index = add_bytecode_register(bc, expr->Index_Expr.index->type);
            emit_value_expression(bc, expr->Index_Expr.index, array_index);
            result = add_bytecode_register(bc, expr->Index_Expr.array->type);
            bc_array_access(bc, type, result, array_ptr, array_index);
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

void
emit_function_call_to_bytecode(Bytecode_Builder* bc, Type* type, array(Ast*)* args, 
                               int result_index, int function_ptr_index) {
    assert(type && type->kind == TypeKind_Function);
    
    if (type->Function.is_intrinsic) {
        bool handled = false;
        
        switch (type->Function.ident) {
            case Sym_rdtsc: {
                int* arg_indices = bc_intrinsic(bc, BC_X64_RDTSC, 1, 0);
                arg_indices[0] = result_index;
                handled = true;
            } break;
            
            case Sym_debug_break: {
                bc_intrinsic(bc, BC_DEBUG_BREAK, 0, 0);
                handled = true;
            } break;
        }
        
        if (handled) {
            return;
        }
    }
    
    // NOTE(Alexander): return registers are pushed first followed by arguments
    
    array(int)* arg_indices = 0;
    if (is_valid_type(type->Function.return_type)) {
        if (result_index == -1) {
            // Make sure we give an actual result when required
            if (is_aggregate_type(type->Function.return_type)) {
                result_index = bc_local(bc, type->Function.return_type);
            } else {
                result_index = add_bytecode_register(bc, type->Function.return_type);
            }
        }
        
        array_push(arg_indices, result_index);
    }
    
    for (int i = 0; i < array_count(args); i++) {
        int reg_index;
        Ast* arg = args[i];
        Type* arg_type = arg->type;
        
        if (is_aggregate_type(arg_type)) {
            // TODO: Create copy (except for ARRAY types) and pass it via ptr
            reg_index = emit_reference_expression(bc, arg);
        } else {
            reg_index = add_bytecode_register(bc, arg_type);
            emit_value_expression(bc, arg, reg_index);
        }
        
        array_push(arg_indices, reg_index);
    }
    
    u32 caller_arg_count;
    Compilation_Unit* cu = type->Function.unit;
    if (cu) {
        assert(cu->bytecode_function);
        Bytecode_Function* func = cu->bytecode_function;
        caller_arg_count = func->arg_count;
        bc_call(bc, func->type_index, arg_indices); 
        
    } else if (function_ptr_index != -1) {
        int ret_count = is_valid_type(type->Function.return_type);
        caller_arg_count = (u32) array_count(type->Function.arg_types);
        if (is_valid_type(type->Function.return_type) && is_aggregate_type(type->Function.return_type)) {
            caller_arg_count++;
        }
        bc_call_indirect(bc, function_ptr_index, ret_count, arg_indices); 
        
    } else {
        caller_arg_count = 0;
        verify_not_reached();
    }
    
    // TODO(Alexander): this should maybe not always include the return
    bc->curr_function->max_caller_arg_count = max(bc->curr_function->max_caller_arg_count, caller_arg_count);
}

internal void
emit_non_const_aggregate_fields(Bytecode_Builder* bc, Ast* expr, int base_ptr, int offset) {
    Type* type = expr->type;
    
    int field_index = (int) expr->Aggregate_Expr.first_index;
    for_compound(expr->Aggregate_Expr.elements, field) {
        assert(field->kind == Ast_Argument);
        
        string_id ident = try_unwrap_ident(field->Argument.ident);
        Ast* assign = field->Argument.assign;
        
        Struct_Field_Info field_info = {};
        if (type->kind == TypeKind_Struct || type->kind == TypeKind_Union)  {
            if (ident) {
                field_info = get_field_info(&type->Struct_Like, ident);
            } else {
                field_info = get_field_info_by_index(&type->Struct_Like, field_index);
            }
        } else if (type->kind == TypeKind_Array) {
            Type* elem_type = type->Array.type;
            smm aligned_size = get_array_element_size(elem_type);
            field_info.type = elem_type;
            field_info.offset = field_index*aligned_size;
        } else {
            compiler_bug("unexpected aggregate type");
        }
        field_info.offset += offset;
        
        if (assign->kind == Ast_Aggregate_Expr) {
            //pln("Adding recursive field % (offset = %", f_ast(assign), f_int(field_info.offset));
            emit_non_const_aggregate_fields(bc, assign, base_ptr, (int) field_info.offset);
            
        } else if (assign->kind != Ast_Value) {
            //pln("Adding non-constant field: %", f_ast(assign));
            
            int dest_ptr = base_ptr;
            if (field_info.offset != 0) {
                dest_ptr = add_bytecode_register(bc, t_void_ptr);
                bc_field_access(bc, dest_ptr, base_ptr, (int) field_info.offset);
            }
            emit_initializing_expression(bc, assign, dest_ptr);
            
#if 0
            if (is_aggregate_type(field_info.type)) {
                int src = emit_reference_expression(bc, assign);
                bc_instruction(bc, BC_MEMCPY, dest, src, field_info.type->size);
            } else {
                int src = add_bytecode_register(bc, assign->type);
                emit_value_expression(bc, assign, src);
                bc_store(bc, dest, src);
            }
#endif
        }
        
        field_index++;
    }
}

void
emit_initializing_expression(Bytecode_Builder* bc, Ast* expr, int dest_ptr) {
    switch (expr->kind) {
        case Ast_None: {
            if (is_aggregate_type(expr->type)) {
                bc_memset(bc, dest_ptr, 0, expr->type->size);
            } else {
                int src = add_bytecode_register(bc, t_s64);
                bc_const_int(bc, src, 0);
                bc_store(bc, dest_ptr, src);
            }
        } break;
        
        case Ast_Ident:
        case Ast_Exported_Data: {
            if (is_aggregate_type(expr->type)) {
                int src = emit_reference_expression(bc, expr);
                bc_memcpy(bc, dest_ptr, src, expr->type->size);
            } else {
                int src = add_bytecode_register(bc, expr->type);
                emit_value_expression(bc, expr, src);
                bc_store(bc, dest_ptr, src);
            }
        } break;
        
        case Ast_Aggregate_Expr: {
            Type* type = expr->type;
            
            int global_index = -1;
            if (is_valid_ast(expr->Aggregate_Expr.elements->Compound.node)) {
                global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY, type->size, type->align);
                Bytecode_Global* g = &bc->bytecode.globals[global_index];
                convert_aggregate_literal_to_memory(expr, g->address);
            }
            
            if (global_index >= 0) {
                int data = add_bytecode_register(bc, t_void_ptr);
                bc_global(bc, data, global_index);
                bc_memcpy(bc, dest_ptr, data, type->size);
                emit_non_const_aggregate_fields(bc, expr, dest_ptr, 0);
            } else {
                bc_memset(bc, dest_ptr, 0, type->size);
            }
        } break;
        
        case Ast_Value: {
            if (is_string(expr->Value)) {
                smm string_count = expr->Value.data.str.count;
                int global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY, 
                                                       string_count, 1,
                                                       expr->Value.data.str.data);
                int src_data_ptr = add_bytecode_register(bc, t_void_ptr);
                bc_global(bc, src_data_ptr, global_index);
                
                bc_store(bc, dest_ptr, src_data_ptr);
                
                int src_count = add_bytecode_register(bc, t_s64);
                bc_const_int(bc, src_count, string_count);
                int tmp = add_bytecode_register(bc, t_void_ptr);
                bc_field_access(bc, tmp, dest_ptr, 8);
                bc_store(bc, tmp, src_count);
            } else {
                
                int src = add_bytecode_register(bc, expr->type);
                emit_value_expression(bc, expr, src);
                bc_store(bc, dest_ptr, src);
            }
        } break;
        
        case Ast_Unary_Expr: {
            switch (expr->Unary_Expr.op) {
                case Op_Address_Of: {
                    int src = emit_reference_expression(bc, expr->Unary_Expr.first);
                    bc_store(bc, dest_ptr, src);
                } break;
                
                default: unimplemented;
            }
        } break;
        
        case Ast_Binary_Expr: {
            //int src_ptr = add_bytecode_register(bc, t_void_ptr);
            emit_value_expression(bc, expr, dest_ptr);
            //bc_memcpy(bc, dest_ptr, src_ptr, expr->type->size);
        } break;
        
        case Ast_Cast_Expr: {
            Type* t_dest = normalize_type_for_casting(expr->type);
            Type* t_src = normalize_type_for_casting(expr->Cast_Expr.expr->type);
            
            if (t_dest->kind == TypeKind_Array && t_src->kind == TypeKind_Array) {
                emit_array_type_cast(bc, t_dest, t_src, expr->Cast_Expr.expr, dest_ptr);
            } else {
                int result = add_bytecode_register(bc, t_dest);
                emit_type_cast(bc, expr, result);
            }
        } break;
        
        //case Ast_Call_Expr: {
        
        //} break;
        
        default: unimplemented;
    }
}

void
emit_value_expression(Bytecode_Builder* bc, Ast* expr, int _result) {
    switch (expr->kind) {
        case Ast_None: {
            bc_const_zero(bc, expr->type, _result);
        } break;
        
        case Ast_Ident: {
            string_id ident = ast_unwrap_ident(expr);
            Bc_Local local = map_get(bc->locals, ident);
            if (local.ptr != -1) { 
                if (_result != -1) {
                    bc_load(bc, expr->type, _result, local.ptr);
                }
            } else if (local.val != -1) {
                if (_result != -1) {
                    bc_copy(bc, _result, local.val);
                }
            } else {
                int src = emit_reference_expression(bc, expr);
                bc_copy(bc, _result, src);
            }
        } break;
        
        case Ast_Value: {
            Type* type = expr->type;
            if (is_integer(expr->Value)) {
                // TODO(Alexander): is value_to_s64 safe to use?
                bc_const_int(bc, _result, value_to_s64(expr->Value));
                
            } else if (is_floating(expr->Value)) {
                if (type->size == 8) {
                    bc_const_f64(bc, _result, (f64) expr->Value.data.floating);
                } else {
                    bc_const_f32(bc, _result, (f32) expr->Value.data.floating);
                }
            } else if (is_cstring(expr->Value)) {
                if (expr->Value.data.cstr) {
                    smm string_count = cstring_count(expr->Value.data.cstr);
                    
                    int global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY,
                                                           string_count + 1, 1,
                                                           (void*) expr->Value.data.cstr);
                    bc_global(bc, _result, global_index);
                } else {
                    bc_const_int(bc, _result, 0);
                }
            } else {
                verify_not_reached();
            }
        } break;
        
        case Ast_Unary_Expr: {
            Type* result_type = expr->type;
            Type* type = expr->Unary_Expr.first->type;
            Operator op = expr->Unary_Expr.op;
            
            switch (op) {
                case Op_Address_Of: {
                    int ref = emit_reference_expression(bc, expr->Unary_Expr.first);
                    if (_result != ref) {
                        bc_copy(bc, _result, ref);
                    }
                } break;
                
                case Op_Dereference: {
                    if (type->kind == TypeKind_Pointer) {
                        int ptr = add_bytecode_register(bc);
                        emit_value_expression(bc, expr->Unary_Expr.first, ptr);
                        bc_load(bc, result_type, _result, ptr);
                    } else {
                        emit_value_expression(bc, expr->Unary_Expr.first, _result);
                    }
                } break;
                
                case Op_Pre_Increment:
                case Op_Pre_Decrement: {
                    Bc_Local local = emit_reference_expression2(bc, expr->Unary_Expr.first);
                    if (local.ptr == -1) {
                        Bytecode_Operator opcode = (op == Op_Post_Increment || 
                                                    op == Op_Pre_Increment) ? BC_INC : BC_DEC;
                        bc_unary_arith(bc, opcode, local.val, local.val);
                        if (_result != -1) {
                            bc_copy(bc, _result, local.val);
                        }
                    } else {
                        unimplemented;
                    }
                } break;
                
                case Op_Post_Increment:
                case Op_Post_Decrement: {
                    bool is_increment = op == Op_Post_Increment;
                    
                    Bc_Local local = emit_reference_expression2(bc, expr->Unary_Expr.first);
                    if (local.ptr == -1) {
                        if (_result != -1) {
                            bc_copy(bc, _result, local.val);
                        }
                        emit_unary_increment(bc, type, local.val, is_increment);
                        
                    } else {
                        int first_ptr = local.ptr;
                        if (_result != -1) {
                            bc_load(bc, type, _result, first_ptr);
                        }
                        int new_first = add_bytecode_register(bc);
                        bc_load(bc, type, new_first, first_ptr);
                        emit_unary_increment(bc, type, new_first, is_increment);
                        bc_store(bc, first_ptr, new_first);
                    }
                } break;
                
                case Op_Bitwise_Not: {
                    emit_value_expression(bc, expr->Unary_Expr.first, _result);
                    if (_result != -1) {
                        bc_unary_arith(bc, BC_NOT, _result, _result);
                    }
                } break;
                
                case Op_Logical_Not: {
                    emit_value_expression(bc, expr->Unary_Expr.first, _result);
                    if (_result != -1) {
                        emit_zero_compare(bc, type, _result, _result, false);
                    }
                } break;
                
                case Op_Negate: {
                    emit_value_expression(bc, expr->Unary_Expr.first, _result);
                    if (_result != -1) {
                        bc_unary_arith(bc, BC_NEG, _result, _result);
                    }
                } break;
                
                default: unimplemented;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Type* result_type = expr->type;
            Type* type = expr->Binary_Expr.first->type;
            Operator op = expr->Binary_Expr.op;
            
            if (expr->Binary_Expr.overload) {
                array(Ast*)* args = 0;
                array_push(args, expr->Binary_Expr.first);
                array_push(args, expr->Binary_Expr.second);
                emit_function_call_to_bytecode(bc, expr->Binary_Expr.overload, args, _result, -1);
                array_free(args);
                
#if 0
            } else if (type->kind == TypeKind_Pointer) {
                switch (op) {
                    case Op_Add:
                    case Op_Subtract: {
                        int first = emit_value_fetch_expression(bc, expr->Binary_Expr.first);
                        int index = emit_value_fetch_expression(bc, expr->Binary_Expr.second, _result);
                        if (op == Op_Subtract) {
                            bc_unary_arith(bc, BC_NEG, _result, index);
                            index = _result;
                        }
                        bc_array_access(bc, type->Pointer, _result, first, index);
                    } break;
                    
                    case Op_Add_Assign:
                    case Op_Subtract_Assign: {
                        int first = emit_value_fetch_expression(bc, expr->Binary_Expr.first);
                        int index = emit_value_fetch_expression(bc, expr->Binary_Expr.second, _result);
                        
                        
                    } break;
                    
                    case Op_Assign: {
                        Bc_Local first = emit_reference_expression2(bc, expr->Binary_Expr.first);
                        int value = emit_value_fetch_expression(bc, expr->Binary_Expr.second);
                        if (first.ptr == -1) {
                            emit_value_fetch_expression(bc, expr->Binary_Expr.second);
                        } else {
                            bc_store(bc, first, value);
                        }
                    } break;
                    
                    default: verify_not_reached();
                }
#endif
                
            } else if (op == Op_Logical_And) {
                
                bc_begin_block(bc);
                bc_begin_block(bc);
                
                int cond = add_bytecode_register(bc, t_bool);
                emit_condition_expression(bc, expr->Binary_Expr.first, cond, true);
                bc_branch_if(bc, bc->block_depth, cond);
                
                emit_condition_expression(bc, expr->Binary_Expr.second, cond, true);
                bc_branch_if(bc, bc->block_depth, cond);
                
                bc_const_int(bc, _result, true);
                bc_branch(bc, bc->block_depth - 1);
                bc_end_block(bc);
                
                bc_const_int(bc, _result, false);
                bc_end_block(bc);
            } else if (op == Op_Logical_Or) {
                bc_begin_block(bc);
                bc_begin_block(bc);
                
                int cond = add_bytecode_register(bc, t_bool);
                emit_condition_expression(bc, expr->Binary_Expr.first, cond, false);
                bc_branch_if(bc, bc->block_depth, cond);
                
                emit_condition_expression(bc, expr->Binary_Expr.second, cond, false);
                bc_branch_if(bc, bc->block_depth, cond);
                
                bc_const_int(bc, _result, false);
                bc_branch(bc, bc->block_depth - 1);
                bc_end_block(bc);
                
                bc_const_int(bc, _result, true);
                bc_end_block(bc);
                
            } else if (operator_is_assign(op)) {
                Bytecode_Operator opcode = to_bytecode_opcode(op, type);
                emit_assignment_expression(bc, opcode,
                                           expr->Binary_Expr.first,
                                           expr->Binary_Expr.second);
            } else {
                Bytecode_Operator opcode = to_bytecode_opcode(op, type);
                emit_binary_expression(bc, opcode, 
                                       expr->Binary_Expr.first,
                                       expr->Binary_Expr.second, _result);
            }
        } break;
        
        case Ast_Ternary_Expr: {
            bc_begin_block(bc);
            bc_begin_block(bc);
            int cond = add_bytecode_register(bc, t_bool);
            emit_condition_expression(bc, expr->Ternary_Expr.first, cond, true);
            bc_branch_if(bc, bc->block_depth, cond);
            
            emit_value_expression(bc, expr->Ternary_Expr.second, _result);
            
            bc_branch(bc, bc->block_depth - 1);
            bc_end_block(bc);
            
            emit_value_expression(bc, expr->Ternary_Expr.third, _result);
            bc_end_block(bc);
        } break;
        
        case Ast_Call_Expr: {
            array(Ast*)* args = 0;
            for_compound(expr->Call_Expr.args, arg) {
                array_push(args, arg->Argument.assign);
            }
            
            Type* type = expr->Call_Expr.function_type;
            assert(type->kind == TypeKind_Function);
            
            int function_ptr = -1;
            if (!type->Function.unit && !type->Function.is_intrinsic) {
                function_ptr = add_bytecode_register(bc, t_void_ptr);
                emit_value_expression(bc, expr->Call_Expr.ident, function_ptr);
            }
            emit_function_call_to_bytecode(bc, type, args, _result, function_ptr);
            array_free(args);
        } break;
        
        case Ast_Cast_Expr: {
            emit_type_cast(bc, expr, _result);
        } break;
        
        case Ast_Index_Expr: {
            int ptr = emit_reference_expression(bc, expr);
            bc_load(bc, expr->type, _result, ptr);
        } break;
        
        case Ast_Field_Expr: {
            int ptr = emit_reference_expression(bc, expr);
            Type* type = expr->Field_Expr.var->type;
            if (type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace) {
                // TODO: for inplace arrays the field expression is essentially a noop
                bc_copy(bc, _result, ptr);
            } else {
                bc_load(bc, expr->type, _result, ptr);
            }
        } break;
        
        case Ast_Paren_Expr: {
            emit_value_expression(bc, expr->Paren_Expr.expr, _result);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
}

int
emit_value_fetch_expression(Bytecode_Builder* bc, Ast* expr, int result) {
    // Fetch the value directly without making unnecessary copies
    if (expr->kind == Ast_Ident) {
        string_id ident = ast_unwrap_ident(expr);
        Bc_Local local = map_get(bc->locals, ident);
        if (local.ptr != -1) {
            if (result == -1) {
                result = add_bytecode_register(bc);
            }
            bc_load(bc, expr->type, result, local.ptr);
            
        } else if (local.val != -1) {
            result = local.val;
        } else {
            result = emit_reference_expression(bc, expr);
        }
        
    } else {
        if (result == -1) {
            result = add_bytecode_register(bc, expr->type);
        }
        emit_value_expression(bc, expr, result);
    }
    
    return result;
}

inline void
emit_unary_increment(Bytecode_Builder* bc, Type* type, int result, bool increment) {
    if (type->kind == TypeKind_Pointer) {
        int index = add_bytecode_register(bc, t_s64);
        bc_const_int(bc, index, increment ? 1 : -1);
        bc_array_access(bc, type->Pointer, result, result, index);
    } else {
        bc_unary_arith(bc, increment ? BC_INC : BC_DEC, result, result);
    }
}

inline void
emit_binary_arithmetic(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                       Type* type, int result, int first, int second) {
    assert(opcode != BC_NOOP);
    
    if (type->kind == TypeKind_Pointer) {
        if (opcode == BC_SUB) {
            int tmp = add_bytecode_register(bc, t_s64);
            bc_unary_arith(bc, BC_NEG, tmp, second);
            second = tmp;
        }
        bc_array_access(bc, type->Pointer, result, first, second);
    } else {
        Bytecode_Type res_type;
        if (opcode >= BC_EQ && opcode <= BC_NEQ) {
            res_type = BC_BOOL;
        } else {
            res_type = to_bytecode_type(type);
        }
        bc_binary_arith(bc, opcode, res_type, result, first, second);
    }
}

inline void
emit_binary_expression(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                       Ast* lexpr, Ast* rexpr, int result) {
    int first = emit_value_fetch_expression(bc, lexpr, result);
    int second = emit_value_fetch_expression(bc, rexpr, (first != result) ? result : -1);
    emit_binary_arithmetic(bc, opcode, lexpr->type, result, first, second);
}

void
emit_assignment_expression(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                           Ast* lexpr, Ast* rexpr) {
    
    Type* type = lexpr->type;
    if (is_aggregate_type(type)) {
        int dest = emit_reference_expression(bc, lexpr);
        emit_initializing_expression(bc, rexpr, dest);
        
    } else {
        Bc_Local first_ref = emit_reference_expression2(bc, lexpr);
        if (opcode == BC_NOOP) {
            if (first_ref.ptr == -1) {
                int second = emit_value_fetch_expression(bc, rexpr, first_ref.val);
                if (first_ref.val != second) {
                    bc_copy(bc, first_ref.val, second);
                }
            } else {
                int second = emit_value_fetch_expression(bc, rexpr);
                bc_store(bc, first_ref.ptr, second);
            }
            
        } else {
            if (first_ref.ptr == -1) {
                // Assign to value
                int second = emit_value_fetch_expression(bc, rexpr);
                emit_binary_arithmetic(bc, opcode, type, first_ref.val, first_ref.val, second);
                
            } else {
                int result = add_bytecode_register(bc);
                bc_load(bc, lexpr->type, result, first_ref.ptr);
                int second = emit_value_fetch_expression(bc, rexpr);
                emit_binary_arithmetic(bc, opcode, type, result, result, second);
                bc_store(bc, first_ref.ptr, result);
            }
        }
    }
}

void
emit_type_cast(Bytecode_Builder* bc, Ast* expr, int result) {
    
    Type* t_dest = normalize_type_for_casting(expr->type);
    Type* t_src = normalize_type_for_casting(expr->Cast_Expr.expr->type);
    
    // TODO(Alexander): optimize by using a lookup table
    if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
        Bytecode_Operator opcode = BC_COPY;
        if (t_dest->Basic.flags & BasicFlag_Boolean &&
            t_src->Basic.flags & BasicFlag_Integer) {
            emit_condition_expression(bc, expr->Cast_Expr.expr, result, false);
            
        } else if (t_dest->Basic.flags & BasicFlag_Integer &&
                   t_src->Basic.flags & BasicFlag_Integer) {
            if (t_dest->size < t_src->size) {
                opcode = BC_TRUNCATE;
                
            } else if (t_dest->size > t_src->size) {
                opcode = BC_EXTEND;
            }
            
        } else if (t_dest->Basic.flags & BasicFlag_Integer &&
                   t_src->Basic.flags & BasicFlag_Floating) {
            opcode = BC_FLOAT_TO_INT;
            
        } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                   t_src->Basic.flags & BasicFlag_Integer) {
            opcode = BC_INT_TO_FLOAT;
            
        } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                   t_src->Basic.flags & BasicFlag_Floating) {
            opcode = BC_FLOAT_TO_FLOAT;
            
        } else {
            unimplemented;
        }
        
        if (opcode) {
            int src = add_bytecode_register(bc, expr->Cast_Expr.expr->type);
            emit_value_expression(bc, expr->Cast_Expr.expr, src);
            bc_cast(bc, opcode, result, src);
        } else {
            emit_value_expression(bc, expr->Cast_Expr.expr, result);
        }
        
        //}  else if (t_dest->kind == TypeKind_Pointer && t_src->kind == TypeKind_Pointer) {
        //result = 
        
    } else {
        unimplemented;
    }
}

inline void
emit_array_type_cast(Bytecode_Builder* bc, Type* t_dest, Type* t_src, Ast* src_ast, int array_ptr) {
    
    int src_ptr = emit_reference_expression(bc, src_ast);
    if (t_dest->Array.kind == ArrayKind_Fixed &&
        t_src->Array.kind == ArrayKind_Fixed_Inplace) {
        
        // Converts inplace fixed array to "wide"-pointer array
        bc_store(bc, array_ptr, src_ptr);
        
        int count = add_bytecode_register(bc, t_s64);
        bc_const_int(bc, count, t_src->Array.capacity);
        int count_ptr = add_bytecode_register(bc, t_void_ptr);
        bc_field_access(bc, count_ptr, array_ptr, 8);
        bc_store(bc, count_ptr, count);
    } else {
        unimplemented;
    }
}

inline void
emit_zero_compare(Bytecode_Builder* bc, Type* type, int result, int value, bool invert_condition) {
    int zero = add_bytecode_register(bc, type);
    bc_const_zero(bc, type, zero);
    bc_binary_arith(bc, invert_condition ? BC_NEQ : BC_EQ, BC_BOOL, result, value, zero);
}

void
emit_condition_expression(Bytecode_Builder* bc, Ast* cond, int result, bool invert_condition) {
    Type* type = cond->type;
    
    if (cond->kind == Ast_Binary_Expr && operator_is_comparator_table[cond->Binary_Expr.op]) {
        Bytecode_Operator opcode = to_bytecode_opcode(cond->Binary_Expr.op, 
                                                      cond->Binary_Expr.first->type);
        if (invert_condition) {
            opcode = (Bytecode_Operator) (BC_EQ + BC_NEQ - opcode);
        }
        emit_binary_expression(bc, opcode, 
                               cond->Binary_Expr.first,
                               cond->Binary_Expr.second, result);
        
    } else {
        emit_value_expression(bc, cond, result);
        emit_zero_compare(bc, type, result, result, invert_condition);
    }
}

void
emit_statement(Bytecode_Builder* bc, Ast* stmt, s32 break_label, s32 continue_label) {
    switch (stmt->kind) {
        case Ast_Decl_Stmt: {
            Ast* decl = stmt->Decl_Stmt.stmt;
            if (is_ast_stmt(decl)) {
                emit_statement(bc, decl, break_label, continue_label);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            Ast* expr = stmt->Expr_Stmt;
            if (is_valid_ast(expr)) {
                emit_value_expression(bc, expr);
            }
        } break;
        
        case Ast_Assign_Stmt: {
            Type* type = stmt->type;
            
            Bc_Local local = {};
            if (is_aggregate_type(type)) {
                local.ptr = bc_local(bc, type);
                emit_initializing_expression(bc, stmt->Assign_Stmt.expr, local.ptr);
            } else {
                local.ptr = -1;
                local.val = add_bytecode_register(bc, type);
                emit_value_expression(bc, stmt->Assign_Stmt.expr, local.val);
            }
            
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            map_put(bc->locals, ident, local);
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                emit_statement(bc, it, break_label, continue_label);
            }
        } break;
        
        case Ast_If_Stmt: {
            bc_begin_block(bc);
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                bc_begin_block(bc);
            }
            
            int cond = add_bytecode_register(bc, t_bool);
            emit_condition_expression(bc, stmt->If_Stmt.cond, cond, true);
            bc_branch_if(bc, bc->block_depth, cond);
            
            // Then case
            emit_statement(bc, stmt->If_Stmt.then_block, break_label, continue_label);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                // Else case
                bc_branch(bc, bc->block_depth - 1);
                
                bc_end_block(bc);
                emit_statement(bc, stmt->If_Stmt.else_block, break_label, continue_label);
            }
            bc_end_block(bc);
            
        } break;
        
        case Ast_For_Stmt: {
            // init
            emit_statement(bc, stmt->For_Stmt.init, 0, 0);
            
            bc_begin_block(bc);
            bc_begin_loop_block(bc);
            
            // Condition
            if (is_valid_ast(stmt->For_Stmt.cond)) {
                int cond = add_bytecode_register(bc, t_bool);
                emit_condition_expression(bc, stmt->For_Stmt.cond, cond, true);
                bc_branch_if(bc, bc->block_depth - 1, cond);
            }
            
            // Block
            bc_begin_block(bc);
            emit_statement(bc, stmt->For_Stmt.block, bc->block_depth - 2, bc->block_depth);
            bc_end_block(bc);
            
            // Update
            emit_value_expression(bc, stmt->For_Stmt.update, -1);
            bc_branch(bc, bc->block_depth);
            
            // Exit
            bc_end_block(bc);
            bc_end_block(bc);
        } break;
        
        case Ast_While_Stmt: {
            bc_begin_block(bc);
            bc_begin_loop_block(bc);
            
            // Condition
            if (is_valid_ast(stmt->While_Stmt.cond)) {
                int cond = add_bytecode_register(bc, t_bool);
                emit_condition_expression(bc, stmt->While_Stmt.cond, cond, true);
                bc_branch_if(bc, bc->block_depth - 1, cond);
            }
            
            emit_statement(bc, stmt->While_Stmt.block, bc->block_depth - 1, bc->block_depth);
            bc_branch(bc, bc->block_depth);
            
            // Exit
            bc_end_block(bc);
            bc_end_block(bc);
        } break;
        
        case Ast_Switch_Stmt: {
            bc_begin_block(bc);
            Ast* default_stmt = 0;
            int outer_block = bc->block_depth;
            int switch_cond = emit_value_fetch_expression(bc, stmt->Switch_Stmt.cond);
            
            {
                for_compound(stmt->Switch_Stmt.cases, it) {
                    if (!is_valid_ast(it->Switch_Case.cond)) {
                        assert(default_stmt == 0 && "two default statements");
                        default_stmt = it->Switch_Case.stmt;
                    } else if (is_valid_ast(it->Switch_Case.stmt)) {
                        bc_begin_block(bc);
                    }
                }
            }
            
            int case_cond = add_bytecode_register(bc, t_bool);
            int multi_case_block = -1;
            for_compound(stmt->Switch_Stmt.cases, it) {
                if (is_valid_ast(it->Switch_Case.cond)) {
                    emit_value_expression(bc, it->Switch_Case.cond, case_cond);
                    
                    if (is_valid_ast(it->Switch_Case.stmt)) {
                        bc_binary_arith(bc, BC_NEQ, BC_BOOL, case_cond, switch_cond, case_cond);
                        if (multi_case_block == -1) {
                            bc_branch_if(bc, bc->block_depth, case_cond);
                        } else {
                            bc_branch_if(bc, bc->block_depth - 1, case_cond);
                            bc_end_block(bc);
                            multi_case_block = -1;
                        }
                        
                        emit_statement(bc, it->Switch_Case.stmt, break_label, continue_label);
                        
                        if (bc->curr_insn->opcode != BC_RETURN) {
                            bc_branch(bc, outer_block);
                        }
                        bc_end_block(bc);
                        
                    } else {
                        // Case contains multiple cases (similar to logical or)
                        if (multi_case_block == -1) {
                            bc_begin_block(bc);
                            multi_case_block = bc->block_depth;
                        }
                        
                        bc_binary_arith(bc, BC_EQ, BC_BOOL, case_cond, switch_cond, case_cond);
                        bc_branch_if(bc, multi_case_block, case_cond);
                    }
                }
            }
            
            if (default_stmt) {
                emit_statement(bc, default_stmt, break_label, continue_label);
            }
            
            bc_end_block(bc);
            
        } break;
        
        case Ast_Return_Stmt: {
            int result = -1;
            if (is_valid_ast(stmt->Return_Stmt.expr)) {
                if (bc->curr_function->return_as_first_arg) {
                    int src_ptr = emit_reference_expression(bc, stmt->Return_Stmt.expr);
                    Bytecode_Function_Arg src_type = function_ret_types(bc->curr_function)[0];
                    bc_memcpy(bc, 0, src_ptr, src_type.size);
                } else {
                    result = emit_value_fetch_expression(bc, stmt->Return_Stmt.expr);
                }
            }
            bc_return(bc, result);
        } break;
        
        case Ast_Break_Stmt: {
            bc_branch(bc, break_label);
        } break;
        
        case Ast_Continue_Stmt: {
            bc_branch(bc, continue_label);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
}


Bytecode_Function*
emit_function(Bytecode_Builder* bc, Bytecode_Function* func, Ast* ast,
              bool is_main, bool insert_debug_break) {
    assert(ast->type->kind == TypeKind_Function);
    assert(ast->kind == Ast_Decl_Stmt);
    
    if (!is_valid_ast(ast->Decl_Stmt.stmt)) {
        return 0;
    }
    
    Type* type = ast->type;
    
    // Build the function
    bc->curr_function = func;
    bc->curr_insn = 0;
    bc->block_depth = 0;
    if (is_main) {
        bc->bytecode.entry_func_index = func->type_index;
        Bytecode_Export main_export = {};
        main_export.function = ast_unwrap_ident(ast->Decl_Stmt.ident);
        main_export.func_index = func->type_index;;
        array_push(bc->bytecode.exports, main_export);
    }
    
    for_array_v(type->Function.arg_idents, arg_ident, i) {
        int arg_index = func->return_as_first_arg ? (i + 1) : i;
        Type* arg_type = type->Function.arg_types[i];
        
        Bc_Local arg = {};
        if (is_aggregate_type(arg_type)) {
            arg.ptr = arg_index;
        } else {
            arg.val = arg_index;
            arg.ptr = -1;
        }
        map_put(bc->locals, arg_ident, arg);
    }
    
    if (insert_debug_break) {
        bc_intrinsic(bc, BC_DEBUG_BREAK, 0, 0);
    }
    
    emit_statement(bc, ast->Decl_Stmt.stmt, 0, 0);
    
    return func;
}


Bytecode_Function*
add_bytecode_function(Bytecode_Builder* bc, Type* type) {
    assert(type && type->kind == TypeKind_Function && "not a function type");
    
    int ret_count = is_valid_type(type->Function.return_type) ? 1 : 0;
    int arg_count = (int) array_count(type->Function.arg_types);
    
    int size = (sizeof(Bytecode_Function) + 
                (ret_count + arg_count)*sizeof(Bytecode_Function_Arg));
    
    Bytecode_Function* func = (Bytecode_Function*) arena_push_size(&bc->arena, size, 
                                                                   alignof(Bytecode_Function));
    func->relative_ptr = (u32) arena_relative_pointer(&bc->arena, func);
    func->type_index = bc->next_type_index++;
    func->ret_count = ret_count;
    func->arg_count = arg_count;
    
    bc->curr_function = func;
    bc->curr_insn = 0;
    
    if (type->Function.unit) {
        type->Function.unit->bytecode_function = func;
    }
    
    Bytecode_Function_Arg* curr_arg = function_ret_types(func);
    if (ret_count == 1) {
        Type* ret_type = type->Function.return_type;
        if (is_aggregate_type(ret_type)) {
            add_bytecode_register(bc, type->Function.return_type);
            func->ret_count--;
            func->arg_count++;
            func->return_as_first_arg = true;
        }
        
        curr_arg->type = to_bytecode_type(ret_type);
        curr_arg->size = ret_type->size;
        curr_arg->align = ret_type->align;
        curr_arg++;
    }
    
    for (int i = 0; i < arg_count; i++) {
        string_id arg_ident = type->Function.arg_idents[i];
        Type* arg_type = type->Function.arg_types[i];
        curr_arg->type = to_bytecode_type(arg_type);
        curr_arg->size = arg_type->size;
        curr_arg->align = arg_type->align;
        add_bytecode_register(bc, arg_type);
        curr_arg++;
    }
    
    array_push(bc->bytecode.functions, func);
    array_push(bc->bytecode.function_names, type->Function.ident);
    
    return func;
}

int
add_bytecode_global(Bytecode_Builder* bc, 
                    Bytecode_Memory_Kind kind, 
                    smm size, smm align, void* init) {
    
    Memory_Arena* arena = (kind == BC_MEM_READ_ONLY ? 
                           &bc->data_packer->rdata_arena : 
                           &bc->data_packer->data_arena);
    void* data = arena_push_size(arena, size, align);
    int offset = (int) arena_relative_pointer(arena, data);
    if (init) {
        memcpy(data, init, size);
    }
    
    Bytecode_Global global_var = {};
    global_var.address = data;
    global_var.offset = offset;
    global_var.size = (u32) size;
    global_var.align = (u32) align;
    global_var.kind = kind;
    
    int global_index = (int) array_count(bc->bytecode.globals);
    array_push(bc->bytecode.globals, global_var);
    return global_index;
}

Bytecode_Instruction*
add_bytecode_instruction(Bytecode_Builder* bc, 
                         Bytecode_Operator opcode, 
                         umm size, umm align) {
    
    assert(bc->curr_function && "cannot add instruction outside function scope");
    
    // TODO(Alexander): when we run out of memory we need to make sure we have pointer to next instruction
    Bytecode_Instruction* insn = (Bytecode_Instruction*) arena_push_size(&bc->arena, size, align);
    insn->opcode = opcode;
    
    if (!bc->curr_function->first_insn) {
        bc->curr_function->first_insn = (u32) ((u8*) insn - (u8*) bc->curr_function);
    }
    
    if (bc->curr_insn) {
        bc->curr_insn->next_insn = (u32) ((u8*) insn - (u8*) bc->curr_insn);
    }
    bc->curr_insn = insn;
    bc->curr_function->insn_count++;
    
    return insn;
}

inline void
string_builder_dump_bytecode_function_name(String_Builder* sb, Bytecode* bc, Bytecode_Function* func) {
    string_id ident = 0;
    if (bc->function_names) {
        ident = bc->function_names[func->type_index];
    }
    if (ident) {
        string_builder_push_format(sb, "%", f_var(ident));
    } else {
        string_builder_push_format(sb, "func%", f_int(func->type_index));
    }
}

inline void
string_builder_dump_bytecode_call_args(String_Builder* sb, int arg_count, int* args) {
    // args
    string_builder_push(sb, "(");
    for (int i = 0; i < arg_count; i++) {
        string_builder_push_format(sb, "r%", f_int(args[i]));
        if (i + 1 < arg_count) {
            string_builder_push(sb, ", ");
        }
    }
    string_builder_push(sb, ")");
}

inline void
string_builder_dump_bytecode_opcode(String_Builder* sb, Bytecode_Instruction* insn) {
    //if (insn->type) {
    //string_builder_push_format(sb, "%.", f_cstring(bc_type_names[insn->type]));
    //}
    string_builder_push(sb, bc_operator_names[insn->opcode]);
    string_builder_push(sb, " ");
}

void
string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn, int block_depth) {
    
    smm from_byte = sb->curr_used;
    string_builder_pad(sb, sb->curr_used, block_depth*2);
    
    
    switch (insn->opcode) {
        case BC_BLOCK:
        case BC_LOOP:
        case BC_END: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "(label = %)", f_int(block_depth));
        } break;
        
        case BC_BRANCH: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "%", f_u32(((Bytecode_Branch*) insn)->label_index));
            
            if (((Bytecode_Branch*) insn)->cond != -1) {
                string_builder_push(sb, ", ");
                string_builder_push_format(sb, "r%", f_int(((Bytecode_Branch*) insn)->cond));
            }
        } break;
        
        case BC_INT_CONST: {
            Bytecode_Const_Int* const_int = (Bytecode_Const_Int*) insn;
            string_builder_push_format(sb, "r% = ", f_int(const_int->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "%", f_int(const_int->val));
        } break;
        
        case BC_F32_CONST: {
            Bytecode_Const_F32* const_f32 = (Bytecode_Const_F32*) insn;
            string_builder_push_format(sb, "r% = ", f_int(const_f32->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "%", f_float(const_f32->val));
        } break;
        
        case BC_F64_CONST: {
            Bytecode_Const_F64* const_f64 = (Bytecode_Const_F64*) insn;
            string_builder_push_format(sb, "r% = ", f_int(const_f64->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "%", f_float(const_f64->val));
        } break;
        
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            Bytecode_Function* func = bc->functions[call->func_index];
            int* args = bc_call_args(call);
            
            if (func->ret_count == 1) {
                string_builder_push_format(sb, "r% = ", f_int(args[0]));
            }
            string_builder_dump_bytecode_opcode(sb, insn);
            
            string_builder_dump_bytecode_function_name(sb, bc, func);
            string_builder_dump_bytecode_call_args(sb, func->arg_count, args + func->ret_count);
        } break;
        
        case BC_CALL_INDIRECT: {
            Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) insn;
            int* args = bc_call_args(call);
            if (call->ret_count == 1) {
                string_builder_push_format(sb, "r% = ", f_int(args[0]));
            }
            string_builder_dump_bytecode_opcode(sb, insn);
            
            string_builder_push_format(sb, "r%", f_int(call->func_ptr_index));
            string_builder_dump_bytecode_call_args(sb, call->arg_count, args + call->ret_count);
        } break;
        
        case BC_LOCAL: {
            Bytecode_Local* local = (Bytecode_Local*) insn;
            string_builder_push_format(sb, "r% = ", f_int(local->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "size %, align %", f_int(local->size), f_int(local->align));
        } break;
        
        case BC_GLOBAL: {
            Bytecode_Assign* assign = (Bytecode_Assign*) insn;
            string_builder_push_format(sb, "r% = ", f_int(assign->dest_index));
            string_builder_push_format(sb, "ptr.global[%]", f_int(assign->src_index));
        } break;
        
        case BC_TRUNCATE:
        case BC_EXTEND:
        case BC_INT_TO_FLOAT:
        case BC_FLOAT_TO_INT:
        case BC_FLOAT_TO_FLOAT:
        case BC_LOAD:
        case BC_COPY: {
            Bytecode_Assign* assign = (Bytecode_Assign*) insn;
            string_builder_push_format(sb, "r% = ", f_int(assign->dest_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "r%", f_int(assign->src_index));
        } break;
        
        case BC_STORE: {
            Bytecode_Assign* assign = (Bytecode_Assign*) insn;
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "r%, r%", 
                                       f_int(assign->dest_index), 
                                       f_int(assign->src_index));
        } break;
        
        case BC_INC:
        case BC_DEC: 
        case BC_NEG: 
        case BC_NOT: {
            Bytecode_Unary* unary = (Bytecode_Unary*) insn;
            string_builder_push_format(sb, "r% = ", f_int(unary->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "r%", 
                                       f_int(unary->a_index));
        } break;
        
        case BC_ADD:
        case BC_SUB:
        case BC_MUL:
        case BC_DIV_S:
        case BC_DIV_U:
        case BC_MOD_S:
        case BC_MOD_U:
        case BC_AND:
        case BC_OR:
        case BC_XOR:
        case BC_SHL:
        case BC_SAR:
        case BC_SHR:
        case BC_EQ:
        case BC_GT_S:
        case BC_GT_U:
        case BC_GE_S:
        case BC_GE_U:
        case BC_LT_U:
        case BC_LT_S:
        case BC_LE_U:
        case BC_LE_S:
        case BC_NEQ: {
            Bytecode_Binary* binary = (Bytecode_Binary*) insn;
            string_builder_push_format(sb, "r% = ", f_int(binary->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "r%, r%", 
                                       f_int(binary->a_index),
                                       f_int(binary->b_index));
        } break;
        
        case BC_MEMCPY: {
            string_builder_dump_bytecode_opcode(sb, insn);
            Bytecode_Memcpy* mem_insn = (Bytecode_Memcpy*) insn;
            string_builder_push_format(sb, "r%, r%, size %",
                                       f_int(mem_insn->dest_index),
                                       f_int(mem_insn->src_index),
                                       f_int(mem_insn->size));
        } break;
        
        case BC_MEMSET: {
            string_builder_dump_bytecode_opcode(sb, insn);
            Bytecode_Memset* mem_insn = (Bytecode_Memset*) insn;
            string_builder_push_format(sb, "r%, %, size %",
                                       f_int(mem_insn->dest_index),
                                       f_int(mem_insn->value),
                                       f_int(mem_insn->size));
        } break;
        
        case BC_ARRAY_ACCESS: {
            Bytecode_Array_Access* array_access = (Bytecode_Array_Access*) insn;
            string_builder_push_format(sb, "r% = ", f_int(array_access->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "r%[r%], stride %",
                                       f_int(array_access->base),
                                       f_int(array_access->index),
                                       f_int(array_access->stride));
        } break;
        
        case BC_FIELD_ACCESS: {
            Bytecode_Field_Access* field_access = (Bytecode_Field_Access*) insn;
            string_builder_push_format(sb, "r% = ", f_int(field_access->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "r%, offset %", 
                                       f_int(field_access->base),
                                       f_int(field_access->offset));
        } break;
        
        case BC_RETURN: {
            Bytecode_Result* result = (Bytecode_Result*) insn;
            string_builder_dump_bytecode_opcode(sb, insn);
            if (result->res_index >= 0) {
                string_builder_push_format(sb, "r%", f_int(result->res_index));
            }
        } break;
        
        default: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push(sb, "(???)");
            //if (bc_insn->arg1_index >= 0) {
            //string_builder_push_format(sb, "r%, ", f_int(bc_insn->arg0_index));
            //string_builder_push_format(sb, "r%", f_int(bc_insn->arg1_index));
            //} else if (bc_insn->arg0_index >= 0) {
            //string_builder_push_format(sb, "r%", f_int(bc_insn->arg0_index));
            //}
        } break;
    }
}

void
string_builder_dump_bytecode_type(String_Builder* sb, Bytecode_Type type) {
    
    switch (type.kind) {
        case BC_TYPE_INT: {
            if (type.flags & BC_FLAG_SIGNED) {
                string_builder_push_format(sb, "s%", f_int((int) type.size*8));
            } else {
                string_builder_push_format(sb, "u%", f_int((int) type.size*8));
            }
        } break;
        
        case BC_TYPE_FLOAT: {
            string_builder_push_format(sb, "f%", f_int((int) type.size*8));
        } break;
        
        case BC_TYPE_PTR: {
            string_builder_push(sb, "ptr");
        } break;
    }
    
}

void
string_builder_dump_bytecode_function(String_Builder* sb, Bytecode* bc, Bytecode_Function* func) {
    if (!func) return;
    
    if (func->is_intrinsic || func->is_imported) {
        return;
    }
    
    if (func->is_imported) {
        string_builder_push(sb, "import ");
    }
    
    Bytecode_Function_Arg* formal_args = function_arg_types(func);
    if (func->ret_count == 1) {
        Bytecode_Function_Arg* formal_ret = function_ret_types(func);
        // TODO(Alexander): multiple returns
        string_builder_dump_bytecode_type(sb, formal_ret[0].type);
        string_builder_push(sb, " ");
    } else {
        string_builder_push(sb, "void ");
    }
    string_builder_dump_bytecode_function_name(sb, bc, func);
    string_builder_push(sb, "(");
    for (int i = 0; i < func->arg_count; i++) {
        string_builder_dump_bytecode_type(sb, formal_args[i].type);
        string_builder_push_format(sb, " r%", f_int(i));
        if (i == 0 && func->return_as_first_arg) {
            string_builder_push(sb, " (ret)");
        }
        if (i + 1 < func->arg_count) {
            string_builder_push(sb, ", ");
        }
    }
    
    int bb_index = 0;
    
    if (func->is_imported) {
        if (func->code_ptr) {
            string_builder_push_format(sb, ") = %;", f_u64_HEX(func->code_ptr));
        } else {
            string_builder_push(sb, ");");
        }
    } else {
        string_builder_push(sb, ") {");
    }
    
    Bytecode_Instruction* curr = iter_bytecode_instructions(func, 0);
    int block_depth = 1;
    while (curr->opcode) {
        if (curr->opcode == BC_END) {
            block_depth--;
        }
        
        string_builder_push(sb, "\n");
        string_builder_dump_bytecode_insn(sb, bc, curr, block_depth);
        
        if (curr->opcode == BC_BLOCK || curr->opcode == BC_LOOP) {
            block_depth++;
        }
        curr = iter_bytecode_instructions(func, curr);
    }
    
    if (!func->is_imported) {
        string_builder_push(sb, "\n}");
    }
    
    string_builder_push(sb, "\n");
}

void
string_builder_dump_bytecode_globals(String_Builder* sb, Bytecode* bc) {
    if (array_count(bc->globals) > 0) {
        string_builder_push(sb, "Globals:");
        for_array(bc->globals, g, global_index) {
            const cstring bc_memory_names[] = { "read only", "read write" };
            string_builder_push_format(sb, "\n  %: size %, align %, kind \"%\" - ",
                                       f_int(global_index),
                                       f_int(g->size),
                                       f_int(g->align),
                                       f_cstring(bc_memory_names[g->kind]));
            
            if (g->address) {
                string_builder_push(sb, "\"");
                u8* curr = (u8*) g->address;
                for (u32 i = 0; i < g->size; i++) {
                    string_builder_push_char_literal(sb, *curr++);
                    //if (i < g->size - 1) {
                    //string_builder_push(sb, " ");
                    //}
                }
                string_builder_push(sb, "\"");
            }
        }
    }
    
}

void
dump_bytecode(Bytecode* bc) {
    String_Builder sb = {};
    for_array_v(bc->functions, func, _) {
        string_builder_dump_bytecode_function(&sb, bc, func);
    }
    string_builder_dump_bytecode_globals(&sb, bc);
    
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}