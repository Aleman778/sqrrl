

struct Value_or_Ref {
    int index;
    bool is_ref;
};

inline Value_or_Ref
get_value_or_emit_reference_expression(Bytecode_Builder* bc, Ast* expr) {
    Value_or_Ref result;
    
    string_id ident = try_unwrap_ident(expr);
    if (ident) {
        smm local_index = map_get_index(bc->locals, ident);
        if (local_index != -1) {
            Bc_Local local = bc->locals[local_index].value;
            result.index = local.index;
            result.is_ref = local.is_ref;
            assert(result.index >= 0);
            return result;
        }
    }
    
    result.is_ref = true;
    result.index = emit_reference_expression(bc, expr);
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
                result = bc->locals[local_index].value.index;
                assert(result >= 0);
                
            } else if (type->kind == TypeKind_Function && type->Function.unit) {
                Bytecode_Function* func = type->Function.unit->bytecode_function;
                result = add_register(bc, t_void_ptr);
                bc_function(bc, result, func->type_index);
                
            } else {
                smm global_index = map_get_index(bc->globals, ident);
                if (global_index != -1) {
                    int global_id = bc->globals[global_index].value;
                    result = add_register(bc, t_void_ptr);
                    bc_global(bc, result, global_id);
                    
                } else {
                    verify_not_reached();
                }
            }
        } break;
        
        case Ast_Unary_Expr: {
            if (expr->Unary_Expr.op == Op_Dereference) {
                result = emit_value_fetch_expression(bc, expr->Unary_Expr.first);
            } else {
                verify_not_reached();
            }
        } break;
        
        case Ast_Binary_Expr: {
            assert(is_valid_type(expr->type) && is_aggregate_type(expr->type));
            result = bc_local(bc, expr->type);
            verify(expr->Binary_Expr.overload);
            
            array(Ast*)* args = 0;
            array_push(args, expr->Binary_Expr.first);
            array_push(args, expr->Binary_Expr.second);
            emit_function_call(bc, expr->Binary_Expr.overload, args, 0, result, -1);
            array_free(args);
        } break;
        
        case Ast_Call_Expr: {
            assert(is_valid_type(expr->type));
            if (expr->type->kind == TypeKind_Pointer) {
                result = add_register(bc);
            } else if (is_aggregate_type(expr->type)) {
                result = bc_local(bc, expr->type);
            } else {
                verify_not_reached();
            }
            emit_value_expression(bc, expr, result);
        } break;
        
        case Ast_Cast_Expr: {
            Type* t_dest = normalize_type_for_casting(expr->type);
            Type* t_src = normalize_type_for_casting(expr->Cast_Expr.expr->type);
            
            if (t_dest->kind == TypeKind_Array && t_src->kind == TypeKind_Array) {
                result = bc_local(bc, t_dest);
                emit_array_type_cast(bc, t_dest, t_src, expr->Cast_Expr.expr, result);
            } else {
                unimplemented;
                //result = add_register(bc, expr->type);
                //emit_type_cast(bc, expr, result);
            }
        } break;
        
        case Ast_Paren_Expr: {
            result = emit_reference_expression(bc, expr->Paren_Expr.expr);
        } break;
        
        case Ast_Field_Expr: {
            Ast* var = expr->Field_Expr.var;
            Type* type = var->type;
            result = emit_reference_expression(bc, expr->Field_Expr.var);
            
            if (type->kind == TypeKind_Pointer) {
                type = type->Pointer;
                
                // TODO(Alexander): HACK this might not work in all cases,
                // check if var is direct or indirect pointer, basically we
                // try to avoid storing struct pointers as two level pointers:
                // Bad: MyStruct* r1 (ptr) -> MyStruct r2 (ptr) -> first byte of data
                // Good: 
                //  - MyStruct  r1 (ptr) -> first byte of data
                //  - MyStruct* r1 (ptr) -> first byte of data
                if (var->kind == Ast_Field_Expr) {
                    //!try_unwrap_ident(var)
                    int tmp = add_register(bc);
                    result = bc_load(bc, type, tmp, result);
                }
            }
            
            string_id ident = ast_unwrap_ident(expr->Field_Expr.field);
            switch (type->kind) {
                case TypeKind_Struct:
                case TypeKind_Union: {
                    Struct_Field_Info field = get_field_info(&type->Struct_Like, ident);
                    
                    if (field.offset != 0) {
                        int tmp = add_register(bc, t_void_ptr);
                        result = bc_field_access(bc, tmp, result, (s32) field.offset);
                    }
                } break;
                
                case TypeKind_Array:
                case TypeKind_Basic: {
                    assert((type->kind == TypeKind_Array ||
                            type->Basic.kind == Basic_string) && "unsupported type");
                    
                    if (ident == Sym_data) {
                        // noop
                        
                    } else if (ident == Sym_count) {
                        int tmp = add_register(bc, type);
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
                verify_not_reached();
            }
            
            int tmp = -1;
            Ast* array_expr = expr->Index_Expr.array;
            int array_ptr = emit_reference_expression(bc, array_expr);
            if (array_type->kind == TypeKind_Pointer) {
                if (!try_unwrap_ident(expr->Field_Expr.var)) {
                    int tmp2 = add_register(bc);
                    array_ptr = bc_load(bc, array_type, tmp2, array_ptr);
                }
            }
            
            
            if (is_aggregate_array_type(array_type)) {
                tmp = add_register(bc, expr->type);
                array_ptr = bc_load(bc, array_type, tmp, array_ptr);
            }
            
            int array_index = add_register(bc, expr->Index_Expr.index->type);
            emit_value_expression(bc, expr->Index_Expr.index, array_index);
            result = add_register(bc, expr->Index_Expr.array->type);
            bc_array_access(bc, type, result, array_ptr, array_index);
            drop_register(bc, array_index);
            if (tmp != -1) {
                drop_register(bc, tmp);
            }
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

int
emit_function_argument(Bytecode_Builder* bc, Ast* arg) {
    int result;
    Type* type = arg->type;
    if (is_aggregate_type(type)) {
        if (type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace) {
            // Avoids copying inplace arrays
            result = emit_reference_expression(bc, arg);
            
        } else {
            // TODO: find ways to remove this copy for (codgen optimization)
            result = bc_local(bc, type);
            emit_initializing_expression(bc, arg, result);
        }
    } else {
        result = emit_value_fetch_expression(bc, arg);
    }
    return result;
}

void
emit_function_call(Bytecode_Builder* bc, Type* type, array(Ast*)* args, Ast* var_args,
                   int result_index, int function_ptr_index) {
    assert(type && type->kind == TypeKind_Function);
    
    if (type->Function.is_intrinsic) {
        bool handled = false;
        
        switch (type->Function.ident) {
            case Sym_rdtsc: {
                int* arg_indices = bc_intrinsic(bc, BC_X64_RDTSC, 1, 1);
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
                result_index = add_register(bc, type->Function.return_type);
            }
        }
        
        array_push(arg_indices, result_index);
    }
    
    int arg_count = (int) array_count(type->Function.arg_types);
    if (var_args && type->Function.is_variadic) {
        arg_count--;
    }
    for (int i = 0; i < arg_count; i++) {
        Ast* arg;
        if (i < array_count(args)) {
            // user provided arg
            arg = args[i];
        } else if (i >= type->Function.first_default_arg_index) {
            // default arg
            arg = type->Function.default_args[i - type->Function.first_default_arg_index];
        } else {
            arg = 0;
            verify_not_reached();
        }
        
        int reg_index = emit_function_argument(bc, arg);
        array_push(arg_indices, reg_index);
    }
    
    if (var_args && type->Function.is_variadic) {
        int var_args_ptr = add_register(bc);
        emit_value_expression(bc, var_args, var_args_ptr);
        array_push(arg_indices, var_args_ptr);
    }
    
    for (int i = arg_count; i < array_count(args); i++) {
        int reg_index = emit_function_argument(bc, args[i]);
        array_push(arg_indices, reg_index);
    }
    
    Compilation_Unit* cu = type->Function.unit;
    if (cu) {
        assert(cu->bytecode_function);
        Bytecode_Function* func = cu->bytecode_function;
        bc_call(bc, type->Function.return_type, func->type_index, arg_indices); 
    } else if (function_ptr_index != -1) {
        int ret_count = (is_valid_type(type->Function.return_type) &&
                         !is_aggregate_type(type->Function.return_type));
        bc_call_indirect(bc, type->Function.return_type, function_ptr_index, ret_count, arg_indices); 
    } else {
        verify_not_reached();
    }
    
    u32 caller_arg_count = (u32) array_count(arg_indices);
    if (is_valid_type(type->Function.return_type) && !is_aggregate_type(type->Function.return_type)) {
        caller_arg_count--;
    }
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
                dest_ptr = add_register(bc, t_void_ptr);
                bc_field_access(bc, dest_ptr, base_ptr, (int) field_info.offset);
            }
            emit_initializing_expression(bc, assign, dest_ptr);
            
#if 0
            if (is_aggregate_type(field_info.type)) {
                int src = emit_reference_expression(bc, assign);
                bc_instruction(bc, BC_MEMCPY, dest, src, field_info.type->size);
            } else {
                int src = add_register(bc, assign->type);
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
                int src = add_register(bc);
                bc_const_zero(bc, expr->type, src);
                bc_store(bc, dest_ptr, src);
            }
        } break;
        
        case Ast_Ident: {
            if (is_aggregate_type(expr->type)) {
                int src = emit_reference_expression(bc, expr);
                bc_memcpy(bc, dest_ptr, src, expr->type->size);
            } else {
                int src = emit_value_fetch_expression(bc, expr);
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
                int data = add_register(bc, t_void_ptr);
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
                int src_data_ptr = add_register(bc, t_void_ptr);
                bc_global(bc, src_data_ptr, global_index);
                
                bc_store(bc, dest_ptr, src_data_ptr);
                
                int src_count = add_register(bc);
                bc_const_int(bc, t_s64, src_count, string_count);
                int tmp = add_register(bc, t_void_ptr);
                bc_field_access(bc, tmp, dest_ptr, 8);
                bc_store(bc, tmp, src_count);
            } else {
                
                int src = add_register(bc, expr->type);
                emit_value_expression(bc, expr, src);
                bc_store(bc, dest_ptr, src);
            }
        } break;
        
        case Ast_Unary_Expr: {
            if (expr->Unary_Expr.op == Op_Dereference) {
                int src_ptr = emit_value_fetch_expression(bc, expr->Unary_Expr.first);
                bc_memcpy(bc, dest_ptr, src_ptr, expr->type->size);
            } else {
                if (is_aggregate_type(expr->type)) {
                    emit_value_expression(bc, expr, dest_ptr);
                } else {
                    int src_ptr = emit_value_fetch_expression(bc, expr);
                    bc_store(bc, dest_ptr, src_ptr);
                }
            }
        } break;
        
        case Ast_Binary_Expr: {
            //int src_ptr = add_register(bc, t_void_ptr);
            // TODO(Alexander): this is a bit awkward
            if (is_aggregate_type(expr->type)) {
                emit_value_expression(bc, expr, dest_ptr);
            } else {
                int src = emit_value_fetch_expression(bc, expr);
                bc_store(bc, dest_ptr, src);
            }
            //bc_memcpy(bc, dest_ptr, src_ptr, expr->type->size);
        } break;
        
        case Ast_Ternary_Expr: {
            bc_begin_block(bc);
            bc_begin_block(bc);
            int cond = add_register(bc, t_bool);
            emit_condition_expression(bc, expr->Ternary_Expr.first, cond, true);
            bc_branch_if(bc, bc->block_depth, cond);
            
            emit_initializing_expression(bc, expr->Ternary_Expr.second, dest_ptr);
            
            bc_branch(bc, bc->block_depth - 1);
            bc_end_block(bc);
            
            emit_initializing_expression(bc, expr->Ternary_Expr.third, dest_ptr);
            bc_end_block(bc);
        } break;
        
        case Ast_Cast_Expr: {
            Type* t_dest = normalize_type_for_casting(expr->type);
            Type* t_src = normalize_type_for_casting(expr->Cast_Expr.expr->type);
            
            if (t_dest->kind == TypeKind_Array && t_src->kind == TypeKind_Array) {
                emit_array_type_cast(bc, t_dest, t_src, expr->Cast_Expr.expr, dest_ptr);
                
            } else if ((t_dest->kind == TypeKind_Struct && t_src->kind == TypeKind_Struct) ||
                       (t_dest->kind == TypeKind_Union && t_src->kind == TypeKind_Union)) {
                emit_initializing_expression(bc, expr->Cast_Expr.expr, dest_ptr);
                //
            } else {
                int result = add_register(bc, t_dest);
                emit_type_cast(bc, expr, result);
            }
        } break;
        
        case Ast_Call_Expr: {
            emit_value_expression(bc, expr, dest_ptr);
        } break;
        
        case Ast_Index_Expr:
        case Ast_Field_Expr: {
            int src_ptr = emit_reference_expression(bc, expr);
            bc_memcpy(bc, dest_ptr, src_ptr, expr->type->size);
        } break;
        
        case Ast_Paren_Expr: {
            emit_initializing_expression(bc, expr->Paren_Expr.expr, dest_ptr);
        } break;
        
        default: unimplemented;
    }
}

void
emit_value_expression(Bytecode_Builder* bc, Ast* expr, int result) {
    int first_register = begin_tmp_scope(bc);
    
    switch (expr->kind) {
        case Ast_None: {
            bc_const_zero(bc, expr->type, result);
        } break;
        
        case Ast_Ident: {
            Value_or_Ref src = get_value_or_emit_reference_expression(bc, expr);
            if (src.is_ref) {
                bc_load(bc, expr->type, result, src.index);
            } else {
                bc_copy(bc, result, src.index);
            }
        } break;
        
        case Ast_Exported_Data: {
            int global_index = add_bytecode_global(bc, expr->Exported_Data);
            if (result != -1) {
                bc_global(bc, result, global_index);
            }
        } break;
        
        case Ast_Value: {
            Type* type = expr->type;
            if (is_integer(expr->Value)) {
                // TODO(Alexander): is value_to_s64 safe to use?
                bc_const_int(bc, type, result, value_to_s64(expr->Value));
                
            } else if (is_floating(expr->Value)) {
                if (type->size == 8) {
                    bc_const_f64(bc, result, (f64) expr->Value.data.floating);
                } else {
                    bc_const_f32(bc, result, (f32) expr->Value.data.floating);
                }
            } else if (is_cstring(expr->Value)) {
                if (expr->Value.data.cstr) {
                    smm string_count = cstring_count(expr->Value.data.cstr);
                    
                    int global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY,
                                                           string_count + 1, 1,
                                                           (void*) expr->Value.data.cstr);
                    bc_global(bc, result, global_index);
                } else {
                    bc_const_int(bc, t_void_ptr, result, 0);
                }
            } else {
                verify_not_reached();
            }
        } break;
        
        case Ast_Unary_Expr: {
            Type* result_type = expr->type;
            Type* type = expr->Unary_Expr.first->type;
            Operator op = expr->Unary_Expr.op;
            Bytecode_Type bcresult_type = to_bytecode_type(result_type);
            
            if (expr->Unary_Expr.overload) {
                array(Ast*)* args = 0;
                array_push(args, expr->Unary_Expr.first);
                emit_function_call(bc, expr->Unary_Expr.overload, args, 0, result, -1);
                array_free(args);
            } else {
                
                switch (op) {
                    case Op_Address_Of: {
                        Value_or_Ref addr = get_value_or_emit_reference_expression(bc, expr->Unary_Expr.first);
                        if (result != -1) {
                            if (addr.is_ref) {
                                bc_copy(bc, result, addr.index);
                            } else {
                                bc_lea(bc, result, addr.index);
                            }
                        }
                    } break;
                    
                    case Op_Dereference: {
                        assert(type->kind == TypeKind_Pointer);
                        int ptr = emit_value_fetch_expression(bc, expr->Unary_Expr.first);
                        if (result != -1) {
                            bc_load(bc, result_type, result, ptr);
                        }
                    } break;
                    
                    case Op_Pre_Increment:
                    case Op_Pre_Decrement: {
                        Value_or_Ref first = get_value_or_emit_reference_expression(bc, expr->Unary_Expr.first);
                        if (first.is_ref) {
                            int tmp = -1;
                            if (result == -1) {
                                tmp = add_register(bc);
                                result = tmp;
                            }
                            result = bc_load(bc, type, result, first.index);
                            emit_unary_increment(bc, type, result, op == Op_Pre_Increment);
                            bc_store(bc, first.index, result);
                            
                            if (tmp != -1) {
                                drop_register(bc, tmp);
                            }
                            
                        } else {
                            emit_unary_increment(bc, type, first.index, op == Op_Pre_Increment);
                            if (result != -1) {
                                bc_copy(bc, result, first.index);
                            }
                        }
                    } break;
                    
                    case Op_Post_Increment:
                    case Op_Post_Decrement: {
                        Value_or_Ref first = get_value_or_emit_reference_expression(bc, expr->Unary_Expr.first);
                        
                        if (first.is_ref) {
                            if (result != -1) {
                                bc_load(bc, type, result, first.index);
                            }
                            
                            int tmp = add_register(bc);
                            bc_load(bc, type, tmp, first.index);
                            emit_unary_increment(bc, type, tmp, op == Op_Post_Increment);
                            bc_store(bc, first.index, tmp);
                            drop_register(bc, tmp);
                            
                        } else {
                            if (result != -1) {
                                bc_copy(bc, result, first.index);
                            }
                            emit_unary_increment(bc, type, first.index, op == Op_Post_Increment);
                        }
                    } break;
                    
                    case Op_Bitwise_Not: {
                        emit_value_expression(bc, expr->Unary_Expr.first, result);
                        if (result != -1) {
                            bc_unary_arith(bc, bcresult_type, BC_NOT, result, result);
                        }
                    } break;
                    
                    case Op_Logical_Not: {
                        emit_value_expression(bc, expr->Unary_Expr.first, result);
                        if (result != -1) {
                            emit_zero_compare(bc, type, result, result, false);
                        }
                    } break;
                    
                    case Op_Negate: {
                        emit_value_expression(bc, expr->Unary_Expr.first, result);
                        if (result != -1) {
                            bc_unary_arith(bc, bcresult_type, BC_NEG, result, result);
                        }
                    } break;
                    
                    default: unimplemented;
                }
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
                emit_function_call(bc, expr->Binary_Expr.overload, args, 0, result, -1);
                array_free(args);
                
            } else if (op == Op_Logical_And) {
                bc_begin_block(bc);
                bc_begin_block(bc);
                
                int cond = add_register(bc, t_bool);
                emit_condition_expression(bc, expr->Binary_Expr.first, cond, true);
                bc_branch_if(bc, bc->block_depth, cond);
                
                emit_condition_expression(bc, expr->Binary_Expr.second, cond, true);
                bc_branch_if(bc, bc->block_depth, cond);
                
                bc_const_int(bc, t_bool, result, true);
                bc_branch(bc, bc->block_depth - 1);
                bc_end_block(bc);
                
                bc_const_int(bc, t_bool, result, false);
                bc_end_block(bc);
                
            } else if (op == Op_Logical_Or) {
                bc_begin_block(bc);
                bc_begin_block(bc);
                
                int cond = add_register(bc, t_bool);
                emit_condition_expression(bc, expr->Binary_Expr.first, cond, false);
                bc_branch_if(bc, bc->block_depth, cond);
                
                emit_condition_expression(bc, expr->Binary_Expr.second, cond, false);
                bc_branch_if(bc, bc->block_depth, cond);
                
                bc_const_int(bc, t_bool, result, false);
                bc_branch(bc, bc->block_depth - 1);
                bc_end_block(bc);
                
                bc_const_int(bc, t_bool, result, true);
                bc_end_block(bc);
                
            } else if (operator_is_assign(op)) {
                Bytecode_Operator opcode = to_bytecode_opcode(op, type);
                emit_assignment_expression(bc, opcode,
                                           expr->Binary_Expr.first,
                                           expr->Binary_Expr.second, result);
            } else {
                Bytecode_Operator opcode = to_bytecode_opcode(op, type);
                emit_binary_expression(bc, opcode, 
                                       expr->Binary_Expr.first,
                                       expr->Binary_Expr.second, result);
            }
        } break;
        
        case Ast_Ternary_Expr: {
            bc_begin_block(bc);
            bc_begin_block(bc);
            int cond = add_register(bc, t_bool);
            emit_condition_expression(bc, expr->Ternary_Expr.first, cond, true);
            bc_branch_if(bc, bc->block_depth, cond);
            
            emit_value_expression(bc, expr->Ternary_Expr.second, result);
            
            bc_branch(bc, bc->block_depth - 1);
            bc_end_block(bc);
            
            emit_value_expression(bc, expr->Ternary_Expr.third, result);
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
                function_ptr = add_register(bc, t_void_ptr);
                emit_value_expression(bc, expr->Call_Expr.ident, function_ptr);
            }
            
            emit_function_call(bc, type, args, expr->Call_Expr.var_args, result, function_ptr);
            array_free(args);
        } break;
        
        case Ast_Cast_Expr: {
            emit_type_cast(bc, expr, result);
        } break;
        
        case Ast_Index_Expr: {
            int ptr = emit_reference_expression(bc, expr);
            bc_load(bc, expr->type, result, ptr);
        } break;
        
        case Ast_Field_Expr: {
            int ptr = emit_reference_expression(bc, expr);
            
            Type* var_type = expr->Field_Expr.var->type;
            if (var_type->kind == TypeKind_Pointer) {
                var_type = var_type->Pointer;
            }
            
            //pln("a.b: %", f_type(expr->type));
            //pln("a  : %", f_type(var_type));
            Type* type = expr->type;
            if ((var_type->kind == TypeKind_Array && var_type->Array.kind == ArrayKind_Fixed_Inplace) ||
                (type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace)) {
                // NOTE: we avoid loading inplace arrays because they don't have data pointer
                bc_copy(bc, result, ptr);
            } else {
                bc_load(bc, expr->type, result, ptr);
            }
        } break;
        
        case Ast_Paren_Expr: {
            emit_value_expression(bc, expr->Paren_Expr.expr, result);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    end_tmp_scope(bc, first_register);
}

int
emit_value_fetch_expression(Bytecode_Builder* bc, Ast* expr) {
    // Fetch the value directly without making unnecessary copies
    int result;
    if (expr->kind == Ast_Ident) {
        Value_or_Ref src = get_value_or_emit_reference_expression(bc, expr);
        //assert(!src.is_ref); // TODO: should we even expect references here???
        
        if (src.is_ref) {
            int tmp = add_register(bc);
            result = bc_load(bc, expr->type, tmp, src.index);
        } else {
            result = src.index;
        }
        //result = src.index;
        
    } else {
        result = add_register(bc, expr->type);
        emit_value_expression(bc, expr, result);
    }
    
    return result;
}

inline void
emit_unary_increment(Bytecode_Builder* bc, Type* type, int result, bool increment) {
    if (type->kind == TypeKind_Pointer) {
        int index = add_register(bc);
        bc_const_int(bc, t_s64, index, increment ? 1 : -1);
        bc_array_access(bc, type->Pointer, result, result, index);
    } else {
        Bytecode_Type bc_result_type = to_bytecode_type(type);
        bc_unary_arith(bc, bc_result_type, increment ? BC_INC : BC_DEC, result, result);
    }
}

inline void
emit_binary_arithmetic(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                       Type* type, int result, int first, int second) {
    assert(opcode != BC_NOOP);
    
    if (type->kind == TypeKind_Pointer && (opcode == BC_ADD || opcode == BC_SUB)) {
        if (opcode == BC_SUB) {
            int tmp = add_register(bc, t_s64);
            Bytecode_Type tmp_type = to_bytecode_type(t_s64);
            bc_unary_arith(bc, tmp_type, BC_NEG, tmp, second);
            second = tmp;
        }
        
        if (result != -1) {
            bc_array_access(bc, type->Pointer, result, first, second);
        }
    } else {
        Bytecode_Type res_type;
        if (opcode >= BC_EQ && opcode <= BC_NEQ) {
            res_type = BC_BOOL;
        } else {
            res_type = to_bytecode_type(type);
        }
        bc_binary_arith(bc, res_type, opcode, result, first, second);
    }
}

inline void
emit_binary_expression(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                       Ast* lexpr, Ast* rexpr, int result) {
    int first = emit_value_fetch_expression(bc, lexpr);
    int second = emit_value_fetch_expression(bc, rexpr);
    emit_binary_arithmetic(bc, opcode, lexpr->type, result, first, second);
}


void
emit_assignment_expression(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                           Ast* lexpr, Ast* rexpr, int result) {
    
    int first_register = begin_tmp_scope(bc);
    
    Type* type = lexpr->type;
    if (is_aggregate_type(type)) {
        assert(opcode == BC_NOOP);
        int dest = emit_reference_expression(bc, lexpr);
        emit_initializing_expression(bc, rexpr, dest);
        
    } else {
        
        Value_or_Ref first = get_value_or_emit_reference_expression(bc, lexpr);
        if (opcode == BC_NOOP) {
            if (first.is_ref) {
                int second = emit_value_fetch_expression(bc, rexpr);
                bc_store(bc, first.index, second);
            } else {
                emit_value_expression(bc, rexpr, first.index);
            }
        } else {
            if (first.is_ref) {
                int first_val = add_register(bc);
                bc_load(bc, type, first_val, first.index);
                int second = emit_value_fetch_expression(bc, rexpr);
                emit_binary_arithmetic(bc, opcode, type, first_val, first_val, second);
                bc_store(bc, first.index, first_val);
                drop_register(bc, first_val);
                
            } else {
                int second = emit_value_fetch_expression(bc, rexpr);
                emit_binary_arithmetic(bc, opcode, type, first.index, first.index, second);
            }
        }
        
        if (result != -1) {
            if (first.is_ref) {
                bc_load(bc, type, result, first.index);
            } else {
                bc_copy(bc, result, first.index);
            }
        }
    }
    
#if 0
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
#endif
    
    end_tmp_scope(bc, first_register);
}

void
emit_type_cast(Bytecode_Builder* bc, Ast* expr, int result) {
    
    Type* t_dest = normalize_type_for_casting(expr->type);
    Type* t_src = normalize_type_for_casting(expr->Cast_Expr.expr->type);
    
    if (t_dest->kind == TypeKind_Void || t_src->kind == TypeKind_Void) {
        emit_value_fetch_expression(bc, expr->Cast_Expr.expr);
        return;
    }
    
    // TODO(Alexander): optimize by using a lookup table
    if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
        Bytecode_Operator opcode = BC_NOOP;
        if (t_dest->Basic.flags & BasicFlag_Boolean &&
            t_src->Basic.flags & BasicFlag_Integer) {
            emit_condition_expression(bc, expr->Cast_Expr.expr, result, false);
            return;
            
        } else if (t_dest->Basic.flags & BasicFlag_Integer &&
                   t_src->Basic.flags & BasicFlag_Integer) {
            if (t_dest->size < t_src->size) {
                opcode = BC_TRUNCATE;
                
            } else if (t_dest->size > t_src->size) {
                opcode = BC_EXTEND;
            } else {
                opcode = BC_COPY;
            }
            
        } else if (t_dest->Basic.flags & BasicFlag_Integer &&
                   t_src->Basic.flags & BasicFlag_Floating) {
            opcode = BC_FLOAT_TO_INT;
            
        } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                   t_src->Basic.flags & BasicFlag_Integer) {
            opcode = BC_INT_TO_FLOAT;
            
        } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                   t_src->Basic.flags & BasicFlag_Floating) {
            if (t_dest->size != t_src->size) {
                opcode = BC_FLOAT_TO_FLOAT;
            } else {
                opcode = BC_COPY;
            }
            
        } else {
            verify_not_reached();
        }
        
        if (opcode != BC_NOOP) {
            int src = emit_value_fetch_expression(bc, expr->Cast_Expr.expr);
            bc_cast(bc, opcode, t_dest, result, src);
        } else {
            verify_not_reached();
        }
        
    } else {
        verify_not_reached();
    }
}

inline void
emit_array_type_cast(Bytecode_Builder* bc, Type* t_dest, Type* t_src, Ast* src_ast, int array_ptr) {
    
    int src_ptr = emit_reference_expression(bc, src_ast);
    if (t_dest->Array.kind == ArrayKind_Fixed &&
        t_src->Array.kind == ArrayKind_Fixed_Inplace) {
        
        // Converts inplace fixed array to "wide"-pointer array
        bc_store(bc, array_ptr, src_ptr);
        
        int count = add_register(bc);
        bc_const_int(bc, t_s64, count, t_src->Array.capacity);
        int count_ptr = add_register(bc, t_void_ptr);
        bc_field_access(bc, count_ptr, array_ptr, 8);
        bc_store(bc, count_ptr, count);
    } else {
        unimplemented;
    }
}

inline void
emit_zero_compare(Bytecode_Builder* bc, Type* type, int result, int value, bool invert_condition) {
    int zero = add_register(bc, type);
    bc_const_zero(bc, type, zero);
    bc_binary_arith(bc, BC_BOOL, invert_condition ? BC_NEQ : BC_EQ, result, value, zero);
}

void
emit_condition_expression(Bytecode_Builder* bc, Ast* cond, int result, bool invert_condition) {
    Type* type = cond->type;
    
    while (cond->kind == Ast_Paren_Expr) {
        cond = cond->Paren_Expr.expr;
    }
    if (cond->kind == Ast_Unary_Expr && cond->Unary_Expr.op == Op_Logical_Not) {
        invert_condition = !invert_condition;
        cond = cond->Unary_Expr.first;
    }
    while (cond->kind == Ast_Paren_Expr) {
        cond = cond->Paren_Expr.expr;
    }
    
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
        emit_zero_compare(bc, type, result, result, !invert_condition);
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
            if (stmt->Assign_Stmt.mods & AstDeclModifier_Local_Persist) {
                int global_index = add_bytecode_global(bc, BC_MEM_READ_WRITE, type->size, type->align, 0, 
                                                       stmt->Assign_Stmt.expr);
                local.is_ref = true;
                local.index = add_register(bc);
                bc_global(bc, local.index, global_index);
                
            } else {
                if (is_aggregate_type(type)) {
                    local.is_ref = true;
                    local.index = bc_local(bc, type);
                    emit_initializing_expression(bc, stmt->Assign_Stmt.expr, local.index);
                } else {
                    local.index = add_register(bc);
                    emit_value_expression(bc, stmt->Assign_Stmt.expr, local.index);
                }
            }
            
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            map_put(bc->locals, ident, local);
            //array_push(array_last(bc->block_scopes), local.index);
        } break;
        
        case Ast_Block_Stmt: {
            int first_register = begin_tmp_scope(bc);
            //array_push(bc->block_scopes, 0);
            
            for_compound(stmt->Block_Stmt.stmts, it) {
                emit_statement(bc, it, break_label, continue_label);
            }
            
            //array(int)* locals = array_pop(bc->block_scopes);
            end_tmp_scope(bc, first_register);
            //for_array_v(locals, index, _) {
            //drop_register(bc, index);
            //}
        } break;
        
        case Ast_If_Stmt: {
            bc_begin_block(bc);
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                bc_begin_block(bc);
            }
            
            int cond = add_register(bc, t_bool);
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
                int cond = add_register(bc, t_bool);
                emit_condition_expression(bc, stmt->For_Stmt.cond, cond, true);
                bc_branch_if(bc, bc->block_depth - 1, cond);
            }
            
            // Block
            bc_begin_block(bc);
            emit_statement(bc, stmt->For_Stmt.block, bc->block_depth - 2, bc->block_depth);
            bc_end_block(bc);
            
            // Update
            if (is_valid_ast(stmt->For_Stmt.update)) {
                emit_value_expression(bc, stmt->For_Stmt.update, -1);
            }
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
                int cond = add_register(bc, t_bool);
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
            int first_register = bc_begin_block(bc);
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
            
            int case_cond = add_register(bc, t_bool);
            int multi_case_block = -1;
            for_compound(stmt->Switch_Stmt.cases, it) {
                if (is_valid_ast(it->Switch_Case.cond)) {
                    emit_value_expression(bc, it->Switch_Case.cond, case_cond);
                    
                    if (is_valid_ast(it->Switch_Case.stmt)) {
                        bc_binary_arith(bc, BC_BOOL, BC_NEQ, case_cond, switch_cond, case_cond);
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
                        
                        bc_binary_arith(bc, BC_BOOL, BC_EQ, case_cond, switch_cond, case_cond);
                        bc_branch_if(bc, multi_case_block, case_cond);
                    }
                }
            }
            
            if (default_stmt) {
                emit_statement(bc, default_stmt, break_label, continue_label);
            }
            
            bc_end_block(bc, first_register);
            
        } break;
        
        case Ast_Return_Stmt: {
            int first_register = begin_tmp_scope(bc);
            int result = -1;
            if (is_valid_ast(stmt->Return_Stmt.expr)) {
                if (bc->curr_function->return_as_first_arg) {
                    // NOTE: the return register will always be at index 0
                    emit_initializing_expression(bc, stmt->Return_Stmt.expr, 0);
                } else {
                    result = emit_value_fetch_expression(bc, stmt->Return_Stmt.expr);
                }
            }
            bc_return(bc, stmt->type, result);
            end_tmp_scope(bc, first_register);
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

void
emit_initializer_function(Bytecode_Builder* bc) {
    Type* return_type = normalize_basic_types(t_int);
    
    Bytecode_Function* main_func = bc->bytecode.functions[bc->bytecode.entry_func_index];
    Bytecode_Function* func = 0;
    for_array(bc->bytecode.globals, it, global_index) {
        Ast* expr = it->initializer;
        if (expr) {
            if (!func) {
                Type func_sig = {};
                func_sig.kind = TypeKind_Function;
                func_sig.Function.return_type = return_type;
                
                func = add_bytecode_function(bc, &func_sig);
                // TODO(Alexander): this is a copy from emit_function
                bc->bytecode.entry_func_index = func->type_index;
                Bytecode_Export init_export = {};
                init_export.function = Sym___start;
                init_export.func_index = func->type_index;
                array_push(bc->bytecode.exports, init_export);
                
                // Build the function
                // TODO(Alexander): this is a copy from emit_function
                map_free(bc->locals);
                bc->curr_function = func;
                bc->curr_insn = 0;
                bc->block_depth = 0;
                bc->next_register = 0; 
            }
            
            
            int dest_ptr = add_register(bc);
            bc_global(bc, dest_ptr, global_index);
            if (is_aggregate_type(expr->type)) {
                emit_initializing_expression(bc, expr, dest_ptr);
            } else {
                int tmp = add_register(bc);
                emit_value_expression(bc, expr, tmp);
                bc_store(bc, dest_ptr, tmp);
                drop_register(bc, tmp);
            }
            drop_register(bc, dest_ptr);
        }
    }
    
    if (func) {
        // Call main and return the result
        int result = add_register(bc);
        array(int)* args = 0;
        array_push(args, result);
        bc_call(bc, return_type, main_func->type_index, args);
        bc_return(bc, return_type, result);
        drop_register(bc, result);
    }
}

bool
type_equals(Bytecode_Type a, Bytecode_Type b) {
    bool same = a.kind == b.kind;
    if (same) {
        bool same_size = a.size == b.size;
        switch (a.kind) {
            case BC_TYPE_INT: {
                bool same_sign = (a.flags & BC_FLAG_SIGNED) == (b.flags & BC_FLAG_SIGNED);
                same = same_sign && same_size;
            } break;
            
            case BC_TYPE_FLOAT: {
                same = same_size;
            } break;
        }
    }
    
    return same;
}

struct Bytecode_Register {
    Bytecode_Instruction* init;
    Bytecode_Type type;
    int uses;
};

struct Bytecode_Validation {
    Bytecode_Register* registers;
    s32 register_count;
    s32 error_count;
};

inline void
bc_type_error(Bytecode_Validation* bc_valid, string message, Bytecode_Operator opcode=BC_NOOP) {
    if (opcode > BC_NOOP) {
        pln("error: % %", f_cstring(bc_operator_names[opcode]), f_string(message));
    } else {
        pln("error: %", f_string(message));
    }
    bc_valid->error_count++;
}

inline void
bc_type_error_mismatch(Bytecode_Validation* bc_valid, Bytecode_Type expect, Bytecode_Type found,
                       Bytecode_Operator opcode=BC_NOOP) {
    bc_type_error(bc_valid,
                  string_print("expected type `%`, found `%`", f_bc_type(expect), f_bc_type(found)),
                  opcode);
}

inline void
assert_type_equals(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn,
                   Bytecode_Type expect, Bytecode_Type found) {
    if (!type_equals(expect, found)) {
        bc_type_error_mismatch(bc_valid, expect, found, insn->opcode);
    }
}

inline void
assert_signed_int(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn, Bytecode_Type type) {
    if (!(type.kind == BC_TYPE_INT && (type.flags & BC_FLAG_SIGNED))) {
        bc_type_error(bc_valid, string_print("expected signed int, found `%`", f_bc_type(type)), insn->opcode);
    }
}

inline void
assert_unsigned_int_or_float(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn, Bytecode_Type type) {
    if (!((type.kind == BC_TYPE_INT && !(type.flags & BC_FLAG_SIGNED)) ||
          type.kind == BC_TYPE_FLOAT)) {
        bc_type_error(bc_valid, string_print("expected unsigned int or float, found `%`", f_bc_type(type)), insn->opcode);
    }
}

inline void
assert_non_void_type(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn, Bytecode_Type type) {
    if (type.kind == BC_TYPE_VOID) {
        bc_type_error(bc_valid, string_print("expected non-void type, found `%`", f_bc_type(type)), insn->opcode);
    }
}

inline void
assert_int_type(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn, Bytecode_Type type) {
    if (type.kind != BC_TYPE_INT) {
        bc_type_error(bc_valid, string_print("expected int, found `%`", f_bc_type(type)), insn->opcode);
    }
}

inline void
assert_float_type(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn, Bytecode_Type type) {
    if (type.kind != BC_TYPE_FLOAT) {
        bc_type_error(bc_valid, string_print("expected float, found `%`", f_bc_type(type)), insn->opcode);
    }
}

inline Bytecode_Type
register_type(Bytecode_Validation* bc_valid, int index) {
    if (index < 0 || index >= bc_valid->register_count) {
        bc_type_error(bc_valid, string_print("invalid register r%", f_int(index)));
        return BC_VOID;
    }
    Bytecode_Register* r = &bc_valid->registers[index];
    if (!r->init) {
        bc_type_error(bc_valid, string_print("use of unallocated register r%", f_int(index)));
        return BC_VOID;
    }
    r->uses++;
    return r->type;
}

inline Bytecode_Register*
drop_register(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn, int index) {
    if (index < 0 || index >= bc_valid->register_count) {
        bc_type_error(bc_valid, string_lit("invalid register found"), insn->opcode);
    }
    
    Bytecode_Register* r = &bc_valid->registers[index];
    if (r->init) {
        if (r->uses == 0) {
            //r->init->opcode = BC_NOOP;
            bc_type_error(bc_valid, string_print("register r% is a dead store", f_int(index)), insn->opcode);
            
        } else if (r->uses == 1) {
            r->init->type.flags |= BC_FLAG_UNIQUE_REGISTER;
            //bc_type_error(bc_valid, string_print("register r% is unique", f_int(index)), insn->opcode);
            
        } else {
            // Need to mark this as shared register
            //bc_type_error(bc_valid, string_print("register r% is not unique", f_int(index)), insn->opcode);
        }
    }
    r->init = 0;
    
    return r;
}

inline void
allocate_register(Bytecode_Validation* bc_valid, Bytecode_Instruction* insn, int index, Bytecode_Type type) {
    if (index < 0 || index >= bc_valid->register_count) {
        bc_type_error(bc_valid, string_lit("invalid register found"), insn->opcode);
    }
    
    Bytecode_Register* r = &bc_valid->registers[index];
    r->type = type;
    r->uses = (r->init != 0) ? (r->uses + 1) : 0; // if we reuse register => count it as a use instead!
    r->init = insn;
}

void
validate_bytecode_function(Bytecode* bytecode, Bytecode_Function* func, string_id func_ident) {
    Bytecode_Validation bytecode_validation = {};
    Bytecode_Validation* bc_valid = &bytecode_validation;
    Bytecode_Function_Arg* func_ret_args = function_ret_types(func);
    
    bc_valid->registers = New_Array(Bytecode_Register, func->register_count);
    bc_valid->register_count = func->register_count;
    
    for_bc_insn(func, insn) {
        Bytecode_Binary* bc = (Bytecode_Binary*) insn;
        switch (bc->opcode) {
            case BC_NOOP:
            case BC_BLOCK:
            case BC_LOOP:
            case BC_END:
            case BC_DEBUG_BREAK: break;
            
            case BC_DROP: {
                drop_register(bc_valid, insn, bc->res_index);
            } break;
            
            case BC_INT_CONST: {
                assert_int_type(bc_valid, insn, bc->type);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_F32_CONST:
            case BC_F64_CONST: {
                assert_float_type(bc_valid, insn, bc->type);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_LOCAL:
            case BC_GLOBAL:
            case BC_FUNCTION: {
                assert_type_equals(bc_valid, insn, BC_PTR, bc->type);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_LEA: {
                assert_non_void_type(bc_valid, insn, register_type(func, bc->res_index));
                allocate_register(bc_valid, insn, bc->res_index, BC_PTR);
            } break;
            
            case BC_MEMCPY: {
                assert_type_equals(bc_valid, insn, BC_PTR, register_type(func, bc->res_index));
                assert_type_equals(bc_valid, insn, BC_PTR, register_type(func, bc->a_index));
                assert_int_type(bc_valid, insn, register_type(func, bc->b_index));
            } break;
            
            case BC_MEMSET: {
                assert_type_equals(bc_valid, insn, BC_PTR, register_type(func, bc->res_index));
                assert_int_type(bc_valid, insn, register_type(func, bc->a_index));
                assert_int_type(bc_valid, insn, register_type(func, bc->b_index));
            } break;
            
            case BC_CALL: {
                Bytecode_Call* call = (Bytecode_Call*) bc;
                int* args = bc_call_args(call);
                Bytecode_Function* target = bytecode->functions[call->func_index];
                Bytecode_Function_Arg* formal_args = function_ret_types(target);
                
                for (int i = target->ret_count; i < call->arg_count; i++) {
                    Bytecode_Type expect = formal_args[i].type;
                    Bytecode_Type found = register_type(bc_valid, args[i]);
                    assert_type_equals(bc_valid, insn, expect, found);
                }
                
                if (target->ret_count) {
                    allocate_register(bc_valid, insn, args[0], bc->type);
                }
                // TODO: Check argument types!
                //assert_type_equals(bc_valid, insn, BC_PTR, base_type);
            } break;
            
            case BC_CALL_INDIRECT: {
                Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) bc;
                int* args = bc_call_args(call);
                for (int i = call->ret_count; i < call->arg_count; i++) {
                    register_type(bc_valid, args[i]);
                }
                if (call->ret_count) {
                    allocate_register(bc_valid, insn, args[0], bc->type);
                }
            } break;
            
            case BC_FIELD_ACCESS: {
                Bytecode_Field_Access* field_access = (Bytecode_Field_Access*) insn;
                Bytecode_Type base_type = register_type(bc_valid, field_access->base);
                assert_type_equals(bc_valid, insn, BC_PTR, base_type);
                if (field_access->offset == 0) {
                    bc_type_error(bc_valid, string_lit("empty offset is a noop"), insn->opcode);
                }
                assert_type_equals(bc_valid, insn, BC_PTR, bc->type);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_ARRAY_ACCESS: {
                Bytecode_Array_Access* array_access = (Bytecode_Array_Access*) insn;
                
                Bytecode_Type base_type = register_type(bc_valid, array_access->base);
                assert_type_equals(bc_valid, insn, BC_PTR, base_type);
                
                Bytecode_Type index_type = register_type(bc_valid, array_access->index);
                assert_type_equals(bc_valid, insn, bc_type(BC_TYPE_INT, BC_FLAG_SIGNED, 8), index_type);
                
                if (array_access->stride == 0) {
                    bc_type_error(bc_valid, string_lit("stride = 0 is not valid"), insn->opcode);
                }
                
                assert_type_equals(bc_valid, insn, BC_PTR, bc->type);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_LOAD: {
                Bytecode_Type src_type = register_type(bc_valid, bc->a_index);
                assert_type_equals(bc_valid, insn, BC_PTR, src_type);
                assert_non_void_type(bc_valid, insn, bc->type);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_STORE: {
                Bytecode_Type dest_type = register_type(bc_valid, bc->res_index);
                assert_type_equals(bc_valid, insn, BC_PTR, dest_type);
            } break;
            
            case BC_COPY: {
                Bytecode_Type src_type = register_type(bc_valid, bc->a_index);
                bc->type = bc->type.kind != BC_TYPE_VOID ? bc->type : src_type;
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_NEG: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                assert_non_void_type(bc_valid, insn, a);
                allocate_register(bc_valid, insn, bc->res_index, a);
            } break;
            
            case BC_INC:
            case BC_DEC:
            case BC_NOT: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                assert_int_type(bc_valid, insn, a);
                allocate_register(bc_valid, insn, bc->res_index, a);
            } break;
            
            case BC_AND:
            case BC_OR:
            case BC_XOR:
            case BC_SHL:
            case BC_SHR:
            case BC_SAR:{
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                Bytecode_Type b = register_type(bc_valid, bc->b_index);
                assert_int_type(bc_valid, insn, a);
                assert_int_type(bc_valid, insn, b);
                allocate_register(bc_valid, insn, bc->res_index, a);
            } break;
            
            case BC_ADD:
            case BC_SUB:
            case BC_MUL: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                Bytecode_Type b = register_type(bc_valid, bc->b_index);
                assert_type_equals(bc_valid, insn, a, b);
                assert_type_equals(bc_valid, insn, bc->type, a);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_DIV_S:
            case BC_MOD_S: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                Bytecode_Type b = register_type(bc_valid, bc->b_index);
                assert_signed_int(bc_valid, insn, a);
                assert_type_equals(bc_valid, insn, a, b);
                assert_type_equals(bc_valid, insn, bc->type, a);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_DIV_U:
            case BC_MOD_U: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                Bytecode_Type b = register_type(bc_valid, bc->b_index);
                assert_unsigned_int_or_float(bc_valid, insn, a);
                assert_type_equals(bc_valid, insn, a, b);
                assert_type_equals(bc_valid, insn, bc->type, a);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_EQ:
            case BC_NEQ: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                Bytecode_Type b = register_type(bc_valid, bc->b_index);
                assert_type_equals(bc_valid, insn, a, b);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_GT_S:
            case BC_GE_S:
            case BC_LT_S:
            case BC_LE_S: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                Bytecode_Type b = register_type(bc_valid, bc->b_index);
                assert_signed_int(bc_valid, insn, a);
                assert_type_equals(bc_valid, insn, a, b);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_GT_U:
            case BC_GE_U:
            case BC_LT_U:
            case BC_LE_U: {
                Bytecode_Type a = register_type(bc_valid, bc->a_index);
                Bytecode_Type b = register_type(bc_valid, bc->b_index);
                assert_unsigned_int_or_float(bc_valid, insn, a);
                assert_type_equals(bc_valid, insn, a, b);
                allocate_register(bc_valid, insn, bc->res_index, bc->type);
            } break;
            
            case BC_EXTEND: {
                Bytecode_Type src_type = register_type(bc_valid, bc->a_index);
                Bytecode_Type dest_type = bc->type;
                assert_int_type(bc_valid, insn, src_type);
                assert_int_type(bc_valid, insn, dest_type);
                if (src_type.size > dest_type.size) {
                    bc_type_error(bc_valid, string_print("cannot convert `%` to `%`", 
                                                         f_bc_type(src_type), f_bc_type(dest_type)), insn->opcode);
                }
                // TODO(Alexander): test sizes
                allocate_register(bc_valid, insn, bc->res_index, dest_type);
            } break;
            
            case BC_TRUNCATE: {
                Bytecode_Type src_type = register_type(bc_valid, bc->a_index);
                Bytecode_Type dest_type = bc->type;
                assert_int_type(bc_valid, insn, src_type);
                assert_int_type(bc_valid, insn, dest_type);
                if (src_type.size < dest_type.size) {
                    bc_type_error(bc_valid, string_print("cannot convert `%` to `%`", 
                                                         f_bc_type(src_type), f_bc_type(dest_type)), insn->opcode);
                }
                allocate_register(bc_valid, insn, bc->res_index, dest_type);
            } break;
            
            case BC_FLOAT_TO_INT: {
                Bytecode_Type src_type = register_type(bc_valid, bc->a_index);
                Bytecode_Type dest_type = bc->type;
                assert_float_type(bc_valid, insn, src_type);
                assert_int_type(bc_valid, insn, dest_type);
                allocate_register(bc_valid, insn, bc->res_index, dest_type);
            } break;
            
            case BC_INT_TO_FLOAT: {
                Bytecode_Type src_type = register_type(bc_valid, bc->a_index);
                Bytecode_Type dest_type = bc->type;
                assert_int_type(bc_valid, insn, src_type);
                assert_float_type(bc_valid, insn, dest_type);
                allocate_register(bc_valid, insn, bc->res_index, dest_type);
            } break;
            
            case BC_FLOAT_TO_FLOAT: {
                Bytecode_Type src_type = register_type(bc_valid, bc->a_index);
                Bytecode_Type dest_type = bc->type;
                assert_float_type(bc_valid, insn, src_type);
                assert_float_type(bc_valid, insn, dest_type);
                allocate_register(bc_valid, insn, bc->res_index, dest_type);
            } break;
            
            case BC_BRANCH: {
                Bytecode_Branch* branch = (Bytecode_Branch*) insn;
                if (branch->cond > 0) {
                    Bytecode_Type cond = register_type(bc_valid, branch->cond);
                    assert_type_equals(bc_valid, insn, BC_BOOL, cond);
                }
                // TODO: check the branch maps to real basic block
            } break;
            
            case BC_RETURN: {
                Bytecode_Type expect = func_ret_args[0].type;
                
                if (func->ret_count == 0) {
                    if (bc->res_index >= 0) {
                        bc_type_error(bc_valid,
                                      string_print("function `%` did not expect any return value", f_var(func_ident)));
                    }
                    
                } else if (func->ret_count == 1) {
                    if (bc->res_index >= 0) {
                        Bytecode_Type found = register_type(bc_valid, bc->res_index);
                        assert_type_equals(bc_valid, insn, expect, found);
                    } else {
                        bc_type_error(bc_valid,
                                      string_print("function `%` expected return value `%`", f_var(func_ident), f_bc_type(expect)));
                    }
                    
                } else {
                    verify_not_reached();
                }
            } break;
            
            default: unimplemented;
        }
    }
    
    if (bc_valid->error_count > 0) {
        pln("result: function `%` compiled with % error(s)\n", f_var(func_ident), f_int(bc_valid->error_count));
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
    //pln("Emitting function `%`", f_var(ast_unwrap_ident(ast->Decl_Stmt.ident)));
    
    map_free(bc->locals);
    
    Type* type = ast->type;
    
    // Build the function
    bc->curr_function = func;
    bc->curr_insn = 0;
    bc->block_depth = 0;
    bc->next_register = func->arg_count;
    if (is_main) {
        bc->bytecode.entry_func_index = func->type_index;
        Bytecode_Export main_export = {};
        main_export.function = ast_unwrap_ident(ast->Decl_Stmt.ident);
        main_export.func_index = func->type_index;
        array_push(bc->bytecode.exports, main_export);
    }
    
    for_array_v(type->Function.arg_idents, arg_ident, i) {
        int arg_index = func->return_as_first_arg ? (i + 1) : i;
        Type* arg_type = type->Function.arg_types[i];
        
        Bc_Local arg = {};
        arg.index = arg_index;
        //arg.is_ref = is_aggregate_type(arg_type);
        map_put(bc->locals, arg_ident, arg);
    }
    
    if (insert_debug_break) {
        bc_intrinsic(bc, BC_DEBUG_BREAK, 0, 0);
    }
    
    emit_statement(bc, ast->Decl_Stmt.stmt, 0, 0);
    
    //for_map(bc->locals, local) {
    //pln("%: r% (is_ref = %)", f_var(local->key), f_int(local->value.index), f_bool(local->value.is_ref));
    //}
    
    return func;
}

void
validate_bytecode(Bytecode* bytecode) {
    for_array_v(bytecode->functions, func, _) {
        if (!func->is_imported) {
            string_id ident = bytecode->function_names[func->type_index];
            validate_bytecode_function(bytecode, func, ident);
        }
    }
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
    func->is_variadic = type->Function.is_variadic;
    func->is_intrinsic = type->Function.is_intrinsic;
    
    bc->curr_function = func;
    bc->curr_insn = 0;
    
    if (type->Function.unit) {
        type->Function.unit->bytecode_function = func;
    }
    
    Bytecode_Function_Arg* curr_arg = function_ret_types(func);
    if (ret_count == 1) {
        Type* ret_type = type->Function.return_type;
        if (is_aggregate_type(ret_type)) {
            add_register(bc, type->Function.return_type);
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
        add_register(bc, arg_type);
        curr_arg++;
    }
    
    array_push(bc->bytecode.functions, func);
    array_push(bc->bytecode.function_names, type->Function.ident);
    
    return func;
}

int
add_bytecode_global(Bytecode_Builder* bc, Exported_Data exported) {
    Bytecode_Global global_var = {};
    switch (exported.section) {
        case Read_Data_Section: global_var.kind = BC_MEM_READ_ONLY; break;
        case Data_Section: global_var.kind = BC_MEM_READ_WRITE; break;
        default: unimplemented;
    }
    global_var.address = exported.data;
    global_var.offset = exported.relative_ptr;
    global_var.size = 0;//(u32) size; TODO: maybe we should include this in Exported_Data?
    global_var.align = 0;//(u32) align;
    
    int global_index = (int) array_count(bc->bytecode.globals);
    array_push(bc->bytecode.globals, global_var);
    return global_index;
}

int
add_bytecode_global(Bytecode_Builder* bc, 
                    Bytecode_Memory_Kind kind, 
                    smm size, smm align, void* init, Ast* initializer) {
    
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
    global_var.initializer = initializer;
    
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
string_builder_dump_bytecode_call_args(String_Builder* sb, int arg_count, int* args, int skip_count=0) {
    // args
    string_builder_push(sb, "(");
    for (int i = skip_count; i < arg_count; i++) {
        string_builder_push_format(sb, "r%", f_int(args[i]));
        if (i + 1 < arg_count) {
            string_builder_push(sb, ", ");
        }
    }
    string_builder_push(sb, ")");
}

void
string_builder_dump_bytecode_type(String_Builder* sb, Bytecode_Type type) {
    switch (type.kind) {
        case BC_TYPE_VOID: {
            string_builder_push(sb, "void");
        } break;
        
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
print_bytecode_type(Bytecode_Type type) {
    String_Builder sb = {};
    string_builder_alloc(&sb, 3);
    string_builder_dump_bytecode_type(&sb, type);
    string result = string_builder_to_string_nocopy(&sb);
    print("%", f_string(result));
    string_builder_free(&sb);
}

inline void
string_builder_dump_bytecode_opcode(String_Builder* sb, Bytecode_Instruction* insn) {
    if (insn->type.kind != BC_TYPE_VOID) {
        string_builder_dump_bytecode_type(sb, insn->type);
        string_builder_push(sb, ".");
    }
    string_builder_push(sb, bc_operator_names[insn->opcode]);
    string_builder_push(sb, " ");
}

void
string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn, int block_depth) {
    
    smm from_byte = sb->curr_used;
    string_builder_pad(sb, sb->curr_used, block_depth*2);
    
    
    switch (insn->opcode) {
        case BC_NOOP: {
            string_builder_push_format(sb, "noop");
        } break;
        
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
            string_builder_dump_bytecode_call_args(sb, call->arg_count, args, func->ret_count);
        } break;
        
        case BC_X64_RDTSC:
        case BC_CALL_INDIRECT: {
            Bytecode_Call_Indirect* call = (Bytecode_Call_Indirect*) insn;
            int* args = bc_call_args(call);
            if (call->ret_count == 1) {
                string_builder_push_format(sb, "r% = ", f_int(args[0]));
            }
            string_builder_dump_bytecode_opcode(sb, insn);
            
            if (call->func_ptr_index >= 0) {
                string_builder_push_format(sb, "r%", f_int(call->func_ptr_index));
            }
            string_builder_dump_bytecode_call_args(sb, call->arg_count, args, call->ret_count);
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
        case BC_LEA:
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
        
        case BC_RETURN:
        case BC_DROP:{
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
    
    if (insn->type.flags & BC_FLAG_UNIQUE_REGISTER) {
        string_builder_push_format(sb, " (r% is unique)", f_int(((Bytecode_Binary*) insn)->res_index));
    }
    
    if (insn->comment) {
        string_builder_pad(sb, from_byte, 25);
        string_builder_push_format(sb, " // %", f_cstring(insn->comment));
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
        if (i + 1 < func->arg_count || func->is_variadic) {
            string_builder_push(sb, ", ");
        }
    }
    if (func->is_variadic) {
        string_builder_push(sb, "...");
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