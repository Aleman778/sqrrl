
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
                    local.ptr = bc_instruction_local(bc, expr->type);
                    bc_instruction_store(bc, local.ptr, local.val);
                    bc->locals[local_index].value = local;
                }
                result = local.ptr;
                
            } else if (type->kind == TypeKind_Function && type->Function.unit) {
                Bytecode_Function* func = type->Function.unit->bytecode_function;
                result = add_bytecode_register(bc, t_void_ptr);
                bc_instruction(bc, BC_FUNCTION, result, func->type_index, -1);
                
            } else {
                smm global_index = map_get_index(bc->globals, ident);
                if (global_index != -1) {
                    int global_id = bc->globals[global_index].value;
                    result = add_bytecode_register(bc, t_void_ptr);
                    bc_instruction_global(bc, result, global_id);
                    
                } else {
                    verify_not_reached();
                }
            }
        } break;
        
        case Ast_Unary_Expr: {
            if (expr->Unary_Expr.op == Op_Dereference) {
                result = emit_reference_expression(bc, expr->Unary_Expr.first);
                int tmp = add_bytecode_register(bc, expr->Unary_Expr.first->type);
                bc_instruction_load(bc, tmp, result);
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Cast_Expr: {
            result = add_bytecode_register(bc, expr->type);
            emit_type_cast(bc, expr, result);
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
                unimplemented;
                //int tmp = add_bytecode_register(bc, type);
                //result = bc_instruction_load(bc, tmp, result);
            }
            
            switch (type->kind) {
                case TypeKind_Struct:
                case TypeKind_Union: {
                    Struct_Field_Info field = get_field_info(&type->Struct_Like, ident);
                    
                    int tmp = add_bytecode_register(bc, t_void_ptr);
                    bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, (int) field.offset);
                    result = tmp;
                } break;
                
                case TypeKind_Array:
                case TypeKind_Basic: {
                    assert((type->kind == TypeKind_Array ||
                            type->Basic.kind == Basic_string) && "unsupported type");
                    
                    if (ident == Sym_data) {
                        if (!(type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace)) {
                            int tmp = add_bytecode_register(bc, type);
                            bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, 0);
                            result = tmp;
                        } else {
                            //bc->curr_function->register_types[result].flags |= BC_FLAG_RVALUE;
                        }
                        
                    } else if (ident == Sym_count) {
                        int tmp = add_bytecode_register(bc, type);
                        bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, 8);
                        result = tmp;
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
                array_type == t_cstring) {
                int tmp = add_bytecode_register(bc, array_type);
                array_ptr = bc_instruction_load(bc, tmp, array_ptr);
                
            } else if (array_type == t_string) {
                int tmp = add_bytecode_register(bc, t_cstring);
                bc_instruction(bc, BC_FIELD_ACCESS, tmp, array_ptr, 0);
                int tmp2 = add_bytecode_register(bc, array_type);
                array_ptr = bc_instruction_load(bc, tmp2, tmp);
            }
            
            int array_index = add_bytecode_register(bc, expr->Index_Expr.index->type);
            emit_value_expression(bc, expr->Index_Expr.index, array_index);
            result = add_bytecode_register(bc, expr->Index_Expr.array->type);
            bc_instruction_array_accesss(bc, type, result, array_ptr, array_index);
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

void
emit_function_call_to_bytecode(Bytecode_Builder* bc, Type* type, array(Ast*)* args, 
                               int result, int function_ptr) {
    assert(type && type->kind == TypeKind_Function);
    
    if (type->Function.is_intrinsic) {
        bool handled = false;
        
        switch (type->Function.ident) {
            case Sym_rdtsc: {
                bc_instruction(bc, BC_X64_RDTSC, result, -1, -1);
                handled = true;
            } break;
            
            case Sym_debug_break: {
                bc_instruction(bc, BC_DEBUG_BREAK, -1, -1, -1);
                handled = true;
            } break;
        }
        
        if (handled) {
            return;
        }
    }
    
    int arg_count = 0;
    array(int)* arg_operands = 0;
    
    bool ret_as_first_arg = false;
    Type* ret_type = type->Function.return_type;
    if (ret_type && is_aggregate_type(ret_type)) {
        array_push(arg_operands, result);
        ret_as_first_arg = true;
        arg_count++;
    }
    
    for_array_v(args, arg, _) {
        Type* arg_type = arg->type;
        int reg = -1;
        if (is_aggregate_type(arg_type)) {
            // Create copy (except for ARRAY types) and pass it via ptr
            reg = emit_reference_expression(bc, arg);
        } else {
            reg = add_bytecode_register(bc, arg_type);
            emit_value_expression(bc, arg, reg);
        }
        
        assert(reg >= 0);
        array_push(arg_operands, reg);
        arg_count++;
    }
    
    Compilation_Unit* cu = type->Function.unit;
    Bytecode_Binary* call = bc_instruction_call(bc, cu, arg_count); 
    call->res_index = -1;
    if (cu) {
        assert(cu->bytecode_function);
        Bytecode_Function* func = cu->bytecode_function;
        call->arg0_index = func->type_index;
    } else {
        call->arg0_index = function_ptr;
    }
    assert(call->arg0_index >= 0);
    call->arg1_index = arg_count;
    
    bc->curr_function->max_caller_arg_count = max(bc->curr_function->max_caller_arg_count, (u32) arg_count);
    
    // TODO(Alexander): multiple args
    if (!ret_as_first_arg && is_valid_type(ret_type)) {
        call->res_index = result;
    }
    
    if (arg_operands) {
        int arg_size = sizeof(int)*arg_count;
        memcpy((int*) (call + 1), arg_operands, arg_size);
        arena_push_size(&bc->arena, arg_size, 1);
        array_free(arg_operands);
    }
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
            smm aligned_size = align_forward(elem_type->size, elem_type->align);
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
                bc_instruction(bc, BC_FIELD_ACCESS, dest_ptr, base_ptr, (int) field_info.offset);
            }
            emit_initializing_expression(bc, assign, dest_ptr);
            
#if 0
            if (is_aggregate_type(field_info.type)) {
                int src = emit_reference_expression(bc, assign);
                bc_instruction(bc, BC_MEMCPY, dest, src, field_info.type->size);
            } else {
                int src = add_bytecode_register(bc, assign->type);
                emit_value_expression(bc, assign, src);
                bc_instruction_store(bc, dest, src);
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
                bc_instruction(bc, BC_MEMSET, dest_ptr, 0, expr->type->size);
            } else {
                int src = add_bytecode_register(bc, t_s64);
                bc_const_int(bc, src, 0);
                bc_instruction_store(bc, dest_ptr, src);
            }
        } break;
        
        case Ast_Ident:
        case Ast_Exported_Data: {
            if (is_aggregate_type(expr->type)) {
                int src = emit_reference_expression(bc, expr);
                bc_instruction(bc, BC_MEMCPY, dest_ptr, src, expr->type->size);
            } else {
                int src = emit_value_expression(bc, expr, -1);
                bc_instruction_store(bc, dest_ptr, src);
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
                bc_instruction_global(bc, data, global_index);
                bc_instruction(bc, BC_MEMCPY, dest_ptr, data, type->size);
                emit_non_const_aggregate_fields(bc, expr, dest_ptr, 0);
            } else {
                bc_instruction(bc, BC_MEMSET, dest_ptr, 0, type->size);
            }
        } break;
        
        case Ast_Value: {
            if (is_string(expr->Value)) {
                smm string_count = expr->Value.data.str.count;
                int global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY, 
                                                       string_count, 1,
                                                       expr->Value.data.str.data);
                int src_data_ptr = add_bytecode_register(bc, t_void_ptr);
                bc_instruction_global(bc, src_data_ptr, global_index);
                
                bc_instruction_store(bc, dest_ptr, src_data_ptr);
                
                int src_count = add_bytecode_register(bc, t_s64);
                bc_const_int(bc, src_count, string_count);
                int tmp = add_bytecode_register(bc, t_void_ptr);
                bc_instruction(bc, BC_FIELD_ACCESS, tmp, dest_ptr, 8);
                bc_instruction_store(bc, tmp, src_count);
            } else {
                int src = emit_value_expression(bc, expr);
                bc_instruction_store(bc, dest_ptr, src);
            }
        } break;
        
        case Ast_Unary_Expr: {
            switch (expr->Unary_Expr.op) {
                case Op_Address_Of: {
                    int src = emit_reference_expression(bc, expr->Unary_Expr.first);
                    bc_instruction_store(bc, dest_ptr, src);
                } break;
                
                default: unimplemented;
            }
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


int
emit_value_expression(Bytecode_Builder* bc, Ast* expr, int _result) {
    
    // TODO: We don't want this in all cases!
    if (_result == -1 && is_valid_type(expr->type)) {
        _result = add_bytecode_register(bc, expr->type);
    }
    
    switch (expr->kind) {
        case Ast_None: break;
        
        case Ast_Ident: {
            string_id ident = ast_unwrap_ident(expr);
            Bc_Local local = map_get(bc->locals, ident);
            if (local.ptr != -1) { 
                bc_instruction_load(bc, _result, local.ptr);
            } else if (local.val != -1) {
                _result = local.val;
            } else {
                _result = emit_reference_expression(bc, expr);
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
                    bc_instruction_global(bc, _result, global_index);
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
                    _result = emit_reference_expression(bc, expr->Unary_Expr.first);
                } break;
                
                case Op_Dereference: {
                    if (type->kind == TypeKind_Pointer) {
                        int src = emit_value_expression(bc, expr->Unary_Expr.first);
                        bc_instruction_load(bc, _result, src);
                    } else {
                        _result= emit_value_expression(bc, expr->Unary_Expr.first, _result);
                    }
                } break;
                
                case Op_Pre_Increment:
                case Op_Pre_Decrement: {
                    int src = emit_reference_expression(bc, expr);
                    bc_instruction_load(bc, _result, src);
                } break;
                
#if 0
                case Op_Post_Increment:
                case Op_Post_Decrement: {
                    int first_ptr = emit_reference_expression(bc, expr->Unary_Expr.first);
                    int first = bc_instruction_load(bc, type, first_ptr);
                    
                    int first_modified;
                    if (type->kind == TypeKind_Pointer) {
                        int index = bc_instruction_const_int(bc, t_s64, 
                                                             (op == Op_Post_Increment ||
                                                              op == Op_Pre_Increment) ? 1 : -1);
                        first_modified = bc_instruction_array_accesss(bc, type->Pointer, first, index);
                    } else {
                        first_modified = add_bytecode_register(bc, type);
                        bc_instruction(bc, (op == Op_Post_Increment ||
                                            op == Op_Pre_Increment) ? BC_INC : BC_DEC,
                                       first_modified, first, -1);
                    }
                    bc_instruction_store(bc, first_ptr, first_modified);
                    
                    result = (op == Op_Post_Increment || 
                              op == Op_Post_Decrement) ? first : first_modified;
                } break;
                
                case Op_Bitwise_Not: {
                    int first = emit_value_expression(bc, expr->Unary_Expr.first);
                    result = add_bytecode_register(bc, type);
                    bc_instruction(bc, BC_NOT, result, first, -1);
                } break;
                
                case Op_Logical_Not: {
                    int first = emit_value_expression(bc, expr->Unary_Expr.first);
                    int second = bc_instruction_const_int(bc, type, 0);
                    result = add_bytecode_register(bc, t_bool);
                    bc_instruction(bc, BC_EQ, result, first, second);
                } break;
                
                case Op_Negate: {
                    int first = emit_value_expression(bc, expr->Unary_Expr.first);
                    result = add_bytecode_register(bc, type);
                    bc_instruction(bc, BC_NEG, result, first, -1);
                } break;
#endif
                
                default: unimplemented;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Type* result_type = expr->type;
            Type* type = expr->Binary_Expr.first->type;
            Operator op = expr->Binary_Expr.op;
            
            if (expr->Binary_Expr.overload) {
                unimplemented;
                array(Ast*)* args = 0;
                array_push(args, expr->Binary_Expr.first);
                array_push(args, expr->Binary_Expr.second);
                //result = emit_function_call_to_bytecode(bc, expr->Binary_Expr.overload, args);
                array_free(args);
                return _result;
            }
            
            if (type->kind == TypeKind_Pointer) {
                
                switch (op) {
                    case Op_Add:
                    case Op_Subtract: {
                        int first = emit_value_expression(bc, expr->Binary_Expr.first);
                        //bc_instruction_load(bc, )
                        int index = emit_value_expression(bc, expr->Binary_Expr.second);
                        if (op == Op_Subtract) {
                            bc_instruction(bc, BC_NEG, index, index, -1);
                        }
                        bc_instruction_array_accesss(bc, type->Pointer, _result, first, index);
                    } break;
                    
                    case Op_Assign: {
                        int first = emit_reference_expression(bc, expr->Binary_Expr.first);
                        int value = emit_value_expression(bc, expr->Binary_Expr.first);
                        bc_instruction_store(bc, first, value);
                    } break;
                    
                    default: verify_not_reached();
                    // TODO: Sub, Add_Assign, Sub_Assign, Assign
                    // ptr = 4; ptr += 4; ptr + 2; ptr - 2;
                }
                
                return _result;
            }
            
            Bytecode_Operator opcode = to_bytecode_opcode(op, type);
            switch (op) {
                case Op_Logical_And: {
                    begin_block(bc);
                    begin_block(bc);
                    
                    int cond = add_bytecode_register(bc, t_bool);
                    emit_condition_expression(bc, expr->Binary_Expr.first, cond, true);
                    bc_instruction_branch(bc, bc->block_depth, cond);
                    
                    emit_condition_expression(bc, expr->Binary_Expr.second, cond, true);
                    bc_instruction_branch(bc, bc->block_depth, cond);
                    
                    bc_const_int(bc, _result, true);
                    bc_instruction_branch(bc, bc->block_depth - 1, -1);
                    end_block(bc);
                    
                    bc_const_int(bc, _result, false);
                    end_block(bc);
                } break;
                
                case Op_Logical_Or: {
                    begin_block(bc);
                    begin_block(bc);
                    
                    int cond = add_bytecode_register(bc, t_bool);
                    emit_condition_expression(bc, expr->Binary_Expr.first, cond, false);
                    bc_instruction_branch(bc, bc->block_depth, cond);
                    
                    emit_condition_expression(bc, expr->Binary_Expr.second, cond, false);
                    bc_instruction_branch(bc, bc->block_depth, cond);
                    
                    bc_const_int(bc, _result, false);
                    bc_instruction_branch(bc, bc->block_depth - 1, -1);
                    end_block(bc);
                    
                    bc_const_int(bc, _result, true);
                    end_block(bc);
                } break;
                
                case Op_Assign:
                case Op_Add_Assign:
                case Op_Subtract_Assign:
                case Op_Multiply_Assign:
                case Op_Divide_Assign:
                case Op_Modulo_Assign:
                case Op_Bitwise_And_Assign:
                case Op_Bitwise_Or_Assign:
                case Op_Bitwise_Xor_Assign:
                case Op_Shift_Left_Assign:
                case Op_Shift_Right_Assign: {
                    emit_assignment_expression(bc, opcode,
                                               expr->Binary_Expr.first,
                                               expr->Binary_Expr.second, _result);
                } break;
                
                default: {
                    emit_binary_expression(bc, opcode, 
                                           expr->Binary_Expr.first,
                                           expr->Binary_Expr.second, _result);
                } break;
            }
        } break;
        
        case Ast_Ternary_Expr: {
            begin_block(bc);
            begin_block(bc);
            int cond = add_bytecode_register(bc, t_bool);
            emit_condition_expression(bc, expr->Ternary_Expr.first, cond, true);
            bc_instruction_branch(bc, bc->block_depth, cond);
            
            emit_value_expression(bc, expr->Ternary_Expr.second, _result);
            
            bc_instruction_branch(bc, bc->block_depth - 1, -1);
            end_block(bc);
            
            emit_value_expression(bc, expr->Ternary_Expr.third, _result);
            end_block(bc);
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
            bc_instruction_load(bc, _result, ptr);
        } break;
        
        case Ast_Field_Expr: {
            int ptr = emit_reference_expression(bc, expr);
            Type* type = expr->Field_Expr.var->type;
            if (type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace) {
                // TODO: for inplace arrays the field expression is essentially a noop
                _result = ptr;
            } else {
                bc_instruction_load(bc, _result, ptr);
            }
        } break;
        
        case Ast_Paren_Expr: {
            _result = emit_value_expression(bc, expr->Paren_Expr.expr, _result);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return _result;
}

inline void
emit_binary_expression(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                       Ast* lexpr, Ast* rexpr, int result) {
    assert(opcode != BC_NOOP);
    
    int first = emit_value_expression(bc, lexpr, result);
    int second = emit_value_expression(bc, rexpr, (first != result) ? result : -1);
    Bytecode_Binary* bc_insn = bc_instruction(bc, opcode, result, first, second);
    bc_insn->type = to_bytecode_type(bc, lexpr->type);
}

inline void
emit_assignment_expression(Bytecode_Builder* bc, Bytecode_Operator opcode, 
                           Ast* lexpr, Ast* rexpr, int result) {
    assert(opcode != BC_NOOP);
    
    if (lexpr->kind == Ast_Ident) {
        string_id ident = ast_unwrap_ident(lexpr);
        Bc_Local local = map_get(bc->locals, ident);
        if (local.ptr == -1) {
            int second = emit_value_expression(bc, rexpr, result);
            bc_instruction(bc, opcode, local.val, local.val, second);
            return;
        }
    }
    
    int first_ptr;
    if (lexpr->kind == Ast_Unary_Expr && lexpr->Unary_Expr.op == Op_Dereference) {
        first_ptr = emit_value_expression(bc, lexpr, result);
    } else {
        first_ptr = emit_reference_expression(bc, lexpr);
    }
    int second = emit_value_expression(bc, rexpr, (first_ptr != result) ? result : -1);
    
    if (opcode != BC_COPY) {
        result = add_bytecode_register(bc, lexpr->type);
        bc_instruction_load(bc, result, first_ptr);
        bc_instruction(bc, opcode, result, result, second);
    } else {
        result = second;
    }
    
    bc_instruction_store(bc, first_ptr, result);
    
#if 0
    case Ast_Unary_Expr: {
        
        
        
    } break;
    
    default: {
        Ast* lvalue = expr->Binary_Expr.first;
        int ptr;
        if (lvalue->kind == Ast_Unary_Expr && lvalue->Unary_Expr.op == Op_Dereference) {
            //ptr = add_bytecode_register(bc, t_void_ptr);
            //emit_value_expression(bc, lvalue->Unary_Expr.first, ptr);
            ptr = emit_reference_expression(bc, lvalue->Unary_Expr.first);
        } else {
            ptr = emit_reference_expression(bc, lvalue);
        }
        
        int result;
        if (opcode != BC_MOVE) {
            result = tmp;
            bc_instruction_load(bc, result, ptr);
            
            int second = emit_value_expression(bc, expr->Binary_Expr.second);
            bc_instruction(bc, opcode, result, result, second);
            //drop_register(second);
        } else {
            result = emit_value_expression(bc, expr->Binary_Expr.second, tmp);
        }
        bc_instruction_store(bc, ptr, result);
        //drop_register(result);
    } break;
#endif
}

void
emit_type_cast(Bytecode_Builder* bc, Ast* expr, int result) {
    
    Type* t_dest = normalize_type_for_casting(expr->type);
    Type* t_src = normalize_type_for_casting(expr->Cast_Expr.expr->type);
    
    // TODO(Alexander): optimize by using a lookup table
    if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
        Bytecode_Operator opcode = BC_NOOP;
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
            bc_instruction(bc, opcode, result, src, -1);
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
        bc_instruction_store(bc, array_ptr, src_ptr);
        
        int count = add_bytecode_register(bc, t_s64);
        bc_const_int(bc, count, t_src->Array.capacity);
        int count_ptr = add_bytecode_register(bc, t_void_ptr);
        bc_instruction(bc, BC_FIELD_ACCESS, count_ptr, array_ptr, 8);
        bc_instruction_store(bc, count_ptr, count);
    } else {
        unimplemented;
    }
}

inline void
emit_zero_compare(Bytecode_Builder* bc, Type* type, int result, int value, bool invert_condition) {
    int zero = add_bytecode_register(bc, type);
    if (type->kind == TypeKind_Basic) {
        if (type->Basic.kind == Basic_f32) {
            bc_const_f32(bc, zero, 0);
        } else if (type->Basic.kind == Basic_f64) {
            bc_const_f64(bc, zero, 0);
        } else {
            bc_const_int(bc, zero, 0);
        } 
    } else {
        bc_const_int(bc, zero, 0);
    }
    bc_instruction(bc, invert_condition ? BC_EQ : BC_NEQ, result, value, zero);
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
        int val = emit_value_expression(bc, cond, result);
        emit_zero_compare(bc, type, result, val, invert_condition);
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
                local.ptr = bc_instruction_local(bc, type);
                emit_initializing_expression(bc, stmt->Assign_Stmt.expr, local.ptr);
            } else {
                local.ptr = -1;
                local.val = emit_value_expression(bc, stmt->Assign_Stmt.expr);
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
            begin_block(bc);
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                begin_block(bc);
            }
            
            int cond = add_bytecode_register(bc, t_bool);
            emit_condition_expression(bc, stmt->If_Stmt.cond, cond, true);
            bc_instruction_branch(bc, bc->block_depth, cond);
            
            // Then case
            emit_statement(bc, stmt->If_Stmt.then_block, break_label, continue_label);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                // Else case
                bc_instruction_branch(bc, bc->block_depth - 1, -1);
                
                end_block(bc);
                emit_statement(bc, stmt->If_Stmt.else_block, break_label, continue_label);
            }
            end_block(bc);
            
        } break;
        
        case Ast_For_Stmt: {
            // init
            emit_statement(bc, stmt->For_Stmt.init, 0, 0);
            
            begin_block(bc);
            begin_block(bc, BC_LOOP);
            
            // Condition
            if (is_valid_ast(stmt->For_Stmt.cond)) {
                int cond = add_bytecode_register(bc, t_bool);
                emit_condition_expression(bc, stmt->For_Stmt.cond, cond, true);
                bc_instruction_branch(bc, bc->block_depth - 1, cond);
            }
            
            // Block
            begin_block(bc);
            emit_statement(bc, stmt->For_Stmt.block, bc->block_depth - 2, bc->block_depth);
            end_block(bc);
            
            // Update
            emit_value_expression(bc, stmt->For_Stmt.update, -1);
            bc_instruction_branch(bc, bc->block_depth, -1);
            
            // Exit
            end_block(bc);
            end_block(bc);
        } break;
        
        case Ast_While_Stmt: {
            begin_block(bc);
            begin_block(bc, BC_LOOP);
            
            // Condition
            if (is_valid_ast(stmt->While_Stmt.cond)) {
                int cond = add_bytecode_register(bc, t_bool);
                emit_condition_expression(bc, stmt->While_Stmt.cond, cond, true);
                bc_instruction_branch(bc, bc->block_depth - 1, cond);
            }
            
            emit_statement(bc, stmt->While_Stmt.block, bc->block_depth - 1, bc->block_depth);
            bc_instruction_branch(bc, bc->block_depth, -1);
            
            // Exit
            end_block(bc);
            end_block(bc);
        } break;
        
        case Ast_Switch_Stmt: {
            begin_block(bc);
            Ast* default_stmt = 0;
            int outer_block = bc->block_depth;
            int switch_cond = emit_value_expression(bc, stmt->Switch_Stmt.cond);
            
            {
                for_compound(stmt->Switch_Stmt.cases, it) {
                    if (!is_valid_ast(it->Switch_Case.cond)) {
                        assert(default_stmt == 0 && "two default statements");
                        default_stmt = it->Switch_Case.stmt;
                    } else if (is_valid_ast(it->Switch_Case.stmt)) {
                        begin_block(bc);
                    }
                }
            }
            
            int case_cond = add_bytecode_register(bc, t_bool);
            int multi_case_block = -1;
            for_compound(stmt->Switch_Stmt.cases, it) {
                if (is_valid_ast(it->Switch_Case.cond)) {
                    emit_value_expression(bc, it->Switch_Case.cond, case_cond);
                    
                    if (is_valid_ast(it->Switch_Case.stmt)) {
                        bc_instruction(bc, BC_NEQ, case_cond, switch_cond, case_cond);
                        if (multi_case_block == -1) {
                            bc_instruction_branch(bc, bc->block_depth, case_cond);
                        } else {
                            bc_instruction_branch(bc, bc->block_depth - 1, case_cond);
                            end_block(bc);
                            multi_case_block = -1;
                        }
                        
                        emit_statement(bc, it->Switch_Case.stmt, break_label, continue_label);
                        
                        if (bc->curr_insn->opcode != BC_RETURN) {
                            bc_instruction_branch(bc, outer_block, -1);
                        }
                        end_block(bc);
                        
                    } else {
                        // Case contains multiple cases (similar to logical or)
                        if (multi_case_block == -1) {
                            begin_block(bc);
                            multi_case_block = bc->block_depth;
                        }
                        
                        bc_instruction(bc, BC_EQ, case_cond, switch_cond, case_cond);
                        bc_instruction_branch(bc, multi_case_block, case_cond);
                    }
                }
            }
            
            if (default_stmt) {
                emit_statement(bc, default_stmt, break_label, continue_label);
            }
            
            end_block(bc);
            
        } break;
        
        case Ast_Return_Stmt: {
            int result = -1;
            if (is_valid_ast(stmt->Return_Stmt.expr)) {
                if (bc->curr_function->return_as_first_arg) {
                    int src_ptr = emit_reference_expression(bc, stmt->Return_Stmt.expr);
                    Bytecode_Function_Arg src_type = function_ret_types(bc->curr_function)[0];
                    bc_instruction(bc, BC_MEMCPY, 0, src_ptr, src_type.size);
                } else {
                    result = emit_value_expression(bc, stmt->Return_Stmt.expr);
                }
            }
            bc_instruction(bc, BC_RETURN, -1, result, -1);
        } break;
        
        case Ast_Break_Stmt: {
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
            branch->cond = -1;
            branch->label_index = break_label;
        } break;
        
        case Ast_Continue_Stmt: {
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
            branch->cond = -1;
            branch->label_index = continue_label;
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
        add_insn(bc, BC_DEBUG_BREAK);
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
            func->arg_count++;
            func->ret_count--;
            func->return_as_first_arg = true;
        }
        
        curr_arg->type = to_bytecode_type(bc, ret_type);
        curr_arg->size = ret_type->size;
        curr_arg->align = ret_type->align;
        curr_arg++;
    }
    
    for (int i = 0; i < arg_count; i++) {
        string_id arg_ident = type->Function.arg_idents[i];
        Type* arg_type = type->Function.arg_types[i];
        curr_arg->type = to_bytecode_type(bc, arg_type);
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
string_builder_dump_bytecode_opcode(String_Builder* sb, Bytecode_Instruction* insn) {
    //if (insn->type) {
    //string_builder_push_format(sb, "%.", f_cstring(bc_instruction_type_names[insn->type]));
    //}
    string_builder_push(sb, bc_operator_names[insn->opcode]);
    string_builder_push_format(sb, " ");
}

void
string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn, int block_depth) {
    
    smm from_byte = sb->curr_used;
    string_builder_pad(sb, sb->curr_used, block_depth*2);
    
    switch (insn->kind) {
        case BytecodeInstructionKind_None: break;
        
        case BytecodeInstructionKind_Base: {
            string_builder_dump_bytecode_opcode(sb, insn);
            
            switch (insn->opcode) {
                case BC_BLOCK:
                case BC_LOOP:
                case BC_END: {
                    string_builder_push_format(sb, "(label = %)", f_int(block_depth));
                } break;
            }
        } break;
        
        case BytecodeInstructionKind_Binary: {
            Bytecode_Binary* bc_instruction_insn = (Bytecode_Binary*) insn;
            if (bc_instruction_insn->res_index >= 0 && 
                insn->opcode != BC_MEMCPY && 
                insn->opcode != BC_MEMSET) {
                string_builder_push_format(sb, "r% = ", f_int(bc_instruction_insn->res_index));
            }
            
            string_builder_dump_bytecode_opcode(sb, insn);
            
            switch (insn->opcode) {
                case BC_INT_CONST: {
                    string_builder_push_format(sb, "%", f_int(bc_instruction_insn->const_i64));
                } break;
                
                case BC_F32_CONST: {
                    string_builder_push_format(sb, "%", f_float(bc_instruction_insn->const_f32));
                } break;
                
                case BC_F64_CONST: {
                    string_builder_push_format(sb, "%", f_float(bc_instruction_insn->const_f64));
                } break;
                
                case BC_CALL:
                case BC_CALL_INDIRECT: {
                    Bytecode_Binary* bc_insn = (Bytecode_Binary*) insn;
                    
                    // target
                    Bytecode_Function* func = 0;
                    if (insn->opcode == BC_CALL) {
                        func = bc->functions[bc_insn->arg0_index];
                    }
                    
                    if (func) {
                        string_builder_dump_bytecode_function_name(sb, bc, func);
                        
                    } else {
                        string_builder_push_format(sb, "r%", f_int(bc_insn->arg0_index));
                    }
                    
                    // args
                    string_builder_push(sb, "(");
                    int* args = (int*) (bc_insn + 1);
                    for (int i = 0; i < bc_insn->arg1_index; i++) {
                        string_builder_push_format(sb, "r%", f_int(args[i]));
                        if (i + 1 < bc_insn->arg1_index) {
                            string_builder_push(sb, ", ");
                        }
                    }
                    string_builder_push(sb, ")");
                } break;
                
                case BC_LOCAL: {
                    string_builder_push_format(sb, "size %, align %", f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index));
                } break;
                
                case BC_GLOBAL: {
                    string_builder_push_format(sb, "%", f_int(bc_instruction_insn->arg0_index));
                } break;
                
                case BC_MEMCPY: {
                    string_builder_push_format(sb, "r%, r%, size %",
                                               f_int(bc_instruction_insn->res_index),
                                               f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index));
                } break;
                
                case BC_MEMSET: {
                    string_builder_push_format(sb, "r%, %, size %",
                                               f_int(bc_instruction_insn->res_index),
                                               f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index));
                } break;
                
                case BC_ARRAY_ACCESS: {
                    string_builder_push_format(sb, "r%, r%, stride %",
                                               f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index),
                                               f_int(bc_instruction_insn->stride));
                } break;
                
                case BC_FIELD_ACCESS: {
                    string_builder_push_format(sb, "r%, %", f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index));
                } break;
                
                case BC_FLOAT_TO_INT: {
                    string_builder_push_format(sb, "r%", f_int(bc_instruction_insn->arg0_index));
                } break;
                
                default: {
                    if (bc_instruction_insn->arg1_index >= 0) {
                        string_builder_push_format(sb, "r%, ", f_int(bc_instruction_insn->arg0_index));
                        string_builder_push_format(sb, "r%", f_int(bc_instruction_insn->arg1_index));
                    } else if (bc_instruction_insn->arg0_index >= 0) {
                        string_builder_push_format(sb, "r%", f_int(bc_instruction_insn->arg0_index));
                    }
                } break;
            }
        } break;
        
        case BytecodeInstructionKind_Block: {
            u32 label_index = ((Bytecode_Block*) insn)->label_index;
            if (label_index > 0) {
                from_byte = sb->curr_used + 5;
                string_builder_push_format(sb, "\nBasic Block %:", f_u32(label_index));
            }
        } break;
        
        case BytecodeInstructionKind_Branch: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "%", f_u32(((Bytecode_Branch*) insn)->label_index));
            
            if (((Bytecode_Branch*) insn)->cond != -1) {
                string_builder_push(sb, ", ");
                string_builder_push_format(sb, "r%", f_int(((Bytecode_Branch*) insn)->cond));
            }
        } break;
        
        case BytecodeInstructionKind_Memory: {
            //string_builder_dump_bytecode_opcode(sb, insn);
            //string_builder_dump_bytecode_operand(sb, ((Bytecode_Memory*) insn)->dest, insn->type);
            //string_builder_push(sb, ", ");
            //string_builder_dump_bytecode_operand(sb, ((Bytecode_Memory*) insn)->src, insn->type);
            //string_builder_push_format(sb, ", %", f_int(((Bytecode_Memory*) insn)->size));
            unimplemented;
        } break;
    }
    
    if (insn->comment) {
        string_builder_pad(sb, from_byte, 30);
        string_builder_push_format(sb, " // %", f_cstring(insn->comment));
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
            string_builder_push_format(sb, " (ret)", f_int(i));
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
    while (curr->kind) {
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
            const cstring bc_instruction_memory_names[] = { "read only", "read write" };
            string_builder_push_format(sb, "\n  %: size %, align %, kind \"%\" - ",
                                       f_int(global_index),
                                       f_int(g->size),
                                       f_int(g->align),
                                       f_cstring(bc_instruction_memory_names[g->kind]));
            
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