
inline int 
bc_find_register_value(Bytecode_Builder* bc, Ast* ast, int tmp=-1) {
    int result = tmp;
    
    int ident = try_unwrap_ident(ast);
    if (ident) {
        result = map_get(bc->locals, ident);
    } else {
        if (result == -1) {
            result = add_bytecode_register(bc, ast->type);
        }
        convert_expression_to_bytecode(bc, ast, result);
    } 
    
    return result;
}

int
convert_lvalue_expression_to_bytecode(Bytecode_Builder* bc, Ast* expr) {
    int result = -1;
    switch (expr->kind) {
        
        case Ast_Ident: {
            Type* type = expr->type;
            string_id ident = ast_unwrap_ident(expr);
            if (map_key_exists(bc->locals, ident)) {
                result = map_get(bc->locals, ident);
                
            } else {
                if (type->kind == TypeKind_Function && type->Function.unit) {
                    Bytecode_Function* func = type->Function.unit->bytecode_function;
                    result = add_bytecode_register(bc, t_void_ptr);
                    bc_instruction(bc, BC_FUNCTION, result, func->type_index, -1);
                    
                } else if (map_key_exists(bc->globals, ident)) { 
                    int global_index = map_get(bc->globals, ident);
                    result = add_bytecode_register(bc, t_void_ptr);
                    bc_instruction_global(bc, result, global_index);
                    
                } else {
                    unimplemented;
                }
            }
        } break;
        
        case Ast_Exported_Data: {
            //result.kind = BytecodeOperand_memory;
            //result.memory_offset = expr->Exported_Data.relative_ptr; 
            //result.memory_kind = BytecodeMemory_read_only;
            unimplemented;
        } break;
        
        
        case Ast_Unary_Expr: {
            if (expr->Unary_Expr.op == Op_Dereference) {
                result = convert_lvalue_expression_to_bytecode(bc, expr->Unary_Expr.first);
                int tmp = add_bytecode_register(bc, expr->Unary_Expr.first->type);
                bc_instruction_load(bc, tmp, result);
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Field_Expr: {
            Type* type = expr->Field_Expr.var->type;
            result = convert_lvalue_expression_to_bytecode(bc, expr->Field_Expr.var);
            string_id ident = ast_unwrap_ident(expr->Field_Expr.field);
            
            if (type->kind == TypeKind_Pointer) {
                type = type->Pointer;
                int flags = register_type(bc->curr_function, result).flags;
                if (!is_bitflag_set(flags, BC_FLAG_RVALUE)) {
                    int tmp = add_bytecode_register(bc, type);
                    result = bc_instruction_load(bc, tmp, result);
                }
                
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
                    assert(type->kind == TypeKind_Array ||
                           type->Basic.kind == Basic_string && "unsupported type");
                    
                    if (ident == Sym_data) {
                        if (!(type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace)) {
                            int tmp = add_bytecode_register(bc, type);
                            bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, 0);
                            result = tmp;
                        } else {
                            bc->curr_function->register_types[result].flags |= BC_FLAG_RVALUE;
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
            
            int array_ptr = convert_lvalue_expression_to_bytecode(bc, expr->Index_Expr.array);
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
            convert_expression_to_bytecode(bc, expr->Index_Expr.index, array_index);
            result = add_bytecode_register(bc, expr->Index_Expr.array->type);
            result = bc_instruction_array_accesss(bc, type, array_ptr, array_index);
        } break;
        
        case Ast_Cast_Expr: {
            result = add_bytecode_register(bc, expr->type);
            convert_type_cast_to_bytecode(bc, expr, result);
        } break;
        
        case Ast_Paren_Expr: {
            result = convert_lvalue_expression_to_bytecode(bc, expr->Paren_Expr.expr);
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

int
convert_function_call_to_bytecode(Bytecode_Builder* bc, Type* type, array(Ast*)* args, int function_ptr) {
    assert(type && type->kind == TypeKind_Function);
    
    int result = -1;
    
    if (type->Function.is_intrinsic) {
        bool handled = false;
        
        switch (type->Function.ident) {
            case Sym_rdtsc: {
                result = add_bytecode_register(bc, t_s64);
                bc_instruction(bc, BC_X64_RDTSC, result, -1, -1);
                handled = true;
            } break;
            
            case Sym_debug_break: {
                bc_instruction(bc, BC_DEBUG_BREAK, -1, -1, -1);
                handled = true;
            } break;
        }
        
        if (handled) {
            return result;
        }
    }
    
    int arg_count = 0;
    array(int)* arg_operands = 0;
    
    bool ret_as_first_arg = false;
    Type* ret_type = type->Function.return_type;
    if (ret_type && is_aggregate_type(ret_type)) {
        result = bc_instruction_local(bc, ret_type);
        array_push(arg_operands, result);
        ret_as_first_arg = true;
        arg_count++;
    }
    
    for_array_v(args, arg, _) {
        Type* arg_type = arg->type;
        int reg = -1;
        if (is_aggregate_type(arg_type)) {
            // Create copy (except for ARRAY types) and pass it via ptr
            int copy = -1;
            if (arg_type->kind != TypeKind_Array) {
                copy = bc_instruction_local(bc, arg_type);
                
                if (convert_initializer_to_bytecode(bc, arg, copy)) {
                    reg = copy;
                }
            }
            
            if (reg == -1) {
                reg = convert_lvalue_expression_to_bytecode(bc, arg);
                if (copy != -1) {
                    bc_instruction(bc, BC_MEMCPY, copy, reg, arg_type->size);
                    reg = copy;
                }
            }
            
        } else {
            reg = add_bytecode_register(bc, arg_type);
            convert_expression_to_bytecode(bc, arg, reg);
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
        result = add_bytecode_register(bc, ret_type);
        call->res_index = result;
    }
    
    if (arg_operands) {
        int arg_size = sizeof(int)*arg_count;
        memcpy((int*) (call + 1), arg_operands, arg_size);
        arena_push_size(&bc->arena, arg_size, 1);
        array_free(arg_operands);
    }
    
    return result;
}

internal void
convert_non_constant_aggregate_initializer_to_bytecode(Bytecode_Builder* bc, Ast* expr, int base, int offset) {
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
            convert_non_constant_aggregate_initializer_to_bytecode(bc, assign, base, (int) field_info.offset);
            
        } else if (assign->kind != Ast_Value) {
            //pln("Adding non-constant field: %", f_ast(assign));
            
            int dest = add_bytecode_register(bc, t_void_ptr);
            bc_instruction(bc, BC_FIELD_ACCESS, dest, base, (int) field_info.offset);
            if (is_aggregate_type(field_info.type)) {
                int src = convert_lvalue_expression_to_bytecode(bc, assign);
                bc_instruction(bc, BC_MEMCPY, dest, src, field_info.type->size);
            } else {
                int src = add_bytecode_register(bc, assign->type);
                convert_expression_to_bytecode(bc, assign, src);
                bc_instruction_store(bc, dest, src);
            }
        }
        
        field_index++;
    }
}

bool
convert_initializer_to_bytecode(Bytecode_Builder* bc, Ast* expr, int dest_ptr) {
    bool initialized = false;
    
    switch (expr->kind) {
        case Ast_Value: {
            if (is_string(expr->Value)) {
                smm string_count = expr->Value.data.str.count;
                int global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY, 
                                                       string_count, 1,
                                                       expr->Value.data.str.data);
                int src_data_ptr = add_bytecode_register(bc, t_void_ptr);
                bc_instruction_global(bc, src_data_ptr, global_index);
                
                int tmp = add_bytecode_register(bc, t_void_ptr);
                bc_instruction(bc, BC_FIELD_ACCESS, tmp, dest_ptr, 0);
                bc_instruction_store(bc, tmp, src_data_ptr);
                
                int src_count = add_bytecode_register(bc, t_s64);
                bc_const_int(bc, src_count, string_count);
                tmp = add_bytecode_register(bc, t_void_ptr);
                bc_instruction(bc, BC_FIELD_ACCESS, tmp, dest_ptr, 8);
                bc_instruction_store(bc, tmp, src_count);
                initialized = true;
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
                convert_non_constant_aggregate_initializer_to_bytecode(bc, expr, dest_ptr, 0);
                initialized = true;
            }
            
        } break;
    }
    
    return initialized;
}

void
convert_expression_to_bytecode(Bytecode_Builder* bc, Ast* expr, int _result) {
    switch (expr->kind) {
        case Ast_None:
        case Ast_Aggregate_Expr: break;
        
        case Ast_Ident:
        case Ast_Exported_Data: {
            int src = convert_lvalue_expression_to_bytecode(bc, expr);
            bc_instruction_load(bc, _result, src);
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
            } else if (is_string(expr->Value)) {
                // TODO(Alexander): I think we should handle this only from the initializer above!!!
                unimplemented;
#if 0
                smm string_count = expr->Value.data.str.count;
                int global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY, 
                                                       string_count, 1,
                                                       expr->Value.data.str.data);
                int src_data_ptr = bc_instruction_global(bc, global_index);
                result = bc_instruction_local(bc, t_string);
                
                int tmp = add_bytecode_register(bc, t_void_ptr);
                bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, 0);
                bc_instruction_store(bc, tmp, src_data_ptr);
                
                int src_count = bc_instruction_const_int(bc, t_s64, string_count);
                tmp = add_bytecode_register(bc, t_void_ptr);
                bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, 8);
                bc_instruction_store(bc, tmp, src_count);
#endif
                
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
                unimplemented;
            }
        } break;
        
        case Ast_Unary_Expr: {
            unimplemented;
#if 0
            Type* type = expr->type;
            Operator op = expr->Unary_Expr.op;
            
            switch (op) {
                case Op_Address_Of: {
                    result = convert_lvalue_expression_to_bytecode(bc, expr->Unary_Expr.first);
                } break;
                
                case Op_Dereference: {
                    result = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    result = bc_instruction_load(bc, type, result);
                } break;
                
                case Op_Post_Increment:
                case Op_Post_Decrement:
                case Op_Pre_Increment:
                case Op_Pre_Decrement: {
                    int first_ptr = convert_lvalue_expression_to_bytecode(bc, expr->Unary_Expr.first);
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
                    int first = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    result = add_bytecode_register(bc, type);
                    bc_instruction(bc, BC_NOT, result, first, -1);
                } break;
                
                case Op_Logical_Not: {
                    int first = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    int second = bc_instruction_const_int(bc, type, 0);
                    result = add_bytecode_register(bc, t_bool);
                    bc_instruction(bc, BC_EQ, result, first, second);
                } break;
                
                case Op_Negate: {
                    int first = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    result = add_bytecode_register(bc, type);
                    bc_instruction(bc, BC_NEG, result, first, -1);
                } break;
                
                default: unimplemented;
            }
#endif
        } break;
        
        case Ast_Binary_Expr: {
            Type* result_type = expr->type;
            Type* type = expr->Binary_Expr.first->type;
            
            Operator op = expr->Binary_Expr.op;
            Bytecode_Operator opcode = to_bytecode_opcode(op, type);
            
            if (expr->Binary_Expr.overload) {
                unimplemented;
                array(Ast*)* args = 0;
                array_push(args, expr->Binary_Expr.first);
                array_push(args, expr->Binary_Expr.second);
                //result = convert_function_call_to_bytecode(bc, expr->Binary_Expr.overload, args);
                array_free(args);
                //return result;
            }
            
            
            switch (op) {
                case Op_Logical_And: {
                    begin_block(bc);
                    begin_block(bc);
                    
                    int cond = add_bytecode_register(bc, t_bool);
                    convert_condition_to_bytecode(bc, expr->Binary_Expr.first, cond, true);
                    bc_instruction_branch(bc, bc->block_depth, cond);
                    
                    convert_condition_to_bytecode(bc, expr->Binary_Expr.second, cond, true);
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
                    convert_condition_to_bytecode(bc, expr->Binary_Expr.first, cond, false);
                    bc_instruction_branch(bc, bc->block_depth, cond);
                    
                    convert_condition_to_bytecode(bc, expr->Binary_Expr.second, cond, false);
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
                    convert_binary_assignment_to_bytecode(bc, opcode, expr, _result);
                } break;
                
                default: {
                    convert_binary_to_bytecode(bc, opcode, expr, _result);
                } break;
            }
        } break;
        
        case Ast_Ternary_Expr: {
            _result = add_bytecode_register(bc, expr->type);
            
            begin_block(bc);
            begin_block(bc);
            int cond = add_bytecode_register(bc, t_bool);
            convert_condition_to_bytecode(bc, expr->Ternary_Expr.first, cond, true);
            bc_instruction_branch(bc, bc->block_depth, cond);
            
            convert_expression_to_bytecode(bc, expr->Ternary_Expr.second, _result);
            
            bc_instruction_branch(bc, bc->block_depth - 1, -1);
            end_block(bc);
            
            convert_expression_to_bytecode(bc, expr->Ternary_Expr.third, _result);
            end_block(bc);
        } break;
        
#if 0
        case Ast_Call_Expr: {
            array(Ast*)* args = 0;
            for_compound(expr->Call_Expr.args, arg) {
                array_push(args, arg->Argument.assign);
            }
            
            Type* type = expr->Call_Expr.function_type;
            assert(type->kind == TypeKind_Function);
            
            int function_ptr = -1;
            if (!type->Function.unit && !type->Function.is_intrinsic) {
                function_ptr = convert_expression_to_bytecode(bc, expr->Call_Expr.ident);
            }
            result = convert_function_call_to_bytecode(bc, type, args, function_ptr);
            array_free(args);
        } break;
        
#endif
        
        case Ast_Cast_Expr: {
            convert_type_cast_to_bytecode(bc, expr, _result);
        } break;
        
#if 0
        
        case Ast_Index_Expr:
        case Ast_Field_Expr: {
            result = convert_lvalue_expression_to_bytecode(bc, expr);
            int flags = register_type(bc->curr_function, result).flags;
            if (!is_bitflag_set(flags, BC_FLAG_RVALUE)) {
                result = bc_instruction_load(bc, expr->type, result);
            }
        } break;
        
        case Ast_Paren_Expr: {
            return convert_expression_to_bytecode(bc, expr->Paren_Expr.expr);
        } break;
#endif
        
        default: {
            unimplemented;
        } break;
    }
}

inline void
convert_binary_to_bytecode(Bytecode_Builder* bc, Bytecode_Operator opcode, Ast* expr, int result) {
    assert(opcode != BC_NOOP);
    assert(expr->kind == Ast_Binary_Expr);
    
    int first = bc_find_register_value(bc, expr->Binary_Expr.first, result);
    int second = bc_find_register_value(bc, expr->Binary_Expr.second,
                                        first != result ? result : -1);
    bc_instruction(bc, opcode, result, first, second);
}

inline void
convert_binary_assignment_to_bytecode(Bytecode_Builder* bc, Bytecode_Operator opcode, Ast* expr, int tmp) {
    assert(opcode != BC_NOOP);
    assert(expr->kind == Ast_Binary_Expr);
    
    switch (expr->Binary_Expr.first->kind) {
        case Ast_Ident: {
            int first = bc_find_register_value(bc, expr->Binary_Expr.first);
            int second = bc_find_register_value(bc, expr->Binary_Expr.second, tmp);
            bc_instruction(bc, opcode, first, first, second);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
}

void
convert_type_cast_to_bytecode(Bytecode_Builder* bc, Ast* expr, int result) {
    
    Type* t_dest = expr->type;
    Type* t_src = expr->Cast_Expr.expr->type;
    if (t_src->kind == TypeKind_Pointer || t_src->kind == TypeKind_Type || t_src == t_cstring) {
        t_src = t_s64;
    }
    
    if (t_dest->kind == TypeKind_Pointer || t_dest->kind == TypeKind_Type || t_dest == t_cstring) {
        t_dest = t_s64;
    }
    
    if (t_src->kind == TypeKind_Enum) { 
        t_src = t_src->Enum.type;
    }
    
    if (t_dest->kind == TypeKind_Enum) { 
        t_dest = t_dest->Enum.type;
    }
    
    // TODO(Alexander): optimize by using a lookup table
    if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
        Bytecode_Operator opcode = BC_NOOP;
        if (t_dest->Basic.flags & BasicFlag_Boolean &&
            t_src->Basic.flags & BasicFlag_Integer) {
            convert_condition_to_bytecode(bc, expr->Cast_Expr.expr, result, false);
            
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
            convert_expression_to_bytecode(bc, expr->Cast_Expr.expr, src);
            bc_instruction(bc, opcode, result, src, -1);
        } else {
            convert_expression_to_bytecode(bc, expr->Cast_Expr.expr, result);
        }
        
    } else if (t_dest->kind == TypeKind_Array && t_src->kind == TypeKind_Array) {
        int src_ptr = convert_lvalue_expression_to_bytecode(bc, expr->Cast_Expr.expr);
        
        if (t_dest->Array.kind == ArrayKind_Fixed &&
            t_src->Array.kind == ArrayKind_Fixed_Inplace) {
            
            unimplemented;
            
#if 0
            // Converts inplace fixed array to "wide"-pointer array
            result = bc_instruction_local(bc, t_dest);
            int array_ptr = add_bytecode_register(bc, t_src);
            bc_instruction(bc, BC_FIELD_ACCESS, array_ptr, result, 0);
            bc_instruction_store(bc, array_ptr, src_ptr);
            
            int count = bc_instruction_const_int(bc, t_s64, t_src->Array.capacity);
            int count_ptr = add_bytecode_register(bc, t_s64);
            bc_instruction(bc, BC_FIELD_ACCESS, count_ptr, result, 8);
            bc_instruction_store(bc, count_ptr, count);
#endif
        } else {
            unimplemented;
        }
        
    } else if (t_dest->kind == TypeKind_Struct && t_src->kind == TypeKind_Struct) {
        // NOTE(Alexander): this is a NOOP, not possible to cast to different struct
        convert_expression_to_bytecode(bc, expr->Cast_Expr.expr, result);
        
    } else {
        unimplemented;
    }
}

inline void
convert_zero_compare_to_bytecode(Bytecode_Builder* bc, Type* type, int result, int value, bool invert_condition) {
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
convert_condition_to_bytecode(Bytecode_Builder* bc, Ast* cond, int result, bool invert_condition) {
    Type* type = cond->type;
    
    switch (cond->kind) {
        case Ast_Ident: {
            int ident = ast_unwrap_ident(cond);
            int local = map_get(bc->locals, ident);
            convert_zero_compare_to_bytecode(bc, type, result, local, invert_condition);
        } break;
        
        case Ast_Binary_Expr: {
            if (operator_is_comparator_table[cond->Binary_Expr.op]) {
                Bytecode_Operator opcode = to_bytecode_opcode(cond->Binary_Expr.op, 
                                                              cond->Binary_Expr.first->type);
                if (invert_condition) {
                    opcode = (Bytecode_Operator) (BC_EQ + BC_NEQ - opcode);
                }
                convert_binary_to_bytecode(bc, opcode, cond, result);
                
            } else {
                convert_expression_to_bytecode(bc, cond, result);
                convert_zero_compare_to_bytecode(bc, type, result, result, invert_condition);
            }
        } break;
        
        case Ast_Paren_Expr: {
            convert_condition_to_bytecode(bc, cond->Paren_Expr.expr, result, invert_condition);
        } break;
        
        default: {
            convert_expression_to_bytecode(bc, cond, result);
            convert_zero_compare_to_bytecode(bc, type, result, result, invert_condition);
        } break;
    }
}

void
convert_statement_to_bytecode(Bytecode_Builder* bc, Ast* stmt, s32 break_label, s32 continue_label) {
    switch (stmt->kind) {
        case Ast_Decl_Stmt: {
            Ast* decl = stmt->Decl_Stmt.stmt;
            if (is_ast_stmt(decl)) {
                convert_statement_to_bytecode(bc, decl, break_label, continue_label);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            Ast* expr = stmt->Expr_Stmt;
            if (is_valid_ast(expr)) {
                int tmp = add_bytecode_register(bc, expr->type);
                convert_expression_to_bytecode(bc, expr, tmp);
            }
        } break;
        
        case Ast_Assign_Stmt: {
            Type* type = stmt->type;
            int local = add_bytecode_register(bc, type);
            
            if (is_valid_ast(stmt->Assign_Stmt.expr)) {
                convert_expression_to_bytecode(bc, stmt->Assign_Stmt.expr, local);
            } else {
                bc_const_int(bc, local, 0);
            }
            
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            map_put(bc->locals, ident, local);
            
#if 0
            int local;
            if (stmt->Assign_Stmt.mods & AstDeclModifier_Local_Persist) {
                local = -1;
                unimplemented;
            } else {
                local = bc_instruction_local(bc, type);
            }
            
            int src = -1;
            bool initialized = false;
            if (is_valid_ast(stmt->Assign_Stmt.expr)) {
                initialized = convert_initializer_to_bytecode(bc, stmt->Assign_Stmt.expr, local);
                if (!initialized) {
                    src = add_bytecode_register(bc, stmt->type);
                    src = convert_expression_to_bytecode(bc, stmt->Assign_Stmt.expr);
                }
            }
            
            if (!initialized) {
                if (src >= 0) {
                    if (is_aggregate_type(type)) {
                        bc_instruction(bc, BC_MEMCPY, local, src, type->size);
                    } else {
                        bc_instruction_store(bc, local, src);
                    }
                } else {
                    if (is_aggregate_type(type)) {
                        bc_instruction(bc, BC_MEMSET, local, 0, type->size);
                    } else {
                        src = bc_instruction_const_int(bc, t_s64, 0);
                        bc_instruction_store(bc, local, src);
                    }
                }
            }
            
            assert(stmt->Assign_Stmt.ident->kind == Ast_Ident);
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            map_put(bc->locals, ident, local);
#endif
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                convert_statement_to_bytecode(bc, it, break_label, continue_label);
            }
        } break;
        
#if 0
        case Ast_If_Stmt: {
            begin_block(bc);
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                begin_block(bc);
            }
            
            int cond = convert_condition_to_bytecode(bc, stmt->If_Stmt.cond, true);
            bc_instruction_branch(bc, bc->block_depth, cond);
            
            // Then case
            convert_statement_to_bytecode(bc, stmt->If_Stmt.then_block, break_label, continue_label);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                // Else case
                bc_instruction_branch(bc, bc->block_depth - 1, -1);
                
                end_block(bc);
                convert_statement_to_bytecode(bc, stmt->If_Stmt.else_block, break_label, continue_label);
            }
            end_block(bc);
            
        } break;
#endif
        
        case Ast_For_Stmt: {
            // init
            convert_statement_to_bytecode(bc, stmt->For_Stmt.init, 0, 0);
            
            begin_block(bc);
            begin_block(bc, BC_LOOP);
            
            // Condition
            if (is_valid_ast(stmt->For_Stmt.cond)) {
                int cond = add_bytecode_register(bc, t_bool);
                convert_condition_to_bytecode(bc, stmt->For_Stmt.cond, cond, true);
                bc_instruction_branch(bc, bc->block_depth - 1, cond);
            }
            
            // Block
            begin_block(bc);
            convert_statement_to_bytecode(bc, stmt->For_Stmt.block, bc->block_depth - 2, bc->block_depth);
            end_block(bc);
            
            // Update
            convert_expression_to_bytecode(bc, stmt->For_Stmt.update, -1);
            bc_instruction_branch(bc, bc->block_depth, -1);
            
            // Exit
            end_block(bc);
            end_block(bc);
        } break;
        
#if 0
        case Ast_While_Stmt: {
            begin_block(bc);
            begin_block(bc, BC_LOOP);
            
            // Condition
            if (is_valid_ast(stmt->While_Stmt.cond)) {
                int cond = convert_condition_to_bytecode(bc, stmt->While_Stmt.cond, true);
                bc_instruction_branch(bc, bc->block_depth - 1, cond);
            }
            
            convert_statement_to_bytecode(bc, stmt->While_Stmt.block, bc->block_depth - 1, bc->block_depth);
            bc_instruction_branch(bc, bc->block_depth, -1);
            
            // Exit
            end_block(bc);
            end_block(bc);
        } break;
        
        case Ast_Switch_Stmt: {
            begin_block(bc);
            Ast* default_stmt = 0;
            int outer_block = bc->block_depth;
            int switch_cond = convert_expression_to_bytecode(bc, stmt->Switch_Stmt.cond);
            
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
            
            int multi_case_block = -1;
            for_compound(stmt->Switch_Stmt.cases, it) {
                if (is_valid_ast(it->Switch_Case.cond)) {
                    int case_cond = convert_expression_to_bytecode(bc, it->Switch_Case.cond);
                    int branch_cond = add_bytecode_register(bc, t_bool);
                    
                    if (is_valid_ast(it->Switch_Case.stmt)) {
                        bc_instruction(bc, BC_NEQ, branch_cond, switch_cond, case_cond);
                        if (multi_case_block == -1) {
                            bc_instruction_branch(bc, bc->block_depth, branch_cond);
                        } else {
                            bc_instruction_branch(bc, bc->block_depth - 1, branch_cond);
                            end_block(bc);
                            multi_case_block = -1;
                        }
                        
                        convert_statement_to_bytecode(bc, it->Switch_Case.stmt, break_label, continue_label);
                        
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
                        
                        bc_instruction(bc, BC_EQ, branch_cond, switch_cond, case_cond);
                        bc_instruction_branch(bc, multi_case_block, branch_cond);
                    }
                }
            }
            
            if (default_stmt) {
                convert_statement_to_bytecode(bc, default_stmt, break_label, continue_label);
            }
            
            end_block(bc);
            
        } break;
#endif
        
        case Ast_Return_Stmt: {
            int result = -1;
            if (is_valid_ast(stmt->Return_Stmt.expr)) {
                if (bc->curr_function->return_as_first_arg) {
                    int src_ptr = convert_lvalue_expression_to_bytecode(bc, stmt->Return_Stmt.expr);
                    Bytecode_Function_Arg src_type = function_ret_types(bc->curr_function)[0];
                    bc_instruction(bc, BC_MEMCPY, 0, src_ptr, src_type.size);
                } else {
                    result = bc_find_register_value(bc, stmt->Return_Stmt.expr);
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
convert_function_to_bytecode(Bytecode_Builder* bc, Bytecode_Function* func, Ast* ast,
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
        map_put(bc->locals, arg_ident, arg_index);
    }
    
    if (insert_debug_break) {
        add_insn(bc, BC_DEBUG_BREAK);
    }
    
    convert_statement_to_bytecode(bc, ast->Decl_Stmt.stmt, 0, 0);
    
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
        
        int arg = add_bytecode_register(bc, arg_type);
        func->register_types[arg].flags |= BC_FLAG_RVALUE;
        
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
            if (bc_instruction_insn->res_index >= 0 && insn->opcode != BC_MEMCPY) {
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