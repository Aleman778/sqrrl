
Interp_Value 
interp_expression(Interp* interp, Ast* ast) {
    assert(is_ast_expr(ast) || ast->kind == Ast_Value || ast->kind == Ast_Ident || ast->kind == Ast_None);
    
    Interp_Value result = create_interp_value(interp);
    
    switch (ast->kind) {
        case Ast_Value: {
            result.value = ast->Value;
        } break;
        
        case Ast_Ident: {
            result = get_interp_value(interp, ast->type, ast->Ident);
            if (is_void(result.value)) {
                if (interp->set_undeclared_to_zero) {
                    result.value = {};
                    result.value.type = Value_signed_int;
                    result.type = *t_int;
                    
                } else {
                    interp_error(interp, string_format("undeclared identifier `%`", 
                                                       f_string(vars_load_string(ast->Ident))));
                }
            }
        } break;
        
        case Ast_Unary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Unary_Expr.first);
            result.type = *ast->type;
            switch (ast->Unary_Expr.op) {
                case Op_Negate: {
                    if (is_integer(first_op.value)) {
                        result.value.data.signed_int = -first_op.value.data.signed_int;
                        result.value.type = Value_signed_int;
                    } else if(is_floating(first_op.value)) {
                        result.value.data.floating = -first_op.value.data.floating;
                        result.value.type = Value_floating;
                    } else {
                        interp_error(interp, string_lit("unary negate expects numeric type"));
                    }
                } break;
                
                case Op_Logical_Not: {
                    if (is_integer(first_op.value)) {
                        result.value.data.boolean = !value_to_bool(first_op.value);
                        result.value.type = Value_boolean;
                    } else {
                        interp_error(interp, string_lit("unary not expects integer type"));
                    }
                } break;
                
                case Op_Address_Of: {
                    Ast* expr = ast->Unary_Expr.first;
                    Type* type = expr->type;
                    
                    if (expr->kind == Ast_Ident) {
                        string_id ident = ast_unwrap_ident(expr);
                        void* data = get_interp_value_pointer(interp, ident);
                        Value value;
                        value.type = Value_pointer;
                        value.data.data = data;
                        result = value_to_interp_value(interp, value, data);
                    } else {
                        interp_error(interp, string_lit("address of operator expects an identifier"));
                    }
                } break;
                
                case Op_Dereference: {
                    Interp_Value first = interp_expression(interp, ast->Unary_Expr.first);
                    
                    if (first.value.type == Value_pointer && first.data) {
                        // TODO(Alexander): we should check the pointer to make sure it's within the interpreter memory address space.
                        result = interp_value_load_from_memory(interp, ast->type, first.data);
                    } else {
                        interp_error(interp, string_lit("dereference of null pointer"));
                    }
                } break;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Binary_Expr.first);
            
            
            
            if (ast->Binary_Expr.op == Op_Logical_And) {
                result.value.type = Value_boolean;
                result.value.data.boolean = false;
                
                if (!is_integer(first_op.value)) {
                    interp_error(interp, string_lit("type error: mismatched types"));
                }
                
                if (value_to_bool(first_op.value)) {
                    Interp_Value second_op = interp_expression(interp, ast->Binary_Expr.second);
                    if (!is_integer(second_op.value)) {
                        interp_error(interp, string_lit("type error: mismatched types"));
                    }
                    
                    result.value.data.boolean = value_to_bool(second_op.value);
                }
                
            } else if (ast->Binary_Expr.op == Op_Logical_Or) {
                result.value.type = Value_boolean;
                result.value.data.boolean = false;
                
                if (!is_integer(first_op.value)) {
                    interp_error(interp, string_lit("type error: mismatched types"));
                }
                
                if (value_to_bool(first_op.value)) {
                    result.value.data.boolean = true;
                } else {
                    Interp_Value second_op = interp_expression(interp, ast->Binary_Expr.second);
                    if (!is_integer(second_op.value)) {
                        interp_error(interp, string_lit("type error: mismatched types"));
                    }
                    
                    result.value.data.boolean = value_to_bool(second_op.value);
                }
            } else {
                Interp_Value second_op = interp_expression(interp, ast->Binary_Expr.second);
                
                // TODO(Alexander): check illegal types such as arrays and struct literals!
                Value first = first_op.value;
                Value second = second_op.value;
                
                // NOTE(Alexander): Type rules
                // int + int;
                // float + int -> float + float;
                // int + float -> float + float;
                // float -x-> int (is a no no, it has to be an explicit cast)
                
                if (is_floating(first) || is_floating(second)) {
                    // NOTE(Alexander): Make sure both types are floating
                    if (is_integer(first)) {
                        first.data.floating  = value_to_f64(first);
                        first.type = Value_floating;
                        result.type = second_op.type;
                    } else if (is_integer(second)) {
                        second.data.floating = value_to_f64(second);
                        second.type = Value_floating;
                        result.type = first_op.type;
                    } else if (is_floating(first) && is_floating(second)) {
                        result.type = first_op.type;
                    } else {
                        interp_error(interp, string_lit("type error: mismatched types"));
                    }
                    
                    first = value_floating_binary_operation(first, second, ast->Binary_Expr.op);
                    
                    result.value = first;
                } else if (is_integer(first) || is_integer(second)) {
                    
                    // NOTE(Alexander): integer math
                    first.data.signed_int = value_integer_binary_operation(first, second, ast->Binary_Expr.op);
                    
                    result.value = first;
                    result.type = *ast->type;
                } else {
                    interp_mismatched_types(interp, &first_op.type, &second_op.type);
                }
                
                // NOTE(Alexander): handle assign binary expression
                if (!is_void(result.value) && operator_is_assign(ast->Binary_Expr.op)) {
                    
                    void* data = first_op.data;
                    if (ast->Binary_Expr.first->kind == Ast_Ident) {
                        string_id ident = ast_unwrap_ident(ast->Binary_Expr.first);
                        data = get_interp_value_pointer(interp, ident);
                    }
                    
                    if (data) {
                        result = interp_value_load_from_memory(interp, ast->type, data);
                    } else {
                        interp_error(interp, string_lit("assignment isn't possible"));
                    }
                }
            }
        } break;
        
        case Ast_Ternary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Ternary_Expr.first);
            Interp_Value second_op = interp_expression(interp, ast->Ternary_Expr.second);
            Interp_Value third_op = interp_expression(interp, ast->Ternary_Expr.third);
            
            if (is_integer(first_op.value)) {
                if (first_op.value.data.boolean) {
                    result = second_op;
                } else {
                    result = third_op;
                }
            } else {
                interp_error(interp, string_lit("type error: expected left operand to a boolean"));
            }
            
            
        } break;
        
        case Ast_Call_Expr: {
            string_id ident = ast_unwrap_ident(ast->Call_Expr.ident);
            result = interp_function_call(interp, ast->Call_Expr.args, ast->Call_Expr.function_type);
            result.modifier = InterpValueMod_None; // NOTE(Alexander): avoids returing multiple times
        } break;
        
        case Ast_Field_Expr: {
            Interp_Value var = interp_expression(interp, ast->Field_Expr.var);
            
            assert(ast->Field_Expr.field->kind == Ast_Ident); // TODO(Alexander): turn into an error, where?
            string_id ident = ast->Field_Expr.field->Ident;
            
            result = interp_field_expr(interp, var, ident);
        } break;
        
        case Ast_Cast_Expr: {
            Type* type = ast->type;
            Interp_Value expr = interp_expression(interp, ast->Cast_Expr.expr);
            Value value = expr.value;
            
            switch (type->kind) {
                case TypeKind_Basic: {
                    result.value = value_cast(value, type->Basic.kind);
                } break;
                
                case TypeKind_Pointer: {
                    if (value.type == Value_array) {
                        result.value.type = Value_pointer;
                        result.value.data.data = value.data.array.elements;
                        
                    } else if (is_integer(value)) {
                        result.value = value;
                        
                    } else {
                        interp_error(interp, string_lit("cannot type cast non-pointer value to pointer, use dereference instead"));
                    }
                } break;
                
                default: {
                    assert(0 && "unimplemented :(");
                } break;
                
            }
            
            result.type = *type;
        } break;
        
        case Ast_Paren_Expr: {
            result = interp_expression(interp, ast->Paren_Expr.expr);
        } break;
        
        case Ast_Index_Expr: {
            Interp_Value array = interp_expression(interp, ast->Index_Expr.array);
            
            // TODO(Alexander): also allow pointer numerics
            if (array.value.type == Value_array) {
                
                Interp_Value index = interp_expression(interp, ast->Index_Expr.index);
                if (is_integer(index.value)) {
                    Array_Value array_value = array.value.data.array;
                    
                    smm array_index = value_to_smm(index.value);
                    if (array_index < array_value.count) {
                        Type* elem_type = array.type.Array.type;
                        
                        smm elem_size = elem_type->size;
                        assert(elem_size > 0 && "not valid array element size");
                        
                        void* data = (void*) array_value.elements;
                        data = (u8*) data + array_index * elem_size;
                        
                        result = interp_value_load_from_memory(interp, ast->type, data);
                    } else {
                        interp_error(interp, string_lit("array index out of bounds"));
                    }
                } else {
                    interp_error(interp, string_lit("type error: expected numeric array index"));
                }
            } else {
                interp_error(interp, string_lit("type error: expected array value"));
            }
        } break;
        
        case Ast_Aggregate_Expr: {
            result.value.type = Value_pointer;
            result.value.data.data = convert_aggregate_literal_to_memory(ast);
        } break;
        
        case Ast_Tuple_Expr: {
            unimplemented;
        } break;
    }
    
    return result;
}

Interp_Value
interp_function_call(Interp* interp, Ast* args, Type* function_type) {
    Interp_Value result = create_interp_value(interp);
    Type* type = function_type;
    
    if (type) {
        if (type->kind == TypeKind_Function) {
            
            Type_Function* func = &type->Function;
            int formal_arg_count = (int) array_count(func->arg_types);
            
            // First evaluate and push the arguments on the new scope
            // NOTE: arguments are pushed on the callers stack space
            
            // TODO(Alexander): this is temporary since we don't have variadic argument support yet.
            array(Interp_Value)* variadic_arguments = 0;
            
            Interp_Scope new_scope = {};
            new_scope.prev_scope = interp->curr_scope;
            interp->curr_scope = &new_scope;
            
            int arg_index = 0;
            if (args) { 
                for_compound(args, arg_ast) {
                    Ast* expr = arg_ast->Argument.assign;
                    Interp_Value arg = interp_expression(interp, expr);
                    
                    // Only interpret as many args as formally declared
                    if (arg_index < formal_arg_count) {
                        // TODO(Alexander): assign binary expressions needs to be handled here
                        string_id arg_ident = func->arg_idents[arg_index];
                        Type* formal_type = func->arg_types[arg_index];
                        
                        // NOTE(Alexander): we need to allocate it in order to pass it to the function
                        // Currently we only pass variables to functions through references
                        push_interp_value(interp, formal_type, arg_ident, arg);
                    } else if (type->Function.is_variadic) {
                        array_push(variadic_arguments, arg);
                    }
                    
                    arg_index++;
                }
            }
            
            // Store the old base pointer on the stack and set the base pointer
            // to point at the top of the stack, then push the new scope
            smm* old_base = (smm*) arena_push_size(&interp->stack, sizeof(smm));
            smm new_base = interp->stack.curr_used - sizeof(smm);
            *old_base = interp->base_pointer;
            interp->base_pointer = new_base;
            
            // NOTE(Alexander): secondly push the evaluated arguments on stack
            if (args) {
                if (arg_index != formal_arg_count && !type->Function.is_variadic) {
                    
                    // NOTE(Alexander): it is allowed to have more arguments only if the function is variadic
                    interp_error(interp, string_format("function `%` did not take % arguments, expected % arguments", 
                                                       f_var(function_type->Function.ident), 
                                                       f_int(arg_index), 
                                                       f_int(formal_arg_count)));
                    return result;
                }
            } else if (formal_arg_count != 0) {
                interp_error(interp, string_format("function `%` expected % arguments",
                                                   f_var(function_type->Function.ident), 
                                                   f_int(formal_arg_count)));
            }
            
            Ast* block = type->Function.unit->ast;
            if (block) {
                result = interp_statement(interp, block);
                // TODO(alexander): only write to result if it is an actual return value!
                
            } else {
                // TODO(Alexander): this is not supposed to ever be the case, maybe assert instead!
                // NOTE(Alexander): what about FFI?
                
                if (type->Function.intrinsic) {
                    result.value = type->Function.interp_intrinsic(interp, variadic_arguments);
                } else {
                    interp_error(interp,
                                 string_format("`%` function has no definition and is no intrinsic",
                                               f_var(function_type->Function.ident)));
                }
            }
            
            // Pop the scope and free the data stored in the scope
            map_free(interp->curr_scope->locals);
            array_free(interp->curr_scope->local_stack);
            interp->curr_scope = interp->curr_scope->prev_scope;
            
            // Pop the stack by restoring the old base pointer
            interp->stack.curr_used = interp->base_pointer;
            interp->base_pointer = *(smm*) ((u8*) interp->stack.base + interp->base_pointer);
            
        } else {
            interp_error(interp, string_format("`%` is not a function", 
                                               f_var(function_type->Function.ident)));
        }
    } else {
        interp_unresolved_identifier_error(interp, function_type->Function.ident);
    }
    
    return result;
}

Interp_Value
interp_field_expr(Interp* interp, Interp_Value var, string_id ident) {
    Interp_Value result = {};
    
    switch (var.type.kind) {
        case TypeKind_Struct: {
            assert(var.value.type == Value_pointer);
            Struct_Like_Info* type = &var.type.Struct_Like;
            
            smm field_index = map_get(type->ident_to_index, ident);
            Type* field_type = type->types[field_index];
            if (field_type && var.value.type == Value_pointer && var.data) {
                void* data = var.data;
                smm field_offset = type->offsets[field_index];
                data = (u8*) data + field_offset;
                result = interp_value_load_from_memory(interp, field_type, data);
            } else {
                interp_error(interp, string_format("`%` is not a field of type `%`",
                                                   f_string(vars_load_string(ident)), f_string(vars_load_string(var.type.ident))));
            }
        } break;
        
        case TypeKind_Union: {
            unimplemented;
        } break;
        
        case TypeKind_Enum: {
            result.value = map_get(var.type.Enum.values, ident);
            result.type = *var.type.Enum.type;
        } break;
        
        case TypeKind_Pointer: {
            if (var.value.type == Value_pointer && var.data) {
                Type* deref_type = var.type.Pointer;
                var = interp_value_load_from_memory(interp, deref_type, var.value.data.data);
                result = interp_field_expr(interp, var, ident);
                
            } else {
                interp_error(interp, string_lit("dereference operator expects identifier"));
            }
        } break;
        
        
        default: {
            interp_error(interp, string_format("left of `.%` must be a pointer, struct, union or enum",
                                               f_string(vars_load_string(ident))));
        } break;
    }
    
    return result;
}

Interp_Value
interp_statement(Interp* interp, Ast* ast) {
    assert(is_ast_stmt(ast) || ast->kind == Ast_None);
    
    Interp_Value result = {};
    
    switch (ast->kind) {
        case Ast_Assign_Stmt: {
            result = interp_expression(interp, ast->Assign_Stmt.expr);
            
            // TODO(Alexander): check expr.type and type
            Type* type = ast->type;
            string_id ident = ast->Assign_Stmt.ident->Ident;
            result.data = push_interp_value(interp, type, ident, result);
        } break;
        
        case Ast_Expr_Stmt: {
            result = interp_expression(interp, ast->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            result = interp_block(interp, ast->Block_Stmt.stmts);
        } break;
        
        case Ast_Break_Stmt: {
            result.modifier = InterpValueMod_Break;
            result.label = ast->Break_Stmt.ident->Ident;
        } break;
        
        case Ast_Continue_Stmt: {
            result.modifier = InterpValueMod_Continue;
            result.label = ast->Continue_Stmt.ident->Ident;
        } break;
        
        case Ast_If_Stmt: {
            Interp_Value condition = interp_expression(interp, ast->If_Stmt.cond);
            if (is_integer(condition.value)) {
                if (value_to_bool(condition.value)) {
                    result = interp_statement(interp, ast->If_Stmt.then_block);
                } else {
                    result = interp_statement(interp, ast->If_Stmt.else_block);
                }
            } else {
                interp_error(interp, string_lit("type error: expected boolean condition"));
            }
        } break;
        
        case Ast_For_Stmt: {
            interp_statement(interp, ast->For_Stmt.init);
            Interp_Value condition = interp_expression(interp, ast->For_Stmt.cond);
            if (condition.value.type == Value_void || is_integer(condition.value)) {
                // NOTE(Alexander): type check on condition
                while (condition.value.type == Value_void || value_to_bool(condition.value)) {
                    Interp_Value block = interp_statement(interp, ast->For_Stmt.block);
                    if (block.modifier == InterpValueMod_Return) {
                        result = block;
                        break;
                    } else if (block.modifier == InterpValueMod_Break) {
                        break;
                    }
                    
                    interp_expression(interp, ast->For_Stmt.update);
                    condition = interp_expression(interp, ast->For_Stmt.cond);
                    if (condition.value.type != Value_void && !is_integer(condition.value)) {
                        interp_error(interp, string_lit("type error: expected boolean condition"));
                        break;
                    }
                }
            } else {
                interp_error(interp, string_lit("type error: expected boolean condition"));
            }
            
        } break;
        
        case Ast_While_Stmt: {
            Interp_Value condition = interp_expression(interp, ast->While_Stmt.cond);
            if (is_integer(condition.value)) {
                while (value_to_bool(condition.value)) {
                    Interp_Value block = interp_statement(interp, ast->While_Stmt.block);
                    if (block.modifier == InterpValueMod_Return) {
                        result = block;
                        break;
                    } else if (block.modifier == InterpValueMod_Break) {
                        break;
                    }
                    
                    condition = interp_expression(interp, ast->While_Stmt.cond);
                    if (!is_integer(condition.value)) {
                        interp_error(interp, string_lit("type error: expected boolean condition"));
                        break;
                    }
                }
            } else {
                interp_error(interp, string_lit("type error: expected boolean condition"));
            }
        } break;
        
        case Ast_Return_Stmt: {
            result = interp_expression(interp, ast->Return_Stmt.expr);
            result.modifier = InterpValueMod_Return;
        } break;
    }
    
    return result;
}

Interp_Value
interp_block(Interp* interp, Ast* ast) {
    assert(interp->curr_scope);
    interp->block_depth++;
    
    Interp_Scope* current_scope = interp->curr_scope;
    smm local_base_pointer = 0;
    local_base_pointer = (smm) array_count(interp->curr_scope->local_stack);
    
    Interp_Value result = {};
    while (ast->kind == Ast_Compound) {
        result = interp_statement(interp, ast->Compound.node);
        if (interp->error_count) {
            break;
        }
        
        if (result.modifier == InterpValueMod_Return ||
            result.modifier == InterpValueMod_Continue ||
            result.modifier == InterpValueMod_Break) {
            break;
        }
        
        ast = ast->Compound.next;
    }
    
    // Remove locals declared inside the block scope
    smm take = (smm) array_count(current_scope->local_stack) - local_base_pointer;
    while (take-- > 0) {
        string_id ident = array_pop(current_scope->local_stack);
        map_remove(current_scope->locals, ident);
    }
    
    interp->block_depth--;
    return result;
}

Value
interp_intrinsic_print_format(Interp* interp, array(Interp_Value)* var_args) {
    Interp_Value format = get_interp_value(interp, t_cstring, vars_save_cstring("format"));
    
    if (format.value.type == Value_string || 
        (format.type.kind == TypeKind_Basic && format.type.Basic.kind == Basic_string)) {
        
        if (var_args) {
            String_Builder sb = {};
            
            // TODO(Alexander): this is a hack, bytecode use Memory_String
            u8* scan;
            u8* scan_end;
            if (interp->flag_running_in_bytecode) {
                Memory_String format_string = format.value.data.mstr;
                scan = (u8*) format_string;
                scan_end = scan + memory_string_count(format_string);
            } else {
                string format_string = format.value.data.str;
                int format_count = (int) format_string.count;
                scan = (u8*) format_string.data;
                scan_end = scan + format_count;
            }
            u8* scan_at_prev_percent = scan;
            
            int var_arg_index = 0;
            int count_until_percent = 0;
            while (scan < scan_end) {
                if (*scan++ == '%') {
                    if (count_until_percent > 0) {
                        string substring = create_string(count_until_percent, scan_at_prev_percent);
                        string_builder_push(&sb, substring);
                        count_until_percent = 0;
                    }
                    scan_at_prev_percent = scan;
                    
                    if (*scan == '%') {
                        string_builder_push(&sb, "%");
                        scan += 2;
                        continue;
                    }
                    
                    if (var_arg_index >= array_count(var_args)) {
                        interp_error(interp, string_format("not enough arguments passed to function"));
                        string_builder_free(&sb);
                        return {};
                    }
                    
                    Interp_Value* var_arg;
                    Format_Type format_type;
                    if (interp->flag_running_in_bytecode) {
                        Interp_Value* format_arg = var_args + var_arg_index++;
                        var_arg = var_args + var_arg_index++;
                        format_type = (Format_Type) value_to_s64(format_arg->value);
                    } else {
                        var_arg = var_args + var_arg_index++;
                        format_type = convert_type_to_format_type(&var_arg->type);
                        if (format_type == FormatType_None) {
                            // NOTE(Alexander): if type didn't help then we guess based on value
                            format_type = convert_value_type_to_format_type(var_arg->value.type);
                        }
                    }
                    
                    void* value_data;
                    if (var_arg->value.type == Value_string) {
                        value_data = &var_arg->value.data;
                    } else {
                        value_data = var_arg->value.data.data;
                    }
                    
                    if (format_type == FormatType_None) {
                        string_builder_push(&sb, "%");
                    } else {
                        string_builder_push_format(&sb, "%", format_type, value_data);
                    }
                } else {
                    count_until_percent++;
                }
            }
            if (count_until_percent > 0) {
                string substring = create_string(count_until_percent, scan_at_prev_percent);
                string_builder_push(&sb, substring);
            }
            printf("%.*s", (int) sb.curr_used, (char*) sb.data);
            string_builder_free(&sb);
        } else {
            pln("%", f_string(format.value.data.str));
        }
    } else {
        interp_error(interp, string_format("expected `string` as first argument, found `%`",
                                           f_type(&format.type)));
    }
    
    return {};
}

Value
interp_intrinsic_debug_break(Interp* interp, array(Interp_Value)* var_args) {
    // TODO(Alexander): this is a msvc intrinsic
    //__debugbreak();
    
    // __builtin_trap or inline int3 asm should be used for gcc etc.
    Value result = {};
    return result;
}

Value
interp_intrinsic_assert(Interp* interp, array(Interp_Value)* var_args) {
    Interp_Value expr = get_interp_value(interp, t_s32, vars_save_cstring("expr"));
    
    if (expr.value.type == Value_signed_int || 
        (expr.type.kind == TypeKind_Basic &&
         is_bitflag_set(expr.type.Basic.flags, BasicFlag_Integer))) {
        intrinsic_assert((int) expr.value.data.signed_int);
    } else {
        interp_error(interp, string_format("expected `int` as first argument, found `%`",
                                           f_type(&expr.type)));
    }
    
    Value result = {};
    return result;
}
