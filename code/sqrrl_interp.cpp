
void
interp_save_value(Type* type, void* dest, Value_Data src) {
    value_store_in_memory(type, dest, src);;
}

Interp_Value
interp_load_value(Interp* interp, Type* type, void* data) {
    Interp_Value result = create_interp_value(interp);
    result.value = value_load_from_memory(type, data);
    result.type = *type;
    result.data = data;
    return result;
}

Interp_Value 
interp_expression(Interp* interp, Ast* ast) {
    assert(is_ast_expr(ast) || ast->kind == Ast_Value || ast->kind == Ast_Ident || ast->kind == Ast_None);
    
    Interp_Value result = create_interp_value(interp);
    
    switch (ast->kind) {
        case Ast_Value: {
            result.value = ast->Value.value;
            result.type = *ast->type;
        } break;
        
        case Ast_Ident: {
            result = interp_load_value(interp, ast->Ident);
            if (!result.data && result.type.kind == TypeKind_Unresolved) {
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
                case UnaryOp_Negate: {
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
                
                case UnaryOp_Logical_Not: {
                    if (is_integer(first_op.value)) {
                        result.value.data.boolean = !value_to_bool(first_op.value);
                        result.value.type = Value_boolean;
                    } else {
                        interp_error(interp, string_lit("unary not expects integer type"));
                    }
                } break;
                
                case UnaryOp_Address_Of: {
                    Ast* expr = ast->Unary_Expr.first;
                    
                    if (expr->kind == Ast_Ident) {
                        Interp_Entity entity = interp_load_entity_from_current_scope(interp, expr->Ident);
                        if (entity.is_valid) {
                            
                            if (entity.data && entity.type) {
                                result.value.type = Value_pointer;
                                result.value.data.data = entity.data;
                                
                                Type type = {};
                                type.kind = TypeKind_Pointer;
                                type.Pointer = entity.type;
                                result.type = type;
                            } else {
                                interp_error(interp, string_format("cannot take address of uninitialized variable `%`",
                                                                   f_string(vars_load_string(expr->Ident))));
                            }
                        } else {
                            interp_error(interp, string_format("`%` is an undeclared identifier", 
                                                               f_string(vars_load_string(expr->Ident))));
                        }
                    } else {
                        interp_error(interp, string_lit("address of operator expects an identifier"));
                    }
                } break;
                
                case UnaryOp_Dereference: {
                    Interp_Value op = interp_expression(interp, ast->Unary_Expr.first);
                    
                    if (op.value.type == Value_pointer && op.type.kind == TypeKind_Pointer) {
                        Type* deref_type = op.type.Pointer;
                        result = interp_load_value(interp, deref_type, op.value.data.data);
                    } else {
                        interp_error(interp, string_lit("dereference operator expects identifier"));
                    }
                } break;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Binary_Expr.first);
            
            
            
            if (ast->Binary_Expr.op == BinaryOp_Logical_And) {
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
                
            } else if (ast->Binary_Expr.op == BinaryOp_Logical_Or) {
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
                if (result.value.type != Value_void && is_binary_assign(ast->Binary_Expr.op)) {
                    
                    if (ast->Binary_Expr.first->kind == Ast_Ident) {
                        string_id ident = ast->Binary_Expr.first->Ident;
                        Interp_Entity entity = interp_load_entity_from_current_scope(interp, ident);
                        if (interp_entity_is_declared(interp, &entity, ident)) {
                            if (entity.data) {
                                interp_save_value(entity.type, entity.data, result.value.data);
                            } else {
                                entity.data = interp_push_value(interp, entity.type, result.value.data);
                            }
                        }
                    } else if (first_op.data && first_op.type.kind) {
                        interp_save_value(&first_op.type, first_op.data, first.data);
                        
                    } else {
                        interp_error(interp, string_lit("unexpected assignment"));
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
            result = interp_function_call(interp, ident, ast->Call_Expr.args, ast->Call_Expr.function_type);
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
                        
                        result = interp_load_value(interp, elem_type, data);
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
        
        case Ast_Aggregate_Expr:
        case Ast_Tuple_Expr: {
            result.value.type = Value_ast_node;
            result.value.data.ast = ast;
        } break;
    }
    
    return result;
}

Interp_Value
interp_function_call(Interp* interp, string_id ident, Ast* args, Type* function_type) {
    Interp_Value result = create_interp_value(interp);
    Type* type = function_type;
    
    if (!type) {
        // Try to find local registered type using identifier provided
        type = interp_load_entity_from_current_scope(interp, ident).type;
    }
    
    if (type) {
        if (type->kind == TypeKind_Function) {
            
            Type_Function* func = &type->Function;
            int formal_arg_count = (int) array_count(func->arg_types);
            
            // First evaluate and push the arguments on the new scope
            // NOTE: arguments are pushed on the callers stack space
            
            // TODO(Alexander): this is temporary since we don't have variadic argument support yet.
            array(Interp_Value)* variadic_arguments = 0;
            
            Interp_Scope new_scope = {};
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
                        arg.data = interp_push_value(interp, formal_type, arg.value.data);
                        interp_push_entity_to_scope(&new_scope, arg_ident, arg.data, formal_type);
                        
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
            array_push(interp->scopes, new_scope);
            
            // NOTE(Alexander): secondly push the evaluated arguments on stack
            if (args) {
                if (arg_index != formal_arg_count && !type->Function.is_variadic) {
                    
                    // NOTE(Alexander): it is allowed to have more arguments only if the function is variadic
                    interp_error(interp, string_format("function `%` did not take % arguments, expected % arguments", 
                                                       f_string(vars_load_string(ident)), 
                                                       f_int(arg_index), 
                                                       f_int(formal_arg_count)));
                    return result;
                }
            } else if (formal_arg_count != 0) {
                interp_error(interp, string_format("function `%` expected % arguments",
                                                   f_string(vars_load_string(ident)),
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
                    interp_error(interp, string_format("`%` function has no definition and is no intrinsic", f_string(vars_load_string(ident))));
                }
            }
            
            // Pop the scope and free the data stored in the scope
            Interp_Scope old_scope = array_pop(interp->scopes);
            map_free(old_scope.locals);
            array_free(old_scope.local_stack);
            
            // Pop the stack by restoring the old base pointer
            interp->stack.curr_used = interp->base_pointer;
            interp->base_pointer = *(smm*) ((u8*) interp->stack.base + interp->base_pointer);
            
        } else {
            interp_error(interp, string_format("`%` is not a function", f_string(vars_load_string(ident))));
        }
    } else {
        interp_unresolved_identifier_error(interp, ident);
    }
    
    return result;
}

Interp_Value
interp_field_expr(Interp* interp, Interp_Value var, string_id ident) {
    Interp_Value result = {};
    
    switch (var.type.kind) {
        case TypeKind_Struct: {
            assert(var.value.type == Value_pointer);
            Type_Struct* type = &var.type.Struct;
            
            smm field_index = map_get(type->ident_to_index, ident);
            Type* field_type = type->types[field_index];
            if (field_type) {
                void* data = var.value.data.data;
                smm field_offset = type->offsets[field_index];
                data = (u8*) data + field_offset;
                result = interp_load_value(interp, field_type, data);
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
            if (var.value.type == Value_pointer && var.type.kind == TypeKind_Pointer) {
                Type* deref_type = var.type.Pointer;
                var = interp_load_value(interp, deref_type, var.value.data.data);
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
            Interp_Value expr = interp_expression(interp, ast->Assign_Stmt.expr);
            
            // TODO(Alexander): check expr.type and type
            Type* type = ast->type;
            string_id ident = ast->Assign_Stmt.ident->Ident;
            
            {
                // NOTE(Alexander): check that ident isn't already occupied
                Interp_Scope* scope = interp_get_current_scope(interp);
                if (scope && map_get(scope->locals, ident).is_valid) {
                    interp_error(interp, string_format("cannot redeclare previous local variable `%`",
                                                       f_string(vars_load_string(ident))));
                    break;
                }
            }
            
            switch (type->kind) {
                case TypeKind_Array: {
                    Array_Value array = {};
                    
                    if (is_ast_node(expr.value)) {
                        ast = expr.value.data.ast;
                        if (ast && ast->kind == Ast_Aggregate_Expr) {
                            Ast* elements = ast->Aggregate_Expr.elements;
                            Type* elem_type = type->Array.type;
                            
                            while (elements && elements->kind == Ast_Compound) {
                                Ast* element = elements->Compound.node->Argument.assign;
                                elements = elements->Compound.next;
                                
                                Value elem_value = interp_expression(interp, element).value;
                                if (!is_void(elem_value)) {
                                    void* elem = interp_push_value(interp, elem_type, elem_value.data);
                                    array.count++;
                                    if (!array.elements) {
                                        array.elements = elem;
                                    }
                                } else {
                                    interp_error(interp, string_lit("type error: expected literal value"));
                                }
                            }
                        }
                    } else {
                        // TODO(Alexander): pre allocate array, MUST have a capacity
                        
                        
                        
                        assert(0 && "unimplemented :(");
                    }
                    
                    Value value;
                    value.type = Value_array;
                    value.data.array = array;
                    void* data = interp_push_value(interp, type, value.data);
                    interp_push_entity_to_current_scope(interp, ident, data, type);
                } break;
                
                case TypeKind_Struct: {
                    Type_Struct* t_struct = &type->Struct;
                    assert(type->size > 0);
                    
                    void* base_address = arena_push_size(&interp->stack, 
                                                         (umm) type->size, 
                                                         (umm) type->align);
                    
                    if (is_ast_node(expr.value)) {
                        assert(expr.value.type == Value_ast_node &&
                               expr.value.data.ast->kind == Ast_Aggregate_Expr);
                        
                        // Struct initialization
                        Ast* fields = expr.value.data.ast->Aggregate_Expr.elements;
                        map(string_id, Value)* field_values = 0;
                        
                        // NOTE(Alexander): push elements onto the stack in the order defined by the type
                        // so first push the compound actual values into a auxillary hash map.
                        for_compound(fields, field) {
                            assert(field->kind == Ast_Argument);
                            
                            Interp_Value field_expr = interp_expression(interp, field->Argument.assign);
                            
                            assert(field->Argument.ident);
                            string_id field_ident = field->Argument.ident->Ident;
                            
                            // NOTE(Alexander): check that the actual type matches its definition
                            s32 field_index = map_get(t_struct->ident_to_index, field_ident);
                            Type* def_type = t_struct->types[field_index];
                            if (type_equals(&field_expr.type, def_type)) {
                                // TODO(Alexander): check that the value conforms to def_type
                            } else {
                                interp_mismatched_types(interp, def_type, &field_expr.type);
                            }
                            
                            map_put(field_values, field_ident, field_expr.value);
                        }
                        
                        // Store the result
                        if (field_values) {
                            for_map(field_values, field) {
                                string_id field_ident = field->key;
                                s32 field_index = map_get(t_struct->ident_to_index, field_ident);
                                Type* field_type = t_struct->types[field_index];
                                
                                // TODO(Alexander): handle also nested structs
                                assert(field_type->kind == TypeKind_Basic);
                                
                                Value field_value = value_cast(field->value, field_type->Basic.kind);
                                smm offset = t_struct->offsets[field_index];
                                void* storage = (u8*) base_address + offset;
                                interp_save_value(field_type, storage, field_value.data);
                            }
                        } else {
                            // TODO(Alexander): no fields specified, should we clear the memory maybe?
                            memset((u8*) base_address, 0, (umm) type->size);
                        }
                        
                    } else if (expr.value.type == Value_pointer) {
                        copy_memory(base_address, expr.value.data.data, (umm) type->size);
                    } else {
                        // TODO(Alexander): for now we will clear entire struct/union memory
                        // we will want the user to be able to disable this behaviour
                        memset((u8*) base_address, 0, (umm) type->size);
                    }
                    
                    Value value;
                    value.type = Value_pointer;
                    value.data.data = base_address;
                    void* data = interp_push_value(interp, type, value.data);
                    interp_push_entity_to_current_scope(interp, ident, data, type);
                } break;
                
                case TypeKind_Union: {
                    unimplemented;
                } break;
                
                default: {
                    void* data = interp_push_value(interp, type, expr.value.data);
                    interp_push_entity_to_current_scope(interp, ident, data, type);
                } break;
            }
            
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
        
        case Ast_Decl_Stmt: {
            interp_declaration_statement(interp, ast);
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
    interp->block_depth++;
    
    smm local_base_pointer = 0;
    Interp_Scope* current_scope = interp_get_current_scope(interp);
    if (current_scope) {
        local_base_pointer = (smm) array_count(current_scope->local_stack);
    }
    
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
    
    if (current_scope) {
        // Remove locals declared inside the block scope
        smm take = (smm) array_count(current_scope->local_stack) - local_base_pointer;
        while (take-- > 0) {
            string_id ident = array_pop(current_scope->local_stack);
            map_remove(current_scope->locals, ident);
        }
    }
    
    interp->block_depth--;
    return result;
}

Value
interp_intrinsic_print_format(Interp* interp, array(Interp_Value)* var_args) {
    Interp_Value format = interp_load_value(interp, vars_save_cstring("format"));
    
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
    Interp_Value expr = interp_load_value(interp, vars_save_cstring("expr"));
    
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

void
interp_declaration_statement(Interp* interp, Ast* ast) {
    assert(ast->kind == Ast_Decl_Stmt);
    
    Type* type = ast->type;
    
    assert(ast->Decl_Stmt.ident->kind == Ast_Ident);
    string_id ident = ast->Decl_Stmt.ident->Ident;
    interp_push_entity_to_current_scope(interp, ident, 0, type);
}

void
interp_ast_declarations(Interp* interp, Ast_Decl_Table* decls) {
    Ast** interp_statements = 0;
    
    for_map (decls, it) {
        string_id ident = it->key;
        Ast* decl = it->value;
        
        if (decl->kind == Ast_Decl_Stmt) {
            interp_declaration_statement(interp, decl);
        } else if (decl->type) {
            interp_push_entity_to_current_scope(interp, ident, 0, decl->type);
        }
    }
    
    // HACK(alexander): this should be merged with the loop above,
    // but for the time being we don't want to run any code before injecting the types.
    for_map (decls, decl) {
        Ast* stmt = decl->value;
        
        if (is_ast_stmt(stmt) && stmt->kind != Ast_Decl_Stmt) {
            Interp_Value interp_result = interp_statement(interp, stmt);
            if (!is_void(interp_result.value)) {
                void* data = interp_push_value(interp, &interp_result.type, interp_result.value.data);
                if (!data) {
                    interp_push_entity_to_current_scope(interp, decl->key, data, &interp_result.type);
                }
            }
        }
    }
}
