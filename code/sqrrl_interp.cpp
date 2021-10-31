
inline Entity
symbol_table_resolve_identifier(Symbol_Table* table, string_id ident) {
    return map_get(table, ident);
}

inline Value*
symbol_table_resolve_value(Symbol_Table* table, string_id ident) {
    Entity entity = symbol_table_resolve_identifier(table, ident);
    if (entity.kind == EntityKind_Value) {
        return entity.value;
    }
    return 0;
}

// NOTE(Alexander): resolved type is a type definition and not a value that has a type.
//                  typedef s32 b32; b32 resolves to a typedef,  s32 x = 10; x resolves to  null.
inline Type*
symbol_table_resolve_type(Symbol_Table* table, string_id ident) {
    Entity entity = symbol_table_resolve_identifier(table, ident);
    if (entity.kind == EntityKind_Type) {
        return entity.type;
    }
    return 0;
}

Value*
symbol_table_store_value(Symbol_Table* table, Arena* arena, string_id ident) {
    Value* result = arena_push_struct(arena, Value);
    Entity entity;
    entity.kind = EntityKind_Value;
    entity.value = result;
    map_put(table, ident, entity);
    return result;
}

// NOTE(Alexander): only type definitions
Type*
symbol_table_store_type(Symbol_Table* table, Arena* arena, string_id ident) {
    Type* result = arena_push_struct(arena, Type);
    Entity entity;
    entity.kind = EntityKind_Type;
    entity.type = result;
    map_put(table, ident, entity);
    return result;
}

Interp_Value 
interp_expression(Interp* interp, Ast* ast) {
    assert(is_ast_expr(ast) || ast.type == Ast_Value || ast.type == Ast_Ident);
    
    Interp_Value result = create_interp_value(interp);
    
    switch (ast->type) {
        // TODO(alexander): do we want values to be expressions?
        case Ast_Value: {
            result = value_to_interp_value(interp, ast->Value);
        } break;
        
        // TODO(alexander): do we want identifiers to be expressions?
        case Ast_Ident: {
            Value* value = symbol_table_resolve_value(interp->symbol_table, ast->Ident);
            if (value) {
                result = value_to_interp_value(interp, *value);
            }
        } break;
        
        case Ast_Unary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Unary_Expr.first);
            switch (ast->Unary_Expr.op) {
                case UnaryOp_Negate: {
                    if (first_op.type == InterpValueType_Numeric) {
                        result.value.signed_int = -first_op.value.signed_int;
                    }
                } break;
                
                case UnaryOp_Not: {
                    if (first_op.type == InterpValueType_Numeric) {
                        result.value.boolean = !value_to_u64(first_op.value);
                        result.type = InterpValueType_Numeric;
                    } else {
                        interp_error(interp, string_lit("not a number"));
                    }
                } break;
                
                case UnaryOp_Dereference: {
                    assert(0 && "unimplemented :(");
                } break;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Binary_Expr.first);
            Interp_Value second_op = interp_expression(interp, ast->Binary_Expr.second);
            
            // TODO(Alexander): check illegal types such as arrays and struct literals!
            
            if (first_op.type  == InterpValueType_Numeric &&
                second_op.type == InterpValueType_Numeric) {
                
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
                        first.type = Value_floating;
                        first.floating  = value_to_f64(first);
                    }
                    if (is_integer(second)) {
                        second.type = Value_floating;
                        second.floating = value_to_f64(second);
                    }
                    
                    first.floating = value_floating_binary_operation(first, second, ast->Binary_Expr.op);
                    
                    // TODO(Alexander): for assign update memory
                    result.type = InterpValueType_Numeric;
                    result.value = first;
                } else if (is_integer(first) || is_integer(second)) {
                    // NOTE(Alexander): integer math
                    
                    first.signed_int = value_integer_binary_operation(first, second, ast->Binary_Expr.op);
                    
                    // TODO(Alexander): for assign update memory
                    result.type = InterpValueType_Numeric;
                    result.value = first;
                } else {
                    interp_error(interp, string_lit("type error: mismatched types"));
                }
            } else {
                interp_error(interp, string_lit("type error: expected numeric value"));
            }
        } break;
        
        case Ast_Ternary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Ternary_Expr.first);
            Interp_Value second_op = interp_expression(interp, ast->Ternary_Expr.second);
            Interp_Value third_op = interp_expression(interp, ast->Ternary_Expr.third);
            
            if (first_op.type == InterpValueType_Numeric && is_integer(first_op.value)) {
                if (first_op.value.boolean) {
                    result = second_op;
                } else {
                    result = third_op;
                }
            } else {
                interp_error(interp, string_lit("type error: expected left operand to a boolean"));
            }
            
            
        } break;
        
        case Ast_Call_Expr: {
            string_id ident = ast->Call_Expr.ident->Ident;
            interp_function_call(interp, ident);
        } break;
        
        case Ast_Field_Expr: {
            //Value value = interp_expression(interp, ast->Field_Expr.var);
            //assert(ast->Field_Expr.field == Ast_Ident);
            
        } break;
        
        case Ast_Cast_Expr: {
            Type* type = interp_type(interp, ast->Cast_Expr.type);
            Interp_Value value = interp_expression(interp, ast->Cast_Expr.expr);
        } break;
        
        case Ast_Paren_Expr: {
            result = interp_expression(interp, ast->Paren_Expr.expr);
        } break;
        
        case Ast_Index_Expr: {
            Interp_Value array = interp_expression(interp, ast->Index_Expr.array);
            
            // TODO(Alexander): also allow pointer numerics
            if (array.type == InterpValueType_Array) {
                
                Interp_Value index = interp_expression(interp, ast->Index_Expr.index);
                if (index.type == InterpValueType_Numeric && is_integer(index.value)) {
                    Array_Value array_value = array.value.array;
                    Value* elements = array_value.elements;
                    
                    smm array_index = value_to_smm(index.value);
                    if (array_index < array_value.count) {
                        result.value = elements[array_index];
                        result.type = InterpValueType_Numeric; // TODO(Alexander): not always numeric!!!
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
        
        case Ast_Array_Expr: {
            Ast* elements = ast->Array_Expr.elements;
            
            Array_Value array = {};
            
            while (elements && elements->type == Ast_Compound) {
                Ast* element = elements->Compound.node;
                elements = elements->Compound.next;
                
                Interp_Value elem = interp_expression(interp, element);
                if (elem.type == InterpValueType_Numeric) {
                    Value* value = arena_push_struct(&interp->stack, Value);
                    *value = elem.value;
                    array.count++;
                    if (!array.elements) {
                        array.elements = value;
                    }
                } else {
                    interp_error(interp, string_lit("type error: expected literal value"));
                }
            }
            
            result.type = InterpValueType_Array;
            result.value.type = Value_array;
            result.value.array = array;
        } break;
        
        case Ast_Struct_Expr: {
            
        } break;
        
        case Ast_Tuple_Expr: {
            
        } break;
    }
    
    return result;
}

Interp_Value
interp_function_call(Interp* interp, string_id ident) {
    Interp_Value result = create_interp_value(interp);
    
    Type* decl_type = symbol_table_resolve_type(interp->symbol_table, ident);
    if (decl_type) {
        
        if (decl_type->kind == TypeKind_Function) {
            Ast* block = decl_type->Function.block;
            if (block) {
                
                // Save sold base pointer on the stack
                smm new_base = interp->stack.curr_used;
                smm* old_base = (smm*) arena_push_size(&interp->stack, sizeof(smm));
                *old_base = interp->base_pointer;
                interp->base_pointer = new_base;
                
                result = interp_statement(interp, block);
                // TODO(alexander): only write to result if it is an actual return value!
                
                interp->stack.curr_used = interp->base_pointer;
                interp->base_pointer = *(smm*) ((u8*) interp->stack.base + interp->base_pointer);
            } else {
                // TODO(Alexander): this is not supposed to ever be the case, maybe assert instead!
                interp_error(interp, string_format("`%` function has no definition", f_string(vars_load_string(ident))));
            }
        } else {
            interp_error(interp, string_format("`%` is not a function", f_string(vars_load_string(ident))));
        }
    } else {
        interp_unresolved_identifier_error(interp, ident);
    }
    
    return result;
}

Interp_Value
interp_statement(Interp* interp, Ast* ast) {
    assert(is_ast_stmt(ast));
    
    Interp_Value result = {};
    
    switch (ast->type) {
        case Ast_Assign_Stmt: {
            Interp_Value expr = interp_expression(interp, ast->Assign_Stmt.expr);
            Type* type = interp_type(interp, ast->Assign_Stmt.type);
            string_id ident = ast->Assign_Stmt.ident->Ident;
            Value* value = symbol_table_store_value(interp->symbol_table, &interp->stack, ident);
            *value = expr.value;
        } break;
        
        case Ast_Expr_Stmt: {
            result = interp_expression(interp, ast->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            result = interp_block(interp, ast->Block_Stmt.stmts);
        } break;
        
        case Ast_Break_Stmt: {
            result.type = InterpValueType_Break;
            result.label = ast->Break_Stmt.ident->Ident;
        } break;
        
        case Ast_Continue_Stmt: {
            result.type = InterpValueType_Continue;
            result.label = ast->Continue_Stmt.ident->Ident;
        } break;
        
        case Ast_Decl_Stmt: {
        } break;
        
        case Ast_If_Stmt: {
        } break;
        
        case Ast_For_Stmt: {
        } break;
        
        case Ast_While_Stmt: {
        } break;
        
        case Ast_Loop_Stmt: {
        } break;
        
        case Ast_Return_Stmt: {
            result = interp_expression(interp, ast->Return_Stmt.expr);
        } break;
    }
    
    return result;
}

Interp_Value
interp_block(Interp* interp, Ast* ast) {
    interp->block_depth++;
    
    Interp_Value result = {};
    while (ast->type == Ast_Compound) {
        result = interp_statement(interp, ast->Compound.node);
        ast = ast->Compound.next;
        
        if (result.type == InterpValueType_Return ||
            result.type == InterpValueType_Continue ||
            result.type == InterpValueType_Break) {
            break;
        }
    }
    
    interp->block_depth--;
    return result;
}


internal Type_Table*
interp_formal_arguments(Interp* interp, Ast* arguments) {
    Type_Table* result = 0;
    while (arguments && arguments->type == Ast_Compound) {
        Ast* argument = arguments->Compound.node;
        arguments = arguments->Compound.next;
        assert(argument->type == Ast_Argument);
        
        
        if (!argument->Argument.type) {
            break;
        }
        
        Type* type = interp_type(interp, argument->Argument.type);
        string_id ident = argument->Argument.ident->Ident;
        
        // TODO(Alexander): NEED TO HANDLE THE TYPE TABLE HASH MAP MEMORY
        map_put(result, ident, type);
    }
    
    return result;
}

Type*
interp_type(Interp* interp, Ast* ast) {
    assert(is_ast_type(ast));
    // TODO(Alexander): right now we store types on the stack, maybe use separate storage?
    // since this will likely get used elsewhere in the system.
    
    Type* result = 0;
    switch (ast->type) {
        case Ast_Named_Type: {
            string_id ident = ast->Named_Type->Ident;
            Type* type = symbol_table_resolve_type(interp->symbol_table, ident);
            if (type) {
                result = type;
            } else {
                interp_unresolved_identifier_error(interp, ident);
            }
        } break;
        
        case Ast_Array_Type: {
            Type* elem_type = interp_type(interp, ast->Array_Type.elem_type);
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Array;
            result->Array.type = elem_type;
            // TODO(Alexander): what is the shape, expression, I assume right now it's an integer?
            Interp_Value capacity = interp_expression(interp, ast->Array_Type.shape); 
            if (capacity.type == InterpValueType_Numeric) {
                result->Array.capacity= value_to_smm(capacity.value);
            } else {
                result->Array.capacity = 0;
            }
        } break;
        
        
        case Ast_Pointer_Type: {
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Pointer;
            result->Pointer = interp_type(interp, ast->Pointer_Type);
        } break;
        
        case Ast_Tuple_Type: {
            // TODO(Alexander): implement this
            assert(0 && "unimplemented");
        } break;
        
        case Ast_Infer_Type: {
            // TODO(Alexander): implement this
            assert(0 && "unimplemented");
        } break;
        
        case Ast_Function_Type: {
            
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Function;
            
            // NOTE(Alexander): Loads in the function arguments
            result->Function.arguments = interp_formal_arguments(interp, ast->Function_Type.arg_types);
            
            result->Function.return_value = interp_type(interp, ast->Function_Type.return_type);
            assert(ast->Function_Type.ident && ast->Function_Type.ident->type == Ast_Ident);
            result->Function.ident = ast->Function_Type.ident->Ident;
        } break;
        
        case Ast_Struct_Type: {
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Struct;
            result->Struct.fields = interp_formal_arguments(interp, ast->Struct_Type.fields);
        } break;
        
        case Ast_Union_Type: {
            result = arena_push_struct(&interp->stack, Type);
            result->kind = TypeKind_Union;
            result->Union.fields = interp_formal_arguments(interp, ast->Union_Type.fields);
        } break;
        
        case Ast_Enum_Type: {
            // TODO(Alexander): implement this
            assert(0 && "unimplemented");
        } break;
        
        case Ast_Typedef: {
            result = interp_type(interp, ast->Typedef.type);
        } break;
    }
    
    return result;
}

void
interp_register_primitive_types(Interp* interp) {
    for (int i = 0; i < fixed_array_count(global_primitive_types); i++) {
        Type* type = &global_primitive_types[i];
        string_id ident = (string_id) (Kw_int + type->Primitive.kind);
        Entity entity;
        entity.kind = EntityKind_Type;
        entity.type = type;
        map_put(interp->symbol_table, ident, entity);
    }
}

void
interp_ast_declarations(Interp* interp, Ast_Decl_Entry* decls) {
    Ast** interp_statements = 0;
    
    for (int i = 0; i < map_count(decls); i++) {
        Ast_Decl_Entry decl = decls[i];
        assert(decl.value->type == Ast_Type_Decl);
        Ast* stmt = decl.value->Decl.stmt;
        
        Entity entity = {};
        if (stmt->type == Ast_Decl_Stmt) {
            Type* type = interp_type(interp, stmt->Decl_Stmt.type);
            if (type->kind == TypeKind_Function) {
                type->Function.block = stmt->Decl_Stmt.decl;;
            }
            entity.kind = EntityKind_Type;
            entity.type = type;
            map_put(interp->symbol_table, decl.key, entity);
        }
    }
    
    // HACK(alexander): this should be merged with the loop above,
    // but for the time being we don't want to run any code before injecting the types.
    for (int i = 0; i < map_count(decls); i++) {
        Ast_Decl_Entry decl = decls[i];
        assert(decl.value->type == Ast_Type_Decl);
        Ast* stmt = decl.value->Decl.stmt;
        
        Entity entity = {};
        if (stmt->type != Ast_Decl_Stmt) {
            Interp_Value interp_result = interp_statement(interp, stmt);
            if (interp_result.type != InterpValueType_Void) {
                Value* value = arena_push_struct(&interp->stack, Value);
                *value = interp_result.value;
                
                entity.kind = EntityKind_Value;
                entity.value = value;
                map_put(interp->symbol_table, decl.key, entity);
            }
        }
    }
}
