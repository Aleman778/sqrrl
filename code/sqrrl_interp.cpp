
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
            result.value = ast->Value;
            result.type = InterpValueType_Numeric;
        } break;
        
        // TODO(alexander): do we want identifiers to be expressions?
        case Ast_Ident: {
            Value* value = symbol_table_resolve_value(interp->symbol_table, ast->Ident);
            if (value) {
                result.value = *value;
                result.type = InterpValueType_Numeric;
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
                        interp_error(interp, "not a number");
                    }
                } break;
                
                case UnaryOp_Dereference: {
                    
                } break;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Binary_Expr.first);
            Interp_Value second_op = interp_expression(interp, ast->Binary_Expr.second);
        } break;
        
        case Ast_Ternary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Ternary_Expr.first);
            Interp_Value second_op = interp_expression(interp, ast->Ternary_Expr.second);
            Interp_Value third_op = interp_expression(interp, ast->Ternary_Expr.third);
            
            if (first_op.type == InterpValueType_Numeric) {
                if (first_op.value.boolean) {
                    return second_op;
                } else {
                    return third_op;
                }
            } else {
                interp_error(interp, "type error: expected left operand to be of numeric type");
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
            
        } break;
        
        case Ast_Index_Expr: {
            
        } break;
        
        case Ast_Array_Expr: {
            
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
    
    Ast* decl = 0; // TODO!
    if (decl) {
        decl = decl->Type_Decl.decl;
        if (decl->type == Ast_Decl_Stmt) {
            Ast* type = decl->Decl_Stmt.type;
            if (type->type == Ast_Function_Type) {
                decl = decl->Decl_Stmt.decl;
                
                // Save sold base pointer on the stack
                smm new_base = interp->stack_pointer;
                smm* old_base = (smm*) interp_stack_push(interp, sizeof(smm));
                *old_base = interp->base_pointer;
                interp->base_pointer = new_base;
                
                result = interp_statement(interp, decl);
                // TODO(alexander): only write to result if it is an actual return value!
                
                interp->stack_pointer = interp->base_pointer;
                interp->base_pointer = *(smm*) ((u8*) interp->stack.base + interp->base_pointer);
            } else {
                interp_error(interp, string_format("`%` is not a function", f_string(vars_load_string(ident))));
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

Type*
interp_type(Interp* interp, Ast* ast) {
    assert(is_ast_type(ast));
    
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
            //Type type = interp_type();
        } break;
    }
    
    return result;
}

void
interp_register_primitive_types(Interp* interp) {
}

void
interp_ast_declarations(Interp* interp, Ast_Decl_Entry* decls) {
    for (int i = 0; i < map_count(decls); i++) {
        Ast_Decl_Entry decl = decls[0];
        Type* type = interp_type(interp, decl.value);
        Entity entity;
        entity.kind = EntityKind_Type;
        entity.type = type;
        map_put(interp->symbol_table, decl.key, entity);
    }
}