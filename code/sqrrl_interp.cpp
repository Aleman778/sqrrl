
Interp_Value
interp_resolve_identifier(Interp* interp, string_id ident) {
    Interp_Value result = create_interp_value(interp);
    Value* value = map_get(interp->symbol_table, ident);
    if (!value) {
        return result;
    }

    result.value = *value;
    result.type = InterpValueType_Numeric;
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
            result = interp_resolve_identifier(interp, ast->Ident);
        } break;
            
        case Ast_Unary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Unary_Expr.first);
            switch (ast->Unary_Expr.op) {
                case UnaryOp_Negate: {
                    
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
            
        } break;
        
        case Ast_Call_Expr: {
            
        } break;
        
        case Ast_Field_Expr: {
            
        } break;
        
        case Ast_Cast_Expr: {
            
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
interp_function_call(Interp* interp, Ast* ast) {
    return {};
}

Interp_Value
interp_statement(Interp* interp, Ast* ast) {
    assert(is_ast_stmt(ast));
    
    Interp_Value result = {};
    
    switch (ast->type) {
        case Ast_Assign_Stmt: {
            
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
