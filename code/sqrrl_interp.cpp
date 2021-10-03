
Interp_Value 
interp_expression(Interp* interp, Ast* ast) {
    assert(is_ast_expr(ast));
    
    Interp_Value result = {};
    
    switch (ast->type) {
        case Ast_Unary_Expr: {
            Interp_Value first_op = interp_expression(interp, ast->Unary_Expr.first);
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
            result.result_type = InterpResultType_Break;
            result.label = ast->Break_Stmt.ident->Ident;
        } break;
        
        case Ast_Continue_Stmt: {
            result.result_type = InterpResultType_Continue;
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
        
        if (result.result_type == InterpResultType_Return ||
            result.result_type == InterpResultType_Continue ||
            result.result_type == InterpResultType_Break) {
            break;
        }
    }
    
    interp->block_depth--;
    return result;
}
