
Ast_Expression*
parse_expression(Lexer* lexer) {
    Ast_Expression* result = 0;
    
    switch (lex(lexer)) {
        case Token_Int_Literal: {
            result = push_ast_expression(lexer, Expr_Literal);
            result->literal.type = Type_Int;
            result->literal.u64_value = lexer->curr_token.u64_value;
        } break;
        
        case Token_Float_Literal: {
            result = push_ast_expression(lexer, Expr_Literal);
            result->literal.type = Type_Float;
            result->literal.f64_value = lexer->curr_token.f64_value;
        } break;
    }
    
    return result;
}

Ast_Type*
parse_type(Lexer* lexer) {
    Ast_Type* result = 0;
    
    switch (lex(lexer)) {
        case Token_Int: {
            string_id ident = lexer->curr_token.ident;
            if (is_builtin_type_keyword(ident)) {
                result = push_ast_type(lexer, (Ast_Type_Kind) (ident - builtin_types_begin));
            }
        } break;
        
        case Token_Ident: {
            // Alias type
            unimplemented;
        } break;
        
        default: {
            unlex(lexer);
        } break;
    }
    
    return result;
}

Ast_Expression*
parse_statement(Lexer* lexer) {
    Ast_Expression* result = 0;
    
    switch (lex(lexer)) {
        case Token_Break: {
            unimplemented;
        } break;
        
        case Token_Continue: {
            unimplemented;
        } break;
        
        case Token_If: {
            unimplemented;
        } break;
        
        case Token_For: {
            unimplemented;
        } break;
        
        case Token_While: {
            unimplemented;
        } break;
        
        case Token_Switch: {
            unimplemented;
        } break;
        
        case Token_Return: {
            unimplemented;
        } break;
        
        case '#': {
            unimplemented; // parse_directive
        } break;
        
        case '{': {
            unimplemented; // parse_block
        } break;
        
        default: {
            unlex(lexer);
            Ast_Type* type = parse_type(lexer);
            
            if (type && lex_if_matched(lexer, Token_Ident)) {
                result = push_ast_expression(lexer, Expr_Assign);
                result->assign.type = type;
                
                string_id ident = lexer->curr_token.ident;
                lex_if_matched(lexer, Token_Assign);
                
                result->assign.expr = parse_expression(lexer);
                if (!result->assign.expr) {
                    pln("syntax error: expected expression");
                }
            } else {
                unimplemented;
            }
        } break;
    }
    
    return result;
}