
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
        case Token_Void:
        case Token_Bool:
        case Token_S8:
        case Token_S16:
        case Token_S32:
        case Token_S64:
        case Token_Smm:
        case Token_Int:
        case Token_U8:
        case Token_U16:
        case Token_U32:
        case Token_U64:
        case Token_Umm:
        case Token_Uint:
        case Token_F32:
        case Token_F64:
        case Token_String:
        case Token_Cstring:
        case Token_Typeid: {
            Identifier ident = lexer->curr_token.ident;
            if (is_builtin_type_keyword(ident)) {
                result = push_ast_type(lexer, (Type_Kind) (ident - builtin_types_begin));
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
parse_block_statement(Lexer* lexer) {
    assert(lexer->curr_token.kind == '{');
    
    
    return 0;
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
                
                Identifier ident = lexer->curr_token.ident;
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

Ast_Proc_Argument*
parse_argument_list(Lexer* lexer) {
    Ast_Proc_Argument* result = 0;
    Ast_Proc_Argument* prev_arg = 0;
    while (lex(lexer) != ')') {
        unlex(lexer);
        Ast_Proc_Argument* arg = arena_push_struct(lexer->ast_arena, Ast_Proc_Argument);
        arg->type = parse_type(lexer);
        lex(lexer);
        arg->ident = lexer->curr_token.ident;
        
        if (!prev_arg) {
            prev_arg->next = arg;
        }
        if (!result) {
            result = arg;
        }
        prev_arg = arg;
    }
    
    return result;
}

Ast_Declaration*
parse_declaration(Lexer* lexer) {
    Ast_Declaration* result = 0;
    Ast_Type* type = parse_type(lexer);
    
    switch (lex(lexer)) {
        case Token_Ident: {
            Identifier ident = lexer->curr_token.ident;
            
            if (lex_if_matched(lexer, '(')) {
                Ast_Declaration* decl = push_ast_declaration(lexer, Decl_Procedure);
                decl->proc.ident = ident;
                decl->proc.signature.return_type = type;
                decl->proc.signature.args = parse_argument_list(lexer);
                if (lex_if_matched(lexer, '{')) {
                    decl->proc.block = parse_block_statement(lexer);
                }
                
            } else if (lex_if_matched(lexer, '=')) {
                
            } else {
                unimplemented;
            }
        } break;
        
        case Token_Extern: {
            unimplemented; // extern block
        } break;
        
        default: {
            unlex(lexer);
        } break;
    }
    
    while (lex_if_matched(lexer, ';')); // optionally end with semicolon
    
    return result;
}