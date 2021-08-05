bool
next_token_if_matched(Parser* parser, Token_Type expected, bool report_error) {
    Token token = peek_token(parser);
    if (token.type == expected) {
        next_token(parser);
        return true;
    } else {
        if (report_error) {
            parse_error_unexpected_token(parser, expected, token.type);
        }
        return false;
    }
}

Keyword
parse_keyword(Parser* parser, bool report_error) {
    Token token = peek_token(parser);
    if (token.type == Token_Ident) {
        str_id id = vars_save_str(token.source);
        if (id > keyword_first && id <= keyword_last) {
            next_token(parser);
            return (Keyword) id;
        }
    }
    
    if (report_error) {
        parse_error(parser, token, str_format("expected keyword, found `%s`", f_str(token.source)));
    }
    
    return Kw_invalid;
}

Ast*
parse_identifier(Parser* parser, bool report_error) {
    Ast* result = 0;
    
    Token token = next_token(parser);
    if (token.type == Token_Ident) {
        str_id id = vars_save_str(token.source);
        if (id < keyword_last) {
            if (report_error) parse_error(parser, token, 
                                          str_format("expected `identifier` found keyword `%`", f_str(token.source)));
            return 0;
        }
        
        result = push_ast_node(parser);
        result->type = Ast_Ident;
        result->Ident = vars_save_str(token.source);
    } else if (report_error) {
        parse_error_unexpected_token(parser, Token_Ident, token.type);
    }
    
    return result;
}

Ast*
parse_expression(Parser* parser, bool report_error) {
    Ast* result = 0;
    
    (void) parser;
    
    return result;
}

Ast*
parse_statement(Parser* parser, bool report_error) {
    Ast* result = 0;
    (void) parser;
    return result;
}

Ast*
parse_struct_or_union_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->type = Ast_Argument;
    result->Argument.type = parse_type(parser);
    result->Argument.ident = parse_identifier(parser);
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    }
    next_token_if_matched(parser, Token_Semi);
    return result;
}

Ast*
parse_enum_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->type = Ast_Argument;
    result->Argument.ident = parse_identifier(parser);
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    }
    next_token_if_matched(parser, Token_Comma, false);
    return result;
}

Ast*
parse_formal_function_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->Argument.ident = parse_identifier(parser);
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    }
    next_token_if_matched(parser, Token_Comma, false);
    return result;
}

Ast*
parse_actual_function_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->type = Ast_Argument;
    result->Argument.ident = parse_identifier(parser);
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    }
    next_token_if_matched(parser, Token_Comma, false);
    return result;
}


Ast*
parse_compound(Parser* parser, 
               Token_Type begin, Token_Type end, Token_Type separator,
               Ast* (*element_parser)(Parser* parser)) {
    Ast* result = push_ast_node(parser);
    Ast* curr = result;
    
    next_token_if_matched(parser, begin);
    while (peek_token(parser).type != end) {
        Ast* element = element_parser(parser);
        curr->Compound.next = element;
        curr = element;
        
        if (parser->current_token.type != separator) {
            break;
        }
    }
    
    next_token_if_matched(parser, end);
    return result;
}

Ast*
parse_type(Parser* parser) {
    
    Token token = peek_token(parser);
    
    if (token.type == Token_Open_Paren) {
        // TODO(alexander): tuple type
    }
    
    if (token.type != Token_Ident) {
        parse_error(parser, token, str_format("expected `type` found `%`", f_str(token.source)));
        return 0;
    }
    
    next_token(parser);
    
    Ast* base = 0;
    str_id ident = vars_save_str(token.source);
    
    if (ident >= builtin_types_begin && ident <= builtin_types_end || ident > keyword_last) {
        base = push_ast_node(parser);
        base->type = Ast_Named_Type;
        base->Named_Type = ident;
    } else {
        switch (ident) {
            case Kw_struct: {
                base = push_ast_node(parser);
                base->type = Ast_Struct_Type;
                base->Struct_Type.ident = parse_identifier(parser);
                base->Struct_Type.fields = parse_compound(parser,
                                                          Token_Open_Brace, Token_Close_Brace, Token_Semi,
                                                          &parse_struct_or_union_argument);
            } break;
            
            case Kw_union: {
                base = push_ast_node(parser);
                base->type = Ast_Union_Type;
                base->Struct_Type.ident = parse_identifier(parser);
                base->Struct_Type.fields = parse_compound(parser,
                                                          Token_Open_Brace, Token_Close_Brace, Token_Semi,
                                                          &parse_struct_or_union_argument);
            } break;
            
            case Kw_enum: { 
                
                base = push_ast_node(parser);
                base->type = Ast_Enum_Type;
                base->Enum_Type.ident = parse_identifier(parser);
                if (next_token_if_matched(parser, Token_Assign, false)) {
                    base->Enum_Type.elem_type = parse_type(parser);
                }
                base->Enum_Type.fields = parse_compound(parser,
                                                        Token_Open_Brace, Token_Close_Brace, Token_Comma,
                                                        &parse_enum_argument);
            } break;
            
            case Kw_typedef: {
                base = push_ast_node(parser);
                base->type = Ast_Enum_Type;
                base->Typedef.type = parse_type(parser);
                base->Typedef.ident = parse_identifier(parser);
                return base;
            } break;
            
            default: {
                parse_error(parser, token, str_format("expected `type` found `%`", f_str(token.source)));
                return 0;
            } break;
        }
    }
    
    Ast* result = base;
    if (base) {
        Token second_token = peek_token(parser);
        switch (token.type) {
            case Token_Ident: {
                next_token(parser);
                
                // TODO(alexander): check what the base type is, e.g. cannnot be struct type as return type
                result = push_ast_node(parser);
                result->type = Ast_Function_Type;
                result->Function_Type.return_type = base;
                result->Function_Type.ident = parse_identifier(parser);
                result->Function_Type.arg_types = parse_compound(parser,
                                                                 Token_Open_Paren, Token_Close_Paren, Token_Comma,
                                                                 &parse_formal_function_argument);
                return result;
            } break;
            
            case Token_Open_Paren: {
                next_token(parser);
                
                // TODO(alexander): check what the base type is, e.g. cannnot be struct type as return type
                if (next_token_if_matched(parser, Token_Mul)) {
                    // TODO(alexander): maybe should be it's own ast node
                    Ast* function = push_ast_node(parser);
                    function->type = Ast_Function_Type;
                    
                    // TODO(alexander): parse function pointer
                    result = push_ast_node(parser);
                    result->type = Ast_Pointer_Type;
                    result->Pointer_Type = function;
                    
                    function->Function_Type.ident = parse_identifier(parser);
                    next_token_if_matched(parser, Token_Close_Paren);
                    function->Function_Type.arg_types = parse_compound(parser,
                                                                       Token_Open_Paren, Token_Close_Paren, Token_Comma,
                                                                       &parse_formal_function_argument);
                }
            } break;
            
            case Token_Open_Bracket: {
                next_token(parser);
                result = push_ast_node(parser);
                result->type = Ast_Array_Type;
                result->Array_Type.elem_type = 
                    result->Array_Type.shape = parse_expression(parser);
                
                next_token_if_matched(parser, Token_Mul);
            } break;
            
            case Token_Mul: {
                next_token(parser);
                result = push_ast_node(parser);
                result->type = Ast_Pointer_Type;
                result->Pointer_Type = base;
            } break;
        }
    }
    
    return result;
}


Ast*
parse_top_level_declaration(Parser* parser) {
    Ast* result = push_ast_node(parser);
    
    result->type = Ast_Type_Decl;
    result->Type_Decl.mods = AstDeclModified_None;
    
    // parse first a modified
    Keyword keyword = parse_keyword(parser, false);
    switch (keyword) {
        case Kw_internal: {
        } break;
        
        case Kw_inline: {
        } break;
    }
    
    result->Type_Decl.type = parse_type(parser);
    result->Type_Decl.stmt = parse_statement(parser);
    
    return result;
}

Ast_File
parse_file(Parser* parser) {
    Ast_File result = {};
    result.ast = parse_top_level_declaration(parser);
    print_ast(result.ast, 0);
    
    
    
    
    return result;
}


internal inline Token
next_semantical_token(Parser* parser) {
    //NOTE(alexander): filtering of comments and whitespace, maybe parameterize this later...
    Token token = advance_token(parser->tokenizer);
    while (!is_semantical_token(token)) {
        token = advance_token(parser->tokenizer);
    }
    return token;
}

Token
next_token(Parser* parser) {
    if (parser->num_peeked_tokens > 0) {
        parser->current_token = parser->peeked_tokens[0];
        parser->peeked_tokens[0] = parser->peeked_tokens[1];
        parser->num_peeked_tokens--;
    } else {
        parser->current_token = next_semantical_token(parser);
    }
    
    if (parser->current_token.type == Token_EOF) {
        // TODO(alexander): add help to remove e.g. mismatched brace.
        parse_error(parser, parser->current_token, "reached end of file while parsing");
    }
    
    return parser->current_token;
}

Token 
peek_token(Parser* parser) {
    if (parser->num_peeked_tokens == 0) {
        parser->peeked_tokens[0] = next_semantical_token(parser);
        parser->num_peeked_tokens++;
    }
    return parser->peeked_tokens[0];
}

Token 
peek_second_token(Parser* parser) {
    if (parser->num_peeked_tokens > 1) {
        return parser->peeked_tokens[1];
    }
    
    if (parser->num_peeked_tokens == 0) {
        parser->peeked_tokens[0] = next_semantical_token(parser);
        parser->num_peeked_tokens++;
    }
    
    parser->peeked_tokens[1] = next_semantical_token(parser);
    parser->num_peeked_tokens++;
    return parser->peeked_tokens[1];
}
