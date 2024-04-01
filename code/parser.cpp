
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
            result = &ast_basic_types[ident - builtin_types_begin];
        } break;
        
        case Token_Ident: {
            Identifier ident = lexer->curr_token.ident;
            result = push_ast_node(lexer, Ast_Alias_Type);
            result->alias = ident;
        } break;
        
        case Token_Struct:
        case Token_Union: {
            Ast_Struct_Type* type = push_ast_node(lexer, Ast_Struct_Type);
            type->token = lexer->curr_token;
            if (lex_if_matched(lexer, Token_Ident)) {
                type->alias = lexer->curr_token.ident;
            }
            
            if (lex_if_matched(lexer, '{')) {
                parse_struct_declaration(lexer, type);
            }
            result = type;
        } break;
        
        case Token_Enum: {
            unimplemented;
        } break;
        
        default: {
            unlex(lexer);
        } break;
    }
    
    
    if (result) {
        result = parse_aggregate_type(lexer, result);
    }
    
    return result;
}


Ast_Type*
parse_aggregate_type(Lexer* lexer, Ast_Type* base_type) {
    Ast_Type* result = base_type;
    
    switch (lex(lexer)) {
        case Token_Operator: {
            unimplemented;
        } break;
        
        case '(': {
            Ast_Procedure_Type* proc = push_ast_node(lexer, Ast_Procedure_Type);
            proc->return_type = base_type;
            proc->args = parse_call_argument_list(lexer);
            result = (Ast_Type*) proc;
        } break;
        
        default: {
            unlex(lexer);
        } break;
    }
    
    return result;
}

Ast_Argument_List*
parse_call_argument_list(Lexer* lexer) {
    Ast_Argument_List* result = 0;
    while (lex(lexer) != Token_EOF) {
        unlex(lexer);
        
        Ast_Argument arg = {};
        arg.initializer = parse_expression(lexer);
        if (!arg.initializer) {
            syntax_error(lexer, string_print("expected expression, found `%`", f_token(lexer->curr_token)));
            return 0;
        }
        array_push(result, arg);
        
        if (!lex_if_matched(lexer, ',')) {
            lex_expect(lexer, ')');
            break;
        }
    }
    
    return result;
}

Ast_Expression*
parse_leaf_expression(Lexer* lexer) {
    Ast_Expression* result = 0;
    
    switch (lex(lexer)) {
        case Token_Int_Literal: {
            Ast_Literal* literal = push_ast_node(lexer, Ast_Literal);
            literal->type = TYPE_INT;
            literal->u64_value = lexer->curr_token.u64_value;
            result = literal;
        } break;
        
        case Token_Float_Literal: {
            Ast_Literal* literal = push_ast_node(lexer, Ast_Literal);
            literal->type = TYPE_FLOAT;
            literal->f64_value = lexer->curr_token.f64_value;
            result = literal;
        } break;
        
        case Token_Ident: {
            Ast_Identifier* identifier = push_ast_node(lexer, Ast_Identifier);
            identifier->identifier = lexer->curr_token.ident;
            result = identifier;
        } break;
        
        case '{': {
            Ast_Struct_Literal* literal = push_ast_node(lexer, Ast_Struct_Literal);
            parse_struct_initializer_list(lexer, literal);
            result = literal;
        } break;
        
        default: {
            unlex(lexer);
            result = parse_type(lexer);
        } break;
    }
    
    return result;
}

int
parse_binary_operator(Token token) {
    switch (token.kind) {
        case '+': return OP_ADD;
        case '-': return OP_SUB;
        case '*': return OP_MUL;
        case '/': return OP_DIV;
        default: return 0;
    }
}

int
get_precedence(Token token) {
    switch (token.kind) {
        case '+': return 10;
        case '-': return 10;
        default: return 0;
    }
}

internal inline Ast_Expression*
parse_binary_expression(Lexer* lexer, Ast_Expression* left, int min_prec) {
    lex(lexer);
    
    Token token = lexer->curr_token;
    int operator_type = parse_binary_operator(token);
    if (!operator_type) {
        unlex(lexer);
        return left;
    }
    
    int next_prec = get_precedence(token);
    if (next_prec <= min_prec) {
        unlex(lexer);
        return left;
    }
    
    Ast_Binary* binary = push_ast_node(lexer, Ast_Binary);
    binary->left = left;
    binary->right = parse_expression(lexer, next_prec);
    binary->token = token;
    binary->operator_type = operator_type;
    return binary;
}

Ast_Expression*
parse_expression(Lexer* lexer, int min_prec) {
    Ast_Expression* left = parse_leaf_expression(lexer);
    
    for (;;) {
        Token_Kind kind = lex(lexer);
        if (kind == '(') {
            Ast_Call* call = push_ast_node(lexer, Ast_Call);
            call->proc = left;
            call->args = parse_call_argument_list(lexer);
            left = call;
            
        } else if (kind == '.') {
            lex_expect(lexer, Token_Ident);
            Ast_Binary* binary = push_ast_node(lexer, Ast_Binary);
            binary->left = left;
            binary->access_identifier = lexer->curr_token.ident;
            binary->operator_type = OP_SCOPE_ACCESS;
            left = binary;
            
        } else if (kind == '[') {
            Ast_Binary* binary = push_ast_node(lexer, Ast_Binary);
            binary->left = left;
            binary->token = lexer->curr_token;
            binary->right = parse_expression(lexer);
            binary->operator_type = OP_SUBSCRIPT;
            lex_expect(lexer, ']');
            left = binary;
            
        } else if (kind == '{' && left && left->kind == AST_IDENTIFIER) {
            Ast_Struct_Literal* literal = push_ast_node(lexer, Ast_Struct_Literal);
            literal->identifier = ast_unwrap_ident(left);
            parse_struct_initializer_list(lexer, literal);
            left = literal;
            
        } else {
            unlex(lexer);
            break;
        }
    }
    
    
    for (;;) {
        Ast_Expression* binary = parse_binary_expression(lexer, left, 0);
        if (left == binary) break;
        
        left = binary;
    }
    
    return left;
}

Ast_Block*
parse_block(Lexer* lexer) {
    assert(lexer->curr_token.kind == '{');
    
    Ast_Block* result = push_ast_node(lexer, Ast_Block);
    
    while (lex(lexer) != '}') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        
        Ast_Expression* expr = parse_statement(lexer);
        if (!expr) {
            break;
        }
        array_push(result->statements, expr);
    }
    
    return result;
}

void
parse_struct_declaration(Lexer* lexer, Ast_Struct_Type* struct_type) {
    assert(lexer->curr_token.kind == '{');
    
    while (lex(lexer) != '}') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        
        Ast_Declaration* decl = push_ast_node(lexer, Ast_Declaration);
        decl->type = parse_type(lexer);
        
        lex_expect(lexer, Token_Ident);
        decl->identifier = lexer->curr_token.ident;
        
        if (lex_if_matched(lexer, '=')) {
            decl->initializer = parse_expression(lexer);
            if (!decl->initializer) {
                syntax_error(lexer, string_lit("expected expression after `=`"));
                return;
            }
        }
        
        lex_expect(lexer, ';');
        array_push(struct_type->declarations, decl);
    }
}

void
parse_struct_initializer_list(Lexer* lexer, Ast_Struct_Literal* literal) {
    assert(lexer->curr_token.kind == '{');
    
    while (lex(lexer) != '}') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        
        Ast_Declaration* decl = push_ast_node(lexer, Ast_Declaration);
        if (lex_if_matched(lexer, Token_Ident)) {
            decl->identifier = lexer->curr_token.ident;
            
            if (lex_if_matched(lexer, '=')) {
                decl->initializer = parse_expression(lexer);
            } else {
                Ast_Identifier* expr = push_ast_node(lexer, Ast_Identifier);
                expr->identifier = decl->identifier;
                
                decl->identifier = 0;
                decl->initializer = expr;
            }
        } else {
            decl->initializer = parse_expression(lexer);
        }
        
        
        if (!decl->initializer) {
            if (decl->identifier) { 
                syntax_error(lexer, string_lit("expected expression after `=`"));
            } else {
                syntax_error(lexer, string_lit("expected expression in struct initializer list"));
            }
            break;
        }
        
        array_push(literal->initializers, decl);
        
        if (!lex_if_matched(lexer, ',')) {
            lex_expect(lexer, '}');
            break;
        }
    }
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
        
        case Token_Defer: {
            unimplemented;
        } break;
        
        case Token_Return: {
            Ast_Return* ret = push_ast_node(lexer, Ast_Return);
            ret->expr = parse_expression(lexer);
            result = ret;
            lex_expect(lexer, ';');
        } break;
        
        case '#': {
            unimplemented; // parse_directive
        } break;
        
        case '{': {
            unimplemented; // parse_block
        } break;
        
        default: {
            unlex(lexer);
            
            result = parse_declaration(lexer);
            if (!result) {
                result = parse_expression(lexer);
            }
            
            if (!result) {
                syntax_error(lexer, string_print("unexpected token `%`", f_token(lexer->curr_token)));
            }
        } break;
    }
    
    return result;
}

Ast_Argument_List*
parse_type_argument_list(Lexer* lexer, bool expect_ident=true) {
    Ast_Argument_List* result = 0;
    while (lex(lexer) != ')') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        if (result) {
            lex_expect(lexer, ',');
        }
        
        Ast_Argument arg = {};
        arg.type = parse_type(lexer);
        
        if (!arg.type) {
            syntax_error(lexer, string_lit("missing type specifier"));
            break;
        }
        
        if (expect_ident) {
            if (!lex_expect(lexer, Token_Ident)) {
                break;
            }
        } else {
            lex_if_matched(lexer, Token_Ident);
        }
        arg.identifier = lexer->curr_token.ident;
        
        array_push(result, arg);
    }
    
    return result;
}

Ast_Declaration*
parse_declaration(Lexer* lexer) {
    Ast_Declaration* result = 0;
    Ast_Type* type = parse_type(lexer);
    
    switch (lex(lexer)) {
        case Token_Ident: {
            assert(type && "syntax error? Expects type before named declaration");
            Identifier identifier = lexer->curr_token.ident;
            
            if (lex_if_matched(lexer, '(')) {
                result = push_ast_node(lexer, Ast_Declaration);
                result->identifier = identifier;
                
                Ast_Procedure_Type* sig = push_ast_node(lexer, Ast_Procedure_Type);
                sig->return_type = type;
                sig->args = parse_type_argument_list(lexer);
                result->type = sig;
                
                if (lex_if_matched(lexer, '{')) {
                    result->initializer = parse_block(lexer);
                }
                
            } else if (lex_if_matched(lexer, '=')) {
                result = push_ast_node(lexer, Ast_Declaration);
                result->identifier = identifier;
                result->type = type;
                
                result->initializer = parse_expression(lexer);
                if (!result->initializer) {
                    syntax_error(lexer, string_lit("expected expression after `=`"));
                    return 0;
                }
                
                lex_expect(lexer, ';');
                
            } else {
                unimplemented;
            }
        } break;
        
        case Token_Extern: {
            unimplemented; // extern block
        } break;
        
        default: {
            unlex(lexer);
            
            if (type && type->alias) {
                result = push_ast_node(lexer, Ast_Declaration);
                result->type = type;
                
                switch (type->kind) {
                    case AST_STRUCT_TYPE: {
                        result->identifier = type->alias;
                    } break;
                }
            } 
            
            if (!result) {
                syntax_error(lexer, string_lit("expected declaration"));
            }
        } break;
    }
    
    while (lex_if_matched(lexer, ';')); // optionally end with semicolon
    
    return result;
}