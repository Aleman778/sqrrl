
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
            Identifier ident = lexer->curr_token.identifier;
            result = &ast_basic_types[ident - builtin_types_begin];
        } break;
        
        case Token_Ident: {
            Ast_Identifier* alias = push_ast_node(lexer, Ast_Identifier);
            alias->identifier = lexer->curr_token.identifier;
            result = alias;
        } break;
        
        case Token_Enum: {
            unimplemented;
        } break;
        
        default: {
            unlex(lexer);
        } break;
    }
    
    return result;
}

array(Ast_Argument)*
parse_call_argument_list(Lexer* lexer) {
    array(Ast_Argument)* result = 0;
    while (lex(lexer) != Token_EOF) {
        unlex(lexer);
        
        if (result) {
            lex_expect(lexer, ',');
        }
        
        Ast_Argument arg = {};
        arg.expression = parse_expression(lexer);
        if (!arg.expression) {
            syntax_error(lexer, string_print("expected expression, found `%`", f_token(lexer->curr_token)));
            return 0;
        }
        
        
        if (lex_if_matched(lexer, '=')) {
            if (arg.expression->kind == AST_IDENTIFIER) {
                arg.identifier = (Ast_Identifier*) arg.expression;
            } else {
                // TODO(Alexander): report error
                unimplemented;
            }
            
            
            arg.expression = parse_expression(lexer);
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
            literal->u64_overflow = lexer->curr_token.u64_overflow;
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
            identifier->identifier = lexer->curr_token.identifier;
            result = identifier;
        } break;
        
        case '-': {
            Ast_Unary* unary = push_ast_node(lexer, Ast_Unary);
            unary->subexpression = parse_expression(lexer, get_precedence(OP_NEG));
            unary->op = OP_NEG;
            result = unary;
        } break;
        
        case '{': {
            Ast_Struct_Literal* literal = push_ast_node(lexer, Ast_Struct_Literal);
            literal->block = parse_struct_initializer_list(lexer);
            result = literal;
        } break;
        
        default: {
            unlex(lexer);
            result = parse_type(lexer);
        } break;
    }
    
    return result;
}

Operator_Kind
parse_binary_operator(Token token) {
    switch (token.kind) {
        case '+': return OP_ADD;
        case '-': return OP_SUB;
        case '*': return OP_MUL;
        case '/': return OP_DIV;
        default:  return OP_NONE;
    }
}

internal inline Ast_Expression*
parse_binary_expression(Lexer* lexer, Ast_Expression* left, int min_prec) {
    lex(lexer);
    
    Token token = lexer->curr_token;
    Operator_Kind op = parse_binary_operator(token);
    if (!op) {
        unlex(lexer);
        return left;
    }
    
    int next_prec = get_precedence(op);
    if (next_prec <= min_prec) {
        unlex(lexer);
        return left;
    }
    
    Ast_Binary* binary = push_ast_node(lexer, Ast_Binary);
    binary->left = left;
    binary->right = parse_expression(lexer, next_prec);
    binary->token = token;
    binary->op = op;
    return binary;
}

Ast_Expression*
parse_expression(Lexer* lexer, int min_prec) {
    Ast_Expression* left = parse_leaf_expression(lexer);
    
    for (;;) {
        Token_Kind kind = lex(lexer);
        if (kind == '(') {
            Ast_Procedure_Call* call = push_ast_node(lexer, Ast_Procedure_Call);
            call->proc = left;
            call->args = parse_call_argument_list(lexer);
            left = call;
            
        } else if (kind == '.') {
            lex_expect(lexer, Token_Ident);
            Ast_Binary* binary = push_ast_node(lexer, Ast_Binary);
            binary->left = left;
            binary->access_identifier = lexer->curr_token.identifier;
            binary->op = OP_SCOPE_ACCESS;
            left = binary;
            
        } else if (kind == '[') {
            Ast_Binary* binary = push_ast_node(lexer, Ast_Binary);
            binary->left = left;
            binary->token = lexer->curr_token;
            binary->right = parse_expression(lexer);
            binary->op = OP_SUBSCRIPT;
            lex_expect(lexer, ']');
            left = binary;
            
        } else if (kind == '{' && left && left->kind == AST_IDENTIFIER) {
            Ast_Struct_Literal* literal = push_ast_node(lexer, Ast_Struct_Literal);
            literal->identifier = unwrap_identifier(left);
            literal->block = parse_struct_initializer_list(lexer);
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

Ast_Expression*
parse_statement(Lexer* lexer, Ast_Block* block) {
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
            ret->expression = parse_expression(lexer);
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
            
            result = parse_declaration(lexer, block);
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

Ast_Block*
parse_block(Lexer* lexer) {
    assert(lexer->curr_token.kind == '{');
    
    Ast_Block* result = push_ast_node(lexer, Ast_Block);
    
    while (lex(lexer) != '}') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        
        Ast_Expression* expr = parse_statement(lexer, result);
        if (!expr) {
            break;
        }
        array_push(result->statements, expr);
    }
    
    return result;
}

Ast_Block*
parse_struct_declaration(Lexer* lexer) {
    assert(lexer->curr_token.kind == '{');
    
    Ast_Block* result = push_ast_node(lexer, Ast_Block);
    
    while (lex(lexer) != '}') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        
        Ast_Declaration* decl = push_ast_node(lexer, Ast_Declaration);
        decl->type = parse_type(lexer);
        
        lex_expect(lexer, Token_Ident);
        decl->identifier = lexer->curr_token.identifier;
        
        if (lex_if_matched(lexer, '=')) {
            decl->initializer = parse_expression(lexer);
            if (!decl->initializer) {
                syntax_error(lexer, string_lit("expected expression after `=`"));
                break;
            }
        }
        
        lex_expect(lexer, ';');
        
        add_member(result, decl);
    }
    
    return result;
}

Ast_Block*
parse_struct_initializer_list(Lexer* lexer) {
    assert(lexer->curr_token.kind == '{');
    
    Ast_Block* result = push_ast_node(lexer, Ast_Block);
    
    while (lex(lexer) != '}') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        
        Ast_Declaration* decl = push_ast_node(lexer, Ast_Declaration);
        if (lex_if_matched(lexer, Token_Ident)) {
            decl->identifier = lexer->curr_token.identifier;
            
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
        
        array_push(result->statements, decl);
        
        if (!lex_if_matched(lexer, ',')) {
            lex_expect(lexer, '}');
            break;
        }
    }
    
    return result;
}

Ast_Block*
parse_type_argument_list(Lexer* lexer, bool expect_ident) {
    Ast_Block* result = push_ast_node(lexer, Ast_Block);
    while (lex(lexer) != ')') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        
        Ast_Declaration* arg = push_ast_node(lexer, Ast_Declaration);
        arg->type = parse_type(lexer);
        
        if (!arg->type) {
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
        arg->identifier = lexer->curr_token.identifier;
        
        add_member(result, arg);
        
        if (!lex_if_matched(lexer, ',')) {
            lex_expect(lexer, ')');
            break;
        }
    }
    
    return result;
}

Ast_Declaration*
parse_declaration(Lexer* lexer, Ast_Block* block) {
    Ast_Declaration* result = 0;
    
    switch (lex(lexer)) {
        case Token_Struct:
        case Token_Union: {
            Ast_Struct* decl = push_ast_node(lexer, Ast_Struct);
            
            // TODO(Alexander): allow anonymous structs within struct blocks
            lex_expect(lexer, Token_Ident);
            decl->identifier = lexer->curr_token.identifier;
            
            lex_expect(lexer, '{');
            decl->args = parse_struct_declaration(lexer);
            result = decl;
            
        } break;
        
        case Token_Enum: {
            unimplemented;
        } break;
        
        
        default: {
            unlex(lexer);
            
            Ast_Type* type = parse_type(lexer);
            if (type && lex_if_matched(lexer, Token_Ident)) {
                Identifier identifier = lexer->curr_token.identifier;
                
                if (lex_if_matched(lexer, '{')) {
                    result = push_ast_node(lexer, Ast_Declaration);
                    result->identifier = identifier;
                    result->type = type;
                    
                    if (type->kind == AST_STRUCT) {
                        result->initializer = parse_struct_declaration(lexer);
                    } else {
                        syntax_error_expected(lexer, (Token_Kind) '=');
                    }
                    
                } else if (lex_if_matched(lexer, '(')) {
                    Ast_Procedure* proc = push_ast_node(lexer, Ast_Procedure);
                    proc->identifier = identifier;
                    proc->return_type = type;
                    proc->args = parse_type_argument_list(lexer);
                    
                    lex_expect(lexer, '{');
                    proc->body = parse_block(lexer);
                    result = proc;
                    
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
                    
                    result = push_ast_node(lexer, Ast_Declaration);
                    result->identifier = identifier;
                    result->type = type;
                    lex_expect(lexer, ';');
                }
                
                add_member(block, result);
            }
            
            if (!result) {
                if (type) {
                    syntax_error(lexer, string_lit("expected identifier in declaration"));
                } else {
                    syntax_error(lexer, string_lit("expected declaration"));
                }
            }
            
        } break;
    }
    
    while (lex_if_matched(lexer, ';')); // optionally end with semicolon
    
    return result;
}

Ast_File*
parse_file(Lexer* lexer) {
    Ast_File* result = arena_push_struct(lexer->ast_arena, Ast_File);
    
    while (lex(lexer) != Token_EOF) {
        unlex(lexer);
        
        Ast_Declaration* decl = parse_declaration(lexer, &result->block);
        if (!decl) {
            lex_finish(lexer);
            break;
        }
        
        array_push(result->block.statements, decl);
    }
    
    return result; 
}