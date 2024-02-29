
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
                result = push_ast_basic_type(lexer, (Ast_Type_Kind) (ident - builtin_types_begin));
            }
        } break;
        
        case Token_Ident: {
            Identifier ident = lexer->curr_token.ident;
            Ast_Alias_Type* alias = push_ast_type(lexer, Alias);
            alias->ident = ident;
            result = alias;
        } break;
        
        case Token_Struct: {
            unimplemented;
        } break;
        
        case Token_Union: {
            unimplemented;
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
            Ast_Maybe_Proc_Type* proc = push_ast_type(lexer, Maybe_Proc);
            proc->return_type = base_type;
            proc->args = parse_call_argument_list(lexer);
            result = proc;
        } break;
        
        default: {
            unlex(lexer);
        } break;
    }
    
    return result;
}

Ast_Call_Argument*
parse_call_argument_list(Lexer* lexer) {
    Ast_Call_Argument* result = 0;
    Ast_Call_Argument* prev_arg = 0;
    while (lex(lexer) != Token_EOF) {
        unlex(lexer);
        
        Ast_Call_Argument* arg = arena_push_struct(lexer->ast_arena, Ast_Call_Argument);
        arg->expr = parse_expression(lexer);
        if (!arg->expr) {
            syntax_error(lexer, string_print("expected expression, found `%`", f_token(lexer->curr_token)));
            return 0;
        }
        
        if (prev_arg) {
            prev_arg->next = arg;
        }
        if (!result) {
            result = arg;
        }
        prev_arg = arg;
        
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
            Ast_Literal_Expression* literal = push_ast_expression(lexer, Literal);
            literal->type = Type_Int;
            literal->u64_value = lexer->curr_token.u64_value;
            result = literal;
        } break;
        
        case Token_Float_Literal: {
            Ast_Literal_Expression* literal = push_ast_expression(lexer, Literal);
            literal->type = Type_Float;
            literal->f64_value = lexer->curr_token.f64_value;
            result = literal;
        } break;
        
        case Token_Ident: {
            Ast_Ident_Expression* ident = push_ast_expression(lexer, Ident);
            ident->ident = lexer->curr_token.ident;
            result = ident;
        } break;
        
        default: {
            unlex(lexer);
            Ast_Type* type = parse_type(lexer);
            result = type;
        } break;
    }
    
    return result;
}

bool
is_binary_operator(Token token) {
    switch (token.kind) {
        case '+':
        case '-':
        case '/':
        case '*': return true;
        
        default: return false;
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
    if (!is_binary_operator(token)) {
        unlex(lexer);
        return left;
    }
    
    int next_prec = get_precedence(token);
    if (next_prec <= min_prec) {
        unlex(lexer);
        return left;
    }
    
    Ast_Binary_Expression* binary = push_ast_expression(lexer, Binary);
    binary->left = left;
    binary->right = parse_expression(lexer, next_prec);
    binary->token = token;
    return binary;
}

Ast_Expression*
parse_expression(Lexer* lexer, int min_prec) {
    Ast_Expression* left = parse_leaf_expression(lexer);
    
    for (;;) {
        Token_Kind kind = lex(lexer);
        if (kind == '(') {
            Ast_Call_Expression* call = push_ast_expression(lexer, Call);
            call->proc = left;
            call->args = parse_call_argument_list(lexer);
            left = call;
            
        } else if (kind == '.') {
            lex_expect(lexer, Token_Ident);
            Ast_Field_Expression* field = push_ast_expression(lexer, Field);
            field->expr = left;
            field->ident = lexer->curr_token.ident;
            left = field;
            
        } else if (kind == '[') {
            Ast_Index_Expression* index = push_ast_expression(lexer, Index);
            index->expr = left;
            index->index = parse_expression(lexer);
            lex_expect(lexer, ']');
            left = index;
            
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

Ast_Expression_List*
parse_block_statement(Lexer* lexer) {
    assert(lexer->curr_token.kind == '{');
    
    Ast_Expression_List* result = 0;
    Ast_Expression_List* curr = 0;
    while (lex(lexer) != '}') {
        if (lexer->curr_token.kind == Token_EOF) break;
        
        unlex(lexer);
        
        Ast_Expression_List* next = arena_push_struct(lexer->ast_arena, Ast_Expression_List);
        next->expr = parse_statement(lexer);
        if (!next->expr) {
            break;
        }
        
        if (curr) {
            curr->next = next;
        }
        curr = next;
        
        if (!result) {
            result = next;
        }
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
        
        case Token_Defer: {
            unimplemented;
        } break;
        
        case Token_Return: {
            Ast_Return_Expression* ret = push_ast_expression(lexer, Return);
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
            
            result = parse_expression(lexer);
            if (result) {
                if (lex_if_matched(lexer, Token_Ident)) {
                    Ast_Assign_Expression* assign = push_ast_expression(lexer, Assign);
                    assign->type = result;
                    assign->ident = lexer->curr_token.ident;
                    
                    Identifier ident = lexer->curr_token.ident;
                    if (lex_if_matched(lexer, Token_Assign)) {
                        assign->expr = parse_expression(lexer);
                        if (!assign->expr) {
                            syntax_error(lexer, string_lit("expected expression after `=`"));
                            return 0;
                        }
                    }
                    result = assign;
                }
                
                lex_expect(lexer, ';');
            }
            
            if (!result) {
                syntax_error(lexer, string_print("unexpected token `%`", f_token(lexer->curr_token)));
            }
        } break;
    }
    
    return result;
}

Ast_Proc_Argument*
parse_argument_list(Lexer* lexer, bool expect_ident=true) {
    Ast_Proc_Argument* result = 0;
    Ast_Proc_Argument* prev_arg = 0;
    while (lex(lexer) != ')') {
        if (lexer->curr_token.kind == Token_EOF) break;
        unlex(lexer);
        if (prev_arg) {
            lex_expect(lexer, ',');
        }
        
        Ast_Proc_Argument* arg = arena_push_struct(lexer->ast_arena, Ast_Proc_Argument);
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
        arg->ident = lexer->curr_token.ident;
        
        if (prev_arg) {
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
    // TODO: seems dumb
    lex(lexer); unlex(lexer);
    Token first_token = lexer->curr_token;
    
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
                result = decl;
                
            } else if (lex_if_matched(lexer, '=')) {
                
            } else {
                unimplemented;
            }
        } break;
        
        case Token_Extern: {
            unimplemented; // extern block
        } break;
        
        default: {
            syntax_error(lexer, string_lit("expected declaration"), &first_token);
        } break;
    }
    
    while (lex_if_matched(lexer, ';')); // optionally end with semicolon
    
    return result;
}