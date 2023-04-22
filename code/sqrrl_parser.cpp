
inline bool
peek_token_match(Parser* parser, Token_Type expected, bool report_error) {
    Token token = peek_token(parser);
    if (token.type == expected) {
        return true;
    } else {
        if (report_error) {
            parse_error_unexpected_token(parser, expected, token);
        }
        return false;
    }
}

bool
next_token_if_matched(Parser* parser, Token_Type expected, bool report_error) {
    Token token = peek_token(parser);
    if (token.type == expected) {
        next_token(parser);
        return true;
    } else {
        if (report_error) {
            parse_error_unexpected_token(parser, expected, token);
        }
        return false;
    }
}

bool
parse_keyword(Parser* parser, Var expected, bool report_error) {
    Token token = peek_token(parser);
    if (token.type == Token_Ident) {
        string_id id = vars_save_string(token.source);
        if (id == expected) {
            next_token(parser);
            return true;
        }
    }
    
    if (report_error) {
        string expect = vars_load_string(expected);
        parse_error(parser, token, string_print("expected keyword `%`, found `%`", 
                                                f_string(expect), f_string(token.source)));
    }
    
    return false;
}

Ast*
parse_identifier(Parser* parser, bool report_error) {
    Ast* result = push_ast_node(parser);
    
    Token token = peek_token(parser);
    if (token.type == Token_Ident) {
        string_id id = vars_save_string(token.source);
        if (is_builtin_keyword(id)) {
            if (report_error) parse_error(parser, token, 
                                          string_print("expected `identifier` found keyword `%`", f_string(token.source)));
            return 0;
        }
        
        next_token(parser);
        result = push_ast_node(parser);
        result->kind = Ast_Ident;
        result->Ident = vars_save_string(token.source);
        // NOTE(Alexander): for easier debugging of code
        result->Ident_Data.contents = vars_load_string(result->Ident);
    } else if (report_error) {
        parse_error_unexpected_token(parser, Token_Ident, token);
    }
    
    return result;
}

Ast*
parse_actual_identifier(Parser* parser) {
    return parse_identifier(parser, true);
}

internal u32
parse_escape_character(Parser* parser, u8*& curr, u8* end, bool byte) {
    u8 c = *curr++;
    switch (c) {
        case 'x': {
            verify(curr + 2 < end && "string out of bounds");
            s32 d1 = hex_digit_to_s32(*curr++);
            s32 d2 = hex_digit_to_s32(*curr++);
            return (u32) (d1 << 4 | d2);
        }
        
        case 'u': {
            if (byte) {
                parse_error(parser, parser->current_token, 
                            string_lit("unicode escape characters cannot be used as bytes or in byte string"));
            }
            u32 value = 0;
            verify(*curr++ == '{');
            while ((c = *curr++) != '}' && curr < end) {
                value = value * 16 + hex_digit_to_s32(c);
                if (value > 0x10FFFF) {
                    parse_error(parser, parser->current_token, 
                                string_lit("invalid unicode escape character, expected at most 10FFFF"));
                    return 0;
                }
            }
            return value;
        }
        
        case 'n':  return '\n';
        case 't':  return '\t';
        case 'r':  return '\r';
        case '\\': return '\\';
        case '\'': return '\'';
        case '"':  return '"';
        case '0':  return '\0';
    }
    return 0;
}

internal Type*
parse_type_from_value_suffix(Parser* parser, Type* default_type, s32 flags) {
    Token token = parser->current_token;
    Type* result = default_type;
    
    if (token.suffix_start != token.source.count) {
        string suffix = string_view(token.source.data + token.suffix_start, 
                                    token.source.data + token.source.count);
        
        string_id sym = vars_save_string(suffix);
        switch (sym) {
#define BASIC(ident, flags, keyword, ...) case keyword: result = t_##ident; break;
            DEF_BASIC_TYPES
#undef BASIC
            
            default: {
                if (suffix.count == 1 && *suffix.data == 'f') {
                    result = t_f32;
                    
                } else if (is_bitflag_set(token.c_int_type, CIntType_Unsigned)) {
                    if (is_bitflag_set(token.c_int_type, CIntType_Long_Long)) {
                        result = t_u64;
                    } else if (is_bitflag_set(token.c_int_type, CIntType_Long)) {
                        result = t_u32;
                    } else {
                        result = t_u32;
                    }
                    
                } else {
                    if (is_bitflag_set(token.c_int_type, CIntType_Long_Long)) {
                        result = t_s64;
                    } else if (is_bitflag_set(token.c_int_type, CIntType_Long)) {
                        result = t_s32;
                    } else {
                        result = t_int;
                    }
                }
            } break;
        }
        
        if (result && !is_bitflag_set(flags, result->Basic.flags)) {
            if (is_bitflag_set(flags, BasicFlag_Integer)) {
                parse_error(parser, token, 
                            string_print("expected integer literal suffix, found `%`", f_string(suffix)));
            } else if (is_bitflag_set(flags, BasicFlag_Floating)) {
                parse_error(parser, token, 
                            string_print("expected float literal suffix, found `%`", f_string(suffix)));
            }
        }
    }
    
    return result;
}

internal Ast*
parse_char(Parser* parser) {
    assert(parser->current_token.type == Token_Char);
    
    Type* type = parse_type_from_value_suffix(parser, t_u8, BasicFlag_Integer);
    string str = parser->current_token.source;
    u8* curr = str.data;
    u8* end = str.data + str.count;
    verify(*curr++ == '\'');
    
    u32 character = 0;
    if (*curr == '\\') {
        curr++;
        character = parse_escape_character(parser, curr, end, false);
    } else {
        // NOTE(alexander): extracts utf-32 character, simplified from advance_utf8_character
        u8 num_bytes = utf8_calculate_num_bytes(*curr);
        character = *curr++ & utf8_first_byte_mask[num_bytes - 1];
        for (int i = 1; i < num_bytes; i++) {
            character <<= 6;
            character |= *curr++ & 0x3F;
        }
    }
    
    if (*curr++ != '\'' && curr < end) {
        __debugbreak();
        parse_error(parser, parser->current_token, string_lit("character literal may only contain one codepoint"));
    }
    
    // TODO(alexander): should we create a char value type?
    return push_ast_value(parser, create_unsigned_int_value(character), t_u8);
}

internal Ast*
parse_string(Parser* parser) {
    assert(parser->current_token.type == Token_String);
    
    string str = parser->current_token.source;
    u8* curr = str.data;
    u8* end = str.data + str.count;
    
    // TODO(alexander): create str builder
    String_Builder sb = {};
    string_builder_alloc(&sb, str.count + 10);
    
    verify(*curr++ == '"');
    u8* last_push = curr;
    while (curr < end) {
        char c = *curr;
        if (c == '\\') {
            if (last_push != curr) {
                string_builder_push(&sb, string_view(last_push, curr));
            }
            curr++;
            
            u32 value = parse_escape_character(parser, curr, end, false);
            if (value > 0x7F) {
                // TODO(alexander): create string builder!
                string_builder_ensure_capacity(&sb, 4);
                sb.curr_used += utf32_convert_to_utf8(value, sb.data + sb.curr_used);
            } else {
                string_builder_ensure_capacity(&sb, 1);
                sb.data[sb.curr_used++] = (char) value;
            }
            
            last_push = curr;
            continue;
        } else if (c == '"') {
            curr++;
            break;
        }
        
        u8 n = utf8_calculate_num_bytes((u8) c);
        curr += n;
    }
    
    if (last_push < curr - 1) {
        string_builder_push(&sb, string_view(last_push, curr - 1));
    }
    
    string result = string_builder_to_string(&sb);
    string_builder_free(&sb);
    return push_ast_value(parser, create_string_value(result), t_string);
}


struct Int_Value_Result {
    u64 value;
    b32 is_too_large;
};

Parse_U64_Value_Result
parse_u64_value(Token token) {
    Parse_U64_Value_Result result = {};
    
    int base = 10;
    int curr_index = 0;
    switch (token.int_base) {
        case IntBase_Binary: {
            base = 2;
            curr_index += 2;
        } break;
        
        case IntBase_Octal: {
            base = 8;
            curr_index += 2;
        } break;
        
        case IntBase_Hexadecimal: {
            base = 16;
            curr_index += 2;
        } break;
    }
    
    u64 value = 0;
    while (curr_index < token.suffix_start) {
        u8 c = token.source.data[curr_index++];
        if (c == '_') continue;
        
        s32 d = hex_digit_to_s32(c);
        assert(d != -1 && "tokenization bug");
        u64 x = value * base;
        if (value != 0 && x / base != value) {
            result.is_too_large = true;
            
            break;
        }
        
        u64 y = x + d;
        if (y < x) { // NOTE(alexander): this should work since d is small compared to x
            result.is_too_large = true;
            break;
        }
        
        value = y;
    }
    
    result.value = value;
    return result;
}

internal Ast*
parse_int(Parser* parser) {
    Token token = parser->current_token;
    assert(token.type == Token_Int);
    
    Type* type = parse_type_from_value_suffix(parser, 0, BasicFlag_Integer);
    
    Parse_U64_Value_Result parsed_result = parse_u64_value(token);
    if (parsed_result.is_too_large) {
        parse_error(parser, token, string_lit("integer literal is too large"));
    }
    u64 value = parsed_result.value;
    
    return push_ast_value(parser, create_signed_int_value(value), type);
}

internal Ast*
parse_float(Parser* parser) {
    Token token = parser->current_token;
    assert(token.type == Token_Float);
    
    if (token.int_base != IntBase_Decimal) {
        parse_error(parser, token, string_lit("float literals does not support int bases"));
        return 0;
    }
    
    Type* type = parse_type_from_value_suffix(parser, t_f64, BasicFlag_Floating);
    
    u32 curr_index = 0;
    f64 value = 0.0;
    u8 c = 0;
    while (curr_index < token.suffix_start) {
        c = token.source.data[curr_index++];
        if (c == '_') continue;
        if (c == 'f' || c == 'e' || c == 'E' || c == '.') break;
        f64 d = (f64) hex_digit_to_s32(c);
        value = value * 10.0 + d;
    }
    
    if (c == '.') {
        f64 numerator = 0.0;
        f64 denominator = 1.0;
        while (curr_index < token.source.count) {
            c = token.source.data[curr_index++];
            if (c == '_') continue;
            if (c == 'f' || c == 'e' || c == 'E') break;
            numerator = numerator * 10.0 + (f64) hex_digit_to_s32(c);
            denominator = denominator * 10.0;
        }
        value += numerator/denominator;
    }
    
    if (c == 'e' || c == 'E') {
        c = token.source.data[curr_index++];
        f64 exponent = 0.0;
        f64 sign = 1.0;
        if (c == '-') sign = -1.0;
        
        while (curr_index < token.source.count) {
            c = token.source.data[curr_index++];
            if (c == '_') continue;
            if (c == 'f') break;
            exponent = exponent * 10.0 + (f64) hex_digit_to_s32(c);
        }
        value = value*pow(10.0, sign*exponent);
    }
    
    return push_ast_value(parser, create_floating_value(value), type);
}

Ast*
parse_atom(Parser* parser, bool report_error, u8 min_prec) {
    Ast* result = 0;
    Token token = peek_token(parser);
    
    switch (token.type) {
        case Token_Ident: {
            string_id sym = vars_save_string(token.source);
            switch (sym) {
                case Kw_false: {
                    next_token(parser);
                    result = push_ast_value(parser, create_boolean_value(false), t_bool);
                } break;
                
                case Kw_true: {
                    next_token(parser);
                    result = push_ast_value(parser, create_boolean_value(true), t_bool);
                } break;
                
                case Kw_cast: {
                    next_token(parser);
                    next_token_if_matched(parser, Token_Open_Paren);
                    Ast* type = parse_type(parser);
                    next_token_if_matched(parser, Token_Close_Paren);
                    
                    Ast* inner_expr = parse_expression(parser, report_error, 13);
                    Ast* node = push_ast_node(parser);
                    node->kind = Ast_Cast_Expr;
                    node->Cast_Expr.type = type;
                    node->Cast_Expr.expr = inner_expr;
                    result = node;
                } break;
                
                default: {
                    result = parse_identifier(parser, report_error);
                } break;
            }
            
        } break;
        
        case Token_Raw_Ident: {
            result = parse_identifier(parser);
        } break;
        
        case Token_Int: {
            //local_persist int ints = 0;
            //ints++;
            //if (ints == 45) {
            //__debugbreak();
            //}
            next_token(parser);
            result = parse_int(parser);
            //if (true) {
            //pln("[0x%] %: %", f_u64_HEX(result), f_int(ints), f_ast(result));
            //}
        } break;
        
        case Token_Float: {
            next_token(parser);
            result = parse_float(parser);
        } break;
        
        case Token_Char: {
            next_token(parser);
            result = parse_char(parser);
        } break;
        
        case Token_String: {
            next_token(parser);
            result = parse_string(parser);
        } break;
        
        case Token_Open_Paren: {
            
            //__debugbreak();
            // TODO(Alexander): this is a hack, parenthesized and cast expressions are sightly ambiguous 
            next_token(parser);
            Tokenizer_State begin_tokenizer = save_tokenizer(parser->tokenizer);
            
            Token first_token = peek_token(parser);
            result = push_ast_node(parser, &token);
            Ast* inner = parse_type(parser, false);
            if (inner && is_ast_type(inner)) {
                if (!peek_token_match(parser, Token_Close_Paren, false)) {
                    if (inner) {
                        arena_rewind(&parser->ast_arena);
                    }
                    inner = 0;
                }
            }
            
            if (!inner || is_ast_none(inner)) {
                restore_tokenizer(&begin_tokenizer);
                parser->num_peeked_tokens = 0;
                if (inner) {
                    arena_rewind(&parser->ast_arena);
                }
                
                inner = parse_expression(parser);
                Token peek = peek_token(parser);
                switch (peek.type) {
                    case Token_Close_Paren: {
                        result->kind = Ast_Paren_Expr;
                        result->Paren_Expr.expr = inner;
                    } break;
                    
                    case Token_Comma: {
                        result = parse_compound(parser, 
                                                Token_Comma, Token_Close_Paren, Token_Comma, 
                                                &parse_actual_argument);
                    } break;
                    
                    default: {
                        if (report_error) {
                            parse_error(parser, peek, string_print("expected `)` or `,` found `%` ", f_token(peek.type)));
                        }
                    } break;
                }
            }
            
            next_token_if_matched(parser, Token_Close_Paren);
            
            if (is_ast_type(inner)) {
                Ast* expr = parse_expression(parser, false, 20);
                if (is_valid_ast(expr)) {
                    result->kind = Ast_Cast_Expr;
                    result->Cast_Expr.type = inner;
                    result->Cast_Expr.expr = expr;
                } else {
                    if (inner->kind == Ast_Named_Type && !is_builtin_type_keyword(try_unwrap_ident(inner->Named_Type))) {
                        // NOTE(Alexander): not an actualy type cast instead just (identifier)
                        result->kind = Ast_Paren_Expr;
                        result->Paren_Expr.expr = inner->Named_Type;
                    } else {
                        //unimplemented;
                        // TODO(Alexander): improve error message
                        parse_error(parser, first_token, 
                                    string_print("Failed to parse type cast, please try `cast(%)` instead", 
                                                 f_string(first_token.source)));
                    }
                }
            }
        } break;
        
        case Token_Open_Brace: {
            result = push_ast_node(parser, &token);
            result->kind = Ast_Aggregate_Expr;
            result->Aggregate_Expr.elements = parse_compound(parser, 
                                                             Token_Open_Brace, 
                                                             Token_Close_Brace, 
                                                             Token_Comma, 
                                                             &parse_actual_struct_or_union_argument);
        } break;
        
        default: {
            Operator unop = parse_unary_op(parser);
            
            if (unop != Op_None) {
                u8 prec = operator_prec_table[unop];
                Assoc assoc = operator_assoc_table[unop];
                
                if (prec >= min_prec) {
                    if (assoc == Assoc_Left) {
                        min_prec += 1;
                    }
                    
                    next_token(parser);
                    result = push_ast_node(parser, &token);
                    result->kind = Ast_Unary_Expr;
                    result->Unary_Expr.op = unop;
                    result->Unary_Expr.first = parse_expression(parser, true, prec);
                }
            }
            
        } break;
    }
    
    if (!result) {
        if (report_error) {
            parse_error_unexpected_token(parser, token);
        }
        return result;
    }
    
    update_span(parser, result);
    
    return result;
}

Ast*
parse_expression(Parser* parser, bool report_error, u8 min_prec, Ast* atom_expr) {
    if (!atom_expr) {
        atom_expr = parse_atom(parser, report_error, min_prec);
        
        if (!atom_expr) {
            atom_expr = push_ast_node(parser);
            return atom_expr;
        }
    }
    
    // Some expressions are build by combining multiple atoms e.g. `atom1[atom2]`.
    Ast* lhs_expr = atom_expr;
    for (;;) {
        
        Ast* rhs_expr = lhs_expr;
        Token token = peek_token(parser);
        switch (token.type) {
            case Token_Right_Arrow: {
                if (!parser->c_compatibility_mode) {
                    break;
                }
            } // Fallthrough
            
            case Token_Dot: {
                next_token(parser);
                lhs_expr = push_ast_node(parser);
                lhs_expr->kind = Ast_Field_Expr;
                lhs_expr->Field_Expr.var = rhs_expr;
                Ast* field = parse_atom(parser, true);
                if (field->kind == Ast_Ident) {
                    lhs_expr->Field_Expr.field = field;
                } else {
                    // NOTE(Alexander): we have to unwrap the expression so we first fetch the field identifier then perform the expression
                    // TODO(Alexander): this assumes that first node is identifier, otherwise we fail this is quite ugly
                    assert(field->children[0] && field->children[0]->kind == Ast_Ident);
                    lhs_expr->Field_Expr.field = field->children[0];
                    field->children[0] = lhs_expr;
                    lhs_expr = field;
                }
                update_span(parser, lhs_expr);
            } continue;
            
            case Token_Open_Paren: {
                lhs_expr = push_ast_node(parser);
                lhs_expr->kind = Ast_Call_Expr;
                lhs_expr->Call_Expr.ident = rhs_expr;
                lhs_expr->Call_Expr.args = parse_compound(parser, 
                                                          Token_Open_Paren, Token_Close_Paren, Token_Comma, 
                                                          &parse_actual_function_argument);
                update_span(parser, lhs_expr);
            } continue;
            
            case Token_Open_Bracket: {
                next_token(parser);
                lhs_expr = push_ast_node(parser);
                lhs_expr->kind = Ast_Index_Expr;
                lhs_expr->Index_Expr.array = rhs_expr;
                lhs_expr->Index_Expr.index = parse_expression(parser);
                next_token_if_matched(parser, Token_Close_Bracket);
                update_span(parser, lhs_expr);
            } continue;
            
#if 0
            // TODO(Alexander): do we still care about this syntax: v3 {x: 10.0f, y: 20.0f, z: 30.0f} ?
            //                                                      ^^
            case Token_Open_Brace: {
                if (rhs_expr->kind == Ast_Ident) {
                    lhs_expr = push_ast_node(parser);
                    
                    lhs_expr = push_ast_node(parser, &token);
                    lhs_expr->kind = Ast_Aggregate_Expr;
                    lhs_expr->Aggregate_Expr.ident = rhs_expr;
                    lhs_expr->Aggregate_Expr.elements = parse_compound(parser, 
                                                                       Token_Open_Brace, 
                                                                       Token_Close_Brace, 
                                                                       Token_Comma, 
                                                                       &parse_actual_struct_or_union_argument);
                } else {
                    parse_error(parser, token, string_lit("struct literals expects identifier before `{`"));
                    return rhs_expr;
                }
                update_span(parser, lhs_expr);
            } continue;
#endif
        }
        
        break;
    }
    
    // Parse binary expression using precedence climbing, is applicable
    Operator binary_op = parse_binary_op(parser);
    Token token = peek_token(parser); // HACK(Alexander): want to get ternary to fix precedence
    while (binary_op || 
           token.type == Token_Question || 
           token.type == Token_Increment || 
           token.type == Token_Decrement) {
        
        u8 prec;
        Assoc assoc;
        
        if (token.type == Token_Increment || token.type == Token_Decrement) {
            prec = 13;
            assoc = Assoc_Right;
        } else if (token.type == Token_Question) {
            prec = 1;
            assoc = Assoc_Right;
        } else {
            prec = operator_prec_table[binary_op];
            assoc = operator_assoc_table[binary_op];
        }
        
        if (prec < min_prec) {
            break;
        }
        
        u8 next_min_prec = prec;
        if (assoc == Assoc_Left) {
            next_min_prec += 1;
        }
        
        next_token(parser);
        if (token.type == Token_Increment || token.type == Token_Decrement) {
            Ast* unary_expr = push_ast_node(parser);
            unary_expr->kind = Ast_Unary_Expr;
            unary_expr->Unary_Expr.op = token.type == Token_Increment ? 
                Op_Post_Increment : Op_Post_Decrement;
            unary_expr->Unary_Expr.first = lhs_expr;
            lhs_expr = unary_expr;
            
        } else if (token.type == Token_Question) {
            Ast* ternary_expr = push_ast_node(parser);
            ternary_expr->kind = Ast_Ternary_Expr;
            ternary_expr->Ternary_Expr.first = lhs_expr;
            ternary_expr->Ternary_Expr.second = parse_expression(parser);
            next_token_if_matched(parser, Token_Colon);
            ternary_expr->Ternary_Expr.third = parse_expression(parser);
            lhs_expr = ternary_expr;
        } else {
            Ast* rhs_expr = parse_expression(parser, true, next_min_prec);
            Ast* node = push_ast_node(parser);
            node->kind = Ast_Binary_Expr;
            node->Binary_Expr.op = binary_op;
            node->Binary_Expr.first = lhs_expr;
            node->Binary_Expr.second = rhs_expr;
            node->span = span_combine(lhs_expr->span, rhs_expr->span);
            lhs_expr = node;
        }
        
        binary_op = parse_binary_op(parser);
        token = peek_token(parser);
    }
    
    return lhs_expr;
}

inline internal Ast*
parse_assign_statement(Parser* parser, Ast* type) {
    
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Assign_Stmt;
    result->Assign_Stmt.type = type;
    result->Assign_Stmt.ident = parse_identifier(parser);
    
    if (peek_token_match(parser, Token_Comma, false)) {
        Ast* ident_list_cont = parse_prefixed_compound(parser, Token_Comma, 
                                                       &parse_actual_identifier);
        Ast* ident_list_head = push_ast_node(parser);
        ident_list_head->kind = Ast_Compound;
        ident_list_head->Compound.node = result->Assign_Stmt.ident;
        ident_list_head->Compound.next = ident_list_cont;
        result->Assign_Stmt.ident = ident_list_head;
    }
    
    if (next_token_if_matched(parser, Token_Assign, false)) {
        // TODO(alexander): add support for int x = 5, y = 10;
        result->Assign_Stmt.expr = parse_expression(parser);
    } else {
        result->Assign_Stmt.expr = push_ast_node(parser);
    }
    
    return result;
}

inline internal Ast*
parse_block_statement(Parser* parser, Token* token=0) {
    Ast* result = push_ast_node(parser, token);
    result->kind = Ast_Block_Stmt;
    result->Block_Stmt.stmts = parse_compound(parser, Token_Open_Brace, Token_Close_Brace, Token_Semi,
                                              &parse_actual_statement);
    return result;
}

Ast*
parse_statement(Parser* parser, bool report_error) {
    Token token = peek_token(parser);
    Ast* result = 0;
    
    Ast* attributes = 0;
    if (token.type == Token_Attribute) {
        attributes = parse_prefixed_compound(parser, Token_Attribute, 
                                             &parse_declaration_attribute);
        token = peek_token(parser);
    }
    
    if (token.type == Token_Ident) {
        Var keyword = (Var) vars_save_string(token.source);
        switch (keyword) {
            case Kw_break: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->kind = Ast_Break_Stmt;
                result->Break_Stmt.ident = parse_identifier(parser, false);
            } break;
            
            case Kw_continue: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->kind = Ast_Continue_Stmt;
                result->Continue_Stmt.ident = parse_identifier(parser, false);
            } break;
            
            case Kw_if: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->kind = Ast_If_Stmt;
                bool opened_paren = next_token_if_matched(parser, Token_Open_Paren, false);
                result->If_Stmt.cond = parse_expression(parser);
                next_token_if_matched(parser, Token_Close_Paren, opened_paren);
                result->If_Stmt.then_block = parse_block_or_single_statement(parser);
                
                if (parse_keyword(parser, Kw_else, false)) {
                    result->If_Stmt.else_block = parse_block_or_single_statement(parser);
                } else {
                    // TODO(Alexander): maybe reference a "null" pre allocated node instead?
                    result->If_Stmt.else_block = push_ast_node(parser);
                }
            } break;
            
            case Kw_for: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->kind = Ast_For_Stmt;
                result->For_Stmt.label = parse_identifier(parser, false);
                if (result->For_Stmt.label->kind != Ast_None) {
                    next_token_if_matched(parser, Token_Colon);
                }
                bool opened_paren = next_token_if_matched(parser, Token_Open_Paren, false);
                // TODO(alexander): should probably not be statement, move out variable decl
                result->For_Stmt.init = parse_statement(parser, false);
                next_token_if_matched(parser, Token_Semi);
                result->For_Stmt.cond = parse_expression(parser, false);
                next_token_if_matched(parser, Token_Semi);
                result->For_Stmt.update = parse_expression(parser, false);
                next_token_if_matched(parser, Token_Close_Paren, opened_paren);
                result->For_Stmt.block = parse_block_or_single_statement(parser);
            } break;
            
            case Kw_while: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->kind = Ast_While_Stmt;
                result->While_Stmt.label = parse_identifier(parser, false);
                if (result->While_Stmt.label->kind != Ast_None) {
                    next_token_if_matched(parser, Token_Colon);
                }
                bool opened_paren = next_token_if_matched(parser, Token_Open_Paren, false);
                result->While_Stmt.cond = parse_expression(parser);
                next_token_if_matched(parser, Token_Close_Paren, opened_paren);
                result->While_Stmt.block = parse_block_or_single_statement(parser);
            } break;
            
            case Kw_switch: {
                next_token(parser);
                result = push_ast_node(parser);
                result->kind = Ast_Switch_Stmt;
                bool opened_paren = next_token_if_matched(parser, Token_Open_Paren, false);
                result->Switch_Stmt.cond = parse_expression(parser);
                next_token_if_matched(parser, Token_Close_Paren, opened_paren);
                result->Switch_Stmt.cases = parse_compound(parser, Token_Open_Brace, 
                                                           Token_Close_Brace, Token_Invalid,
                                                           &parse_switch_case);
            } break;
            
            case Kw_return: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->kind = Ast_Return_Stmt;
                result->Return_Stmt.expr = parse_expression(parser, false);
            } break;
            
            default: {
                Ast* type = parse_type(parser, false);
                
                if (type) {
                    switch (type->kind) {
                        case Ast_Struct_Type:
                        case Ast_Union_Type:
                        case Ast_Enum_Type: {
                            result = push_ast_node(parser, &token);
                            result->kind = Ast_Decl_Stmt;
                            result->Decl_Stmt.ident = type->children[0];
                            result->Decl_Stmt.type = type;
                            result->Decl_Stmt.stmt = push_ast_node(parser);
                        } break;
                        
                        case Ast_Function_Type: {
                            type->Function_Type.attributes = attributes;
                            
                            result = push_ast_node(parser, &token);
                            result->kind = Ast_Decl_Stmt;
                            result->Decl_Stmt.ident = type->Function_Type.ident;
                            result->Decl_Stmt.type = type;
                            if (peek_token_match(parser, Token_Open_Brace, false)) { 
                                result->Decl_Stmt.stmt = parse_block_statement(parser);
                            }
                        } break;
                        
                        case Ast_Typedef: {
                            result = type;
                        } break;
                        
                        case Ast_Named_Type: {
                            Token peek = peek_token(parser);
                            if (peek.type == Token_Ident) {
                                result = parse_assign_statement(parser, type);
                            } else {
                                Ast* ident = type->Named_Type;
                                
                                result = push_ast_node(parser);
                                result->kind = Ast_Expr_Stmt;
                                result->Expr_Stmt = parse_expression(parser, true, 1, ident);
                            }
                        } break;
                        
                        default: {
                            result = parse_assign_statement(parser, type);
                        } break;
                    }
                }
            }
        }
    } else if (token.type == Token_String) {
        next_token(parser);
        
        Ast* context = parse_string(parser);
        token = next_token(parser);
        result = parse_block_statement(parser, &token);
        result->Block_Stmt.context = context;
    } else if (token.type == Token_Open_Brace) {
        result = parse_block_statement(parser, &token);
    } if (token.type == Token_Open_Bracket) {
        Ast* type = parse_type(parser, true);
        result = parse_assign_statement(parser, type);
    }
    
    if (!result) {
        result = push_ast_node(parser);
        result->kind = Ast_Expr_Stmt;
        result->Expr_Stmt = parse_expression(parser, report_error);
    }
    
    update_span(parser, result);
    return result;
}

Ast*
parse_block_or_single_statement(Parser* parser, bool report_error) {
    Ast* stmt = parse_statement(parser, report_error);
    if (should_ast_stmt_end_with_semicolon(stmt)) {
        next_token_if_matched(parser, Token_Semi, true);
    }
    return stmt;
}

Operator
parse_unary_op(Parser* parser) {
    Token token = peek_token(parser);
    switch (token.type) {
        case Token_Sub:         return Op_Negate;
        case Token_Logical_Not: return Op_Logical_Not;
        case Token_Bit_Not:     return Op_Bitwise_Not;
        case Token_Bit_And:     return Op_Address_Of;
        case Token_Mul:         return Op_Dereference;
        case Token_Increment:   return Op_Pre_Increment;
        case Token_Decrement:   return Op_Pre_Decrement;
        default:                return Op_None;
    }
}

Operator
parse_binary_op(Parser* parser) {
    Token token = peek_token(parser);
    switch (token.type) {
        case Token_Equals:         return Op_Equals;
        case Token_Lt:             return Op_Less_Than;
        case Token_Gt:             return Op_Greater_Than;
        case Token_Add:            return Op_Add;
        case Token_Sub:            return Op_Subtract;
        case Token_Mul:            return Op_Multiply;
        case Token_Div:            return Op_Divide;
        case Token_Bit_And:        return Op_Bitwise_And;
        case Token_Bit_Or:         return Op_Bitwise_Or;
        case Token_Bit_Xor:        return Op_Bitwise_Xor;
        case Token_Mod:            return Op_Modulo;
        case Token_Assign:         return Op_Assign;
        case Token_Add_Assign:     return Op_Add_Assign;
        case Token_Sub_Assign:     return Op_Subtract_Assign;
        case Token_Mul_Assign:     return Op_Multiply_Assign;
        case Token_Div_Assign:     return Op_Divide_Assign;
        case Token_Mod_Assign:     return Op_Modulo_Assign;
        case Token_Bit_And_Assign: return Op_Bitwise_And_Assign;
        case Token_Bit_Or_Assign:  return Op_Bitwise_Or_Assign;
        case Token_Bit_Xor_Assign: return Op_Bitwise_Xor_Assign;
        case Token_Shl_Assign:     return Op_Shift_Left_Assign;
        case Token_Shr_Assign:     return Op_Shift_Left_Assign;
        case Token_Not_Equals:     return Op_Not_Equals;
        case Token_Logical_And:    return Op_Logical_And;
        case Token_Logical_Or:     return Op_Logical_Or;
        case Token_Lt_Equals:      return Op_Less_Equals;
        case Token_Gt_Equals:      return Op_Greater_Equals;
        case Token_Shl:            return Op_Shift_Left;
        case Token_Shr:            return Op_Shift_Right;
        default:                   return Op_None;
    }
}

Ast*
parse_formal_struct_or_union_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Argument;
    result->Argument.type = parse_type(parser);
    result->Argument.ident = parse_identifier(parser, false);
    
    if (parser->c_compatibility_mode &&
        next_token_if_matched(parser, Token_Open_Bracket, false)) {
        // C-style array
        result->Argument.type = parse_array_type(parser, result->Argument.type);
    }
    
    if (result->Argument.ident && 
        next_token_if_matched(parser, Token_Comma, false)) {
        
        Ast* curr = push_ast_node(parser);
        curr->kind = Ast_Compound;
        curr->Compound.node = result->Argument.ident;
        result->Argument.ident = curr;
        
        curr->Compound.next = push_ast_node(parser);
        curr = curr->Compound.next;
        do {
            curr->kind = Ast_Compound;
            curr->Compound.node = parse_identifier(parser);
            
            if (parser->c_compatibility_mode &&
                next_token_if_matched(parser, Token_Open_Bracket, false)) {
                unimplemented;
                // C-style array
                //result->Argument.type = parse_array_type(parser, result->Argument.type);
            }
            
            curr->Compound.next = push_ast_node(parser);
            curr = curr->Compound.next;
        } while (next_token_if_matched(parser, Token_Comma, false));
    }
    
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    } else {
        if (next_token_if_matched(parser, Token_Colon, false)) {
            result->Argument.assign = parse_atom(parser);
        } else {
            result->Argument.assign = push_ast_node(parser);
        }
    }
    
    return result;
}

Ast*
parse_actual_struct_or_union_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Argument;
    
    if (peek_token_match(parser, Token_Ident, false)) {
        result->Argument.ident = parse_identifier(parser, true);
        if (next_token_if_matched(parser, Token_Colon, false)) {
            result->Argument.assign = parse_expression(parser);
        } else {
            result->Argument.assign = parse_expression(parser, true, 1, result->Argument.ident);
            result->Argument.ident = 0;
        }
    } else {
        result->Argument.assign = parse_expression(parser);
    }
    return result;
}

Ast*
parse_formal_enum_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Argument;
    result->Argument.ident = parse_identifier(parser);
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    } else {
        result->Argument.assign = push_ast_node(parser);
    }
    return result;
}

Ast*
parse_formal_function_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Argument;
    
    if (next_token_if_matched(parser, Token_Ellipsis, false)) {
        result->Argument.type = push_ast_node(parser);
        result->Argument.type->kind = Ast_Ellipsis;
        result->Argument.ident = parse_identifier(parser, false);
        return result;
    }
    
    result->Argument.type = parse_type(parser);
    result->Argument.ident = parse_identifier(parser, false);
    
    if (parser->c_compatibility_mode &&
        next_token_if_matched(parser, Token_Open_Bracket, false)) {
        // C-style array
        result->Argument.type = parse_array_type(parser, result->Argument.type);
    }
    
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    } else {
        result->Argument.assign = push_ast_node(parser);
    }
    return result;
}

Ast*
parse_actual_function_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Argument;
    
    // TODO(Alexander): we don't support variable args yet! Also this way is broken anyways.
    //result->Argument.ident = parse_identifier(parser, false);
    //if (result->Argument.ident->kind) {
    //if (next_token_if_matched(parser, Token_Assign, false)) {
    //result->Argument.assign = parse_expression(parser);
    //} else {
    //result->Argument.assign = result->Argument.ident;
    //result->Argument.ident = push_ast_node(parser);
    //}
    //} else {
    result->Argument.assign = parse_expression(parser, false);
    if (result->Argument.assign->kind == Ast_None) {
        result->Argument.type = parse_type(parser, false);
    }
    //}
    return result;
}

Ast*
parse_switch_case(Parser* parser) {
    Ast* result = push_ast_node(parser);
    
    
    // TODO(Alexander): add support for fallthrough to next case
    if (parse_keyword(parser, Kw_case)) {
        result->kind = Ast_Switch_Case;
        result->Switch_Case.cond = parse_expression(parser, false);
        next_token_if_matched(parser, Token_Colon);
        if (peek_token_match(parser, Token_Open_Brace, false)) {
            result->Switch_Case.stmt = parse_block_statement(parser, false);
        }
    } else {
        Token token = next_token(parser);
        while (token.type != Token_EOF) {
            if (peek_token_match(parser, Token_Close_Brace, false)) {
                break;
            }
            token = next_token(parser);
        }
    }
    return result;
}

Ast*
parse_declaration_attribute(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Attribute;
    result->Attribute.ident = parse_identifier(parser);
    result->Attribute.expr = parse_expression(parser, true, 1, result->Attribute.ident);
    return result;
}

Ast*
parse_actual_argument(Parser* parser) {
    return parse_expression(parser, false);
}

Ast*
parse_actual_statement(Parser* parser) {
    return parse_statement(parser);
}

Ast*
parse_compound(Parser* parser, 
               Token_Type begin, Token_Type end, Token_Type separator,
               Ast* (*element_parser)(Parser* parser)) {
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Compound;
    
    next_token_if_matched(parser, begin);
    if (next_token_if_matched(parser, end, false)) {
        result->Compound.node = push_ast_node(parser);
        result->Compound.next = push_ast_node(parser);
        return result;
    }
    
    Ast* curr = result;
    for (;;) {
        Token token = peek_token(parser);
        if (token.type == Token_EOF) {
            return result;
        }
        
        if (token.type == end) {
            next_token(parser);
            break;
        }
        
        curr->kind = Ast_Compound;
        Ast* node = element_parser(parser);
        curr->Compound.node = node;
        curr->Compound.next = push_ast_node(parser);
        curr = curr->Compound.next;
        
        if (separator == Token_Invalid) {
            if (next_token_if_matched(parser, end, false)) {
                break;
            } else {
                continue;
            }
        }
        
        if (separator == Token_Semi && node && !should_ast_stmt_end_with_semicolon(node)) {
            // TODO(Alexander): We can allow unecessary semi colon, or should it be an error?
            next_token_if_matched(parser, separator, false);
            //if (next_token_if_matched(parser, separator, false)) {
            //parse_error_unexpected_token(parser, parser->current_token);
            //}
            continue;
        }
        
        if (!next_token_if_matched(parser, separator, false)) {
            if (next_token_if_matched(parser, end, false)) {
                break;
            } else {
                token = peek_token(parser);
                parse_error_unexpected_token(parser, separator, token);
                
                int depth = 1;
                for (;;) {
                    token = next_token(parser);
                    if      (token.type == begin) depth++;
                    else if (token.type == end)   depth--;
                    
                    if (token.type == Token_EOF || depth <= 0) {
                        return result;
                    }
                    
                    if (token.type == separator && depth == 1) {
                        break;
                    }
                }
            }
        }
    }
    
    return result;
}

Ast*
parse_prefixed_compound(Parser* parser, Token_Type prefix,
                        Ast* (*element_parser)(Parser* parser)) {
    // Parse elements until the prefix doesn't match anymore
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Compound;
    
    if (!next_token_if_matched(parser, prefix, false)) {
        result->Compound.node = push_ast_node(parser);
        result->Compound.next = push_ast_node(parser);
        return result;
    }
    
    Ast* curr = result;
    for (;;) {
        Token token = peek_token(parser);
        curr->kind = Ast_Compound;
        Ast* node = element_parser(parser);
        curr->Compound.node = node;
        curr->Compound.next = push_ast_node(parser);
        curr = curr->Compound.next;
        
        if (!next_token_if_matched(parser, prefix, false)) {
            break;
        }
    }
    
    return result;
}

internal inline Ast*
parse_type_modifiers(Parser* parser, Ast* result) {
    
    // TODO(Alexander): this is a hack for now we probably want to put these in a bitfield or something
    while (peek_token_match(parser, Token_Ident, false)) {
        if (parse_keyword(parser, Kw_const, false)) {
            Ast* const_type = push_ast_node(parser);
            const_type->kind = Ast_Const_Type;
            const_type->Const_Type = result;
            result = const_type;
        } else if (parse_keyword(parser, Kw_volatile, false)) {
            Ast* volatile_type = push_ast_node(parser);
            volatile_type->kind = Ast_Volatile_Type;
            volatile_type->Volatile_Type = result;
            result = volatile_type;
        } else if (parse_keyword(parser, Kw_local_persist, false)) {
            Ast* local_persist_type = push_ast_node(parser);
            local_persist_type->kind = Ast_Local_Persist_Type;
            local_persist_type->Local_Persist_Type = result;
            result = local_persist_type;
        } else if (parser->c_compatibility_mode && parse_keyword(parser, Sym___unaligned, false)) {
            Ast* unaligned_type = push_ast_node(parser);
            unaligned_type->kind = Ast_Volatile_Type;
            unaligned_type->Volatile_Type = result;
            result = unaligned_type;
        } else if (parser->c_compatibility_mode && parse_keyword(parser, Sym___declspec, false)) {
            Ast* declspec_type = push_ast_node(parser);
            declspec_type->kind = Ast_Declspec_Type;
            declspec_type->Declspec_Type.type = result;
            declspec_type->Declspec_Type.spec = parse_expression(parser);
            result = declspec_type;
        } else if (parser->c_compatibility_mode && parse_keyword(parser, Sym___pragma, false)) {
            Ast* declspec_type = push_ast_node(parser);
            declspec_type->kind = Ast_Declspec_Type;
            declspec_type->Declspec_Type.type = result;
            declspec_type->Declspec_Type.spec = parse_expression(parser);
            result = declspec_type;
        } else {
            break;
        }
    }
    
    return result;
}

internal inline Ast_Decl_Modifier
parse_procedure_type_mods(Parser* parser, Ast** attributes) {
    Ast_Decl_Modifier result = AstDeclModifier_None;
    {
        Token curr_token = peek_token(parser);
        while (curr_token.type == Token_Ident || curr_token.type == Token_Attribute) {
            if (curr_token.type == Token_Attribute) {
                *attributes = parse_prefixed_compound(parser, Token_Attribute, 
                                                      &parse_declaration_attribute);
                curr_token = peek_token(parser);
                continue;
            }
            
            string_id ident = vars_save_string(curr_token.source);
            switch (ident) {
                case Sym___cdecl: {
                    result |= AstDeclModifier_Cconv_cdecl;
                    next_token(parser);
                    curr_token = peek_token(parser);
                } break;
                
                case Sym___fastcall: {
                    result |= AstDeclModifier_Cconv_fastcall;
                    next_token(parser);
                    curr_token = peek_token(parser);
                } break;
                
                case Sym___stdcall: {
                    result |= AstDeclModifier_Cconv_stdcall;
                    next_token(parser);
                    curr_token = peek_token(parser);
                } break;
                
                default: {
                    curr_token.type = Token_Invalid;
                } break;
            }
        }
    }
    
    return result;
}

inline Ast* 
parse_array_type(Parser* parser, Ast* elem_type, Ast_Decl_Modifier mods) {
    assert(parser->current_token.type == Token_Open_Bracket);
    
    if (elem_type && elem_type->kind == Ast_Ident) {
        Ast* tmp_type = push_ast_node(parser);
        tmp_type->kind = Ast_Named_Type;
        tmp_type->Named_Type = elem_type;
        tmp_type->span = elem_type->span;
        elem_type = tmp_type;
    }
    
    Ast* result = push_ast_node(parser);
    result->kind = Ast_Array_Type;
    result->Array_Type.shape = parse_atom(parser, false);
    result->Array_Type.is_dynamic = next_token_if_matched(parser, Token_Range, false);
    next_token_if_matched(parser, Token_Close_Bracket);
    
    if (!elem_type) {
        elem_type = parse_type(parser, true, mods);
    }
    result->Array_Type.elem_type = elem_type;
    return result;
}

Ast*
parse_type(Parser* parser, bool report_error, Ast_Decl_Modifier mods) {
    Token token = peek_token(parser);
    
    if (token.type == Token_Open_Paren) {
        // TODO(alexander): tuple type
    }
    
    
    if (token.type == Token_Open_Bracket) {
        next_token(parser);
        return parse_array_type(parser, 0, mods);
    }
    
    
    if (token.type != Token_Ident) {
        if (report_error) {
            parse_error_expected_type(parser, token);
        }
        return 0;
    }
    
    Ast* base = 0;
    Ast* base_mod = parse_type_modifiers(parser, base);
    
    token = peek_token(parser);
    string_id ident = vars_save_string(token.source);
    
    //string_id key = vars_save_cstring("DEBUG_Read_File_Result");
    //if (key == ident) {
    //__debugbreak();
    //}
    
    if (parser->c_compatibility_mode &&
        (ident == Sym_char || ident == Sym_short  || ident == Sym_long  || ident == Sym_unsigned || 
         ident == Sym_signed || ident == Sym_float || ident == Sym_double)) {
        
        base = push_ast_node(parser);
        base->kind = Ast_Named_Type;
        base->Named_Type = push_ast_node(parser);
        base->Named_Type->kind = Ast_Ident;
        base->Named_Type->Ident = Kw_invalid;
        
        bool is_unsigned = false;
        bool is_integer_type = false;
        
        if (parse_keyword(parser, Sym_unsigned, false)) {
            is_unsigned = true;
            is_integer_type = true;
            base->Named_Type->Ident = Kw_uint;
        } else if (parse_keyword(parser, Sym_signed, false)) {
            is_integer_type = true;
            base->Named_Type->Ident = Kw_int;
        }
        
        if (parse_keyword(parser, Sym_short, false)) {
            parse_keyword(parser, Kw_int, false);
            base->Named_Type->Ident = is_unsigned ? Kw_u16 : Kw_s16;
            
        } else if (parse_keyword(parser, Sym_long, false)) {
            
            if (parse_keyword(parser, Sym_long, false)) {
                base->Named_Type->Ident = is_unsigned ? Kw_u64 : Kw_s64;
            } else {
                base->Named_Type->Ident = is_unsigned ? Kw_u32 : Kw_s32;
            }
            
            parse_keyword(parser, Kw_int, false);
            
            // TODO(Alexander): there is also a `long double` => s128 (which we don't support yet).
            if (parse_keyword(parser, Sym_double, false)) {
                unimplemented;
            }
            
        } else if (parse_keyword(parser, Sym_char, false)) {
            base->Named_Type->Ident = is_unsigned ? Kw_u8 : Kw_s8;
            
        } else if (parse_keyword(parser, Kw_int, false)) {
            base->Named_Type->Ident = is_unsigned ? Kw_uint : Kw_int;
            
        } else if (parse_keyword(parser, Sym___int8, false)) {
            // TODO(Alexander): __int8, __int16, __int32, __int64 are microsoft specific types!
            // https://docs.microsoft.com/en-us/cpp/cpp/int8-int16-int32-int64
            base->Named_Type->Ident = is_unsigned ? Kw_u8 : Kw_s8;
            
        } else if (parse_keyword(parser, Sym___int16, false)) {
            base->Named_Type->Ident = is_unsigned ? Kw_u16 : Kw_s16;
            
        } else if (parse_keyword(parser, Sym___int32, false)) {
            base->Named_Type->Ident = is_unsigned ? Kw_u32 : Kw_s32;
            
        } else if (parse_keyword(parser, Sym___int64, false)) {
            base->Named_Type->Ident = is_unsigned ? Kw_u64 : Kw_s64;
            
            
        } else if (!is_integer_type && parse_keyword(parser, Sym_float, false)) {
            base->Named_Type->Ident = Kw_f32;
            
        } else if (!is_integer_type && parse_keyword(parser, Sym_double, false)) {
            base->Named_Type->Ident = Kw_f64;
            
        } else {
            
            // TODO(Alexander): need to report error for invalid ctypes
            unimplemented;
        }
        
    } else {
        
        next_token(parser);
        
        if (is_builtin_type_keyword(ident) || is_not_builtin_keyword(ident)) {
            
            base = push_ast_node(parser);
            base->kind = Ast_Named_Type;
            base->Named_Type = push_ast_node(parser);
            base->Named_Type->kind = Ast_Ident;
            base->Named_Type->Ident = ident;
            
        } else {
            switch (ident) {
                case Kw_struct: {
                    base = push_ast_node(parser);
                    base->kind = Ast_Struct_Type;
                    Ast* mod = parse_type_modifiers(parser, base);
                    base->Struct_Type.ident = parse_identifier(parser, false);
                    if (peek_token_match(parser, Token_Open_Brace, !parser->c_compatibility_mode)) {
                        // NOTE(Alexander): forward declaration is only relevant in C code
                        base->Struct_Type.fields = parse_compound(parser,
                                                                  Token_Open_Brace,
                                                                  Token_Close_Brace,
                                                                  Token_Semi,
                                                                  &parse_formal_struct_or_union_argument);
                    }
                    base = mod;
                } break;
                
                case Kw_union: {
                    base = push_ast_node(parser);
                    base->kind = Ast_Union_Type;
                    base->Union_Type.ident = parse_identifier(parser, false);
                    if (peek_token_match(parser, Token_Open_Brace, !parser->c_compatibility_mode)) {
                        base->Union_Type.fields = parse_compound(parser,
                                                                 Token_Open_Brace, 
                                                                 Token_Close_Brace, 
                                                                 Token_Semi,
                                                                 &parse_formal_struct_or_union_argument);
                    }
                    
                    //if (base->Union_Type.fields) {
                    //return base;
                    //}
                } break;
                
                case Kw_enum: { 
                    base = push_ast_node(parser);
                    base->kind = Ast_Enum_Type;
                    base->Enum_Type.ident = parse_identifier(parser, !parser->c_compatibility_mode);
                    if (next_token_if_matched(parser, Token_Colon, false)) {
                        base->Enum_Type.elem_type = parse_type(parser);
                    }
                    base->Enum_Type.fields = parse_compound(parser,
                                                            Token_Open_Brace, Token_Close_Brace, Token_Comma,
                                                            &parse_formal_enum_argument);
                    return base;
                } break;
                
                case Kw_typedef: {
                    base = push_ast_node(parser);
                    base->kind = Ast_Typedef;
                    base->Typedef.type = parse_type(parser);
                    
                    Ast* base_type = base->Typedef.type;
                    if (base_type->kind == Ast_Pointer_Type) {
                        base_type = base->Typedef.type->Pointer_Type;
                    }
                    
                    if (base_type->kind == Ast_Function_Type) {
                        base->Typedef.ident = base_type->Function_Type.ident;
                    }
                    
                    Ast* base_ident = base->Typedef.ident;
                    
                    if (!base_ident) {
                        base->Typedef.ident = parse_identifier(parser);
                    }
                    
#if 0
                    if (parser->c_compatibility_mode) {
                        Ast* curr = push_ast_node(parser);
                        curr->kind = Ast_Compound;
                        if (base->Typedef.ident) {
                            curr->Compound.node = base->Typedef.ident;
                            curr->Compound.next = push_ast_node(parser);
                            
                            base->Typedef.ident = curr;
                            curr = curr->Compound.next;
                        } else {
                            base->Typedef.ident = curr;
                        }
                        
                        for (;;) {
                            token = peek_token(parser);
                            if (token.type == Token_Semi) {
                                next_token(parser);
                                break;
                            }
                            
                            // TODO(Alexander): for now we parse an expression
                            // e.g. you can have typedefs in C land that looks like this
                            // `typedef struct element element, *list, elements[5];`
                            // so for now we should be able to get the most of our 
                            // resuing the expression parser.
                            curr->Compound.node = parse_expression(parser);
                            curr->Compound.next = push_ast_node(parser);
                            curr = curr->Compound.next;
                            
                            if (!next_token_if_matched(parser, Token_Comma, false)) {
                                if (next_token_if_matched(parser, Token_Semi, false)) {
                                    break;
                                } else {
                                    token = peek_token(parser);
                                    parse_error_unexpected_token(parser, Token_Comma, token);
                                    for (;;) {
                                        token = next_token(parser);
                                        if (!is_token_valid(token) || token.type == Token_Semi) {
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        base->Typedef.ident = parse_identifier(parser);
                    }
#endif
                    
                    return base;
                } break;
                
                default: {
                    if (report_error) {
                        parse_error_expected_type(parser, token);
                    }
                    return 0;
                } break;
            }
        }
    }
    
    Ast* result = 0;
    if (base) {
        if (base_mod) {
            // TODO(Alexander): hack to join base_mod -> base
            Ast* it = base_mod;
            while (it->children[0]) it = it->children[0];
            it->children[0] = base;
            base = base_mod;
        }
        
        base = parse_type_modifiers(parser, base);
        
        result = parse_complex_type(parser, base, report_error, mods);
    } else {
        if (report_error) {
            parse_error_expected_type(parser, token);
        }
    }
    
    
    return result;
}

Ast*
parse_complex_type(Parser* parser, Ast* base_type, bool report_error, Ast_Decl_Modifier mods) {
    Ast* result = base_type;
    
    Token token = peek_token(parser);
    switch (token.type) {
        case Token_Attribute:
        case Token_Ident: {
            Ast* attributes = 0;
            Ast_Decl_Modifier function_mods = parse_procedure_type_mods(parser, &attributes);
            Operator overload_operator = Op_None;
            
            Token peek2 = peek_second_token(parser);
            
            Token op_token = {};
            if (parse_keyword(parser, Kw_operator, false)) {
                overload_operator = parse_binary_op(parser);
                if (overload_operator == Op_None) {
                    overload_operator = parse_unary_op(parser);
                }
                op_token = next_token(parser);
                if (overload_operator == Op_None) {
                    parse_error(parser, op_token, string_print("expected binary operator, found `%`",
                                                               f_string(op_token.source)));
                }
                
                if (peek_token_match(parser, Token_Open_Paren, true)) {
                    peek2.type = Token_Open_Paren;
                }
            }
            
            if (peek2.type == Token_Open_Paren) {
                // TODO(alexander): check what the base type is, e.g. cannot be struct type as return type
                result = push_ast_node(parser);
                result->kind = Ast_Function_Type;
                result->Function_Type.return_type = base_type;
                result->Function_Type.mods = function_mods | mods;
                result->Function_Type.attributes = attributes;
                result->Function_Type.overload_operator = overload_operator;
                if (overload_operator == Op_None) {
                    result->Function_Type.ident = parse_identifier(parser);
                } else {
                    Ast* ident = push_ast_node(parser, &op_token);
                    ident->kind = Ast_Ident;
                    ident->Ident = Kw_operator;
                    result->Function_Type.ident = ident;
                }
                result->Function_Type.arguments = parse_compound(parser,
                                                                 Token_Open_Paren, Token_Close_Paren, Token_Comma,
                                                                 &parse_formal_function_argument);
                return result;
            } else {
                if (function_mods != AstDeclModifier_None) {
                    parse_error_unexpected_token(parser, Token_Open_Paren, peek_second_token(parser));
                }
            }
        } break;
        
        case Token_Mul: {
            next_token(parser);
            result = push_ast_node(parser);
            result->kind = Ast_Pointer_Type;
            result->Pointer_Type = base_type;
            
            token = peek_token(parser);
            if (parser->c_compatibility_mode && token.type == Token_Ident) {
                string_id ident = vars_save_string(token.source);
                if (ident == Sym___ptr32 || ident == Sym___ptr64) {
                    // TODO(Alexander): discard these I don't think they are useful
                    next_token(parser);
                }
            }
            
            result = parse_complex_type(parser, result, false, mods);
        } break;
    }
    
    if (parse_keyword(parser, Kw_const, false)) {
        Ast* const_type = push_ast_node(parser);
        const_type->kind = Ast_Const_Type;
        const_type->Const_Type = result;
        result= const_type;
    }
    
    return result;
}


internal void
push_static_ast_node(Parser* parser, Ast_File* ast_file, Ast* node) {
    if (!ast_file->static_block_first || !ast_file->static_block_last) {
        Ast* first = push_ast_node(parser);
        first->kind = Ast_Compound;
        ast_file->static_block_first = first;
        ast_file->static_block_last = first;
    }
    
    Ast* next = push_ast_node(parser);
    next->kind = Ast_Compound;
    Ast* last = ast_file->static_block_last;
    last->Compound.node = node;
    last->Compound.next = next;
    ast_file->static_block_last = last->Compound.next;
}

void
register_top_level_declaration(Parser* parser, Ast_File* ast_file, 
                               Ast* decl, Ast* attributes, Ast_Decl_Modifier mods) {
    if (decl->kind < Ast_Stmt_Begin && decl->kind > Ast_Stmt_End) {
        // TODO(Alexander): we need to decide what to do about this?
        unimplemented;
        return;
    }
    
    switch (decl->kind) {
        case Ast_Block_Stmt: {
            for_compound(decl->Block_Stmt.stmts, it) {
                register_top_level_declaration(parser, ast_file, it, attributes, mods);
            }
        } break;
        
        case Ast_Decl_Stmt: {
            string_id ident = try_unwrap_ident(decl->Decl_Stmt.ident);
            if (ident) {
                
                // TODO(Alexander): decls is deprecated use units instead
                map_put(ast_file->decls, ident, decl);
                
                // Replicate attributes and modifiers to decl/type
                if (decl->Decl_Stmt.type) {
                    Ast* type_ast = decl->Decl_Stmt.type;
                    
                    switch (decl->Decl_Stmt.type->kind) {
                        case Ast_Function_Type: {
                            type_ast->Function_Type.mods |= mods;
                            
                            //if (attributes) {
                            //pln("fn: %", f_ast(type_ast));
                            //assert(type_ast->Function_Type.attributes == 0 && "overwrite");
                            //}
                            Ast* last_attr = type_ast->Function_Type.attributes;
                            
                            if (last_attr) {
                                while (is_valid_ast(last_attr->Compound.next)) {
                                    last_attr = last_attr->Compound.next;
                                }
                                last_attr->Compound.next = attributes;
                            } else {
                                type_ast->Function_Type.attributes = attributes;
                            }
                        } break;
                        
                        case Ast_Struct_Type: {
                            type_ast->Struct_Type.attributes = attributes;
                        } break;
                        
                        case Ast_Union_Type: {
                            type_ast->Struct_Type.attributes = attributes;
                        } break;
                        
                        case Ast_Enum_Type: {
                            type_ast->Struct_Type.attributes = attributes;
                        } break;
                    }
                }
                
                Compilation_Unit comp_unit = {};
                comp_unit.ident = ident;
                
                //pln("Push decl `%`", f_string(vars_load_string(ident)));
                
                comp_unit.ast = decl->Decl_Stmt.type;
                array_push(ast_file->units, comp_unit);
                
                comp_unit.ast = decl;
                array_push(ast_file->units, comp_unit);
            }
        } break;
        
        case Ast_Assign_Stmt: {
            string_id ident = try_unwrap_ident(decl->Assign_Stmt.ident);
            if (ident) {
                // TODO(Alexander): decls is deprecated use units instead
                map_put(ast_file->decls, ident, decl);
                
                Compilation_Unit comp_unit = {};
                comp_unit.ident = ident;
                comp_unit.ast = decl;
                array_push(ast_file->units, comp_unit);
                
                push_static_ast_node(parser, ast_file, decl);
            }
        } break;
        
        case Ast_Typedef: {
            Compilation_Unit comp_unit = {};
            comp_unit.ast = decl;
            
            if (is_ast_compound(decl->Typedef.ident)) {
                
                for_compound(decl->Typedef.ident, part) {
                    if (part->kind == Ast_Binary_Expr || part->kind == Ast_Index_Expr) {
                        //string_id ident1 = ast_unwrap_ident(part->Binary_Expr.first);
                        //string_id ident2 = ast_unwrap_ident(part->Binary_Expr.second);
                        pln("%", f_ast(part));
                        
                    } else {
                        string_id ident = ast_unwrap_ident(part);
                        map_put(ast_file->decls, ident, decl);
                        
                        comp_unit.ident = ident;
                        array_push(ast_file->units, comp_unit);
                    }
                }
            } else {
                if (decl->Typedef.ident) {
                    string_id ident = ast_unwrap_ident(decl->Typedef.ident);
                    map_put(ast_file->decls, ident, decl);
                    comp_unit.ident = ident;
                    array_push(ast_file->units, comp_unit);
                } else {
                    pln("typedef error: %", f_ast(decl));
                    //type_error(tcx, "")
                }
                
            }
        } break;
        
        default: {
            push_static_ast_node(parser, ast_file, decl);
        } break;
    }
}

void
parse_top_level_declaration(Parser* parser, Ast_File* ast_file) {
    
    Ast* attributes = 0;
    Token token = peek_token(parser);
    if (token.type == Token_Attribute) {
        attributes = parse_prefixed_compound(parser, Token_Attribute, 
                                             &parse_declaration_attribute);
    }
    //token = peek_token(parser);
    
    //if (token.type != Token_Ident) {
    //next_token(parser);
    //parse_error_unexpected_token(parser, Token_Ident, token);
    //return;
    //}
    
    Ast_Decl_Modifier mods = AstDeclModifier_None;
    while (true) {
        token = peek_token(parser);
        if (token.type != Token_Ident) {
            break;
        }
        
        string_id id = vars_save_string(token.source);
        switch (id) {
            case Sym___inline:
            case Kw_inline: {
                next_token(parser);
                // TODO(alexander): check if we already have inline/always_inline/no_inline bit set
                mods |= AstDeclModifier_Inline;
            } continue;
            
            case Sym___forceinline:
            case Kw_always_inline: {
                next_token(parser);
                // TODO(alexander): check if we already have inline/always_inline/no_inline bit set
                mods |= AstDeclModifier_Always_Inline;
            } continue;
            
            case Kw_no_inline: {
                next_token(parser);
                // TODO(alexander): check if we already have inline/always_inline/no_inline bit set
                mods |= AstDeclModifier_No_Inline;
            } continue;
            
            case Kw_extern: {
                next_token(parser);
                mods |= AstDeclModifier_External;
            } continue;
            
            // TODO(alexander): we might want to change this to annotation syntax
            case Kw_internal: {
                next_token(parser);
                // TODO(alexander): check if we already have internal bit set
                mods |= AstDeclModifier_Internal;
            } continue;
            
            case Kw_global: {
                next_token(parser);
                // TODO(alexander): check if we already have global bit set
                mods |= AstDeclModifier_Global;
            } continue;
            
            case Kw_const: {
                next_token(parser);
                // TODO(alexander): check if we already have const bit set
                mods |= AstDeclModifier_Const;
            } continue;
        }
        
        break;
    }
    
    token = peek_token(parser);
    Ast* decl = parse_statement(parser);
    register_top_level_declaration(parser, ast_file, decl, attributes, mods);
    
    while (next_token_if_matched(parser, Token_Semi, false));
}


Ast_File
parse_file(Parser* parser) {
    Ast_File result = {};
    
    // If source_groups are used start by tokenizing the first
    if (parser->source_groups && array_count(parser->source_groups) > 0) {
        Source_Group* group = parser->source_groups + parser->curr_source_group_index;
        tokenizer_set_source_group(parser->tokenizer, group);
        parser->c_compatibility_mode = group->c_compatibility_mode;
    }
    
    Token token = peek_token(parser);
    while (is_token_valid(token)) {
        
        parse_top_level_declaration(parser, &result);
        token = peek_token(parser);
        
        //if (parser->error_count > 10) {
        //pln("Found more than 10 parsing errors, exiting parsing...");
        //break;
        //}
    }
    
    result.error_count = parser->error_count;
    return result;
}


internal inline Token
next_semantical_token(Parser* parser) {
    //NOTE(alexander): filtering of comments and whitespace, maybe parameterize this later...
    Token token = advance_token(parser->tokenizer);
    while (!is_semantical_token(token)) {
        token = advance_token(parser->tokenizer);
    }
    
    if (token.type == Token_EOF) {
        // Try to load the next source group, if used
        if (parser->source_groups) {
            parser->curr_source_group_index++;
            if (array_count(parser->source_groups) > parser->curr_source_group_index) {
                Source_Group* group = parser->source_groups + parser->curr_source_group_index;
                tokenizer_set_source_group(parser->tokenizer, group);
                parser->c_compatibility_mode = group->c_compatibility_mode;
                token = next_semantical_token(parser);
            } else {
                tokenizer_finalize(parser->tokenizer);
            }
        }
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
        parse_error(parser, parser->current_token, string_lit("reached end of file while parsing"));
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
