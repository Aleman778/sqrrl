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
parse_keyword(Parser* parser, Keyword expected, bool report_error) {
    Token token = peek_token(parser);
    if (token.type == Token_Ident) {
        string_id id = vars_save_string(token.source);
        if (id == expected) {
            next_token(parser);
            return true;
        }
    }
    
    if (report_error) {
        parse_error(parser, token, string_format("expected keyword, found `%`", f_string(token.source)));
    }
    
    return false;
}

Ast*
parse_identifier(Parser* parser, bool report_error) {
    Ast* result = push_ast_node(parser);
    
    Token token = peek_token(parser);
    if (token.type == Token_Ident) {
        string_id id = vars_save_string(token.source);
        if (id < keyword_last) {
            if (report_error) parse_error(parser, token, 
                                          string_format("expected `identifier` found keyword `%`", f_string(token.source)));
            return 0;
        }
        
        next_token(parser);
        result = push_ast_node(parser);
        result->type = Ast_Ident;
        result->Ident = vars_save_string(token.source);
        // TODO(Alexander): debuggin code
        result->Ident_String.contents = vars_load_string(result->Ident);
    } else if (report_error) {
        parse_error_unexpected_token(parser, Token_Ident, token);
    }
    
    return result;
}

internal u32
parse_escape_character(Parser* parser, u8*& curr, u8* end, bool byte) {
    u8 c = *curr++;
    switch (c) {
        case 'x': {
            assert(curr + 2 < end && "string out of bounds");
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
            assert(*curr++ == '{');
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

internal Ast*
parse_char(Parser* parser) {
    assert(parser->current_token.type == Token_Char);
    
    string str = parser->current_token.source;
    u8* curr = str.data;
    u8* end = str.data + str.count;
    assert(*curr++ == '\'');
    
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
        parse_error(parser, parser->current_token, string_lit("character literal may only contain one codepoint"));
    }
    
    // TODO(alexander): should we create a char value type?
    return push_ast_value(parser, create_unsigned_int_value(character));
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
    
    assert(*curr++ == '"');
    u8* last_push = curr;
    while (curr < end) {
        char c = *curr++;
        if (c == '\\') {
            if (last_push != curr) {
                string_builder_push(&sb, string_view(last_push, curr));
                last_push = curr;
            }
            
            u32 value = parse_escape_character(parser, curr, end, false);
            if (value > 0x7F) {
                // TODO(alexander): create string builder!
                string_builder_ensure_capacity(&sb, 4);
                sb.curr_used += utf32_convert_to_utf8(value, sb.data + sb.curr_used);
            } else {
                string_builder_ensure_capacity(&sb, 1);
                sb.data[sb.curr_used++] = (char) value;
            }
            continue;
        } else if (c == '"') {
            break;
        }
        
        u8 n = utf8_calculate_num_bytes((u8) c);
        curr += n;
    }
    
    if (last_push != curr - 1) {
        string_builder_push(&sb, string_view(last_push, curr - 1));
    }
    
    string result = string_builder_to_string(&sb);
    string_builder_free(&sb);
    return push_ast_value(parser, create_string_value(result));
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
    
    
    if (token.suffix_start != token.source.count) {
        assert(0 && "number suffixes are not supported yet!");
        
        Type* type = &global_primitive_types[PrimitiveTypeKind_int];
        string suffix = string_view(token.source.data + token.suffix_start, 
                                    token.source.data + token.source.count);
        
        string_id sym = vars_save_string(suffix);
        switch (sym) {
#define PRIMITIVE(name, ...) \
case Kw_##name: type = &global_primitive_types[PrimitiveTypeKind_##name]; break;
            DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
            default:
            if (suffix.count == 1) {
                if (*suffix.data == 'u') {
                    type = &global_primitive_types[PrimitiveTypeKind_uint];
                } else if (*suffix.data == 'f') {
                    type = &global_primitive_types[PrimitiveTypeKind_f32];
                }
            }
            // TODO(alexander): improve this error, e.g. show what types are available?
            //string name = copy_string(suffix, arena_allocator(parser->temp_arena));
            //parse_error(parser, token, "invalid integer type `%s`", lit(name));
            
            assert(0 && "TODO: add error message");
            break;
        }
    }
    
    Parse_U64_Value_Result parsed_result = parse_u64_value(token);
    if (parsed_result.is_too_large) {
        parse_error(parser, token, string_lit("integer literal is too large"));
    }
    u64 value = parsed_result.value;
    
    return push_ast_value(parser, create_signed_int_value(value));
}

internal Ast*
parse_float(Parser* parser) {
    Token token = parser->current_token;
    assert(token.type == Token_Float);
    
    if (token.int_base != IntBase_Decimal) {
        parse_error(parser, token, string_lit("float literals does not support int bases"));
        return 0;
    }
    
    
    if (token.suffix_start != token.source.count) {
        assert(0 && "number suffixes are not supported yet!");
#if 0
        Type type = primitive_types[Primitive_float];
        String suffix = substring_nocopy(token.source, token.suffix_start, token.source.length);
        Symbol sym = find_symbol(suffix);
        switch (sym.index) {
            case Kw_f32: type = primitive_types[Primitive_f32];
            case Kw_f64: type = primitive_types[Primitive_f64];
            default:
            String name = copy_string(token.source, arena_allocator(parser->temp_arena));
            parse_error(parser, token, "invalid float type `%s`, expected `f32` or `f64`", lit(name));
        }
#endif
    }
    
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
    
    return push_ast_value(parser, create_floating_value(value));
}

Ast*
parse_atom(Parser* parser, bool report_error) {
    Ast* result = {};
    Token token = peek_token(parser);
    
    switch (token.type) {
        case Token_Ident: {
            string_id sym = vars_save_string(token.source);
            switch (sym) {
                case Kw_false: {
                    next_token(parser);
                    result = push_ast_value(parser, create_boolean_value(false));
                } break;
                
                case Kw_true: {
                    next_token(parser);
                    result = push_ast_value(parser, create_boolean_value(true));
                } break;
                
                case Kw_cast: {
                    next_token(parser);
                    next_token_if_matched(parser, Token_Open_Paren);
                    Ast* type = parse_type(parser);
                    next_token_if_matched(parser, Token_Close_Paren);
                    
                    Ast* inner_expr = parse_expression(parser);
                    Ast* node = push_ast_node(parser);
                    node->type = Ast_Cast_Expr;
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
            next_token(parser);
            result = parse_int(parser);
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
            next_token(parser);
            result = push_ast_node(parser, &token);
            Ast* inner_expr = parse_expression(parser, false);
            if (inner_expr && inner_expr->type == Ast_None) {
                
                result->type = Ast_Cast_Expr;
                result->Cast_Expr.type = parse_type(parser, true);
                next_token_if_matched(parser, Token_Close_Paren);
                result->Cast_Expr.expr = parse_expression(parser, true);
            } else {
                
                Token peek = peek_token(parser);
                switch (peek.type) {
                    case Token_Close_Paren: {
                        next_token(parser);
                        result->type = Ast_Paren_Expr;
                        result->Paren_Expr.expr = inner_expr;
                        update_span(parser, result);
                    } break;
                    
                    case Token_Comma: {
                        result = parse_compound(parser, 
                                                Token_Comma, Token_Close_Paren, Token_Comma, 
                                                &parse_actual_argument);
                    } break;
                    
                    default: {
                        if (report_error) {
                            parse_error(parser, peek, string_format("expected `)` or `,` found `%` ", f_token(peek.type)));
                        }
                    } break;
                }
            }
        } break;
        
        case Token_Open_Brace: {
            result = push_ast_node(parser, &token);
            result->type = Ast_Array_Expr;
            result->Array_Expr.elements = parse_compound(parser, 
                                                         Token_Open_Brace, 
                                                         Token_Close_Brace, 
                                                         Token_Comma, 
                                                         &parse_actual_argument);
        } break;
        
        default: {
            Unary_Op unop = parse_unary_op(parser);
            if (unop != UnaryOp_None) {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->type = Ast_Unary_Expr;
                result->Unary_Expr.op = unop;
                result->Unary_Expr.first = parse_atom(parser);
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
        atom_expr = parse_atom(parser, report_error);
        
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
            case Token_Dot: {
                next_token(parser);
                lhs_expr = push_ast_node(parser);
                lhs_expr->type = Ast_Field_Expr;
                lhs_expr->Field_Expr.var = rhs_expr;
                Ast* field = parse_atom(parser, true);
                if (field->type == Ast_Ident) {
                    lhs_expr->Field_Expr.field = field;
                } else {
                    // NOTE(Alexander): we have to unwrap the expression so we first fetch the field identifier then perform the expression
                    // TODO(Alexander): this assumes that first node is identifier, otherwise we fail this is quite ugly
                    assert(field->children[0] && field->children[0]->type == Ast_Ident);
                    lhs_expr->Field_Expr.field = field->children[0];
                    field->children[0] = lhs_expr;
                    lhs_expr = field;
                }
                update_span(parser, lhs_expr);
            } continue;
            
            case Token_Open_Paren: {
                lhs_expr = push_ast_node(parser);
                lhs_expr->type = Ast_Call_Expr;
                lhs_expr->Call_Expr.ident = rhs_expr;
                lhs_expr->Call_Expr.args = parse_compound(parser, 
                                                          Token_Open_Paren, Token_Close_Paren, Token_Comma, 
                                                          &parse_actual_argument);
                update_span(parser, lhs_expr);
            } continue;
            
            case Token_Open_Bracket: {
                next_token(parser);
                lhs_expr = push_ast_node(parser);
                lhs_expr->type = Ast_Index_Expr;
                lhs_expr->Index_Expr.array = rhs_expr;
                lhs_expr->Index_Expr.index = parse_atom(parser);
                next_token_if_matched(parser, Token_Close_Bracket);
                update_span(parser, lhs_expr);
            } continue;
            
            case Token_Open_Brace: {
                if (rhs_expr->type == Ast_Ident) {
                    lhs_expr = push_ast_node(parser);
                    lhs_expr->type = Ast_Struct_Expr;
                    lhs_expr->Struct_Expr.ident = rhs_expr;
                    lhs_expr->Struct_Expr.fields = parse_compound(parser,
                                                                  Token_Open_Brace, Token_Close_Brace, Token_Comma, 
                                                                  &parse_actual_struct_or_union_argument);
                }
                update_span(parser, lhs_expr);
            } continue;
        }
        
        break;
    }
    
    // Parse binary expression using precedence climbing, is applicable
    Binary_Op binary_op = parse_binary_op(parser);
    Token token = peek_token(parser); // HACK(Alexander): want to get ternary to fix precedence
    while (binary_op || token.type == Token_Question) {
        u8 prec;
        Assoc assoc;
        
        // Parse ternary operation last
        if (token.type == Token_Question) {
            prec = 1;
            assoc = Assoc_Right;
        } else {
            prec = binary_get_prec(binary_op);
            assoc = binary_get_assoc(binary_op);
        }
        
        if (prec < min_prec) {
            break;
        }
        
        u8 next_min_prec = prec;
        if (assoc == Assoc_Left) {
            next_min_prec += 1;
        }
        
        next_token(parser);
        if (token.type == Token_Question) {
            Ast* ternary_expr = push_ast_node(parser);
            ternary_expr->type = Ast_Ternary_Expr;
            ternary_expr->Ternary_Expr.first = lhs_expr;
            ternary_expr->Ternary_Expr.second = parse_expression(parser);
            next_token_if_matched(parser, Token_Colon);
            ternary_expr->Ternary_Expr.third = parse_expression(parser);
            lhs_expr = ternary_expr;
        } else {
            Ast* rhs_expr = parse_expression(parser, true, next_min_prec);
            Ast* node = push_ast_node(parser);
            node->type = Ast_Binary_Expr;
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
    result->type = Ast_Assign_Stmt;
    result->Assign_Stmt.type = type;
    result->Assign_Stmt.ident = parse_identifier(parser);
    
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
    result->type = Ast_Block_Stmt;
    result->Block_Stmt.stmts = parse_compound(parser, Token_Open_Brace, Token_Close_Brace, Token_Semi,
                                              &parse_actual_statement);
    return result;
}

Ast*
parse_statement(Parser* parser, bool report_error) {
    Token token = peek_token(parser);
    Ast* result = 0;
    
    if (token.type == Token_Ident) {
        Keyword keyword = (Keyword) vars_save_string(token.source);
        switch (keyword) {
            case Kw_break: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->type = Ast_Break_Stmt;
                result->Break_Stmt.ident = parse_identifier(parser, false);
            } break;
            
            case Kw_continue: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->type = Ast_Continue_Stmt;
                result->Continue_Stmt.ident = parse_identifier(parser, false);
            } break;
            
            case Kw_if: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->type = Ast_If_Stmt;
                bool opened_paren = next_token_if_matched(parser, Token_Open_Paren, false);
                result->If_Stmt.cond = parse_expression(parser);
                next_token_if_matched(parser, Token_Close_Paren, opened_paren);
                result->If_Stmt.then_block = parse_statement(parser);
                if (parse_keyword(parser, Kw_else, false)) {
                    result->If_Stmt.else_block = parse_statement(parser);
                } else {
                    // TODO(Alexander): maybe reference a "null" pre allocated node instead?
                    result->If_Stmt.else_block = push_ast_node(parser);
                }
            } break;
            
            case Kw_for: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->type = Ast_For_Stmt;
                result->For_Stmt.label = parse_identifier(parser, false);
                if (result->For_Stmt.label->type != Ast_None) {
                    next_token_if_matched(parser, Token_Colon);
                }
                bool opened_paren = next_token_if_matched(parser, Token_Open_Paren, false);
                // TODO(alexander): should probably not be statement, move out variable decl
                result->For_Stmt.init = parse_statement(parser, false);
                next_token_if_matched(parser, Token_Semi);
                result->For_Stmt.cond = parse_statement(parser, false);
                next_token_if_matched(parser, Token_Semi);
                result->For_Stmt.update = parse_expression(parser, false);
                next_token_if_matched(parser, Token_Close_Paren, opened_paren);
                result->For_Stmt.block = parse_statement(parser);
            } break;
            
            case Kw_while: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->type = Ast_While_Stmt;
                result->While_Stmt.label = parse_identifier(parser, false);
                if (result->While_Stmt.label->type != Ast_None) {
                    next_token_if_matched(parser, Token_Colon);
                }
                bool opened_paren = next_token_if_matched(parser, Token_Open_Paren, false);
                result->While_Stmt.cond = parse_expression(parser);
                next_token_if_matched(parser, Token_Close_Paren, opened_paren);
                result->While_Stmt.block = parse_statement(parser);
            } break;
            
            case Kw_return: {
                next_token(parser);
                result = push_ast_node(parser, &token);
                result->type = Ast_Return_Stmt;
                result->Return_Stmt.expr = parse_expression(parser, false);
            } break;
            
            default: {
                Ast* type = parse_type(parser, false);
                
                if (type) {
                    switch (type->type) {
                        case Ast_Struct_Type:
                        case Ast_Union_Type:
                        case Ast_Enum_Type: {
                            result = push_ast_node(parser, &token);
                            result->type = Ast_Decl_Stmt;
                            result->Decl_Stmt.ident = type->children[0];
                            result->Decl_Stmt.type = type;
                        } break;
                        
                        case Ast_Function_Type: {
                            result = push_ast_node(parser, &token);
                            result->type = Ast_Decl_Stmt;
                            result->Decl_Stmt.ident = type->Function_Type.ident;
                            result->Decl_Stmt.type = type;
                            // TODO(alexander): this is a little bit strange
                            result->Decl_Stmt.decl = parse_block_statement(parser);
                        } break;
                        
                        case Ast_Typedef: {
                            result = push_ast_node(parser, &token);
                            result->type = Ast_Decl_Stmt;
                            result->Decl_Stmt.type = type;
                            result->Decl_Stmt.ident = parse_identifier(parser);
                        } break;
                        
                        case Ast_Named_Type: {
                            Token peek = peek_token(parser);
                            if (peek.type == Token_Ident) {
                                result = parse_assign_statement(parser, type);
                            } else {
                                Ast* ident = type->Named_Type;
                                
                                result = push_ast_node(parser);
                                result->type = Ast_Expr_Stmt;
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
    } else if (token.type == Token_Open_Brace) {
        result = parse_block_statement(parser, &token);
    }
    
    if (!result) {
        result = push_ast_node(parser);
        result->type = Ast_Expr_Stmt;
        result->Expr_Stmt = parse_expression(parser, report_error);
    }
    
    update_span(parser, result);
    return result;
}

Unary_Op
parse_unary_op(Parser* parser) {
    Token token = peek_token(parser);
    switch (token.type) {
        case Token_Sub:         return UnaryOp_Negate;
        case Token_Logical_Not: return UnaryOp_Not;
        case Token_Bit_Not:     return UnaryOp_Bitwise_Not;
        case Token_Bit_And:     return UnaryOp_Address_Of;
        case Token_Mul:         return UnaryOp_Dereference;
        default:                return UnaryOp_None;
    }
}

Binary_Op
parse_binary_op(Parser* parser) {
    Token token = peek_token(parser);
    switch (token.type) {
        case Token_Equals:         return BinaryOp_Equals;
        case Token_Lt:             return BinaryOp_Less_Than;
        case Token_Gt:             return BinaryOp_Greater_Than;
        case Token_Add:            return BinaryOp_Add;
        case Token_Sub:            return BinaryOp_Subtract;
        case Token_Mul:            return BinaryOp_Multiply;
        case Token_Div:            return BinaryOp_Divide;
        case Token_Bit_And:        return BinaryOp_Bitwise_And;
        case Token_Bit_Or:         return BinaryOp_Bitwise_Or;
        case Token_Bit_Xor:        return BinaryOp_Bitwise_Xor;
        case Token_Mod:            return BinaryOp_Modulo;
        case Token_Assign:         return BinaryOp_Assign;
        case Token_Add_Assign:     return BinaryOp_Add_Assign;
        case Token_Sub_Assign:     return BinaryOp_Subtract_Assign;
        case Token_Mul_Assign:     return BinaryOp_Multiply_Assign;
        case Token_Div_Assign:     return BinaryOp_Divide_Assign;
        case Token_Mod_Assign:     return BinaryOp_Modulo_Assign;
        case Token_Bit_And_Assign: return BinaryOp_Bitwise_And_Assign;
        case Token_Bit_Or_Assign:  return BinaryOp_Bitwise_Or_Assign;
        case Token_Bit_Xor_Assign: return BinaryOp_Bitwise_Xor_Assign;
        case Token_Shl_Assign:     return BinaryOp_Shift_Left_Assign;
        case Token_Shr_Assign:     return BinaryOp_Shift_Left_Assign;
        case Token_Not_Equals:     return BinaryOp_Not_Equals;
        case Token_Logical_And:    return BinaryOp_Logical_And;
        case Token_Logical_Or:     return BinaryOp_Logical_Or;
        case Token_Lt_Equals:      return BinaryOp_Less_Equals;
        case Token_Gt_Equals:      return BinaryOp_Greater_Equals;
        case Token_Shl:            return BinaryOp_Shift_Left;
        case Token_Shr:            return BinaryOp_Shift_Right;
        default:                   return BinaryOp_None;
    }
}

Ast*
parse_formal_struct_or_union_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->type = Ast_Argument;
    result->Argument.type = parse_type(parser);
    result->Argument.ident = parse_identifier(parser, false);
    
    if (result->Argument.ident && 
        next_token_if_matched(parser, Token_Comma, false)) {
        
        Ast* curr = push_ast_node(parser);
        curr->type = Ast_Compound;
        curr->Compound.node = result->Argument.ident;
        result->Argument.ident = curr;
        
        do {
            curr->type = Ast_Compound;
            curr->Compound.node = parse_identifier(parser);
            curr->Compound.next = push_ast_node(parser);
            curr = curr->Compound.next;
        } while (next_token_if_matched(parser, Token_Comma, false));
    }
    
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    } else {
        result->Argument.assign = push_ast_node(parser);
    }
    return result;
}

Ast*
parse_actual_struct_or_union_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->type = Ast_Argument;
    result->Argument.ident = parse_identifier(parser, false);
    if (result->Argument.ident) {
        if (next_token_if_matched(parser, Token_Colon, false)) {
            result->Argument.assign = parse_expression(parser);
        } else {
            result->Argument.assign = push_ast_node(parser);
        }
    } else {
        result->Argument.assign = parse_expression(parser);
    }
    return result;
}

Ast*
parse_formal_enum_argument(Parser* parser) {
    Ast* result = push_ast_node(parser);
    result->type = Ast_Argument;
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
    result->type = Ast_Argument;
    result->Argument.type = parse_type(parser);
    result->Argument.ident = parse_identifier(parser);
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
    result->type = Ast_Argument;
    result->Argument.ident = parse_identifier(parser);
    if (next_token_if_matched(parser, Token_Assign, false)) {
        result->Argument.assign = parse_expression(parser);
    } else {
        result->Argument.assign = push_ast_node(parser);
    }
    return result;
}

Ast*
parse_actual_argument(Parser* parser) {
    return parse_expression(parser);
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
    result->type = Ast_Compound;
    
    next_token_if_matched(parser, begin);
    if (next_token_if_matched(parser, end, false)) {
        result->Compound.node = push_ast_node(parser);
        result->Compound.next = push_ast_node(parser);
        return result;
    }
    
    Ast* curr = result;
    for (;;) {
        Token token = peek_token(parser);
        if (token.type == end) {
            next_token(parser);
            break;
        }
        
        curr->type = Ast_Compound;
        Ast* node = element_parser(parser);
        curr->Compound.node = node;
        curr->Compound.next = push_ast_node(parser);
        curr = curr->Compound.next;
        
        // HACK(Alexander): to get avoid requiring separators for block statements
        if (node && (node->type == Ast_Block_Stmt ||
                     node->type == Ast_Decl_Stmt ||
                     node->type == Ast_If_Stmt ||
                     node->type == Ast_For_Stmt ||
                     node->type == Ast_While_Stmt)) {
            continue;
        }
        if (!next_token_if_matched(parser, separator, false)) {
            if (next_token_if_matched(parser, end, false)) {
                break;
            } else {
                token = peek_token(parser);
                parse_error_unexpected_token(parser, separator, token);
                
                for (;;) {
                    token = next_token(parser);
                    if (token.type == Token_EOF ||
                        token.type == end) {
                        return result;
                    }
                    
                    if (token.type == separator) { 
                        break;
                    }
                }
            }
        }
        
    }
    
    return result;
}

Ast*
parse_type(Parser* parser, bool report_error) {
    Token token = peek_token(parser);
    
    if (token.type == Token_Open_Paren) {
        // TODO(alexander): tuple type
    }
    
    if (token.type != Token_Ident) {
        if (report_error) {
            parse_error(parser, token, string_format("expected `type` found `%`", f_string(token.source)));
        }
        return 0;
    }
    
    next_token(parser);
    
    Ast* base = 0;
    string_id ident = vars_save_string(token.source);
    
    if (ident >= builtin_types_begin && ident <= builtin_types_end || ident > keyword_last ||
        ident == Kw_string) {
        base = push_ast_node(parser);
        base->type = Ast_Named_Type;
        base->Named_Type = push_ast_node(parser);
        base->Named_Type->type = Ast_Ident;
        base->Named_Type->Ident = ident;
    } else {
        switch (ident) {
            case Kw_struct: {
                base = push_ast_node(parser);
                base->type = Ast_Struct_Type;
                base->Struct_Type.ident = parse_identifier(parser, false);
                base->Struct_Type.fields = parse_compound(parser,
                                                          Token_Open_Brace, Token_Close_Brace, Token_Semi,
                                                          &parse_formal_struct_or_union_argument);
            } break;
            
            case Kw_union: {
                base = push_ast_node(parser);
                base->type = Ast_Union_Type;
                base->Union_Type.ident = parse_identifier(parser, false);
                base->Union_Type.fields = parse_compound(parser,
                                                         Token_Open_Brace, Token_Close_Brace, Token_Semi,
                                                         &parse_formal_struct_or_union_argument);
            } break;
            
            case Kw_enum: { 
                base = push_ast_node(parser);
                base->type = Ast_Enum_Type;
                base->Enum_Type.ident = parse_identifier(parser, false);
                if (next_token_if_matched(parser, Token_Assign, false)) {
                    base->Enum_Type.elem_type = parse_type(parser);
                }
                base->Enum_Type.fields = parse_compound(parser,
                                                        Token_Open_Brace, Token_Close_Brace, Token_Comma,
                                                        &parse_formal_enum_argument);
            } break;
            
            case Kw_typedef: {
                base = push_ast_node(parser);
                base->type = Ast_Typedef;
                base->Typedef.type = parse_type(parser);
                base->Typedef.ident = parse_identifier(parser);
                return base;
            } break;
            
            default: {
                if (report_error) {
                    parse_error(parser, token,
                                string_format("expected `type` found `%`", f_string(token.source)));
                }
                return 0;
            } break;
        }
    }
    
    Ast* result = base;
    if (base) {
        Token second_token = peek_token(parser);
        switch (second_token.type) {
            case Token_Ident: {
                if (peek_second_token(parser).type == Token_Open_Paren) {
                    // TODO(alexander): check what the base type is, e.g. cannot be struct type as return type
                    result = push_ast_node(parser);
                    result->type = Ast_Function_Type;
                    result->Function_Type.return_type = base;
                    result->Function_Type.ident = parse_identifier(parser);
                    result->Function_Type.arg_types = parse_compound(parser,
                                                                     Token_Open_Paren, Token_Close_Paren, Token_Comma,
                                                                     &parse_formal_function_argument);
                    return result;
                }
            } break;
            
            case Token_Open_Paren: {
                if (peek_second_token(parser).type == Token_Mul) {
                    next_token(parser);
                    next_token(parser);
                    
                    // TODO(alexander): check what the base type is, e.g. cannnot be struct type as return type
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
                result->Array_Type.elem_type = base;
                result->Array_Type.shape = parse_expression(parser, false);
                
                next_token_if_matched(parser, Token_Close_Bracket);
            } break;
            
            case Token_Mul: {
                next_token(parser);
                result = push_ast_node(parser);
                result->type = Ast_Pointer_Type;
                result->Pointer_Type = base;
            } break;
        }
    } else {
        if (report_error) {
            parse_error(parser, token,
                        string_format("expected `type` found `%`", f_string(token.source)));
        }
    }
    
    return result;
}


Ast_Decl_Entry
parse_top_level_declaration(Parser* parser) {
    Ast_Decl_Entry result = {};
    Token peek = peek_token(parser);
    Ast* decl = push_ast_node(parser, &peek);
    
    decl->type = Ast_Decl;
    decl->Decl.mods = AstDeclModifier_None;
    result.value = decl;
    
    while (true) {
        Token token = peek_token(parser);
        if (token.type != Token_Ident) {
            next_token(parser);
            parse_error_unexpected_token(parser, Token_Ident, token);
            return result;
        }
        
        string_id id = vars_save_string(token.source);
        switch (id) {
            case Kw_inline: {
                next_token(parser);
                // TODO(alexander): check if we already have inline/no_inline bit set
                decl->Decl.mods |= AstDeclModifier_Inline;
            } break;
            
            case Kw_no_inline: {
                next_token(parser);
                // TODO(alexander): check if we already have inline/no_inline bit set
                decl->Decl.mods |= AstDeclModifier_No_Inline;
            } break;
            
            // TODO(alexander): we might want to change this to annotation syntax
            case Kw_internal: {
                next_token(parser);
                // TODO(alexander): check if we already have internal bit set
                decl->Decl.mods |= AstDeclModifier_Internal;
            } break;
            
            case Kw_global: {
                next_token(parser);
                // TODO(alexander): check if we already have global bit set
                decl->Decl.mods |= AstDeclModifier_Global;
            } break;
            
            default: {
                Ast* decl_stmt = parse_statement(parser);
                decl->Decl.stmt = decl_stmt;
                
                // TODO(alexander): report error
                if (decl_stmt->type < Ast_Stmt_Begin && decl_stmt->type > Ast_Stmt_End) {
                    parse_error(parser, token, string_format("expected statement found `%`", f_token(token.type)));
                }
                
                if (decl_stmt->type != Ast_Decl_Stmt) {
                    next_token_if_matched(parser, Token_Semi);
                }
                
                // NOTE(alexander): tries to find an identifier to use as search key
                for (int i = 0; i < fixed_array_count(decl_stmt->children); i++) {
                    Ast* node = decl_stmt->children[i];
                    if (!node) continue;
                    if (node->type == Ast_Ident) {
                        decl->Decl.ident = node;
                        result.key = node->Ident;
                        return result;
                    }
                }
                
                parse_error(parser, token, string_lit("cannot declare top level declaration without identifier"));
                return result;
            } break;
        }
    }
    
    return result;
}

Ast_File
parse_file(Parser* parser) {
    Ast_File result = {};
    
    
    while (true) {
        Token token = peek_token(parser);
        if (token.type == Token_EOF || token.type == Token_Error) {
            break;
        }
        
        Ast_Decl_Entry decl = parse_top_level_declaration(parser);
        update_span(parser, decl.value);
        
        map_put(result.decls, decl.key, decl.value);
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
