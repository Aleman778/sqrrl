
Utf8_To_Utf32_Result
utf8_convert_to_utf32(u8* curr, u8* end) {
    Utf8_To_Utf32_Result result = {};
    
    u8 c = *curr++;
    result.num_bytes = utf8_calculate_num_bytes(c);
    if (result.num_bytes == 0) {
        return result;
    }
    
    result.value = c & utf8_first_byte_mask[result.num_bytes - 1];
    for (u32 i = 1; i < result.num_bytes; i++) {
        if (curr >= end) {
            result.num_bytes = 0;
            break;
        }
        
        c = *curr++;
        if ((u8) (c & 0xC0) != (u8) 0x80) {
            result.num_bytes = 0;
            break;
        }
        
        result.value <<= 6;
        result.value |= c & 0x3F;
    }
    
    return result;
}

void
utf8_advance_character(Tokenizer* tokenizer) {
    if (tokenizer->curr && tokenizer->curr != tokenizer->next) {
        tokenizer->column_number++;
        if (*tokenizer->curr == '\n') {
            array_push(tokenizer->lines, (smm) (tokenizer->next - tokenizer->start));
            //pln("Line %: `%`", f_umm(array_count(tokenizer->lines)), f_string(string_view(tokenizer->curr_line, tokenizer->next)));
            tokenizer->curr_line = tokenizer->next;
            tokenizer->line_number++;
            tokenizer->column_number = 0;
        }
    }
    
    tokenizer->curr = tokenizer->next;
    if (tokenizer->next >= tokenizer->end) {
        tokenizer->curr_utf32_character = 0;
        return;
    }
    
    u8 num_bytes = utf8_calculate_num_bytes(*tokenizer->next);
    if (num_bytes == 0) {
        tokenizer->curr_utf32_character = 0;
        return;
    }
    
    Utf8_To_Utf32_Result character = utf8_convert_to_utf32(tokenizer->curr, tokenizer->end);
    if (character.num_bytes == 0) {
        tokenization_error(tokenizer, string_lit("invalid utf-8 formatting detected"));
        return;
    }
    
    tokenizer->curr_utf32_character = character.value;
    tokenizer->next += character.num_bytes;
}

u8 //NOTE(alexander): utf8_string needs atleast 4 slots available, returns number of slots actually used.
utf32_convert_to_utf8(u32 utf32, u8* utf8_string) {
    u8 bytes = 0;
    if (utf32 < 0x80) {
        utf8_string[0] = (u8) utf32;
        return 1;
    } else if (utf32 < 0x800) {
        bytes = 2;
    } else if (utf32 < 0x10000) {
        bytes = 3;
    } else if (utf32 < 0x200000UL){
        bytes = 4;
    } else {
        assert(0 && "utf32 character out of range");
        return 0;
    }
    
    for (int i = bytes - 1; i > 0; i--) {
        utf8_string[i] = (u8) (utf32 | 0x80) & 0xBF;
        utf32 >>= 6;
    }
    utf8_string[0] = ((u8) utf32) | utf8_first_byte_mark[bytes - 1];
    return bytes;
}

internal inline void
scan_escape_character(Tokenizer* tokenizer, u8 quote) {
    if (*tokenizer->next == quote) {
        utf8_advance_character(tokenizer);
        return;
    }
    
    switch (*tokenizer->next) {
        case 'x': { // ASCII escape
            utf8_advance_character(tokenizer);
            int digit = hex_digit_to_s32(*tokenizer->next);
            if (digit == -1) {
                // TODO(alexander): add help message e.g. expected e.g. \x32
                tokenization_error(tokenizer, "ascii escape character is incorrectly formatted");
                return;
            }
            utf8_advance_character(tokenizer);
            if (hex_digit_to_s32(*tokenizer->next) == -1) {
                tokenization_error(tokenizer, "ascii escape character is incorrectly formatted");
                return;
            }
            if (digit > 7) {
                tokenization_error(tokenizer, "escape character out of range, expected [\\x00 - \\x7F]");
                return;
            }
        } break;
        
        case 'u': { // unicode character
            utf8_advance_character(tokenizer);
            if (*tokenizer->next == '{') {
                utf8_advance_character(tokenizer);
                for (int i = 0; i < 6 && tokenizer->next < tokenizer->end; i++) {
                    int d = hex_digit_to_s32(*tokenizer->next);
                    if (d == -1) {
                        if (i == 0) {
                            tokenization_error(tokenizer, "empty unicode escape, expected at least one hex decimal");
                        }
                        break;
                    }
                    
                    if (i == 5 && d > 0x10) {
                        tokenization_error(tokenizer, "invalid unicode escape character, expected at most 10FFFF");
                    }
                    utf8_advance_character(tokenizer);
                }
                if (*tokenizer->next == '}') {
                    utf8_advance_character(tokenizer);
                } else {
                    tokenization_error(tokenizer, string_format("expected `}`, found `%`", f_char(*tokenizer->next)));
                }
            } else {
                tokenization_error(tokenizer, string_format("expected `{`, found `%`", f_char(*tokenizer->next)));
            }
        } break;
        
        case 'n': case 'r': case 't': case '\\': case '0': {
            utf8_advance_character(tokenizer);
        } break;
        
        default: {
            utf8_advance_character(tokenizer);
            tokenization_error(tokenizer, string_format("expected escape character, found `%`", f_char(*tokenizer->curr)));
        } break;
    }
}

internal bool
scan_digits(Tokenizer* tokenizer, int base) {
    bool has_digits = false;
    while (tokenizer->curr < tokenizer->end) {
        if (*tokenizer->curr == '_') {
            utf8_advance_character(tokenizer);
            continue;
        }
        
        int d = hex_digit_to_s32(*tokenizer->curr);
        if (d == -1) {
            break;
        }
        
        // NOTE(alexander): ignores parsing e, E or f, ambigious with float exponent
        if ((d == 14 && base < 14) || (d == 15 && base < 15)) {
            break;
        }
        
        utf8_advance_character(tokenizer);
        if (d >= base) {
            tokenization_error(tokenizer, string_format("expected digit with base %, found `%`", f_int(base), f_char(*tokenizer->curr)));
        }
        has_digits = true;
    }
    return has_digits;
}

internal void
scan_number(Tokenizer* tokenizer, Token& token) {
    assert(*tokenizer->curr >= '0' && *tokenizer->curr <= '9');
    
    int base = 10;
    bool has_integral_digits = false;
    token.type = Token_Int;
    token.int_base = IntBase_Decimal;
    if (*tokenizer->curr == '0') {
        utf8_advance_character(tokenizer);
        switch (*tokenizer->curr) {
            case 'b': {
                utf8_advance_character(tokenizer);
                token.int_base = IntBase_Binary;
                base = 2;
            } break;
            
            case 'o': {
                utf8_advance_character(tokenizer);
                token.int_base = IntBase_Octal;
                base = 8;
            } break;
            
            case 'x': {
                utf8_advance_character(tokenizer);
                token.int_base = IntBase_Hexadecimal;
                base = 16;
            } break;
            
            default: {
                has_integral_digits = true;
            } break;
        }
    }
    has_integral_digits = has_integral_digits | scan_digits(tokenizer, base);
    
    if (*tokenizer->curr == '.' && *tokenizer->next != '.') {
        // Floating-point number with fraction and possibly exponent parts.
        utf8_advance_character(tokenizer);
        token.type = Token_Float;
        // NOTE(alexander): accept any number, handle error on parser.
        bool has_fractional_digits = scan_digits(tokenizer, 10);
        if (!has_fractional_digits || !has_integral_digits) {
            tokenization_error(tokenizer, "expected at least one digit before or after decimal point");
            return;
        }
    }
    
    if (*tokenizer->curr == 'e' || *tokenizer->curr == 'E') {
        token.type = Token_Float;
        utf8_advance_character(tokenizer);
        if (*tokenizer->curr != '+' && *tokenizer->curr != '-') {
            tokenization_error(tokenizer, string_format("expected `+` or `-`, found `%`", f_char(*tokenizer->curr)));
            return;
        }
        
        utf8_advance_character(tokenizer);
        if (!scan_digits(tokenizer, 10)) {
            tokenization_error(tokenizer, "expected at least one digit in exponent");
        }
    }
    
    // Type annotations separated by one ore more underline e.g. 10_u64
    while (*tokenizer->curr == '_' && tokenizer->curr < tokenizer->end) utf8_advance_character(tokenizer);
    token.suffix_start = (smm) (tokenizer->curr - (tokenizer->start + token.offset));
    
    // C type annotations
    if (token.type == Token_Int) {
        if ((*tokenizer->curr == 'u' || *tokenizer->curr == 'U') &&
            tokenizer->curr < tokenizer->end) {
            token.c_int_type |= CIntType_Unsigned;
            utf8_advance_character(tokenizer);
        }
        
        if ((*tokenizer->curr == 'l' || *tokenizer->curr == 'L') &&
            tokenizer->curr < tokenizer->end) {
            
            token.c_int_type |= CIntType_Long;
            
            utf8_advance_character(tokenizer);
            if ((*tokenizer->curr == 'l' || *tokenizer->curr == 'L') &&
                tokenizer->curr < tokenizer->end) {
                
                token.c_int_type |= CIntType_Long_Long;
                utf8_advance_character(tokenizer);
            }
        }
        
    } else if (token.type == Token_Float) {
        if (*tokenizer->curr == 'f') {
            utf8_advance_character(tokenizer);
        }
    }
    
    
    if (is_ident_start(*tokenizer->curr) || token.c_int_type > 0) {
        if (token.c_int_type == 0) {
            utf8_advance_character(tokenizer);
        }
        scan_while(tokenizer, &is_ident_continue);
    }
    
    return;
}

internal void
scan_single_quoted_string(Tokenizer* tokenizer) {
    assert(*tokenizer->curr == '\'');
    
    utf8_advance_character(tokenizer);
    u8* base = tokenizer->curr;
    while (*tokenizer->curr != '\'' && *tokenizer->curr != '\n' && tokenizer->curr < tokenizer->end) {
        if (*tokenizer->curr == '\\') scan_escape_character(tokenizer, '\'');
        utf8_advance_character(tokenizer);
    }
    if (tokenizer->curr - base == 0 && *tokenizer->curr == '\'') {
        tokenization_error(tokenizer, "empty character literal");
    }
}

internal void
scan_double_quoted_string(Tokenizer* tokenizer) {
    assert(*tokenizer->curr == '"');
    
    utf8_advance_character(tokenizer);
    while (*tokenizer->curr != '"' && tokenizer->curr < tokenizer->end) {
        if (*tokenizer->curr == '\\') scan_escape_character(tokenizer, '"');
        utf8_advance_character(tokenizer);
    }
}

internal smm
scan_raw_string(Tokenizer* tokenizer) {
    smm num_hashes = 0;
    if (*tokenizer->curr != '"') {
        num_hashes = scan_while(tokenizer, [](u32 c) { return c == '#'; });
        if (*tokenizer->curr == '"') {
            utf8_advance_character(tokenizer);
        } else {
            tokenization_error(tokenizer, string_format("expected `\"`, found `%c`", *tokenizer->next));
        }
    } else {
        utf8_advance_character(tokenizer);
    }
    
    while (tokenizer->curr < tokenizer->end) {
        if (*tokenizer->curr == '"') {
            utf8_advance_character(tokenizer);
            int found = scan_while(tokenizer, [](u32 c) { return c == '#'; });
            if (num_hashes == found) {
                return num_hashes;
            }
        } else {
            utf8_advance_character(tokenizer);
        }
    }
    return num_hashes;
}

Token
advance_token(Tokenizer* tokenizer) {
    assert(tokenizer->start && "tokenizer is not initialized");
    
    u8* base = tokenizer->curr;
    bool advance_utf32_at_end = true;
    Token token = {};
    token.file = tokenizer->file;
    token.line = tokenizer->line_number;
    token.column = tokenizer->column_number;
    token.offset = (smm) (tokenizer->curr - tokenizer->start);
    token.file_index = tokenizer->file_index;
    
    if (tokenizer->curr >= tokenizer->end) {
        token.type = Token_EOF;
        return token;
    }
    
    if (tokenizer->error_count > 0) {
        token.type = Token_Error;
        return token;
    }
    
    if (is_whitespace(tokenizer->curr_utf32_character)) {
        utf8_advance_character(tokenizer);
        scan_while(tokenizer, &is_whitespace);
        token.type = Token_Whitespace;
        advance_utf32_at_end = false;
        
    } else if (*tokenizer->curr == '\n' || *tokenizer->curr == '\r') {
        bool crlf = *tokenizer->curr == '\r';
        utf8_advance_character(tokenizer);
        if (crlf && *tokenizer->curr == '\n') {
            utf8_advance_character(tokenizer);
        }
        token.type = Token_Newline;
        advance_utf32_at_end = false;
        
    } else if (is_ident_start(*tokenizer->curr)) {
        utf8_advance_character(tokenizer);
        scan_while(tokenizer, &is_ident_continue);
        token.type = Token_Ident;
        advance_utf32_at_end = false;
        
    } else if (*tokenizer->curr == '/') {
        utf8_advance_character(tokenizer);
        
        if (*tokenizer->curr == '/') {
            do {
                utf8_advance_character(tokenizer);
                if (tokenizer->error_count > 0) {
                    break;
                }
            } while (tokenizer->curr < tokenizer->end &&
                     !(*tokenizer->curr == '\n' || *tokenizer->curr == '\r'));
            token.type = Token_Line_Comment;
            
        } else if (*tokenizer->curr == '*') {
            utf8_advance_character(tokenizer);
            int depth = 1;
            while (tokenizer->curr < tokenizer->end) {
                if (tokenizer->error_count > 0) {
                    break;
                }
                
                if (*tokenizer->curr == '/' && *tokenizer->next == '*') {
                    utf8_advance_character(tokenizer);
                    depth++;
                } else if (*tokenizer->curr == '*' && *tokenizer->next == '/') {
                    utf8_advance_character(tokenizer);
                    depth--;
                }
                utf8_advance_character(tokenizer);
                if (depth == 0) break;
            }
            token.type = Token_Block_Comment;
            
        } else if (*tokenizer->curr == '=') {
            token.type = Token_Div_Assign;
        } else {
            token.type = Token_Div;
            advance_utf32_at_end = false;
        }
        
    } else if (*tokenizer->curr >= '0' && *tokenizer->curr <= '9') {
        scan_number(tokenizer, token);
        advance_utf32_at_end = false;
        
    } else {
        switch (*tokenizer->curr) {
            case '\'': {
                scan_single_quoted_string(tokenizer);
                token.type = Token_Char;
            } break;
            
            case '"': {
                scan_double_quoted_string(tokenizer);
                token.type = Token_String;
            } break;
            
            case '=': {
                if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Equals;
                } else if (*tokenizer->next == '>') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Double_Right_Arrow;
                } else {
                    token.type = Token_Assign;
                }
            } break;
            
            case '!': {
                if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Not_Equals;
                } else {
                    token.type = Token_Logical_Not;
                }
            } break;
            
            case '>': {
                if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Gt_Equals;
                } else if (*tokenizer->next == '>') {
                    utf8_advance_character(tokenizer);
                    if (*tokenizer->next == '=') {
                        utf8_advance_character(tokenizer);
                        token.type = Token_Shr_Assign;
                    } else {
                        token.type = Token_Shr;
                    }
                } else {
                    token.type = Token_Gt;
                }
            } break;
            
            case '<': {
                if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Lt_Equals;
                } else if (*tokenizer->next == '<') {
                    utf8_advance_character(tokenizer);
                    if (*tokenizer->next == '=') {
                        utf8_advance_character(tokenizer);
                        token.type = Token_Shl_Assign;
                    } else {
                        token.type = Token_Shl;
                    }
                } else if (*tokenizer->next == '-') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Left_Arrow;
                } else {
                    token.type = Token_Lt;
                }
            } break;
            
            case '+': {
                if (*tokenizer->next == '+') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Increment;
                } else if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Add_Assign;
                } else {
                    token.type = Token_Add;
                }
            } break;
            
            case '-': {
                if (*tokenizer->next == '-') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Decrement;
                } else if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Sub_Assign;
                } else if (*tokenizer->next == '>') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Right_Arrow;
                } else {
                    token.type = Token_Sub;
                }
            } break;
            
            case '*': {
                if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Mul_Assign;
                } else {
                    token.type = Token_Mul;
                }
            } break;
            
            case '%': {
                if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Mod_Assign;
                } else {
                    token.type = Token_Mod;
                }
            } break;
            
            case '&': {
                if (*tokenizer->next == '&') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Logical_And;
                } else if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Bit_And_Assign;
                } else {
                    token.type = Token_Bit_And;
                }
            } break;
            
            case '|': { 
                if (*tokenizer->next == '|') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Logical_Or;
                } else if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Bit_Or_Assign;
                } else {
                    token.type = Token_Bit_Or;
                }
            } break;
            
            case '^': {
                if (*tokenizer->next == '=') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Bit_Xor_Assign;
                } else {
                    token.type = Token_Bit_Xor;
                }
            } break;
            
            case ':': {
                if (*tokenizer->next == ':') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Scope;
                } else {
                    token.type = Token_Colon;
                }
            } break;
            
            case '.': {
                if (*tokenizer->next == '.') {
                    utf8_advance_character(tokenizer);
                    if (*tokenizer->next == '.') {
                        utf8_advance_character(tokenizer);
                        token.type = Token_Ellipsis;
                    } else {
                        token.type = Token_Range;
                    }
                } else {
                    token.type = Token_Dot;
                }
            } break;
            
            case '#': {
                if (*tokenizer->next == '#') {
                    utf8_advance_character(tokenizer);
                    token.type = Token_Concatenator;
                } else {
                    token.type = Token_Directive; 
                }
            } break;
            
            case ',':  token.type = Token_Comma; break;
            case ';':  token.type = Token_Semi; break;
            case '@':  token.type = Token_Attribute; break;
            case '?':  token.type = Token_Question; break;
            case '$':  token.type = Token_Dollar; break;
            case '(':  token.type = Token_Open_Paren; break;
            case ')':  token.type = Token_Close_Paren; break;
            case '[':  token.type = Token_Open_Bracket; break;
            case ']':  token.type = Token_Close_Bracket; break;
            case '{':  token.type = Token_Open_Brace; break;
            case '}':  token.type = Token_Close_Brace; break;
            case '~':  token.type = Token_Bit_Not; break;
            case '\\': token.type = Token_Backslash; break;
            case '\0': token.type = Token_EOF; break;
            
            default: {
                u8 buffer[4];
                umm count = (umm) utf32_convert_to_utf8(tokenizer->curr_utf32_character, buffer);
                string character = create_string(count, buffer);
                tokenization_error(tokenizer, string_format("invalid unicode character `%` (U+%)", f_string(character), f_u64_HEX(tokenizer->curr_utf32_character)));
                token.type = Token_Error;
            } break;
        }
    }
    
    if (advance_utf32_at_end) utf8_advance_character(tokenizer);
    // TODO(Alexander): garbage collection???
    token.source = string_view(base, tokenizer->curr);
    return token;
}