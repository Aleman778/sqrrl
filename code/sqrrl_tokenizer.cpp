
internal void
tokenization_error(Tokenizer* t, str message) {
    pln("tokenization_error: %\n", f_str(message)); // TODO(alexander): real diagnostics
}

internal inline void
tokenization_error(Tokenizer* t, cstr message) {
    tokenization_error(t, str_lit(message));
}

internal inline u8
utf8_calculate_num_bytes(u8 next) {
    if (next & 0x80) {
        u8 mask = 0xC0;
        u8 num_bytes;
        for (num_bytes = 1; ((u8) (next & mask) != (u8) (mask << 1)); num_bytes++) {
            if (num_bytes > 4) {
                return 0;
            }
            mask = (mask >> 1) | 0x80;
        }
        
        if (num_bytes == 1) {
            return 0;
        }
        
        return num_bytes;
    } else {
        return 1;
    }
}

void
utf8_advance_character(Tokenizer* t) {
    if (t->curr) {
        t->column_number++;
        
        if (*t->curr == '\n') {
            t->curr_line = t->next;
            t->line_number++;
            t->column_number = 0;
        }
    }
    
    t->curr = t->next;
    if (t->next >= t->end) {
        t->curr_utf32_character = 0;
        return;
    }
    
    u8 num_bytes = utf8_calculate_num_bytes(*t->next);
    if (num_bytes == 0) {
        t->curr_utf32_character = 0;
        return;
    }
    
    u8 next = *t->next++;
    t->curr_utf32_character = next & utf8_first_byte_mask[num_bytes - 1];
    for (int i = 1; i < num_bytes; i++) {
        if (t->curr >= t->end) {
            //tokenization_error(t, "invalid utf-8 character");
            t->curr_utf32_character = 0;
            return;
        }
        
        if ((u8) (next & 0xC0) == (u8) 0x80) {
            //tokenization_error(t, "invalid utf-8 character");
            t->curr_utf32_character = 0;
            return;
        }
        
        t->curr_utf32_character <<= 6;
        t->curr_utf32_character |= *t->next++ & 0x3F;
    }
    return;
}

internal inline void
scan_escape_character(Tokenizer* t, u8 quote) {
    if (*t->next == quote) {
        utf8_advance_character(t);
        return;
    }
    
    switch (*t->next) {
        case 'x': { // ASCII escape
            utf8_advance_character(t);
            int digit = hex_digit_to_s32(*t->next);
            if (digit == -1) {
                // TODO(alexander): add help message e.g. expected e.g. \x32
                tokenization_error(t, "ascii escape character is incorrectly formatted");
                return;
            }
            utf8_advance_character(t);
            if (hex_digit_to_s32(*t->next) == -1) {
                tokenization_error(t, "ascii escape character is incorrectly formatted");
                return;
            }
            if (digit > 7) {
                tokenization_error(t, "escape character out of range, expected [\\x00 - \\x7F]");
                return;
            }
            break;
        }
        
        case 'u': { // unicode character
            utf8_advance_character(t);
            if (*t->next == '{') {
                utf8_advance_character(t);
                for (int i = 0; i < 6 && t->next < t->end; i++) {
                    int d = hex_digit_to_s32(*t->next);
                    if (d == -1) {
                        if (i == 0) {
                            tokenization_error(t, "empty unicode escape, expected at least one hex decimal");
                        }
                        break;
                    }
                    
                    if (i == 5 && d > 0x10) {
                        tokenization_error(t, "invalid unicode escape character, expected at most 10FFFF");
                    }
                    utf8_advance_character(t);
                }
                if (*t->next == '}') {
                    utf8_advance_character(t);
                } else {
                    tokenization_error(t, str_format("expected `}`, found `%c`", *t->next));
                }
            } else {
                tokenization_error(t, str_format("expected `{`, found `%c`", *t->next));
            }
            break;
        }
        
        case 'n': case 't': case '\\': case '0':
        utf8_advance_character(t);
        break;
        
        default:
        utf8_advance_character(t);
        tokenization_error(t, str_format("expected escape character, found `%c`", *t->curr));
        break;
    }
}

internal bool
scan_digits(Tokenizer* t, int base) {
    bool has_digits = false;
    while (t->curr < t->end) {
        if (*t->curr == '_') {
            utf8_advance_character(t);
            continue;
        }
        
        int d = hex_digit_to_s32(*t->curr);
        if (d == -1) {
            break;
        }
        
        // NOTE(alexander): ignores parsing e or E, ambigious with float exponent
        if (d == 14 && base < 14) {
            break;
        }
        
        utf8_advance_character(t);
        if (d >= base) {
            tokenization_error(t, str_format("expected digit with base %d, found `%c`", base, *t->curr));
        }
        has_digits = true;
    }
    return has_digits;
}

internal void
scan_number(Tokenizer* t, Token& token) {
    assert(*t->curr >= '0' && *t->curr <= '9');
    
    int base = 10;
    bool has_integral_digits = false;
    token.type = Token_Int;
    token.int_base = IntBase_Decimal;
    if (*t->curr == '0') {
        utf8_advance_character(t);
        switch (*t->curr) {
            case 'b': {
                utf8_advance_character(t);
                token.int_base = IntBase_Binary;
                base = 2;
            } break;
            
            case 'o': {
                utf8_advance_character(t);
                token.int_base = IntBase_Octal;
                base = 8;
            } break;
            
            case 'x': {
                utf8_advance_character(t);
                token.int_base = IntBase_Hexadecimal;
                base = 16;
            } break;
            
            default: {
                has_integral_digits = true;
            } break;
        }
    }
    has_integral_digits = has_integral_digits | scan_digits(t, base);
    
    if (*t->curr == '.' && *t->next != '.') {
        // Floating-point number with fraction and possibly exponent parts.
        utf8_advance_character(t);
        token.type = Token_Float;
        // NOTE(alexander): accept any number, handle error on parser.
        bool has_fractional_digits = scan_digits(t, 10);
        if (!has_fractional_digits || !has_integral_digits) {
            tokenization_error(t, "expected at least one digit before or after decimal point");
            return;
        }
    }
    
    if (*t->curr == 'e' || *t->curr == 'E') {
        token.type = Token_Float;
        utf8_advance_character(t);
        if (*t->curr != '+' && *t->curr != '-') {
            tokenization_error(t, str_format("expected `+` or `-`, found `%c`", *t->curr));
            return;
        }
        
        utf8_advance_character(t);
        if (!scan_digits(t, 10)) {
            tokenization_error(t, "expected at least one digit in exponent");
        }
    }
    
    while (*t->curr == '_' && t->curr < t->end) utf8_advance_character(t);
    token.suffix_start = (smm) (t->curr - t->start) - token.offset;
    if (is_ident_start(*t->curr)) {
        scan_while(t, &is_ident_continue);
    }
    
    return;
}

internal void
scan_single_quoted_string(Tokenizer* t) {
    assert(*t->curr == '\'');
    
    utf8_advance_character(t);
    u8* base = t->curr;
    while (*t->curr != '\'' && *t->curr != '\n' && t->curr < t->end) {
        if (*t->curr == '\\') scan_escape_character(t, '\'');
        utf8_advance_character(t);
    }
    if (t->curr - base == 0 && *t->curr == '\'') {
        tokenization_error(t, "empty character literal");
    }
}

internal void
scan_double_quoted_string(Tokenizer* t) {
    assert(*t->curr == '"');
    
    utf8_advance_character(t);
    while (*t->curr != '"' && t->curr < t->end) {
        if (*t->curr == '\\') scan_escape_character(t, '"');
        utf8_advance_character(t);
    }
}

internal smm
scan_raw_string(Tokenizer* t) {
    smm num_hashes = 0;
    if (*t->curr != '"') {
        num_hashes = scan_while(t, [](u32 c) { return c == '#'; });
        if (*t->curr == '"') {
            utf8_advance_character(t);
        } else {
            tokenization_error(t, str_format("expected `\"`, found `%c`", *t->next));
        }
    } else {
        utf8_advance_character(t);
    }
    
    while (t->curr < t->end) {
        if (*t->curr == '"') {
            utf8_advance_character(t);
            int found = scan_while(t, [](u32 c) { return c == '#'; });
            if (num_hashes == found) {
                return num_hashes;
            }
        } else {
            utf8_advance_character(t);
        }
    }
    return num_hashes;
}
