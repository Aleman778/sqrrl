
inline void
lexer_init_source(Lexer* lexer, Memory_Arena* ast_arena, string source, u32 file_index) {
    lexer->begin = source.data;
    lexer->end = source.data + source.count;
    lexer->curr = lexer->begin;
    lexer->ast_arena = ast_arena;
    lexer->loc.file_index = file_index;
    lexer->loc.line_number = 0;
    lexer->loc.column_number = 0;
}

internal inline bool 
is_whitespace(u32 ch) {
    return ch == ' ' || ch == '\n' || ch == '\t' || ch == '\r';
}

internal inline bool
is_ident_start(u32 c) {
    return (('a' <= c && c <= 'z') ||
            ('A' <= c && c <= 'Z') ||
            '_' == c || 
            '$' == c);
}

internal inline bool
is_ident_continue(u32 c) {
    return (('a' <= c && c <= 'z') ||
            ('A' <= c && c <= 'Z') ||
            ('0' <= c && c <= '9') ||
            '_' == c || 
            '$' == c);
}

internal inline bool
is_digit(u32 c) {
    return c >= '0' && c <= '9';
}

internal inline s32
hex_digit_to_s32(u8 c) {
    if      ('0' <= c && c <= '9') return c - (u8) '0';
    else if ('a' <= c && c <= 'f') return c - (u8) 'a' + 10;
    else if ('A' <= c && c <= 'F') return c - (u8) 'A' + 10;
    else                           return -1;
}

internal inline u8
lexer_next_char(Lexer* lexer) {
    u8 ch;
    do {
        ch = *lexer->curr++;
        lexer->loc.column_number++;
        if (ch == '\n') {
            lexer->loc.line_number++;
            lexer->loc.column_number = 0;
        }
    } while (lexer->curr < lexer->end && is_whitespace(ch));
    
    if (lexer->curr >= lexer->end) {
        return 0;
    }
    
    return ch;
}

internal inline Token_Kind
lex_identifier(Lexer* lexer, Identifier* out_ident) {
    string source;
    source.data = lexer->curr - 1;
    while (lexer->curr < lexer->end && is_ident_continue(*lexer->curr)) {
        lexer_next_char(lexer);
    }
    source.count = lexer->curr - source.data;
    Identifier ident = vars_save_string(source);
    *out_ident = ident;
    
    if (is_builtin_keyword(ident)) {
        return (Token_Kind) (Token_Asm + (ident - Kw_asm));
    }
    
    if (is_builtin_type_keyword(ident)) {
        return (Token_Kind) (Token_Void + (ident - Kw_void));
    }
    
    return Token_Ident;
}

inline bool
lex_integer(Lexer* lexer, int base, u64* result) {
    bool has_digits = false;
    while (lexer->curr < lexer->end) {
        if (*lexer->curr == '_') {
            lexer_next_char(lexer);
            continue;
        }
        
        int d = hex_digit_to_s32(*lexer->curr);
        if (d == -1) {
            break;
        }
        
        // NOTE(alexander): ignores parsing e, E or f, ambigious with float exponent
        if ((d == 14 && base < 14) || (d == 15 && base < 15)) {
            break;
        }
        
        lexer_next_char(lexer);
        if (d >= base) {
            unimplemented;
            //tokenization_error(lexer, string_print("expected digit with base %, found `%`", f_int(base), f_char(*lexer->curr)));
        }
        
        *result = *result * base + d;
        has_digits = true;
    }
    return has_digits;
}

inline Token_Kind
lex_number(Lexer* lexer, Token* token, u8 ch) {
    assert(ch >= '0' && ch <= '9');
    
    u64 integral_part = 0;
    bool has_integral_digits;
    
    int base = 10;
    if (ch == '0') {
        switch (*lexer->curr) {
            case 'b': {
                lexer_next_char(lexer);
                base = 2;
            } break;
            
            case 'o': {
                lexer_next_char(lexer);
                base = 8;
            } break;
            
            case 'x': {
                lexer_next_char(lexer);
                base = 16;
            } break;
            
            default: {
                integral_part = 10;
                has_integral_digits = true;
            } break;
        }
    } else {
        integral_part = ch - '0';
        has_integral_digits = true;
    }
    
    if (lex_integer(lexer, base, &integral_part)) {
        has_integral_digits = true;
    }
    
    // TODO(Alexander): add support for float literals
    token->u64_value = integral_part;
    return Token_Int_Literal;
}

void
lex_comment(Lexer* lexer) {
    if (*lexer->curr == '/') {
        lexer_next_char(lexer);
        do {
            lexer_next_char(lexer);
        } while (lexer->curr < lexer->end && *lexer->curr != '\n');
        
    } else if (*lexer->curr == '*') {
        lexer_next_char(lexer);
        lexer_next_char(lexer);
        
        int depth = 1;
        while (lexer->curr < lexer->end) {
            u8 ch = lexer_next_char(lexer);
            if (ch == '/' && *lexer->curr == '*') {
                lexer_next_char(lexer);
                depth++;
            } else if (ch == '*' && *lexer->curr == '/') {
                lexer_next_char(lexer);
                depth--;
            }
            if (depth == 0) break;
        }
    }
}

Token_Kind
lex_finish(Lexer* lexer) {
    lexer->curr = lexer->end;
    lexer->curr_token = {};
    lexer->curr_token.kind = Token_EOF;
    lexer->unlex_token.kind = Token_EOF;
    return lexer->curr_token.kind;
}

Token_Kind
lex(Lexer* lexer) {
    if (lexer->unlex_token.kind != Token_EOF) {
        lexer->curr_token = lexer->unlex_token;
        lexer->unlex_token = {};
        return lexer->curr_token.kind;
    }
    
    u8 ch = lexer_next_char(lexer);
    while (ch == '/') {
        lex_comment(lexer);
        ch = lexer_next_char(lexer);
    }
    
    Token result = {};
    result.loc = lexer->loc;
    result.source.data = lexer->curr - 1;
    result.kind = token_lit(ch);
    if (is_ident_start(ch)) {
        result.kind = lex_identifier(lexer, &result.ident);
        
    } else if (is_digit(ch)) {
        result.kind = lex_number(lexer, &result, ch);
        
    } else if (ch == '=' && *lexer->curr == '=') {
        lexer_next_char(lexer);
        result.kind = Token_Equals;
        
    } else if (ch == '\0') {
        return lex_finish(lexer);
    }
    
    result.source.count = lexer->curr - result.source.data;
    lexer->curr_token = result;
    return result.kind;
}

void
unlex(Lexer* lexer) {
    lexer->unlex_token = lexer->curr_token;
}

bool
lex_if_matched(Lexer* lexer, Token_Kind kind) {
    if (lex(lexer) == kind) {
        return true;
    }
    
    unlex(lexer);
    return false;
}
