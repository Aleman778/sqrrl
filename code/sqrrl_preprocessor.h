
struct Preprocessor_Macro {
    union {
        u64 value;
        string source;
    };
    array(string_id)* args;
    b32 is_integral;
    b32 is_variadic;
};
typedef map(string_id, Preprocessor_Macro) Macro_Table;


//internal Token
//Token token = advance_token(t);

//}

internal void
preprocess_directive(Tokenizer* t, Macro_Table* macros, smm curr_line_number = 0) {
    Token token = advance_token(t);
    
    if (token.type == Token_Ident) {
        string_id keyword = vars_save_string(token.source);
        
        switch (keyword) {
            case Kw_define: {
                int count = 0;
                u8* scan = t->curr_line;
                while (*scan++ != '\n') count++;
                printf("%.*s\n", count, t->curr_line);
            } break;
            
            case Kw_if: {
                
            } break;
            
            case Kw_ifdef: {
                
            } break;
            
            
            case Kw_ifndef: {
                
            } break;
            
            case Kw_error: {
                
            } break;
        }
    }
}

string
preprocess_file(string source, string filepath, Macro_Table* macros = 0) {
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    
    String_Builder sb = {};
    
    // Windows API macros (for debugging)
    Preprocessor_Macro UNICODE = {};
    Preprocessor_Macro WINVER = {};
    WINVER.value = 0x0400;
    WINVER.is_integral = true;
    
    string_id UNICODE_ident = vars_save_cstring("UNICODE");
    string_id WINVER_ident = vars_save_cstring("WINVER");
    
    map_put(macros, UNICODE_ident, UNICODE);
    map_put(macros, WINVER_ident, WINVER);
    
    for (;;) {
        Token token = advance_token(&tokenizer);
        if (token.type == Token_EOF || token.type == Token_Error) {
            break;
        }
        
        if (token.type == Token_Directive) {
            preprocess_directive(&tokenizer, macros, token.line);
        }
    }
    
    return string_builder_to_string_nocopy(&sb);
}



