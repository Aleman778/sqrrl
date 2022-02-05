
struct Preprocessor_Macro {
    u64 integral;
    string source;
    array(string_id)* args;
    b32 is_integral;
    b32 is_functional;
    b32 is_variadic;
    b32 is_valid;
};

struct Preprocessor {
    map(string_id, Preprocessor_Macro)* macros;
};
//#define Macro_Table map(string_id, Preprocessor_Macro)

internal void
preprocessor_error(string message) {
    pln("error: %", f_string(message));
}

internal inline bool
is_valid_token(Token_Type type) {
    return type != Token_EOF && type != Token_Error;
}

internal void
preprocess_directive(Preprocessor* preprocessor, Tokenizer* t) {
    Token token = advance_token(t);
    
    if (token.type == Token_Ident) {
        string_id keyword = vars_save_string(token.source);
        
        switch (keyword) {
            case Kw_define: {
                token = advance_token(t);
                if (token.type != Token_Whitespace) {
                    return;
                }
                
                token = advance_token(t);
                if (token.type != Token_Ident) {
                    preprocessor_error(string_format("expected `identifier`, found `%`", f_token(token.type)));
                    return;
                }
                
                string_id ident = vars_save_string(token.source);
                
                // Make sure we don't already have a macro with the same name
                if (map_key_exists(preprocessor->macros, ident)) {
                    preprocessor_error(string_format("cannot redeclare macro with same identifier `%`", 
                                                     f_string(vars_load_string(ident))));
                    return;
                }
                
                Preprocessor_Macro macro = {};
                
                token = advance_token(t);
                if (token.type == Token_Open_Paren) {
                    // Function macro
                    macro.is_functional = true;
                    
                    // Parse formal arguments
                    token = advance_token(t);
                    while (is_valid_token(token.type) && token.type != Token_Close_Paren) {
                        if (token.type == Token_Ident) {
                            string_id arg_ident = vars_save_string(token.source);
                            array_push(macro.args, arg_ident);
                        } else if (token.type == Token_Ellipsis) {
                            macro.is_variadic = true;
                            
                            token = advance_token(t);
                            if (token.type == Token_Close_Paren) {
                                break;
                            } else if (token.type == Token_Comma) {
                                preprocessor_error(string_lit("expects variadic argument to always be the last argument"));
                                return;
                            } else  {
                                preprocessor_error(string_format("expected `)`, found `%`", f_token(token.type)));
                                return;
                            }
                        } else {
                            preprocessor_error(string_format("expected `identifier` or `...`, found `%`", f_token(token.type)));
                            return;
                        }
                        token = advance_token(t);
                        if (token.type == Token_Comma || token.type == Token_Close_Paren) {
                            preprocessor_error(string_format("expected `,` or `)`, found `%`", f_token(token.type)));
                            return;
                        }
                        token = advance_token(t);
                    }
                    
                    token = advance_token(t);
                }
                
                if (token.type != Token_Whitespace) {
                    preprocessor_error(string_format("expected `whitespace`, found `%`", f_token(token.type)));
                    return;
                }
                
                // This is where the token source will start
                umm curr_line = token.line;
                
                // Parse integral
                token = advance_token(t);
                if (token.type == Token_Int) {
                    Parse_U64_Value_Result parsed_result = parse_u64_value(token);
                    if (parsed_result.is_too_large) {
                        preprocessor_error(string_format("integer `%` literal is too large", f_string(token.source)));
                    }
                    macro.integral = parsed_result.value;
                    macro.is_integral = true;
                }
                
                // Parse source
                String_Builder sb = {};
                while (is_valid_token(token.type) && token.line <= curr_line) {
                    if (token.type == Token_Backslash) {
                        curr_line++;
                        string_builder_push(&sb, "\n");
                    }
                    
                    if (token.type == Token_Line_Comment || 
                        token.type == Token_Block_Comment) {
                        break;
                    }
                    
                    if (token.line == curr_line) {
                        if (token.type == Token_Whitespace) {
                            // Remove everything after newline if detected
                            int char_index;
                            for (char_index = 0; 
                                 char_index < token.source.count;
                                 char_index++) {
                                
                                // TODO(Alexander): not perfect since utf8 chars can
                                // mess up the ASCII bytes, this is however unlikely
                                if (token.source.data[char_index] == '\n') {
                                    break;
                                }
                            }
                            
                            string view = create_string((umm) char_index, token.source.data);
                            break;
                        } else {
                            string_builder_push(&sb, token.source);
                        }
                    }
                    
                    token = advance_token(t);
                }
                macro.source = string_builder_to_string(&sb);
                string_builder_free(&sb);
                
                // Store the macro
                macro.is_valid = true;
                map_put(preprocessor->macros, ident, macro);
                
#if BUILD_DEBUG
                // Debugging code
                string ident_string = vars_load_string(ident);
                if (macro.is_integral) {
                    pln("#define % % (%)", f_string(ident_string), f_string(macro.source),
                        f_u64(macro.integral));
                } else {
                    pln("#define % %", f_string(ident_string), f_string(macro.source));
                }
#endif
                
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
preprocess_file(string source, string filepath) {
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    
    String_Builder sb = {};
    
    // Windows API macros (for debugging)
    Preprocessor_Macro UNICODE = {};
    Preprocessor_Macro WINVER = {};
    WINVER.integral = 0x0400;
    WINVER.is_integral = true;
    
    string_id UNICODE_ident = vars_save_cstring("UNICODE");
    string_id WINVER_ident = vars_save_cstring("WINVER");
    
    Preprocessor preprocessor = {};
    map_put(preprocessor.macros, UNICODE_ident, UNICODE);
    map_put(preprocessor.macros, WINVER_ident, WINVER);
    
    for (;;) {
        Token token = advance_token(&tokenizer);
        if (token.type == Token_EOF || token.type == Token_Error) {
            break;
        }
        
        if (token.type == Token_Directive) {
            preprocess_directive(&preprocessor, &tokenizer);
        } else if (token.type == Token_Ident) {
            string_id ident = vars_save_string(token.source);
            
            Preprocessor_Macro macro = map_get(preprocessor.macros, ident);
            
            if (macro.is_valid) {
                // TODO(Alexander): add functional support
                string_builder_push(&sb, macro.source);
            } else {
                string_builder_push(&sb, token.source);
            }
        } else {
            string_builder_push(&sb, token.source);
        }
    }
    
    return string_builder_to_string_nocopy(&sb);
}



