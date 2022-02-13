
struct Preprocessor_Macro {
    u64 integral;
    string source;
    array(string_id)* args;
    b32 is_integral;
    b32 is_functional;
    b32 is_variadic;
    b32 is_valid;
};

struct Preprocessor_Line {
    string substring;
    u32 curr_line_number;
    u32 next_line_number;
};

struct Preprocessor {
    Tokenizer* t;
    map(string_id, Preprocessor_Macro)* macros;
    smm curr_line_number;
};
//#define Macro_Table map(string_id, Preprocessor_Macro)

internal void
preprocessor_error(string message) {
    pln("error: %", f_string(message));
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
                    while (is_token_valid(token) && token.type != Token_Close_Paren) {
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
                
                //macro.source = string_builder_to_string(&sb);
                //string_builder_free(&sb);
                
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

Preprocessor_Line
preprocess_splice_line(u8* curr, u32 curr_line_number, u8* end) {
    Preprocessor_Line result = {};
    result.curr_line_number = curr_line_number;
    
    u32 next_line_number = curr_line_number + 1;
    u8* begin = curr;
    while (curr < end && curr_line_number < next_line_number) {
        Utf8_To_Utf32_Result character = utf8_convert_to_utf32(curr, end);
        if (character.num_bytes == 0) {
            preprocessor_error(string_lit("invalid utf-8 formatting detected"));
            break;
        }
        
        u8 c = *curr;
        if (c == '\\') {
            next_line_number++;
        } else if (c == '\n') {
            curr_line_number++;
        }
        
        curr += character.num_bytes;
    }
    
    end = curr;
    result.substring = string_view(begin, end);
    result.next_line_number = next_line_number;
    
    return result;
}

string
preprocess_file(string source, string filepath) {
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    
    String_Builder sb = {};
    
    Preprocessor preprocessor = {};
    
    u32 curr_line_number = 0;
    u8* curr = source.data;
    u8* end = source.data + source.count;
    
    while (curr < end) {
        Preprocessor_Line line = preprocess_splice_line(curr, curr_line_number, end);
        curr += line.substring.count;
        curr_line_number = line.next_line_number;
        
        pln("Line: %\n%\n", f_uint(line.curr_line_number + 1), f_string(line.substring));
        
    }
    
    return string_builder_to_string_nocopy(&sb);
}
