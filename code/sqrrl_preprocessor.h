
struct Preprocessor_Macro {
    u64 integral;
    string source;
    map(string_id, u32)* arg_mapper;
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

struct Replacement_List {
    array(string)* list;
    b32 success;
};

struct Preprocessor {
    Tokenizer* t;
    map(string_id, Preprocessor_Macro)* macros;
    smm curr_line_number;
};

internal void
preprocess_error(string message) {
    pln("preprocessor error: %", f_string(message));
    DEBUG_log_backtrace();
}

internal Replacement_List
preprocess_parse_actual_arguments(Tokenizer* t) {
    Replacement_List result = {};
    result.success = true;
    
    // Parse formal arguments
    Token token = advance_token(t);
    if (token.type != Token_Open_Paren) {
        preprocess_error(string_format("function macro expected `(`, found `%`", f_token(token.type)));
        return result;
    }
    
    token = advance_token(t);
    while (is_token_valid(token) && token.type != Token_Close_Paren) {
        u8* begin = token.source.data;
        
        while (is_token_valid(token) && token.type != Token_Comma && token.type != Token_Close_Paren) {
            token = advance_token(t);
        }
        
        u8* end = token.source.data;
        string replacement = string_view(begin, end);
        array_push(result.list, replacement);
        
        
        if (!is_token_valid(token) || token.type == Token_Close_Paren) {
            break;
        }
        token = advance_token(t);
    }
    
    if (token.type != Token_Close_Paren) {
        preprocess_error(string_lit("argument list ended without `)`"));
        result.success = false;
    }
    
    return result;
}

internal void
preprocess_parse_define(Preprocessor* preprocessor, Tokenizer* t) {
    Token token = advance_token(t);
    if (token.type != Token_Whitespace) {
        return;
    }
    
    token = advance_token(t);
    if (token.type != Token_Ident) {
        preprocess_error(string_format("expected `identifier`, found `%`", f_token(token.type)));
        return;
    }
    
    string_id ident = vars_save_string(token.source);
    
    // Make sure we don't already have a macro with the same name
    if (map_key_exists(preprocessor->macros, ident)) {
        preprocess_error(string_format("cannot redeclare macro with same identifier `%`", 
                                       f_string(vars_load_string(ident))));
        return;
    }
    
    Preprocessor_Macro macro = {};
    
    token = advance_token(t);
    if (token.type == Token_Open_Paren) {
        // Function macro
        macro.is_functional = true;
        
        // Parse formal arguments
        int arg_index = 0;
        token = next_semantical_token(t);
        while (is_token_valid(token) && token.type != Token_Close_Paren) {
            if (token.type == Token_Ident) {
                string_id arg_ident = vars_save_string(token.source);
                map_put(macro.arg_mapper, arg_ident, arg_index);
                arg_index++;
            } else if (token.type == Token_Ellipsis) {
                macro.is_variadic = true;
                
                token = next_semantical_token(t);
                if (token.type == Token_Close_Paren) {
                    break;
                } else if (token.type == Token_Comma) {
                    preprocess_error(string_lit("expects variadic argument to always be the last argument"));
                    return;
                } else  {
                    preprocess_error(string_format("expected `)`, found `%`", f_token(token.type)));
                    return;
                }
            } else {
                preprocess_error(string_format("expected `identifier` or `...`, found `%`", f_token(token.type)));
                return;
            }
            token = next_semantical_token(t);
            if (token.type == Token_Close_Paren) {
                break;
            } else if (token.type == Token_Comma) {
                token = next_semantical_token(t);
                continue;
            } else {
                preprocess_error(string_format("expected `,` or `)`, found `%`", f_token(token.type)));
                return;
                
            }
        }
        
        token = advance_token(t);
    }
    
    if (token.type != Token_Whitespace) {
        preprocess_error(string_format("expected `whitespace`, found `%`", f_token(token.type)));
        return;
    }
    
    // This is where the token source will start
    u8* macro_begin = t->curr;
    umm curr_line = token.line;
    
    // Parse integral
    token = advance_token(t);
    if (token.type == Token_Int) {
        Parse_U64_Value_Result parsed_result = parse_u64_value(token);
        if (parsed_result.is_too_large) {
            preprocess_error(string_format("integer `%` literal is too large", f_string(token.source)));
        }
        macro.integral = parsed_result.value;
        macro.is_integral = true;
    }
    
    // Extract the source code for the macro
    macro.source = string_view(macro_begin, t->end);
    
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
}

internal void
preprocess_directive(Preprocessor* preprocessor, Tokenizer* t) {
    Token token = advance_token(t);
    
    if (token.type == Token_Ident) {
        string_id keyword = vars_save_string(token.source);
        
        switch (keyword) {
            case Kw_define: {
                preprocess_parse_define(preprocessor, t);
            } break;
            
            case Kw_undef: {
                token = next_semantical_token(t);
                if (token.type == Token_Ident) {
                    string_id ident = vars_save_string(token.source);
                    map_remove(preprocessor->macros, ident);
                } else {
                    preprocess_error(string_format("expected `identifier`, found `%`", f_token(token.type)));
                }
                
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

internal Preprocessor_Line
preprocess_splice_line(u8* curr, u32 curr_line_number, u8* end) {
    Preprocessor_Line result = {};
    result.curr_line_number = curr_line_number;
    
    u32 next_line_number = curr_line_number + 1;
    u8* begin = curr;
    while (curr < end && curr_line_number < next_line_number) {
        Utf8_To_Utf32_Result character = utf8_convert_to_utf32(curr, end);
        if (character.num_bytes == 0) {
            preprocess_error(string_lit("invalid utf-8 formatting detected"));
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

internal void
preprocess_expand_macro(Preprocessor* preprocessor, String_Builder* sb, Tokenizer* t, Preprocessor_Macro macro, Replacement_List args) {
    
    Token token = advance_token(t);
    while (is_token_valid(token)) {
        switch (token.type) {
            case Token_Ident: {
                string_id ident = vars_save_string(token.source);
                
                if (macro.arg_mapper && map_key_exists(macro.arg_mapper, ident)) {
                    // Expand argument
                    int arg_index = map_get(macro.arg_mapper, ident);
                    if (array_count(args.list) < arg_index) {
                        preprocess_error(string_format("functional macro expected % arguments, found % arguments", 
                                                       f_int(map_count(macro.arg_mapper)),
                                                       f_int(array_count(args.list))));
                        return;
                    }
                    string arg_source = args.list[arg_index];
                    string_builder_push(sb, arg_source);
                } else {
                    // Expand macro inside this macro
                    Preprocessor_Macro inner_macro = map_get(preprocessor->macros, ident);
                    if (inner_macro.is_valid) {
                        Replacement_List inner_macro_args = {};
                        if (inner_macro.is_functional) {
                            inner_macro_args = preprocess_parse_actual_arguments(t);
                        }
                        
                        // TODO(Alexander): record line/ col of this
                        Tokenizer_State state = save_tokenizer(t);
                        tokenizer_set_substring(t, macro.source, 0, 0);
                        preprocess_expand_macro(preprocessor, sb, t, inner_macro, inner_macro_args);
                        restore_tokenizer(&state);
                    } else {
                        string_builder_push(sb, token.source);
                    }
                }
            } break;
            
            case Token_Backslash: break;
            
            default: {
                string_builder_push(sb, token.source);
            } break;
        }
        
        token = advance_token(t);
    }
}

internal void
preprocess_line(Preprocessor* preprocessor, String_Builder* sb, Tokenizer* t) {
    Token token = advance_token(t);
    if (token.type == Token_Directive) {
        preprocess_directive(preprocessor, t);
        return;
    }
    
    
    while (is_token_valid(token)) {
        switch (token.type) {
            case Token_Ident: {
                string_id ident = vars_save_string(token.source);
                
                // NOTE(Alexander): copypasta from above function
                Preprocessor_Macro macro = map_get(preprocessor->macros, ident);
                if (macro.is_valid) {
                    Replacement_List macro_args = {};
                    if (macro.is_functional) {
                        macro_args = preprocess_parse_actual_arguments(t);
                    }
                    
                    // TODO(Alexander): record line/ col of this
                    Tokenizer_State state = save_tokenizer(t);
                    tokenizer_set_substring(t, macro.source, 0, 0);
                    preprocess_expand_macro(preprocessor, sb, t, macro, macro_args);
                    restore_tokenizer(&state);
                } else {
                    string_builder_push(sb, token.source);
                }
            } break;
            
            case Token_Directive: {
                preprocess_error(string_lit("preprocessor directive (#) must start at the beginning of a line"));
            } break;
            
            case Token_Concatenator: {
                preprocess_error(string_lit("preprocessor concatenator (##) can only be used inside macro"));
            } break;
            
            case Token_Backslash: break;
            
            default: {
                string_builder_push(sb, token.source);
            } break;
        }
        
        token = advance_token(t);
    }
    
}
#if 0
case Token_Directive: {
    stringize_token = true;
    token = advance_token(t);
    prev_token = token;
    
    // NOTE(Alexander): copypasta from bottom of func
    if (!is_token_valid(token) && tokenizer_states) {
        // Try to restore previous state
        Tokenizer_State state = array_pop(tokenizer_states);
        if (state.tokenizer) {
            restore_tokenizer(&state);
            token = advance_token(t);
        }
    }
    continue;
} break;

case Token_Concatenator: {
    concatenate_token = true;
    token = advance_token(t);
    
    // NOTE(Alexander): copypasta from bottom of func
    if (!is_token_valid(token) && tokenizer_states) {
        // Try to restore previous state
        Tokenizer_State state = array_pop(tokenizer_states);
        if (state.tokenizer) {
            restore_tokenizer(&state);
            token = advance_token(t);
        }
    }
    continue;
} break;

case Token_Backslash: break;

default: {
    string_builder_push(sb, token.source);
} break;
}

stringize_token = false;
concatenate_token = false;
prev_token = token;
token = advance_token(t);

if (!is_token_valid(token) && tokenizer_states) {
    // Try to restore previous state
    Tokenizer_State state = array_pop(tokenizer_states);
    if (state.tokenizer) {
        restore_tokenizer(&state);
        token = advance_token(t);
    }
}
}
}
#endif

string
preprocess_file(string source, string filepath) {
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    
    String_Builder sb = {};
    
    Preprocessor preprocessor = {};
    
    u32 curr_line_number = 0;
    u8* curr = source.data;
    u8* end = source.data + source.count;
    
    while (curr < end) {
        Preprocessor_Line line = preprocess_splice_line(curr, curr_line_number, end);
        curr += line.substring.count;
        curr_line_number = line.next_line_number;
        
        tokenizer_set_substring(&tokenizer, line.substring, line.curr_line_number, 0);
        
        preprocess_line(&preprocessor, &sb, &tokenizer);
    }
    
    return string_builder_to_string_nocopy(&sb);
}
