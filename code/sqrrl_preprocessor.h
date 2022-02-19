
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
    Token token = advance_semantical_token(t);
    if (token.type != Token_Open_Paren) {
        preprocess_error(string_format("function macro expected `(`, found `%`", f_token(token.type)));
        return result;
    }
    
    token = advance_semantical_token(t);
    while (is_token_valid(token) && token.type != Token_Close_Paren) {
        u8* begin = token.source.data;
        
        while (is_token_valid(token) && token.type != Token_Comma && token.type != Token_Close_Paren) {
            token = advance_semantical_token(t);
        }
        
        u8* end = token.source.data;
        string replacement = string_view(begin, end);
        array_push(result.list, replacement);
        
        if (!is_token_valid(token) || token.type == Token_Close_Paren) {
            break;
        }
        token = advance_semantical_token(t);
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
        token = advance_semantical_token(t);
        while (is_token_valid(token) && token.type != Token_Close_Paren) {
            if (token.type == Token_Ident) {
                string_id arg_ident = vars_save_string(token.source);
                map_put(macro.arg_mapper, arg_ident, arg_index);
                arg_index++;
            } else if (token.type == Token_Ellipsis) {
                macro.is_variadic = true;
                
                token = advance_semantical_token(t);
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
            token = advance_semantical_token(t);
            if (token.type == Token_Close_Paren) {
                break;
            } else if (token.type == Token_Comma) {
                token = advance_semantical_token(t);
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
    token = advance_semantical_token(t);
    if (token.type == Token_Int) {
        Parse_U64_Value_Result parsed_result = parse_u64_value(token);
        if (parsed_result.is_too_large) {
            preprocess_error(string_format("integer `%` literal is too large", f_string(token.source)));
        }
        macro.integral = parsed_result.value;
        macro.is_integral = true;
    }
    
    // Extract the source code for the macro
    u8* macro_end = t->end;
    if (macro_begin != macro_end) {
        macro_end--; // remove new line if possible
    }
    
    macro.source = string_view(macro_begin, macro_end);
    
    // Store the macro
    macro.is_valid = true;
    map_put(preprocessor->macros, ident, macro);
    
#if BUILD_DEBUG && 1
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
                token = advance_semantical_token(t);
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

internal void preprocess_expand_macro(Preprocessor* preprocessor, 
                                      String_Builder* sb, 
                                      Tokenizer* t, 
                                      Preprocessor_Macro macro, 
                                      Replacement_List args);

internal inline bool
perprocess_try_expand_ident(Preprocessor* preprocessor, 
                            String_Builder* sb, 
                            Tokenizer* t, 
                            Preprocessor_Macro parent_macro,
                            Replacement_List args, 
                            string_id ident) {
    
    if (ident == Kw___VA_ARGS__) {
        umm formal_arg_count = map_count(parent_macro.arg_mapper);
        umm actual_arg_count = array_count(args.list);
        
        for (umm arg_index = formal_arg_count; 
             arg_index < actual_arg_count; 
             arg_index++) {
            
            string source = args.list[arg_index];
            Tokenizer_State state = save_tokenizer(t);
            tokenizer_set_substring(t, source, 0, 0);
            preprocess_expand_macro(preprocessor, sb, t, parent_macro, {});
            restore_tokenizer(&state);
            
            if (arg_index + 1 < actual_arg_count) {
                string_builder_push(sb, ",");
            }
        }
        
        return true;
    }
    
    Preprocessor_Macro macro = map_get(preprocessor->macros, ident);
    if (macro.is_valid) {
        Replacement_List macro_args = {};
        if (macro.is_functional) {
            macro_args = preprocess_parse_actual_arguments(t);
            if (!macro_args.success) {
                return false;
            }
        }
        
        umm actual_arg_count = array_count(macro_args.list);
        umm formal_arg_count = map_count(macro.arg_mapper);
        if ((actual_arg_count < formal_arg_count) ||
            (actual_arg_count > formal_arg_count && !macro.is_variadic)) {
            
            preprocess_error(string_format("function-like macro expected % arguments, found % arguments",
                                           f_int(formal_arg_count), f_int(actual_arg_count)));
            return false;
        }
        
        // TODO(Alexander): record line/ col of this
        Tokenizer_State state = save_tokenizer(t);
        tokenizer_set_substring(t, macro.source, 0, 0);
        preprocess_expand_macro(preprocessor, sb, t, macro, macro_args);
        restore_tokenizer(&state);
        return true;
    }
    return false;
}

internal void
preprocess_expand_macro(Preprocessor* preprocessor, 
                        String_Builder* sb, 
                        Tokenizer* t, 
                        Preprocessor_Macro macro, 
                        Replacement_List args) {
    
    Token token = advance_token(t);
    while (is_token_valid(token)) {
        switch (token.type) {
            case Token_Ident: {
                string_id ident = vars_save_string(token.source);
                
                if (macro.arg_mapper && map_key_exists(macro.arg_mapper, ident)) {
                    int arg_index = map_get(macro.arg_mapper, ident);
                    if (array_count(args.list) < arg_index) {
                        preprocess_error(string_format("function-like macro expected % arguments, found % arguments",
                                                       f_int(map_count(macro.arg_mapper)),
                                                       f_int(array_count(args.list))));
                        return;
                    }
                    string arg_source = args.list[arg_index];
                    string_builder_push(sb, arg_source);
                } else {
                    
                    if (!perprocess_try_expand_ident(preprocessor, sb, t, macro, args, ident)) {
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
                
                if (!perprocess_try_expand_ident(preprocessor, sb, t, {}, {}, ident)) {
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

string
unquote_string_nocopy(string s) {
    string result = s;
    
    if (s.data[0] == '"') {
        result.data++;
        result.count--;
    }
    
    if (s.data[s.count - 1] == '"') {
        result.count--;
    }
    
    return result;
}

internal Token
preprocess_next_semantical_token(Tokenizer* t, String_Builder* sb, Token token, bool consume_next_token=true) {
    
    Token next_token = advance_token(t);
    
    // Skip to next semantical token (keep track of this string)
    string non_semantical_text;
    non_semantical_text.data = next_token.source.data;
    non_semantical_text.count = 0;
    while (!is_semantical_token(next_token) && is_token_valid(next_token)) {
        non_semantical_text.count += next_token.source.count;
        next_token = advance_token(t);
    }
    
    if (consume_next_token && next_token.type != Token_Concatenator) {
        string_builder_push(sb, token.source);
    }
    string_builder_push(sb, non_semantical_text);
    
    return next_token;
}

internal void
preprocess_push_concatenated_strings(String_Builder* sb, Token left, Token right) {
    string left_str = unquote_string_nocopy(left.source);
    string right_str = unquote_string_nocopy(right.source);
    string_builder_push(sb, "\"");
    string_builder_push(sb, left_str);
    string_builder_push(sb, right_str);
    string_builder_push(sb, "\"");
}


string
preprocess_finalize_code(string source) {
    Tokenizer tokenizer = {};
    Tokenizer* t = &tokenizer;
    
    tokenizer_set_source(t, source, string_lit("macro"));
    
    String_Builder sb = {};
    
    Token prev_token = {};
    Token token = advance_token(t);
    
    while (is_token_valid(token)) {
        switch (token.type) {
            case Token_Directive: {
                
                prev_token = token;
                token = advance_semantical_token(t);
                string_builder_push_format(&sb, "\"%\"", f_string(token.source));
                
                token = advance_token(t);
                prev_token = {};
            } break;
            
            case Token_Concatenator: {
                Token next_token = advance_semantical_token(t);
                
                if (prev_token.type == Token_String || next_token.type == Token_String) {
                    preprocess_push_concatenated_strings(&sb, prev_token, next_token);
                } else {
                    string_builder_push(&sb, prev_token.source);
                    string_builder_push(&sb, next_token.source);
                }
                
                token = advance_token(t);
                prev_token = {};
            } break;
            
            case Token_String: {
                prev_token = token;
                token = preprocess_next_semantical_token(t, &sb, token, false);
                
                if (token.type == Token_String) {
                    preprocess_push_concatenated_strings(&sb, prev_token, token);
                    
                    token = advance_token(t);
                    prev_token = {};
                } else {
                    if (token.type != Token_Concatenator) {
                        string_builder_push(&sb, prev_token.source);
                    }
                }
            } break;
            
            case Token_Backslash: {
                prev_token = token;
                token = advance_token(t);
            } break;
            
            default: {
                prev_token = token;
                token = preprocess_next_semantical_token(t, &sb, token);
            } break;
        }
    }
    
    return string_builder_to_string_nocopy(&sb);
}

string
preprocess_file(string source, string filepath) {
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    
    String_Builder sb = {};
    string_builder_alloc(&sb, source.count);
    
    Preprocessor preprocessor = {};
    
    u32 curr_line_number = 0;
    u8* curr = source.data;
    u8* end = source.data + source.count;
    
    while (curr < end) {
        Preprocessor_Line line = preprocess_splice_line(curr, curr_line_number, end);
        curr += line.substring.count;
        curr_line_number = line.next_line_number;
        
        tokenizer_set_substring(&tokenizer, line.substring, line.curr_line_number, 0);
        
        umm line_curr_used = sb.curr_used;
        u8* line_begin = sb.data + sb.curr_used;
        preprocess_line(&preprocessor, &sb, &tokenizer);
        u8* line_end = sb.data + sb.curr_used;
        
        sb.curr_used = line_curr_used;
        
        string finalized_string = preprocess_finalize_code(string_view(line_begin, line_end));
        string_builder_push(&sb, finalized_string);
        free(finalized_string.data);
    }
    
    return string_builder_to_string_nocopy(&sb);
}
