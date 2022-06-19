
// TODO(Alexander): this preprocessor was not designed to handle CRLF or CR
//                  line endings, currently it only works with LF.

internal Replacement_List
preprocess_parse_actual_arguments(Preprocessor* preprocessor, Tokenizer* t) {
    Replacement_List result = {};
    result.success = true;
    
    // Parse formal arguments
    Token token = advance_semantical_token(t);
    if (token.type != Token_Open_Paren) {
        preprocess_error(preprocessor, string_format("function macro expected `(`, found `%`", f_token(token.type)));
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
        preprocess_error(preprocessor, string_lit("argument list ended without `)`"));
        result.success = false;
    }
    
    return result;
}

internal bool
preprocess_parse_and_eval_constant_expression(Preprocessor* preprocessor, Tokenizer* t) {
    u8* base = t->curr;
    
    String_Builder sb = {};
    string_builder_alloc(&sb, t->end - t->curr + 64);
    // TODO(Alexander): could really use some scratch memory to allocate the expanded string
    
    preprocess_expand_macro(preprocessor, &sb, t, {}, {});
    
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, string_builder_to_string_nocopy(&sb), string_lit("if"));
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    
    // TODO(Alexander): could really use some scratch memory to allocate the nodes
    Ast* expr = parse_expression(&parser);
    
    if (parser.error_count > 0) {
        pln("\nFailed while parsing source:\n`%`", f_string(tokenizer.source));
        
#if 1
        pln("Parsed AST for #if:\n");
        print_ast(expr, t);
#endif
        
        return false;
    } else {
        
        
    }
    
    // TODO(Alexander): interpreter needs to be able to access macro definitions
    Interp interp = {};
    Interp_Value result = interp_expression(&interp, expr);
    
    preprocessor->error_count += interp.error_count;
    //print_format("#if = ");
    //print_value(&result.value);
    
    bool no_eval_errors = parser.error_count == 0 && interp.error_count == 0;
    
    // TODO(Alexander): clear memory
    if (!is_integer(result.value) && no_eval_errors) {
        preprocess_error(preprocessor, string_lit("constant expression doesn't evaluate to integer value"));
    }
    
    if (!no_eval_errors) {
        pln("Expanded source line:\n`%`\n", f_string(string_view(base, t->end)));
    }
    
    string_builder_free(&sb);
    
    return value_to_bool(result.value);
}

internal void
preprocess_parse_define(Preprocessor* preprocessor, Tokenizer* t) {
    Token token = advance_token(t);
    if (token.type != Token_Whitespace) {
        return;
    }
    
    token = advance_token(t);
    if (token.type != Token_Ident) {
        preprocess_error(preprocessor, string_format("expected `identifier`, found `%`", f_token(token.type)));
        return;
    }
    
    string_id ident = vars_save_string(token.source);
    
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
                    preprocess_error(preprocessor, string_lit("expects variadic argument to always be the last argument"));
                    return;
                } else  {
                    preprocess_error(preprocessor, string_format("expected `)`, found `%`", f_token(token.type)));
                    return;
                }
            } else {
                preprocess_error(preprocessor, string_format("expected `identifier` or `...`, found `%`", f_token(token.type)));
                return;
            }
            token = advance_semantical_token(t);
            if (token.type == Token_Close_Paren) {
                break;
            } else if (token.type == Token_Comma) {
                token = advance_semantical_token(t);
                continue;
            } else {
                preprocess_error(preprocessor, string_format("expected `,` or `)`, found `%`", f_token(token.type)));
                return;
                
            }
        }
        
        token = advance_token(t);
    }
    
    if (token.type != Token_Whitespace) {
        preprocess_error(preprocessor, string_format("expected `whitespace`, found `%`", f_token(token.type)));
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
            preprocess_error(preprocessor, string_format("integer `%` literal is too large", f_string(token.source)));
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
    
    // Make sure we don't already have a macro with the same name
    // NOTE(Alexander): however for some reason you may want to define the exact same macro multiple times
    smm index = map_get_index(preprocessor->macros, ident);
    if (index >= 0) {
        
        Preprocessor_Macro* prev_macro = &preprocessor->macros[index].value;
        if (string_compare(macro.source, prev_macro->source) != 0) {
            
            preprocess_error(preprocessor, string_format("cannot redeclare macro with same identifier `%`", 
                                                         f_string(vars_load_string(ident))));
            return;
        }
    }
    
    
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

internal inline bool
check_if_curr_branch_is_taken(array(bool)* if_stack) {
    bool result = true;
    for_array_v (if_stack, it, _) {
        result = result && it;
    }
    return result;
}

internal void
preprocess_directive(Preprocessor* preprocessor, Tokenizer* t) {
    Token token = advance_semantical_token(t);
    
    if (token.type == Token_Ident) {
        string_id symbol = vars_save_string(token.source);
        
        switch (symbol) {
            case Sym_define: {
                if (preprocessor->curr_branch_taken) {
                    preprocess_parse_define(preprocessor, t);
                }
            } break;
            
            case Sym_undef: {
                if (preprocessor->curr_branch_taken) {
                    token = advance_semantical_token(t);
                    if (token.type == Token_Ident) {
                        string_id ident = vars_save_string(token.source);
                        map_remove(preprocessor->macros, ident);
                    } else {
                        preprocess_error(preprocessor, string_format("expected `identifier`, found `%`", f_token(token.type)));
                    }
                }
            } break;
            
            case Sym_pragma: {
                if (preprocessor->curr_branch_taken) {
                    token = advance_semantical_token(t);
                    if (token.type == Token_Ident) {
                        string_id symbol2 = vars_save_string(token.source);
                        
                        switch (symbol2) {
                            case Sym_once: {
                                if (preprocessor->curr_file_index > 0) {
                                    u32 index = preprocessor->curr_file_index;
                                    u32 num_includes = map_get(preprocessor->loaded_file_indices, index);
                                    if (num_includes != 1) {
                                        preprocessor->abort_curr_file = true;
                                    }
                                }
                                
                            } break;
                            
                            // TODO(Alexander): add more pragmas
                        }
                    }
                    
                }
            } break;
            
            case Sym_include: {
                if (preprocessor->curr_branch_taken) {
                    token = advance_semantical_token(t);
                    
                    Loaded_Source_File included_file = {};
                    if (token.type == Token_Lt) {
                        token = advance_semantical_token(t);
                        string filename = token.source;
                        filename.count = 0;
                        
                        while (token.type != Token_Gt) {
                            if (token.type == Token_EOF) {
                                return;
                            }
                            
                            filename.count += token.source.count;
                            token = advance_token(t);
                        }
                        
                        included_file = read_entire_system_header_file(filename);
                        
                    } else if (token.type == Token_String) {
                        string filename = string_unquote_nocopy(token.source);
                        included_file = read_entire_source_file(filename);
                    }
                    
                    if (included_file.is_valid) {
                        
                        preprocessor->curr_file_index = included_file.index;
                        u32 prev_num_includes = map_get(preprocessor->loaded_file_indices, included_file.index);
                        map_put(preprocessor->loaded_file_indices, included_file.index, prev_num_includes + 1);
                        preprocessor->abort_curr_file = false;
                        
                        preprocess_file(preprocessor, included_file.source, 
                                        included_file.filepath, included_file.index);
                        preprocessor->abort_curr_file = false; // if #pragma once hit then restore it
                    } else {
                        preprocessor->error_count++;
                    }
                }
                
            } break;
            
            case Kw_if: {
                bool value = preprocess_parse_and_eval_constant_expression(preprocessor, t);
                array_push(preprocessor->if_result_stack, value);
                
                preprocessor->curr_branch_taken =
                    check_if_curr_branch_is_taken(preprocessor->if_result_stack);
            } break;
            
            case Sym_elif: {
                if (array_count(preprocessor->if_result_stack) > 0) {
                    bool last_result = array_last(preprocessor->if_result_stack);
                    if (last_result) {
                        preprocessor->curr_branch_taken = false;
                    } else {
                        
                        bool value = preprocess_parse_and_eval_constant_expression(preprocessor, t);
                        array_last(preprocessor->if_result_stack) = value;
                        
                        preprocessor->curr_branch_taken =
                            check_if_curr_branch_is_taken(preprocessor->if_result_stack);
                    }
                } else {
                    preprocess_error(preprocessor, string_lit("#else directive outside #if scope"));
                }
            } break;
            
            case Kw_else: {
                if (array_count(preprocessor->if_result_stack) > 0) {
                    bool last_result = array_last(preprocessor->if_result_stack);
                    if (last_result) {
                        preprocessor->curr_branch_taken = false;
                    } else {
                        array_last(preprocessor->if_result_stack) = !last_result;
                        preprocessor->curr_branch_taken =
                            check_if_curr_branch_is_taken(preprocessor->if_result_stack);
                    }
                } else {
                    preprocess_error(preprocessor, string_lit("#else directive outside #if scope"));
                }
                // TODO(Alexander): detect two else in row
            } break;
            
            case Sym_endif: {
                if (array_count(preprocessor->if_result_stack) > 0) {
                    array_pop(preprocessor->if_result_stack);
                    preprocessor->curr_branch_taken =
                        check_if_curr_branch_is_taken(preprocessor->if_result_stack);
                } else {
                    preprocess_error(preprocessor, string_lit("#endif directive outside #if scope"));
                }
            } break;
            
            case Sym_ifdef:
            case Sym_ifndef: {
                token = advance_semantical_token(t);
                if (token.type == Token_Ident) {
                    string_id ident = vars_save_string(token.source);
                    Preprocessor_Macro macro = map_get(preprocessor->macros, ident);
                    
                    bool value = macro.is_valid;
                    if (symbol == Sym_ifndef) {
                        value = !value;
                    }
                    array_push(preprocessor->if_result_stack, value);
                    preprocessor->curr_branch_taken =
                        check_if_curr_branch_is_taken(preprocessor->if_result_stack);
                    
                } else {
                    preprocess_error(preprocessor, string_format("expected `identifier`, found `%`", 
                                                                 f_token(token.type)));
                    
                }
            } break;
            
            case Kw_error: {
                if (preprocessor->curr_branch_taken) {
                    preprocess_error(preprocessor, string_view(t->curr, t->end));
                }
            } break;
        }
    }
}

internal Preprocessor_Line
preprocess_splice_line(Preprocessor* preprocessor, u8* curr, u32 curr_line_number, u8* end) {
    // TODO(Alexander): this can be improved by using the already implemented tokenizer
    // the only problem right now with the tokenizer is that it doesn't care about new
    // lines it will simply skip them and we cannot tell easily where new line started/ ended.
    
    Preprocessor_Line result = {};
    result.curr_line_number = curr_line_number;
    
    u32 next_line_number = curr_line_number + 1;
    u8* begin = curr;
    
    int block_comment_depth = 0;
    
    while (curr < end && curr_line_number < next_line_number) {
        Utf8_To_Utf32_Result character = utf8_convert_to_utf32(curr, end);
        if (character.num_bytes == 0) {
            preprocess_error(preprocessor, string_lit("invalid utf-8 formatting detected"));
            break;
        }
        
        // TODO(Alexander): make sure this works with UTF-8 strings
        if (character.num_bytes == 1) {
            u8 c = *curr;
            u8 cn = (curr + 1) < end ? *(curr + 1) : 0;
            
            if (c == '/' && cn == '*') {
                block_comment_depth++;
                curr++;
                
            } else if (c == '*' && cn == '/') {
                if (block_comment_depth > 0) {
                    block_comment_depth--;
                }
                curr++;
                
            } else if (c == '\\') {
                next_line_number++;
                
            } else if (c == '\r') {
                curr_line_number++;
                if (block_comment_depth > 0) {
                    next_line_number = curr_line_number + 1;;
                }
                
                if (cn == '\n') {
                    curr++; // TODO(Alexander): is this okay?
                }
            } else if (c == '\n') {
                curr_line_number++;
                if (block_comment_depth > 0) {
                    next_line_number = curr_line_number + 1;;
                }
                
                if (cn == '\r') {
                    curr++; // TODO(Alexander): is this okay?
                }
            }
        }
        
        curr += character.num_bytes;
    }
    
    end = curr;
    result.substring = string_view(begin, end);
    result.next_line_number = next_line_number;
    
    //pln("Line %: `%`", f_u32(result.curr_line_number), f_string(result.substring));
    
    return result;
}

internal inline bool
preprocess_try_expand_ident(Preprocessor* preprocessor, 
                            String_Builder* sb, 
                            Tokenizer* t, 
                            Preprocessor_Macro parent_macro,
                            Replacement_List args, 
                            string_id ident) {
    
    if (ident == Sym_defined) {
        
        string_id macro_ident = Kw_invalid;
        
        // Parse macro identifier e.g. defined(MACRO) or defined MACRO
        Token token = advance_semantical_token(t);
        if (token.type == Token_Open_Paren) {
            token = advance_semantical_token(t);
            
            if (token.type == Token_Ident) {
                macro_ident = vars_save_string(token.source);
                
                token = advance_semantical_token(t);
                if (token.type != Token_Close_Paren) {
                    preprocess_error(preprocessor, string_format("expected `)`, found `%`", f_token(token.type)));
                    return false;
                }
            }
        } else if (token.type == Token_Ident) {
            macro_ident = vars_save_string(token.source);
        }
        
        if (macro_ident != Kw_invalid) {
            Preprocessor_Macro macro = map_get(preprocessor->macros, macro_ident);
            string_builder_push(sb, macro.is_valid ? "1" : "0");
            return true;
        } else {
            preprocess_error(preprocessor, string_lit("built in function-like macro `defined` expects an identifier as argument"));
            return false;
        }
        
    } else if (ident == Sym___VA_ARGS__) {
        
        umm formal_arg_count = map_count(parent_macro.arg_mapper);
        umm actual_arg_count = array_count(args.list);
        
        for (umm arg_index = formal_arg_count; 
             arg_index < actual_arg_count; 
             arg_index++) {
            
            string source = args.list[arg_index];
            
            Tokenizer tokenizer = {};
            tokenizer_set_source(&tokenizer, source, string_lit("args"));
            preprocess_expand_macro(preprocessor, sb, &tokenizer, parent_macro, {});
            
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
            macro_args = preprocess_parse_actual_arguments(preprocessor, t);
            if (!macro_args.success) {
                return false;
            }
        }
        
        umm actual_arg_count = array_count(macro_args.list);
        umm formal_arg_count = map_count(macro.arg_mapper);
        if ((actual_arg_count < formal_arg_count) ||
            (actual_arg_count > formal_arg_count && !macro.is_variadic)) {
            
            preprocess_error(preprocessor, string_format("function-like macro expected % arguments, found % arguments",
                                                         f_int(formal_arg_count), f_int(actual_arg_count)));
            return false;
        }
        
        umm first_used = sb->curr_used;
        
        // TODO(Alexander): record line/ col of this
        Tokenizer tokenizer = {};
        tokenizer_set_source(&tokenizer, macro.source, string_lit("macro"));
        //Tokenizer_State state = save_tokenizer(t);
        //tokenizer_set_substring(t, macro.source, 0, 0);
        preprocess_expand_macro(preprocessor, sb, &tokenizer, macro, macro_args);
        //restore_tokenizer(&state);
        
        string expanded_source = string_view(sb->data + first_used, sb->data + sb->curr_used);
        pln("Expanding macro `%` to:\n`%`", f_string(vars_load_string(ident)), f_string(expanded_source));
        return true;
    }
    return false;
}

void
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
                        preprocess_error(preprocessor, string_format("function-like macro expected % arguments, found % arguments",
                                                                     f_int(map_count(macro.arg_mapper)),
                                                                     f_int(array_count(args.list))));
                        return;
                    }
                    string arg_source = args.list[arg_index];
                    Tokenizer_State t_restore = save_tokenizer(t);
                    
                    tokenizer_set_source(t, arg_source, t->file);
                    //tokenizer_set_substring();
                    preprocess_expand_macro(preprocessor, sb, t, macro, args);
                    restore_tokenizer(&t_restore);
                } else {
                    
                    if (!preprocess_try_expand_ident(preprocessor, sb, t, macro, args, ident)) {
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

internal bool
preprocess_line(Preprocessor* preprocessor, String_Builder* sb, Tokenizer* t) {
    Token token = advance_token(t);
    if (token.type == Token_Whitespace) {
        token = advance_token(t);
    }
    
    if (token.type == Token_Directive) {
        preprocess_directive(preprocessor, t);
        return false;
    }
    
    if (preprocessor->curr_branch_taken) {
        
        while (is_token_valid(token)) {
            switch (token.type) {
                case Token_Ident: {
                    string_id ident = vars_save_string(token.source);
                    
                    if (!preprocess_try_expand_ident(preprocessor, sb, t, {}, {}, ident)) {
                        string_builder_push(sb, token.source);
                    }
                } break;
                
                case Token_Directive: {
                    preprocess_error(preprocessor, string_lit("preprocessor directive (#) must start at the beginning of a line"));
                } break;
                
                case Token_Concatenator: {
                    preprocess_error(preprocessor, string_lit("preprocessor concatenator (##) can only be used inside macro"));
                } break;
                
                case Token_Backslash: break;
                
                default: {
                    string_builder_push(sb, token.source);
                } break;
            }
            
            token = advance_token(t);
        }
    } else {
        return false;
    }
    
    return true;
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
    string left_str = string_unquote_nocopy(left.source);
    string right_str = string_unquote_nocopy(right.source);
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
preprocess_file(Preprocessor* preprocessor, string source, string filepath, int file_index) {
    Tokenizer* prev_tokenizer = preprocessor->tokenizer;
    
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    preprocessor->tokenizer = &tokenizer;
    
    u32 curr_line_number = 0;
    u8* curr = source.data;
    u8* end = source.data + source.count;
    
    String_Builder* sb = &preprocessor->output;
    string_builder_alloc(sb, source.count);
    
    //if (array_count(preprocessor->if_result_stack) == 0) {
    preprocessor->curr_branch_taken = true;
    //}
    
    //array_push(preprocessor->if_result_stack, preprocessor->curr_branch_taken);
    //preprocessor->curr_branch_taken = true;
    
    Source_Group current_group = {};
    current_group.file_index = file_index;
    
    while (curr < end) {
        u8* curr_line = curr;
        
        Preprocessor_Line line = preprocess_splice_line(preprocessor, curr, curr_line_number, end);
        curr += line.substring.count;
        curr_line_number = line.next_line_number;
        
        tokenizer_set_substring(&tokenizer, line.substring, line.curr_line_number, 0);
        
        umm begin_used = sb->curr_used;
        bool run_finalize = preprocess_line(preprocessor, sb, &tokenizer);
        umm end_used = sb->curr_used;
        
        if (preprocessor->error_count > 50) {
            if (preprocessor->error_count < 1000) { // hack to only print once, while it unrolls the recursion
                pln("preprocessor reported more than 50 errors, exiting...");
            }
            preprocessor->error_count = 1000;
            return {};
        }
        
        if (preprocessor->abort_curr_file) {
            break;
        }
        
        if (run_finalize) {
            sb->curr_used = begin_used;
            string expanded_code = create_string(end_used - begin_used, sb->data + begin_used);
            string finalized_string = preprocess_finalize_code(expanded_code);
            string_builder_push(sb, finalized_string);
            
            if (current_group.count == 0) {
                current_group.offset = begin_used;
            }
            current_group.count += (umm) finalized_string.count;
            
            string_free(finalized_string);
        } else {
            // If we skip a line we will start a new source group
            if (current_group.count > 0) {
                array_push(preprocessor->source_groups, current_group);
                current_group = {};
                current_group.offset = sb->curr_used;
                current_group.file_index = file_index;
            }
            
            current_group.line = line.next_line_number;
        }
    }
    
    if (current_group.count > 0) {
        array_push(preprocessor->source_groups, current_group);
    }
    
    preprocessor->tokenizer = prev_tokenizer; // restore previous tokenizer
    //preprocessor->curr_branch_taken = array_pop(preprocessor->if_result_stack);
    
    return string_builder_to_string_nocopy(sb);
}
