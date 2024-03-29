
// TODO(Alexander): add custom string interner to avoid bloating the global one.

#if 0
internal Value
preprocess_parse_and_eval_constant_expression(Preprocessor* preprocessor, Tokenizer* t) {
    u8* base = t->curr;
    
    String_Builder sb = {};
    string_builder_alloc(&sb, t->end - t->curr + 64);
    // TODO(Alexander): could really use some scratch memory to allocate the expanded string
    
    preprocess_expand_macro(preprocessor, &sb, t, {}, {});
    
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, string_builder_to_string_nocopy(&sb), string_lit("if"), 0);
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    
    // TODO(Alexander): could really use some scratch memory to allocate the nodes
    Ast* expr = parse_expression(&parser);
    
    if (parser.error_count > 0) {
        pln("\nFailed while parsing source:\n`%`", f_string(tokenizer.source));
        Value value = {};
        return value;
    }
    
    Type_Context tcx = {};
    tcx.set_undeclared_to_s64 = preprocessor->is_system_header;
    type_infer_expression(&tcx, expr, t_s64, true);
    
    // TODO(Alexander): interpreter needs to be able to access macro definitions
    
    Interp interp = {};
    interp.set_undeclared_to_zero = preprocessor->is_system_header;
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
        preprocess_error(preprocessor, string_lit("location of above error"));
        pln("Expanded parsed tree:");
        print_ast(expr, t);
        pln("Expanded source line:\n`%`\n", f_string(string_view(base, t->end)));
    }
    
    string_builder_free(&sb);
    
    return result.value;
}

Value
preprocess_eval_macro(Preprocessor* preprocessor, string_id ident) {
    Value result = {};
    
    Preprocessor_Macro macro = map_get(preprocessor->macros, ident);
    if (macro.is_valid) {
        Tokenizer tokenizer = {};
        tokenizer_set_source(&tokenizer, macro.source, string_lit("<eval>"), 0);
        result = preprocess_parse_and_eval_constant_expression(preprocessor, &tokenizer);
    }
    
    return result;
}

internal void
preprocess_parse_define(Preprocessor* preprocessor, Tokenizer* t) {
    Token token = advance_semantical_token(t);
    
    if (token.type != Token_Ident) {
        preprocess_error(preprocessor, string_print("expected `identifier`, found `%`", f_token(token.type)));
        return;
    }
    
    //if (string_compare(token.source, string_lit("CreateWindow")) == 0) {
    //assert(0);
    //}
    if (!preprocessor->curr_branch_taken) return;
    
    
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
            if (token.type == Token_Backslash) {
                token = advance_semantical_token(t);
                continue;
            }
            
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
                    preprocess_error(preprocessor, string_print("expected `)`, found `%`", f_token(token.type)));
                    return;
                }
            } else {
                preprocess_error(preprocessor, string_print("expected `identifier` or `...`, found `%`", f_token(token.type)));
                return;
            }
            token = advance_semantical_token(t);
            if (token.type == Token_Close_Paren) {
                break;
            } else if (token.type == Token_Comma) {
                token = advance_semantical_token(t);
                continue;
            } else {
                preprocess_error(preprocessor, string_print("expected `,` or `)`, found `%`", f_token(token.type)));
                return;
                
            }
        }
        
        token = advance_token(t);
    }
    
    if (token.type != Token_Whitespace && token.type != Token_Newline && token.type != Token_Backslash) {
        preprocess_error(preprocessor, string_print("expected `whitespace`, found `%`", f_token(token.type)));
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
            preprocess_error(preprocessor, string_print("integer `%` literal is too large", f_string(token.source)));
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
            
            // TODO(Alexander): is this error Ok to suppress for system headers?
            if (!preprocessor->is_system_header) {
                preprocess_error(preprocessor, string_print("cannot redeclare macro with same identifier `%`", 
                                                            f_string(vars_load_string(ident))));
            }
            return;
        }
    }
    
    
    // Store the macro
    macro.is_valid = true;
    map_put(preprocessor->macros, ident, macro);
    
#if BUILD_DEBUG && 0
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

#if 0
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
                        preprocess_error(preprocessor, string_print("expected `identifier`, found `%`", f_token(token.type)));
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
                b32 prev_system_header_flag = preprocessor->is_system_header;
                
                if (preprocessor->curr_branch_taken) {
                    token = advance_semantical_token(t);
                    
                    string filename = {};
                    Loaded_Source_File included_file = {};
                    if (token.type == Token_Lt) {
                        token = advance_semantical_token(t);
                        filename = token.source;
                        filename.count = 0;
                        
                        while (token.type != Token_Gt) {
                            if (token.type == Token_EOF) {
                                return;
                            }
                            
                            filename.count += token.source.count;
                            token = advance_token(t);
                        }
                        
                        included_file = read_entire_system_header_file(filename);
                        preprocessor->is_system_header = true;
                        
                    } else if (token.type == Token_String) {
                        Loaded_Source_File* curr_file =
                            get_source_file_by_index(preprocessor->curr_file_index);
                        filename = string_unquote_nocopy(token.source);
                        included_file = read_entire_source_file(filename, curr_file);
                        preprocessor->is_system_header = string_ends_with(included_file.filename, string_lit(".h"));
                    }
                    
                    if (included_file.is_valid) {
                        u32 curr_file_index = preprocessor->curr_file_index; // remember this so we don't loose it
                        preprocessor->curr_file_index = included_file.index;
                        u32 prev_num_includes = map_get(preprocessor->loaded_file_indices, included_file.index);
                        map_put(preprocessor->loaded_file_indices, included_file.index, prev_num_includes + 1);
                        preprocessor->abort_curr_file = false;
                        
                        //push_file_to_preprocess(preprocessor, 
                        //included_file.source, 
                        //included_file.abspath, 
                        //included_file.extension,
                        //included_file.index,
                        //prev_system_header_flag);
                        preprocessor->abort_curr_file = false; // if #pragma once hit then restore it
                        preprocessor->curr_file_index = curr_file_index;
                    } else {
                        pln("%:%:%: could not locate file `%` \n\n", f_string(t->file), 
                            f_umm(t->line_number), f_umm(t->column_number), f_string(filename));
                        preprocessor->error_count++;
                    }
                }
                
            } break;
            
            case Kw_if: {
                if (preprocessor->curr_branch_taken) {
                    Value value = preprocess_parse_and_eval_constant_expression(preprocessor, t);
                    array_push(preprocessor->if_result_stack, 
                               value_to_bool(value) ? IfStk_Taken : IfStk_Not_Taken);
                    
                    preprocessor->curr_branch_taken =
                        check_if_curr_branch_is_taken(preprocessor->if_result_stack);
                } else {
                    array_push(preprocessor->if_result_stack, IfStk_Not_Taken);
                }
            } break;
            
            case Sym_elif: {
                if (array_count(preprocessor->if_result_stack) > 0) {
                    If_Stk_Status last_result = array_last(preprocessor->if_result_stack);
                    if (last_result) {
                        preprocessor->curr_branch_taken = false;
                        array_last(preprocessor->if_result_stack) = IfStk_Prev_Taken;
                    } else {
                        
                        Value value = preprocess_parse_and_eval_constant_expression(preprocessor, t);
                        array_last(preprocessor->if_result_stack) = 
                            value_to_bool(value) ? IfStk_Taken : IfStk_Not_Taken;
                        
                        preprocessor->curr_branch_taken =
                            check_if_curr_branch_is_taken(preprocessor->if_result_stack);
                    }
                } else {
                    preprocess_error(preprocessor, string_lit("#else directive outside #if scope"));
                }
            } break;
            
            case Kw_else: {
                if (array_count(preprocessor->if_result_stack) > 0) {
                    If_Stk_Status last_result = array_last(preprocessor->if_result_stack);
                    if (last_result) {
                        preprocessor->curr_branch_taken = false;
                        array_last(preprocessor->if_result_stack) = IfStk_Prev_Taken;
                    } else {
                        array_last(preprocessor->if_result_stack) = IfStk_Taken;
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
                    
                    pln("#ifdef % == %", f_var(ident), f_bool(value));
                    array_push(preprocessor->if_result_stack, value ? IfStk_Taken : IfStk_Not_Taken);
                    preprocessor->curr_branch_taken =
                        check_if_curr_branch_is_taken(preprocessor->if_result_stack);
                    
                } else {
                    preprocess_error(preprocessor, string_print("expected `identifier`, found `%`", 
                                                                f_token(token.type)));
                    
                }
            } break;
            
            case Sym_error: {
                if (preprocessor->curr_branch_taken) {
                    preprocess_error(preprocessor, string_view(t->curr, t->end));
                }
            } break;
        }
    }
}
#endif

internal Preprocessor_Line
preprocess_splice_next_line(Tokenizer* t) {
    if (t->curr != t->end) {
        t->line_number++;
    }
    
    Preprocessor_Line result = {};
    result.curr_line_number = t->line_number;
    
    t->curr = t->end;
    u8* begin = t->curr;
    t->end = t->end_of_file;
    t->next = t->curr;
    utf8_advance_character(t);
    
    umm target_line_number = t->line_number + 1;
    Token token = advance_token(t);
    while (is_token_valid(token) && t->line_number < target_line_number) {
        if (token.type == Token_Backslash) {
            target_line_number++;
        }
        token = advance_token(t);
    }
    
    // restore tokenizer to beginning of this line
    t->end = t->curr;
    t->curr = begin;
    t->next = t->curr;
    t->curr_line = t->curr;
    result.substring = string_view(t->curr, t->end);
    result.next_line_number = t->line_number;
    t->line_number = result.curr_line_number;
    t->column_number = 0;
    utf8_advance_character(t);
    
    //pln("Line %: `%`", f_u32(result.curr_line_number), f_string(result.substring));
    
    return result;
}


internal inline bool
preprocess_try_expand_ident(Preprocessor* preprocessor, 
                            String_Builder* sb, 
                            Tokenizer* t, 
                            Preprocessor_Macro parent_macro,
                            Replacement_List args, 
                            string_id ident);

internal inline Token
preprocess_actual_argument_next_token(Preprocessor* preprocessor, Tokenizer* t, int* paren_depth, String_Builder* sb) {
    // This will merge with below line if it reaches the end line
    
    u8* begin = t->curr;
    Token token = advance_semantical_token(t);
    if (token.source.data) {
        string_builder_push(sb, string_view(begin, token.source.data));
    }
    if (token.type == Token_EOF) {
        preprocess_splice_next_line(t);
        token = advance_semantical_token(t);
    }
    
    if (token.type == Token_Open_Paren) *paren_depth += 1;
    if (token.type == Token_Close_Paren) *paren_depth -= 1;
    
    return token;
}

internal Replacement_List
preprocess_parse_actual_arguments(Preprocessor* preprocessor, Tokenizer* t, Preprocessor_Macro parent_macro, Replacement_List args) {
    Replacement_List result = {};
    result.success = true;
    
    int paren_depth = 1;
    String_Builder sb = {};
    Token token = preprocess_actual_argument_next_token(preprocessor, t, &paren_depth, &sb);
    while (is_token_valid(token) && paren_depth > 0) {
        
        sb.curr_used = 0;
        
        u8* begin = token.source.data;
        
        while (is_token_valid(token)) {
            if ((token.type == Token_Comma && paren_depth == 1) || paren_depth == 0) {
                break;
            }
            
            if (token.type == Token_Ident) {
                string_id ident = vars_save_string(token.source);
                if (!preprocess_try_expand_ident(preprocessor, &sb, t, parent_macro, args, ident)) {
                    string_builder_push(&sb, token.source);
                }
            } else {
                string_builder_push(&sb, token.source);
            }
            
            token = preprocess_actual_argument_next_token(preprocessor, t, &paren_depth, &sb);
        }
        
        
        if (is_token_valid(token)) {
            string replacement = string_builder_to_string(&sb);
            array_push(result.list, replacement);
            
            if (token.type == Token_Close_Paren) {
                break;
            }
        } else {
            break;
        }
        
        token = preprocess_actual_argument_next_token(preprocessor, t, &paren_depth, &sb);
    }
    
    string_builder_free(&sb);
    
    if (token.type == Token_EOF) {
        preprocess_error(preprocessor, string_lit("reached end of file while preprocessing file"));
        result.success = false;
        
    } else if (result.success && token.type != Token_Close_Paren) {
        preprocess_error(preprocessor, string_print("argument list ended without `)`, found `%`",
                                                    f_token(token.type)));
        result.success = false;
    }
    
    return result;
}

internal inline bool
preprocess_try_expand_ident(Preprocessor* preprocessor, 
                            String_Builder* sb, 
                            Tokenizer* t, 
                            Preprocessor_Macro parent_macro,
                            Replacement_List args, 
                            string_id ident) {
    
    switch (ident) {
        case Sym_defined: {
            
            string_id macro_ident = Kw_invalid;
            
            // Parse macro identifier e.g. defined(MACRO) or defined MACRO
            Token token = advance_semantical_token(t);
            if (token.type == Token_Open_Paren) {
                token = advance_semantical_token(t);
                
                if (token.type == Token_Ident) {
                    macro_ident = vars_save_string(token.source);
                    
                    token = advance_semantical_token(t);
                    if (token.type != Token_Close_Paren) {
                        preprocess_error(preprocessor, string_print("expected `)`, found `%`", f_token(token.type)));
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
        } break;
        
        case Sym___VA_ARGS__: {
            umm formal_arg_count = map_count(parent_macro.arg_mapper);
            umm actual_arg_count = array_count(args.list);
            
            for (umm arg_index = formal_arg_count; 
                 arg_index < actual_arg_count; 
                 arg_index++) {
                
                string source = args.list[arg_index];
                
                Tokenizer tokenizer = {};
                tokenizer_set_source(&tokenizer, source, string_lit("args"), 0);
                preprocess_expand_macro(preprocessor, sb, &tokenizer, parent_macro, {});
                
                if (arg_index + 1 < actual_arg_count) {
                    string_builder_push(sb, ",");
                }
            }
            
            return true;
        } break;
        
        case Sym___FILE__: {
            int num_backslash = 0;
            for (int i = 0; i < t->file.count; i++) {
                if (t->file.data[i] == '\\') num_backslash++;
            }
            
            // TODO(Alexander): hacky way to escape backslashes in filename
            string filename = t->file;
            if (num_backslash > 0) {
                filename = string_alloc(t->file.count + num_backslash);
                int last_copy = 0;
                int j = 0;
                for (int i = 0; i < t->file.count; i++) {
                    filename.data[j++] = t->file.data[i];
                    if (t->file.data[i] == '\\') {
                        filename.data[j++] = '\\';
                    }
                }
            }
            
            string_builder_push_format(sb, "\"%\"", f_string(filename));
            return true;
        } break;
        
        case Sym___LINE__: {
            string_builder_push_cformat(sb, "%zu", (umm) t->line_number + 1);
            return true;
        } break;
        
        case Sym___COUNTER__: {
            unimplemented;
        } break;
        
        default: {
            smm arg_index = map_get_index(parent_macro.arg_mapper, ident);
            if (arg_index != -1) {
                // TODO(Alexander): in order to support macro expanding the inserted argument
                // we should make this process in two phases first expand arguments then second
                // phase expands the macros used.
                assert(array_count(args.list) > arg_index);
                string_builder_push(sb, args.list[arg_index]);
                return true;
            }
        } break;
    }
    
    Preprocessor_Macro macro = map_get(preprocessor->macros, ident);
    bool in_use = map_get(preprocessor->macro_in_use, ident);
    if (macro.is_valid && !in_use) {
        
        Replacement_List macro_args = {};
        if (macro.is_functional) {
            
            int paren_depth = 0;
            Tokenizer_State restore_t = save_tokenizer(t);
            Token token = preprocess_actual_argument_next_token(preprocessor, t, &paren_depth, sb);
            
            if (token.type == Token_Open_Paren) {
                macro_args = preprocess_parse_actual_arguments(preprocessor, t, parent_macro, args);
                if (!macro_args.success) {
                    return false;
                }
            } else {
                restore_tokenizer(&restore_t);
                return false;
            }
        }
        
        
        // We should only expand the same macro once, avoid circular dependencies
        map_put(preprocessor->macro_in_use, ident, true);
        
        
        umm actual_arg_count = array_count(macro_args.list);
        umm formal_arg_count = map_count(macro.arg_mapper);
        if ((actual_arg_count < formal_arg_count) ||
            (actual_arg_count > formal_arg_count && !macro.is_variadic)) {
            
            preprocess_error(preprocessor, string_print("function-like macro `%` expected % arguments, found % arguments",
                                                        f_string(vars_load_string(ident)),
                                                        f_int(formal_arg_count), f_int(actual_arg_count)));
            return false;
        }
        
        umm first_used = sb->curr_used;
        
        // TODO(Alexander): record line/ col of this
        Tokenizer tokenizer = {};
        tokenizer_set_source(&tokenizer, macro.source, t->file, t->file_index, t->line_number);
        preprocess_expand_macro(preprocessor, sb, &tokenizer, macro, macro_args);
        
        string expanded_source = string_view(sb->data + first_used, sb->data + sb->curr_used);
        
#if 0
        pln("Expanding macro `%` to:\n`%`", f_string(vars_load_string(ident)), f_string(expanded_source));
#endif
        
        map_put(preprocessor->macro_in_use, ident, false);
        
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
    
    bool inside_stringification = false;
    Token token = advance_token(t);
    while (is_token_valid(token)) {
        switch (token.type) {
            case Token_Ident: {
                string_id ident = vars_save_string(token.source);
                
                if (macro.arg_mapper && map_key_exists(macro.arg_mapper, ident)) {
                    int arg_index = map_get(macro.arg_mapper, ident);
                    if (array_count(args.list) < arg_index) {
                        preprocess_error(preprocessor, string_print("function-like macro expected % arguments, found % arguments",
                                                                    f_int(map_count(macro.arg_mapper)),
                                                                    f_int(array_count(args.list))));
                        return;
                    }
                    string arg_source = args.list[arg_index];
                    if (inside_stringification) { 
                        // NOTE(Alexander): need to escape strings
                        for (int i = 0; i < arg_source.count; i++) {
                            if (arg_source.data[i] == '"') {
                                string_builder_push_char(sb, '\\');
                            }
                            string_builder_push_char(sb, arg_source.data[i]);
                        }
                    } else {
                        string_builder_push(sb, arg_source);
                    }
                } else {
                    if (!preprocess_try_expand_ident(preprocessor, sb, t, macro, args, ident)) {
                        string_builder_push(sb, token.source);
                    }
                }
            } break;
            
            case Token_Directive: {
                string_builder_push(sb, "\"");
                token = advance_token(t);
                inside_stringification = true;
                continue;
            } break;
            
            case Token_Backslash: 
            case Token_Line_Comment: 
            case Token_Block_Comment: break;
            
            default: {
                string_builder_push(sb, token.source);
            } break;
        }
        
        if (inside_stringification) {
            inside_stringification = false;
            string_builder_push(sb, "\"");
        }
        
        token = advance_token(t);
    }
}

internal Token
push_non_semantical_tokens(Tokenizer* t, String_Builder* sb) {
    Token token = advance_token(t);
    while (!is_semantical_token(token) && is_token_valid(token)) {
        string_builder_push(sb, token.source);
        token = advance_token(t);
    }
    
    return token;
}


internal bool
preprocess_line(Preprocessor* preprocessor, String_Builder* sb, Tokenizer* t) {
    u8* base = t->curr;
    
    umm curr_used = sb->curr_used;
    Token token = push_non_semantical_tokens(t, sb);
    Token first_token = token;
    if (token.type == Token_Directive) {
        preprocess_directive(preprocessor, t);
        sb->curr_used = curr_used;
        return false;
    }
    
    if (!preprocessor->curr_branch_taken) {
        sb->curr_used = curr_used;
        return false;
    }
    
    while (is_token_valid(token)) {
        switch (token.type) {
            case Token_Ident: {
                string_id ident = vars_save_string(token.source);
                
                if (!preprocess_try_expand_ident(preprocessor, sb, t, {}, {}, ident)) {
                    string_builder_push(sb, token.source);
                }
            } break;
            
            case Token_Directive: {
                pln("failed on line:\n`%`", f_string(string_view(base, t->end)));
                pln("first token is `%`", f_token(first_token.type));
                if (first_token.type == Token_Ident) {
                    pln("== `%`", f_string(first_token.source));
                }
                preprocess_error(preprocessor, string_lit("preprocessor directive (#) must start at the beginning of a line"));
            } break;
            
            case Token_Concatenator: {
                preprocess_error(preprocessor, string_lit("preprocessor concatenator (##) can only be used inside macro"));
            } break;
            
            case Token_Backslash:
            case Token_Line_Comment:
            case Token_Block_Comment: break;
            
            default: {
                string_builder_push(sb, token.source);
            } break;
        }
        
        token = advance_token(t);
    }
    
    return true;
}

internal inline string
preprocess_get_non_semantical_token_source(Tokenizer* t) {
    
    Token next_token = advance_token(t);
    
    // Skip to next semantical token (keep track of this string)
    string non_semantical_text;
    non_semantical_text.data = next_token.source.data;
    non_semantical_text.count = 0;
    while (!is_semantical_token(next_token) && is_token_valid(next_token)) {
        non_semantical_text.count += next_token.source.count;
        next_token = advance_token(t);
    }
    
    return non_semantical_text;
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
    
    tokenizer_set_source(t, source, string_lit("macro"), 0);
    
    String_Builder sb = {};
    
    umm sb_prev_used = 0;
    Token prev_token = {};
    Token token = advance_token(t);
    
    while (is_token_valid(token)) {
        switch (token.type) {
            case Token_Directive: {
                token = advance_semantical_token(t);
                
                if (prev_token.type == Token_String) {
                    sb.curr_used = sb_prev_used;
                    preprocess_push_concatenated_strings(&sb, prev_token, token);
                } else {
                    sb_prev_used = sb.curr_used;
                    string_builder_push_format(&sb, "\"%\"", f_string(token.source));
                }
                
                string str = string_view(sb.data + sb_prev_used,
                                         sb.data + sb.curr_used);
                prev_token.source = string_copy(str); // TODO(Alexander): temp. allocate
                prev_token.type = Token_String;
                
                token = advance_token(t);
            } break;
            
            case Token_Concatenator: {
                Token next_token = advance_semantical_token(t);
                sb.curr_used = sb_prev_used;
                
                if (prev_token.type == Token_String || next_token.type == Token_String) {
                    preprocess_push_concatenated_strings(&sb, prev_token, next_token);
                    prev_token.type = Token_String;
                } else {
                    string_builder_push(&sb, prev_token.source);
                    string_builder_push(&sb, next_token.source);
                    prev_token.type = Token_Ident;
                }
                
                string str = string_view(sb.data + sb_prev_used,
                                         sb.data + sb.curr_used);
                prev_token.source = string_copy(str); // TODO(Alexander): temp. allocate
                
                token = advance_token(t);
            } break;
            
            case Token_String: {
                // TODO(Alexander): for now we use num_hashes to mark where this 
                // string begins in the finalized string
                
                if (prev_token.type == Token_String) {
                    sb.curr_used = sb_prev_used;
                    preprocess_push_concatenated_strings(&sb, prev_token, token);
                    string str = string_view(sb.data + sb_prev_used,
                                             sb.data + sb.curr_used);
                    token.source = string_copy(str); // TODO(Alexander): temp. allocate
                } else {
                    sb_prev_used = sb.curr_used;
                    string_builder_push(&sb, token.source);
                }
                
                prev_token = token;
                token = advance_token(t);
            } break;
            
            case Token_Backslash: {
                token = advance_token(t);
            } break;
            
            default: {
                if (is_semantical_token(token)) {
                    sb_prev_used = sb.curr_used;
                    prev_token = token;
                }
                
                string_builder_push(&sb, token.source);
                token = push_non_semantical_tokens(t, &sb);
            } break;
        }
    }
    
    return string_builder_to_string_nocopy(&sb);
}

void
push_file_to_preprocess(Preprocessor* preprocessor, 
                        string source, string filepath, string extension, 
                        int file_index, bool is_system_header) {
    Preprocessor_State state = {};
    tokenizer_set_source(&state.tokenizer, source, filepath, file_index);
    state.tokenizer.end = state.tokenizer.curr;
    state.is_c_or_cpp_file = (string_equals(extension, string_lit("c")) ||
                              string_equals(extension, string_lit("h")) ||
                              string_equals(extension, string_lit("cpp")) ||
                              string_equals(extension, string_lit("hpp")));
    state.is_valid = true;
    state.is_system_header = is_system_header;
    array_push(preprocessor->file_stack, state);
}

internal inline Source_Group
begin_source_group(Preprocessor* preprocessor, u32 next_line_number) {
    Source_Group result = {};
    Preprocessor_State* state = array_last_ptr(preprocessor->file_stack);
    if (!state) return result;
    
    result.offset = preprocessor->output.curr_used;
    result.file_index = (u32) state->tokenizer.file_index;
    result.c_compatibility_mode = state->is_c_or_cpp_file;
    result.line = next_line_number;
    return result;
}

internal inline void
end_source_group(Preprocessor* preprocessor, Source_Group group) {
    array_push(preprocessor->source_groups, group);
}

internal inline Preprocessor_State
pop_file(Preprocessor* preprocessor) {
    Preprocessor_State result = {};
    if (array_count(preprocessor->file_stack) > 0) {
        result = array_pop(preprocessor->file_stack); 
    }
    return result;
}
string
preprocess_file(Preprocessor* preprocessor) {
    String_Builder* sb = &preprocessor->output;
    preprocessor->curr_branch_taken = true;
    
    if (!array_count(preprocessor->file_stack)) {
        return string_builder_to_string_nocopy(sb);
    }
    
    Source_Group current_group = begin_source_group(preprocessor, 0);
    Preprocessor_State* prev_state = array_last_ptr(preprocessor->file_stack);
    while (array_count(preprocessor->file_stack)) {
        
        Preprocessor_State* state = array_last_ptr(preprocessor->file_stack);
        if (prev_state != state) {
            pln("== File: % ==", f_string(state->tokenizer.file));
            end_source_group(preprocessor, current_group);
            current_group = begin_source_group(preprocessor, (u32) state->curr_line_number);
        }
        prev_state = state;
        
        if (state->tokenizer.curr >= state->tokenizer.end_of_file) {
            array_pop(preprocessor->file_stack);
            continue;
        }
        
        //u8* curr_line = state->curr_line;
        
        Preprocessor_Line line = preprocess_splice_next_line(&state->tokenizer);
        state->curr_line += line.substring.count;
        state->curr_line_number = line.curr_line_number;
        preprocessor->preprocessed_lines += line.next_line_number - line.curr_line_number;
        
        umm begin_used = sb->curr_used;
        bool run_finalize = preprocess_line(preprocessor, sb, &state->tokenizer);
        umm end_used = sb->curr_used;
        pln("% Line % (range %-%):\n`%`", f_char(preprocessor->curr_branch_taken ? '+' : '-'), f_int(line.curr_line_number + 1), f_int(begin_used), f_int(end_used), f_string(line.substring));
        
        if (preprocessor->abort_curr_file) {
            //Loaded_Source_File* file = get_source_file_by_index(preprocessor->curr_file_index);
            //pln("abort preprossing: `%`", f_string(file->filename));
            break;
        }
        
        if (run_finalize) {
            sb->curr_used = begin_used;
            string expanded_code = create_string(end_used - begin_used, sb->data + begin_used);
            
            //pln("expanded code:\n`%`", f_string(expanded_code));
            string finalized_string = preprocess_finalize_code(expanded_code);
            //pln(" -->\n`%`\n", f_string(finalized_string));
            string_builder_push(sb, finalized_string);
            
            if (current_group.count == 0) {
                current_group.offset = begin_used;
            }
            current_group.count += (umm) finalized_string.count;
            
            string_free(finalized_string);
        } else {
            
            //pln("%", f_string(string_builder_to_string_nocopy(sb)));
            
            if (begin_used != end_used) {
                // Start new group because one or more lines was skipped (due to #if/#else)
                end_source_group(preprocessor, current_group);
                current_group = begin_source_group(preprocessor, (u32) line.next_line_number);
            }
            
            if (current_group.count == 0) {
                current_group.offset = end_used;
            }
            
            int empty_lines = (int) line.next_line_number - (int) line.curr_line_number;
            pln("^- Empty lines = %", f_int(empty_lines));
            for(int i = 0; i < empty_lines; i++) string_builder_push(sb, "\n");
            current_group.count += empty_lines;
        }
    }
    
    if (current_group.count > 0) {
        array_push(preprocessor->source_groups, current_group);
    }
    
#if 0
    if (array_count(preprocessor->if_result_stack) != initial_if_result_stack_count) {
        // TODO(Alexander): add error message and clear if stack
        if (!preprocessor->abort_curr_file) {
            preprocess_error(preprocessor, string_lit("found end of file while #if was not ended"));
        }
        // TODO(Alexander): we should copy the array and restore it intead
        while (array_count(preprocessor->if_result_stack) > initial_if_result_stack_count) {
            array_pop(preprocessor->if_result_stack);
        }
        while (array_count(preprocessor->if_result_stack) < initial_if_result_stack_count) {
            array_push(preprocessor->if_result_stack, IfStk_Taken);
        }
    }
#endif
    
    return string_builder_to_string_nocopy(sb);
}
#endif

inline u8*
preprocess_scan_identifier(u8* curr, u8* end) {
    if (is_ident_start(*curr++)) {
        while (curr < end) {
            if (!is_ident_continue(*curr)) {
                break;
            }
            curr++;
        }
    }
    
    return curr;
}

inline u8*
preprocess_scan_to_char(u8* curr, u8* end, u8 expect) {
    while (curr < end) {
        if (*curr == expect) {
            break;
        }
        curr++;
    }
    return curr;
}

enum {
    State_Ready         = bit(0),
    State_Line_Comment  = bit(1),
    State_Block_Comment = bit(2),
    State_Angle_Bracket = bit(3),
    State_Double_Quote  = bit(4),
    State_Directive     = bit(5),
    State_Ident_Start   = bit(6),
    State_Backslash     = bit(7),
    State_End_Of_Line   = bit(8),
    State_End_Of_File   = bit(9),
    
    End_States_Begin_Filename = State_Double_Quote | State_Angle_Bracket | State_End_Of_Line,
    End_States_Main_Loop = State_Directive | State_Ident_Start,
};

struct Preprocess_Stm_Result {
    u8* next;
    int state;
};

inline Preprocess_Stm_Result
preprocess_stm(u8* curr, u8* end, u32* line_number, int exit_states) {
    int state = State_Ready;
    
    while (state & ~exit_states) {
        if (curr >= end) {
            state = State_End_Of_File;
            break;
        }
        
        u8 ch = *curr++;
        if (state == State_Line_Comment) {
            if (ch == '\\') {
                curr = preprocess_scan_to_char(curr, end, '\n') + 1;
                *line_number += 1;
                
            } else if (ch == '\n') {
                state = State_End_Of_Line;
                *line_number += 1;
                break;
            }
            
        } else if (state == State_Block_Comment) {
            if (ch == '\n') {
                *line_number += 1;
                
            } else if (ch == '*' && *curr == '/') {
                curr++;
                state = State_Ready;
            }
            
        } else {
            state = State_Ready;
            
            if (ch == '/' && *curr == '/') {
                state = State_Line_Comment;
                curr++;
                
            } else if (ch == '/' && *curr == '*') {
                state = State_Block_Comment;
                curr++;
                
            } else if (ch == '\n') {
                *line_number += 1;
                state = State_End_Of_Line;
                
            } else if (ch == '"') {
                state = State_Double_Quote;
                
            } else if (ch == '<') {
                state = State_Angle_Bracket;
                
            } else if (ch == '\\') {
                state = State_Backslash;
                
            } else if (ch == '#') {
                state = State_Directive;
                
            } else if (is_ident_start(ch)) {
                state = State_Ident_Start;
            }
        }
    }
    
    Preprocess_Stm_Result result = { curr, state };
    return result;
}

inline u8*
preprocess_scan_to_next_line(u8* curr, u8* end, u32* line_number) {
    for (;;) {
        curr = preprocess_stm(curr, end, line_number, State_End_Of_Line | State_Backslash).next;
        if (curr[-1] == '\\') {
            *line_number += 1;
            curr = preprocess_scan_to_char(curr, end, '\n') + 1;
        } else {
            break;
        }
    }
    return curr;
}

Value
preprocess_eval_string(Preprocessor* p, string source) {
    // TODO: We need to expand any preprocessor macros first!
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, p->file, p->file_index, p->line_number);
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    
    // TODO(Alexander): could really use some scratch memory to allocate the nodes
    Ast* expr = parse_expression(&parser);
    
    if (parser.error_count > 0) {
        pln("\nFailed while parsing source:\n`%`", f_string(source));
        p->error_count += parser.error_count;
        Value value = {};
        return value;
    }
    
    Type_Context tcx = {};
    //tcx.set_undeclared_to_s64 = preprocessor->is_system_header;
    type_infer_expression(&tcx, expr, t_s64, true);
    
    Interp interp = {};
    //interp.set_undeclared_to_zero = preprocessor->is_system_header;
    Interp_Value result = interp_expression(&interp, expr);
    p->error_count += interp.error_count;
    return result.value;
}

u8*
preprocess_directive(Preprocessor* p) {
    int begin_col_number = p->column_number;
    
    u8* curr = p->src + p->src_offset;
    u8* end = p->src + p->src_count;
    
    u8* ident_begin = curr;
    curr = preprocess_scan_identifier(ident_begin, end);
    string ident_source = string_view(ident_begin, curr);
    pln("%:%:%: directive `#%`", f_string(p->file), f_int(p->line_number + 1), f_int(begin_col_number + 1), f_string(ident_source));
    
    string_id ident = vars_save_string(ident_source);
    switch (ident) {
        case Sym_include: {
            if (p->curr_branch_taken) {
                curr = preprocess_stm(curr, end, &p->line_number, End_States_Begin_Filename).next;
                
                Loaded_Source_File included_file = {};
                u8 ch = curr[-1];
                if (ch == '"') {
                    u8* file_begin = curr;
                    curr = preprocess_scan_to_char(curr, end, '"');
                    string filename = string_view(file_begin, curr++);
                    
                    included_file = read_entire_system_header_file(filename);
                    pln("#include \"%\"\n", f_string(filename));
                    
                    
                } else if (ch == '<') {
                    u8* file_begin = curr;
                    curr = preprocess_scan_to_char(curr, end, '>');
                    string filename = string_view(file_begin, curr++);
                    
                    Loaded_Source_File* curr_file =
                        get_source_file_by_index(p->file_index);
                    included_file = read_entire_source_file(filename, curr_file);
                    pln("#include <%>\n", f_string(filename));
                    
                } else {
                    pln("error: `#include` expected `\"filename\"` or `<filename>`");
                }
                
                if (included_file.is_valid) {
                    unimplemented;
                }
            }
        } break;
        
        case Sym_import: {
            if (p->curr_branch_taken) {
                unimplemented;
            }
        } break;
        
        case Sym_define: {
            if (p->curr_branch_taken) {
                unimplemented;
            }
        } break;
        
        case Kw_if: {
            u8* begin_cond = curr;
            curr = preprocess_scan_to_next_line(curr, end, &p->line_number);
            
            if (p->curr_branch_taken) {
                string cond_source = string_view(begin_cond, curr - 1);
                pln("#if%", f_string(cond_source));
                
                Value value = preprocess_eval_string(p, cond_source);
                If_Stk_Status result = value_to_bool(value) ? IfStk_Taken : IfStk_Not_Taken;
                array_push(p->if_result_stack, result);
                p->curr_branch_taken = check_if_curr_branch_is_taken(p->if_result_stack);
                
            } else {
                array_push(p->if_result_stack, IfStk_Not_Taken);
            }
        } break;
        
        case Sym_elif: {
            u8* begin_cond = curr;
            curr = preprocess_scan_to_next_line(curr, end, &p->line_number);
            
            if (array_count(p->if_result_stack) > 0) {
                If_Stk_Status last_result = array_last(p->if_result_stack);
                if (last_result) {
                    p->curr_branch_taken = false;
                    array_last(p->if_result_stack) = IfStk_Prev_Taken;
                } else {
                    string cond_source = string_view(begin_cond, curr - 1);
                    Value value = preprocess_eval_string(p, cond_source);
                    array_last(p->if_result_stack) = 
                        value_to_bool(value) ? IfStk_Taken : IfStk_Not_Taken;
                    
                    p->curr_branch_taken =
                        check_if_curr_branch_is_taken(p->if_result_stack);
                }
            } else {
                preprocess_error(p, string_lit("`#elif` found outside `#if` scope"));
            }
        } break;
        
        case Kw_else: {
            curr = preprocess_scan_to_next_line(curr, end, &p->line_number);
            
            if (array_count(p->if_result_stack) > 0) {
                If_Stk_Status last_result = array_last(p->if_result_stack);
                if (last_result) {
                    p->curr_branch_taken = false;
                    array_last(p->if_result_stack) = IfStk_Prev_Taken;
                } else {
                    array_last(p->if_result_stack) = IfStk_Taken;
                    p->curr_branch_taken =
                        check_if_curr_branch_is_taken(p->if_result_stack);
                }
            } else {
                preprocess_error(p, string_lit("`#else` found outside `#if` scope"));
            }
            // TODO(Alexander): detect two else in row
        } break;
        
        case Sym_endif: {
            curr = preprocess_scan_to_next_line(curr, end, &p->line_number);
            
            if (array_count(p->if_result_stack) > 0) {
                array_pop(p->if_result_stack);
                p->curr_branch_taken =
                    check_if_curr_branch_is_taken(p->if_result_stack);
            } else {
                preprocess_error(p, string_lit("`#endif` found outside `#if` scope"));
            }
        } break;
    }
    
    p->src_offset = (u64) (curr - p->src);
    return curr;
}

void
preprocess(string source, string file, u32 file_index) {
    
    String_Builder string_builder = {};
    String_Builder* sb = &string_builder;
    
    Preprocessor p = {};
    p.src = source.data;
    p.src_count = source.count;
    p.file_index = file_index;
    p.file = file;
    string_builder_alloc(&p.dest, p.dest.curr_used + source.count);
    
    u8* curr = p.src;
    u8* end = source.data + source.count;
    while (curr < end) {
        Preprocess_Stm_Result stm = preprocess_stm(curr, end, &p.line_number, End_States_Main_Loop);
        if (stm.state == State_Directive) {
            curr++;
            p.src_offset = (u64) (stm.next - p.src);
            curr = preprocess_directive(&p);
            p.column_number = 0;
            
        } else if (stm.state == State_Ident_Start && p.curr_branch_taken) {
            int begin_col_number = p.column_number;
            u8* ident_begin = stm.next - 1;
            curr = preprocess_scan_identifier(ident_begin, end);
            
            string ident_source = string_view(ident_begin, curr);
            pln("%:%:%: ident `%`", f_string(file), f_int(p.line_number + 1), f_int(begin_col_number + 1), f_string(ident_source));
            p.column_number = begin_col_number + (u32) ident_source.count;
            
        } else {
            curr = stm.next;
        }
    }
}
