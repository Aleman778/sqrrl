// Sqrrl compiler test harness

#if BUILD_TEST

typedef void asm_test(void);

// TODO(Alexander): this is just a simple assertion function to get started, improve this
// this is intended to be called from the compiled program
extern "C" bool
intrinsic_test_proc_assert(int expr, cstring msg, cstring file, smm line) {
    if (is_test_mode && curr_test) {
        curr_test->num_tests++;
        if (expr == 0) {
            curr_test->num_failed++;
            string_builder_push_format(&curr_execution->output,
                                       "%:%: Assertion failed: %\n", f_cstring(file), f_smm(line), f_cstring(msg));
            return false;
        } else {
            curr_test->num_passed++;
            return true;
        }
    } else {
        pln("%:%: Assertion failed: %", f_cstring(file), f_smm(line), f_cstring(msg));
        fflush(stdout);
        *(int *)0 = 0;
    }
    return false;
}

extern "C" void
intrinsic_print(cstring format, Var_Args args) {
    string_builder_push_format(&curr_execution->output, "%", 
                               f_cstring(format));
}

void
test_exception_handler(u32 exception_code, string message) {
    string_builder_push_format(&curr_execution->output,
                               "Unhandled exception: % (code %)\n",
                               f_string(message), f_u64_HEX(exception_code));
    
    curr_test->num_failed++;
    curr_execution->failed = true;
    //curr_execution->fatal_error = true;
    if (curr_execution->context) {
        DEBUG_restore_context(curr_execution->context);
    }
}

int
run_test(void* param) {
    Test_Result* test = (Test_Result*) param;
    curr_test = test;
    
    Compilation_Unit* unit = test->unit;
    string test_name = vars_load_string(unit->ident);
    
    
    // Interpreter
    if (is_bitflag_set(test->modes, TestExecutionMode_Interp)) {
        curr_execution = &test->exec[0];
        
        Interp* interp = test->interp;
        u32 prev_num_failed = test->num_failed;
        interp_function_call(interp, 0, unit->ast->type);
        //pln("interp: %, errors = %", f_var(unit->ident), f_int(interp.error_count));
        
        //pln("num_tests = %", f_int(curr_test->num_tests));
        if (interp->error_count > 0) {
            test->num_failed += interp->error_count;
            test->num_tests +=  interp->error_count;
            interp->error_count = 0;
        }
        
        if (prev_num_failed != test->num_failed) {
            curr_execution->failed = true;
        }
    }
    
    // X64 JIT
    if (is_bitflag_set(test->modes, TestExecutionMode_X64_JIT)) {
        curr_execution = &test->exec[1];
        
        u32 prev_num_failed = test->num_failed;
        //pln("Running `%` at memory location: 0x%", f_string(vars_load_string(it->key)),
        //f_u64_HEX(unit->bytecode_function->code_ptr));
        if (!unit->bytecode_function->code_ptr) {
            pln("Failed to run test `%`, invalid function pointer", f_string(test_name));
        }
        
        asm_test* test_func = (asm_test*) unit->bytecode_function->code_ptr;
        
        curr_execution->context = DEBUG_capture_context();
        if (curr_execution->context) {
            test_func();
        }
        
        if (prev_num_failed != test->num_failed) {
            curr_execution->failed = true;
        }
    }
    
    return 0;
}

int
run_compiler_tests(string filename, 
                   void* asm_buffer, umm asm_size, 
                   void (*asm_make_executable)(void*, umm),
                   bool is_debugger_present) {
    
    is_test_mode = true;
    
    // TODO(Alexander): this is hardcoded for now
    t_string->size = sizeof(string);
    t_string->align = alignof(string);
    t_cstring->size = sizeof(cstring);
    t_cstring->align = alignof(cstring);
    t_type->size = sizeof(smm);
    t_type->align = alignof(smm);
    t_void_ptr->size = sizeof(smm);
    t_void_ptr->align = alignof(smm);
    
    vars_initialize_keywords_and_symbols();
    
    // Load source code
    Loaded_Source_File file = read_entire_source_file(filename);
    if (!file.is_valid) {
        return -1;
    }
    
    
    // Preprocess
    Preprocessor preprocessor = {};
    push_file_to_preprocess(&preprocessor, 
                            file.source, file.abspath, file.extension, file.index, false);
    string preprocessed_source = preprocess_file(&preprocessor);
    
    // Parse preprocessed source
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, preprocessed_source, filename, file.index);
    Parser parser = {};
    parser.source_groups = preprocessor.source_groups;
    parser.tokenizer = &tokenizer;
    Ast_File ast_file = parse_file(&parser);
    if (ast_file.error_count > 0) {
        pln("\nErrors found during parsing, exiting...\n");
        return 1;
    }
    
    Data_Packer data_packer = {};
    // TODO: do we need worry about align from zero for JIT!?!?!?
    //rdata_arena.flags |= ArenaPushFlag_Align_From_Zero;
    //data_arena.flags |= ArenaPushFlag_Align_From_Zero;
    
    // Type checking
    Interp interp = {};
    Type_Context tcx = {};
    tcx.data_packer = &data_packer;
    if (type_check_ast_file(&tcx, &ast_file, &interp) != 0) {
        pln("\nErrors found during type checking, exiting...\n");
        return 1;
    }
    
    // Build the bytecode
    Bytecode_Builder bytecode_builder = {};
    bytecode_builder.data_packer = &data_packer;
    bytecode_builder.interp = &interp;
    
    for_array(ast_file.units, cu, _2) {
        if (!cu->bytecode_function && cu->ast->kind == Ast_Decl_Stmt) {
            
            Type* type = cu->ast->type;
            if (type->kind == TypeKind_Function) {
                
                Bytecode_Function* func = add_bytecode_function(&bytecode_builder, type);
                
                if (cu->ident == Sym___assert) {
                    // Route the __assert to our test case assert
                    func->is_imported = true;
                    func->code_ptr = &intrinsic_test_proc_assert;
                }
                
                if (cu->ident == Sym_print) {
                    // Route the __assert to our test case assert
                    func->is_imported = true;
                    func->code_ptr = &intrinsic_print;
                }
            }
            
        } else if (cu->ast->kind == Ast_Assign_Stmt) {
            Type* type = cu->ast->type;
            string_id ident = ast_unwrap_ident(cu->ast->Assign_Stmt.ident);
            
            void* data = get_interp_value_pointer(&interp, ident);
            if (!data) {
                type_error(&tcx, string_print("compiler bug: value of `%` is void", f_var(ident)),
                           cu->ast->span);
                assert(0);
            }
            
            int global_index = add_bytecode_global(&bytecode_builder, BC_MEM_READ_WRITE,
                                                   type->size, type->align, data);
            map_put(bytecode_builder.globals, ident, global_index);
        }
    }
    
    for_array(ast_file.units, cu, _3) {
        if (cu->bytecode_function) {
            bool is_main = cu->ident == Sym_main;
            //pln("Building function `%`...", f_type(cu->ast->type));
            emit_function(&bytecode_builder, cu->bytecode_function, cu->ast,
                          false, false);
        }
    }
    
    // Build initializer
    emit_initializer_function(&bytecode_builder);
    
    // Validate + optimize
    validate_bytecode(&bytecode_builder.bytecode);
    
    
    // Print the bytecode
    String_Builder sb = {};
    for_array(ast_file.units, cu, _4) {
        if (cu->ast && cu->ast->type && 
            cu->ast->type->kind == TypeKind_Function &&
            cu->ast->type->Function.dump_bytecode) {
            
            string_builder_dump_bytecode_function(&sb, &bytecode_builder.bytecode,
                                                  cu->bytecode_function);
        }
    }
    if (sb.data) {
        string_builder_dump_bytecode_globals(&sb, &bytecode_builder.bytecode);
        
        string s = string_builder_to_string_nocopy(&sb);
        pln("\nBytecode:\n%", f_string(s));
        string_builder_free(&sb);
    }
    
    
    // Compile to x64 code
    Buffer buf = {};
    buf.data = (u8*) asm_buffer;
    buf.size = asm_size;
    convert_bytecode_to_x64_machine_code(&bytecode_builder.bytecode,
                                         &buf, &data_packer,
                                         true);
    
    // Compile to WASM
    //convert_to_wasm_module();
    
    
    asm_make_executable(asm_buffer, asm_size);
    
    String_Builder string_builder_dump_bytecode = {};
    String_Builder* sb_dump_bytecode = &string_builder_dump_bytecode;
    
    // Collect all the test to run
    map(string_id, Test_Result)* tests = 0;
    for_array(ast_file.units, unit, _5) {
        Ast* decl = unit->ast;
        
        if (decl->kind == Ast_Decl_Stmt && decl->Decl_Stmt.type &&
            decl->Decl_Stmt.type->kind == Ast_Function_Type) {
            
            Ast* function = decl->Decl_Stmt.type;
            Ast* attributes = function->Function_Type.attributes;
            if (!attributes) continue;
            
            for_compound(attributes, attr) {
                string_id attr_ident = ast_unwrap_ident(attr->Attribute.ident);
                if (attr_ident == Sym_test_proc) {
                    // TODO(Alexander): check the expr part for exec mode
                    Test_Execution_Modes modes = TestExecutionMode_All;
                    
                    Test_Result test = {};
                    test.ident = unit->ident;
                    test.unit = unit;
                    test.modes = modes;
                    test.interp = &interp;
                    
                    if (decl->type->Function.dump_bytecode && unit->bytecode_function) {
                        string_builder_dump_bytecode_function(sb_dump_bytecode,
                                                              &bytecode_builder.bytecode, 
                                                              unit->bytecode_function);
                        
                    }
                    
                    assert(!map_key_exists(tests, unit->ident) && "duplicate test name");
                    map_put(tests, unit->ident, test);
                }
            }
            
            
        } else {
            continue;
        }
    }
    
    if (sb_dump_bytecode->data) {
        string s = string_builder_to_string_nocopy(sb_dump_bytecode);
        pln("\nBytecode:\n%", f_string(s));
        string_builder_free(sb_dump_bytecode);
    }
    
    
    String_Builder string_builder_failure_log = {};
    String_Builder* sb_failure_log = &string_builder_failure_log;
    
    // Run tests
    pln("Running % tests...\n", f_smm(map_count(tests)));
    
    if (!is_debugger_present) {
        DEBUG_begin_test_exception_handler();
    }
    
    // String builder for building result of particular test
    String_Builder string_builder_test_result = {};
    String_Builder* sb_test_result = &string_builder_test_result;
    string_builder_alloc(sb_test_result, 80);
    const umm test_name_max_count = 50;;
    
    Test_Result totals = {};
    totals.num_tests = (u32) map_count(tests);
    for_map(tests, it) {
        Test_Result* test = &it->value;
        string test_name = vars_load_string(it->key);
        if (test_name.count > test_name_max_count) {
            test_name.count = test_name_max_count;
        }
        
        // Print out summary for a particular test case
        string_builder_push_format(sb_test_result, "%", f_string(test_name));
        for (umm i = test_name.count; i < test_name_max_count + 3; i++) {
            string_builder_push_char(sb_test_result, '.');
        }
        print("%", f_string(string_builder_to_string_nocopy(sb_test_result)));
        string_builder_clear(sb_test_result);
        fflush(stdout);
        
        void* thread = DEBUG_create_thread(run_test, test);
        
        bool killed = false;
        if (is_debugger_present) {
            DEBUG_join_thread(thread);
        } else {
            killed = !DEBUG_join_thread_with_timeout(thread, 2000);
        }
        if (killed) {
            if (curr_test) {
                curr_test->num_failed++;
            }
            if (curr_execution) {
                curr_execution->failed = true;
                string_builder_push(&curr_execution->output, 
                                    "Test ran for too long, possibly an infinte loop\n");
            }
        }
        
        //if (test->fatal_error) {
        //totals.num_failed++;
        //string_builder_push_format(sb_test_result, " FATAL ERROR! Test crashed!\n");
        if (test->num_tests == 0) {
            string_builder_push_format(sb_test_result, " WARN! No asserts found!");
            totals.num_skipped++;
        } else if (test->num_failed == 0) {
            totals.num_passed++;
            string_builder_push_format(sb_test_result, " OK!");
        } else {
            totals.num_failed++;
            string_builder_push_format(sb_test_result, " Failed [%/%]", f_u32(test->num_passed), f_u32(test->num_tests));
        }
        pln("%", f_string(string_builder_to_string_nocopy(sb_test_result)));
        string_builder_clear(sb_test_result);
    }
    
    for_map(tests, it) {
        Test_Result* test = &it->value;
        string test_name = vars_load_string(it->key);
        
        for (int mode_index = 0; mode_index < TestExecutionMode_Count; mode_index++) {
            Test_Execution* exec = &test->exec[mode_index];
            if ((test->modes & bit(mode_index)) && exec->failed) {
                string_builder_push_format(sb_failure_log, 
                                           "- `%` failed (%) with output:\n",
                                           f_string(test_name),
                                           f_cstring(test_execution_names[mode_index]));
            } else if (exec->output.data) {
                string_builder_push_format(sb_failure_log, 
                                           "- `%` succeded (%) with output:\n",
                                           f_string(test_name),
                                           f_cstring(test_execution_names[mode_index]));
            }
            
            if (exec->output.data) {
                string s = string_builder_to_string_nocopy(&exec->output);
                string_builder_push(sb_failure_log, s);
                if (s.data && s.count > 0 && s.data[s.count - 1] != '\n') {
                    string_builder_push_char(sb_failure_log, '\n');
                }
                string_builder_push_char(sb_failure_log, '\n');
                string_builder_free(&exec->output);
            }
        }
        
    }
    
    pln("\n%", f_string(string_builder_to_string_nocopy(sb_failure_log)));
    pln("Finished % tests: % passed - % failed - % skipped", 
        f_u32(totals.num_tests),
        f_u32(totals.num_passed), 
        f_u32(totals.num_failed),
        f_u32(totals.num_skipped));
    
    string_builder_free(sb_failure_log);
    string_builder_free(sb_test_result);
    
    return 0;
}

#endif
