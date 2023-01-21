// Sqrrl compiler test harness

#if BUILD_TEST

typedef void asm_test(void);

// TODO(Alexander): this is just a simple assertion function to get started, improve this
// this is intended to be called from the compiled program
void
intrinsic_assert(int expr) {
    if (is_test_mode && curr_test) {
        curr_test->num_tests++;
        if (expr == 0) {
            curr_test->num_failed++;
        } else {
            curr_test->num_passed++;
        }
    } else {
        assert(expr && "assert from external code");
    }
}

int
run_compiler_tests(string filename, 
                   void* asm_buffer, umm asm_size, 
                   void (*asm_make_executable)(void*, umm),
                   bool is_debugger_present) {
    
    is_test_mode = true;
    
    vars_initialize_keywords_and_symbols();
    
    // Load source code
    Loaded_Source_File file = read_entire_source_file(filename);
    if (!file.is_valid) {
        return -1;
    }
    
    // Preprocess
    Preprocessor preprocessor = {};
    string preprocessed_source = preprocess_file(&preprocessor, 
                                                 file.source, file.filename, file.extension, file.index);
    
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
    
    // Type checking
    if (type_check_ast_file(&ast_file) != 0) {
        pln("\nErrors found during type checking, exiting...\n");
        return 1;
    }
    
    // Prepare interpreter
    Interp interp = {};
    interp_ast_declarations(&interp, ast_file.decls);
    
    // Convert to intermediate code
    for_array(ast_file.units, cu, _) {
        if (cu->ast->kind == Ast_Decl_Stmt) {
            Type* type = cu->ast->type;
            if (type->kind == TypeKind_Function) {
                convert_procedure_to_intermediate_code(cu, is_debugger_present);
            }
        }
    }
    
    // Compute the actual stack displacement for each Ic_Arg
    for_array(ast_file.units, cu, _2) {
        Intermediate_Code* ic = cu->ic_first;
        while (ic) {
            if (ic->dest.type & IC_STK) {
                ic->dest.disp = compute_stk_displacement(cu, ic->dest);
            }
            if (ic->src0.type & IC_STK) {
                ic->src0.disp = compute_stk_displacement(cu, ic->src0);
            }
            if (ic->src1.type & IC_STK) {
                ic->src1.disp = compute_stk_displacement(cu, ic->src1);
            }
        }
        
        cu->stk_usage = cu->stk_locals + cu->stk_args + cu->stk_caller_args;
    }
    
    
    // Convert to X64 machine code
    s64 rip = 0;
    for_array(ast_file.units, cu, _3) {
        rip = convert_to_x64_machine_code(cu->ic_first, cu->stk_usage, 0, 0, rip);
    }
    
    s64 rip2 = 0;
    for_array(ast_file.units, cu, _4) {
        rip2 = convert_to_x64_machine_code(cu->ic_first, cu->stk_usage,
                                           (u8*) asm_buffer, (s64) asm_size, rip2);
    }
    assert(rip == rip2);
    
    asm_make_executable(asm_buffer, asm_size);
    
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
                    Test_Execution_Modes modes = (TestExecutionMode_Interp |
                                                  TestExecutionMode_X64);
                    
                    Test_Result test = {};
                    test.unit = unit;
                    test.modes = modes;
                    
                    assert(map_key_exists(tests, unit->ident) && "duplicate test name");
                    map_put(tests, unit->ident, test);
                }
            }
            
            
        } else {
            continue;
        }
    }
    
    // Run tests
    pln("Running % tests...\n", f_smm(map_count(tests)));
    
    
    String_Builder string_builder_test_result = {};
    String_Builder* sb_test_result = &string_builder_test_result;
    string_builder_alloc(sb_test_result, 80);
    
    String_Builder string_builder_failure_log = {};
    String_Builder* sb_failure_log = &string_builder_failure_log;
    
    Test_Result totals = {};
    totals.num_tests = (u32) map_count(tests);
    for_map(tests, it) {
        Test_Result* test = &it->value;
        curr_test = test;
        
        const umm test_name_max_count = 50;
        Compilation_Unit* unit = test->unit;
        string test_name = vars_load_string(unit->ident);
        
        // Interpreter
        if (is_bitflag_set(test->modes, TestExecutionMode_Interp)) {
            u32 prev_num_failed = test->num_failed;
            interp_function_call(&interp, unit->ident, 0, unit->ast->type);
            if (interp.error_count > 0) {
                test->num_failed += interp.error_count;
                test->num_tests +=  interp.error_count;
                interp.error_count = 0;
            }
            
            if (prev_num_failed != test->num_failed) {
                string_builder_push(sb_failure_log, "\n\xE2\x9D\x8C ");
                string_builder_push_format(sb_failure_log, 
                                           "AST interpreter failed procedure `%`\n",
                                           f_string(test_name));
            }
        }
        
        // X64
        if (is_bitflag_set(test->modes, TestExecutionMode_X64)) {
            u32 prev_num_failed = test->num_failed;
            
            s64 offset = unit->bb_first->addr;
            //pln("Running `%` at memory location: 0x%", f_string(vars_load_string(test->ident)),
            //f_u64_HEX((u8*) asm_buffer + offset));
            asm_test* test_func = (asm_test*) ((u8*) asm_buffer + offset);
            test_func();
            if (prev_num_failed != test->num_failed) {
                string_builder_push(sb_failure_log, "\n\xE2\x9D\x8C ");
                string_builder_push_format(sb_failure_log, 
                                           "X64 JIT failed procedure `%`\n",
                                           f_string(test_name));
            }
        }
        
        // Log test result overview
        if (test_name.count > test_name_max_count) {
            test_name.count = test_name_max_count;
        }
        
        string_builder_clear(sb_test_result);
        string_builder_push_format(sb_test_result, "%", f_string(test_name));
        for (umm i = test_name.count; i < test_name_max_count + 3; i++) string_builder_push(sb_test_result, ".");
        
        if (test->num_failed == 0) {
            totals.num_passed++;
            string_builder_push_format(sb_test_result, " OK!");
        } else {
            totals.num_failed++;
            string_builder_push_format(sb_test_result, " Failed [%/%]", f_u32(test->num_passed), f_u32(test->num_tests));
        }
        
        pln("%", f_string(string_builder_to_string_nocopy(sb_test_result)));
    }
    
    pln("%", f_string(string_builder_to_string_nocopy(sb_failure_log)));
    pln("Finished % tests: % passed - % failed - % skipped", 
        f_u32(totals.num_tests),
        f_u32(totals.num_passed), 
        f_u32(totals.num_failed),
        f_u32(totals.num_skipped));
    
    string_builder_free(sb_failure_log);
    string_builder_free(sb_test_result);
    
    
#if 0
    // Create test report in text
    {
        String_Builder test_report_text = {};
        String_Builder* sb = &test_report_text;
        
        for_map(tests, it) {
            Test_Result* test = &it->value;
            
            const umm test_name_max_count = 50;
            string test_name = vars_load_string(test->ident);
            if (test_name.count > test_name_max_count) {
                test_name.count = test_name_max_count;
            }
            
            string_builder_push_format(sb, "%", f_string(test_name));
            for (umm i = test_name.count; i < test_name_max_count + 3; i++) string_builder_push(sb, ".");
            
            if (test->num_failed == 0) {
                string_builder_push(sb, " OK!\n");
            } else {
                string_builder_push(sb, " Failed\n");
            }
        }
        
        pln("%", f_string(string_builder_to_string_nocopy(sb)));
        string_builder_free(sb);
    }
#endif
    
    return 0;
}

#endif
