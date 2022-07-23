// Sqrrl compiler test harness

#if BUILD_TEST

// TODO(Alexander): this is just a simple assertion function to get started, improve this
// this is intended to be called from the compiled program
void
intrinsic_assert(int expr) {
    if (expr == 0) {
        curr_test->num_passed++;
        pln("test passed");
    } else {
        curr_test->num_failed++;
        pln("test failed");
    }
}

int
run_compiler_tests(string filename) {
    
    is_test_mode = true;
    
    // Setup string interning of variables
    vars_initialize();
    
    // Load source code
    Loaded_Source_File file = read_entire_source_file(filename);
    if (!file.is_valid) {
        return -1;
    }
    
    // Preprocess
    Preprocessor preprocessor = {};
    string preprocessed_source = preprocess_file(&preprocessor, file.source, file.filename, file.index);
    
    // Parse preprocessed source
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, preprocessed_source, filename);
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
    
    // Compile to sqrrl bytecode
    Bc_Builder bytecode_builder = {};
    bc_build_from_ast(&bytecode_builder, &ast_file);
    
    // Prepare bytecode interpreter
    Bc_Interp bc_interp = {};
    bc_interp.declarations = bytecode_builder.declarations;
    
    // Compile to X64 machine code
    //X64_Builder x64_builder = {};
    //x64_builder.bc_register_live_lengths = bytecode_builder.live_lengths;
    
    // Make sure to compile entry point function first
    //Bc_Register entry_point_label = { Sym_main, 0 };
    //Value main_decl = map_get(bytecode_builder.declarations, entry_point_label);
    //assert(main_decl.type == Value_basic_block);
    
    //Bc_Basic_Block* main_block = main_decl.data.basic_block;
    //x64_build_function(&x64_builder, main_block);
    
    // Collect all the test to run
    map(string_id, Test_Result)* tests = 0;
    for_map(ast_file.decls, it) {
        Ast* decl = it->value;
        
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
                                                  TestExecutionMode_Bytecode_Interp |
                                                  TestExecutionMode_X64);
                    
                    Test_Result test = {};
                    test.ast = decl;
                    test.ident = it->key;
                    test.modes = modes;
                    
                    map_put(tests, test.ident, test);
                }
            }
            
            
        } else {
            continue;
        }
    }
    
    // Run tests
    pln("Running % tests...", f_smm(map_count(tests)));
    
    
    for_map(tests, it) {
        Test_Result* test = &it->value;
        
        // Interpreter
        if (is_bitflag_set(test->modes, TestExecutionMode_Interp)) {
            interp_function_call(&interp, test->ident, 0, test->ast->type);
            if (interp.error_count > 0) {
                test->num_failed += interp.error_count;
                interp.error_count = 0;
            }
        }
    }
    
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
    
    return 0;
}

#endif
