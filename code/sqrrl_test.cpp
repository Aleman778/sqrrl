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
run_compiler_tests(string filename, void* asm_buffer, umm asm_size, 
                   void (*asm_make_executable)(void*, umm)) {
    
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
    
    // Compile to X64 instructions
    X64_Builder x64_builder = {};
    x64_builder.bc_register_live_lengths = bytecode_builder.live_lengths;
    
    for_map (bytecode_builder.declarations, it) {
        if (it->key.ident == Kw_global || it->key.index != 0) {
            continue;
        }
        
        if (it->value.type == Value_basic_block) {
            Bc_Basic_Block* function_block = it->value.data.basic_block;
            pln("compiling function `%`", f_string(vars_load_string(it->key.ident)));
            String_Builder test_sb = {};
            string_builder_push(&test_sb, function_block);
            //pln("%", f_string(string_builder_to_string_nocopy(&test_sb)));
            string_builder_free(&test_sb);
            x64_build_function(&x64_builder, function_block);
        } else {
            // TODO(Alexander): we need to store the actual value type in the declarations
            x64_build_data_storage(&x64_builder, it->key, it->value.data, &global_primitive_types[PrimitiveType_int]);
        }
    }
    
    // Perform register allocation
    x64_perform_register_allocation(&x64_builder);
    
    // x64 build instruction definitions
    X64_Instruction_Def_Table* x64_instruction_definitions = parse_x86_64_definitions();
    
    // Assemble to X64 machine code
    X64_Assembler assembler = {};
    assembler.bytes = (u8*) asm_buffer;
    assembler.size = asm_size;
    
    x64_assemble_to_machine_code(&assembler,
                                 x64_instruction_definitions,
                                 x64_builder.first_basic_block);
    asm_make_executable(asm_buffer, asm_size);
    
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
                                                  TestExecutionMode_Bc_Interp |
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
        
        // Interpreter
        if (is_bitflag_set(test->modes, TestExecutionMode_Interp)) {
            interp_function_call(&interp, test->ident, 0, test->ast->type);
            if (interp.error_count > 0) {
                test->num_failed += interp.error_count;
                test->num_tests +=  interp.error_count;
                interp.error_count = 0;
            }
        }
        
        // Bytecode interpreter
        if (is_bitflag_set(test->modes, TestExecutionMode_Bc_Interp)) {
            u32 prev_num_failed = test->num_failed;
            bc_interp_bytecode(&bc_interp, test->ident);
            if (prev_num_failed != test->num_failed) {
                Bc_Register reg = {};
                reg.ident = test->ident;
                Value decl = map_get(bc_interp.declarations, reg);
                if (decl.type == Value_basic_block) {
                    string_builder_push(sb_failure_log, decl.data.basic_block);
                }
            }
        }
        
        // X64
        if (is_bitflag_set(test->modes, TestExecutionMode_X64)) {
            u32 prev_num_failed = test->num_failed;
            umm offset = map_get(assembler.label_offsets, test->ident);
            //pln("Running `%` at memory location: 0x%", f_string(vars_load_string(test->ident)),
            //f_u64_HEX((u8*) asm_buffer + offset));
            asm_test* test_func = (asm_test*) ((u8*) asm_buffer + offset);
            test_func();
            if (prev_num_failed != test->num_failed) {
                // TODO(Alexander): add some diagnostics
            }
        }
        
        const umm test_name_max_count = 50;
        string test_name = vars_load_string(test->ident);
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