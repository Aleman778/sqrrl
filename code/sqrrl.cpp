#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"

#include "sqrrl_value.cpp"
#include "sqrrl_types.cpp"
#include "sqrrl_test.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_preprocessor.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_type_checker.cpp"
#include "sqrrl_interp.cpp"
#include "sqrrl_x64_converter.cpp"
#include "sqrrl_pe_converter.cpp"
#include "sqrrl_pdb_converter.cpp"

typedef int asm_main(void);
typedef f32 asm_f32_main(void);


int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[], void* asm_buffer, umm asm_size,
                    void (*asm_make_executable)(void*, umm), bool is_debugger_present) {
    // TODO(Alexander): temporary use of C runtime RNG
    //srand((uint) time(0));
    //rand();
    
#if 0
    // TODO(Alexander): dumb example to print PDBs (turns out they suck, unspecified + no docs)
    dump_pdb();
    return 0;
#else
    
    
    {
        // Put dummy file as index 0
        Loaded_Source_File file = {};
        file.filename = string_lit("invalid");
        file.source = string_lit("");
        array_push(loaded_source_files, file);
        
        string_id ident = Kw_invalid;
        map_put(file_index_table, ident, 0);
    }
    
    string filepath = {};
    
    if (argc > 1) {
#if BUILD_TEST
        if (string_equals(string_lit(argv[1]), string_lit("test"))) {
            if (argc > 2) {
                filepath = string_lit(argv[2]);
            } else {
                filepath = string_lit("../tests/first.sq");
            }
            return run_compiler_tests(filepath, asm_buffer, asm_size, asm_make_executable,
                                      is_debugger_present);
        }
#endif
        
        filepath = string_lit(argv[1]);
        
    } else {
#if 1 // BUILD_DEBUG
        // TODO(Alexander): temporary files for testing
        //filepath = string_lit("../personal/first.sq");
        //filepath = string_lit("../modules/basic.sq");
        filepath = string_lit("../../platformer/code/win32_platform.cpp");
        //filepath = string_lit("../examples/backend_test.sq");
        //filepath = string_lit("../examples/raytracer/first.cpp");
        //filepath = string_lit("../tests/preprocessor.sq");
        
        //filepath = string_lit("../examples/simple.cpp");
        //filepath = string_lit("../examples/even_simpler.sq");
        //filepath = string_lit("simple.exe");
#else
        if (argc <= 1) {
            pln("Usage: sqrrl file.sq");
            return 0;
        }
#endif
    }
    
    string filename = filepath;
    
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
    
    // Read entire source file
    Loaded_Source_File file = read_entire_source_file(filename);
    
    if (!file.is_valid) {
        return -1;
    }
    
    // Print DOS stub
#if 0
    for (int byte_index = 0; byte_index < file.source.count; byte_index++) {
        if (byte_index == 0xb0) {
            return 0;
        }
        u8 byte = file.source.data[byte_index];
        if (byte > 0xF) {
            printf("0x%hhX, ", byte);
        } else {
            printf("0x0%hhX, ", byte);
        }
        
        if (byte_index % 10 == 9) {
            printf("\n");
        }
    }
    printf("\n\n");
#endif
    
    //pe_dump_executable(file.source);
    
    // TODO(Alexander): this is hackish solution, we should rely on OS service to do this
    working_directory = filepath;
    working_directory.count = 0;
    for (int index = 0; index < filepath.count; index++) {
        if (filepath.data[index] == '\\' || filepath.data[index] == '/') {
            working_directory.count = index + 1;
        }
    }
    
    Preprocessor preprocessor = {};
    
#if 0
    {
        // Create global pln macro
        Preprocessor_Macro pln_macro = {};
        pln_macro.source = string_lit("print(format##\"\\n\", __VA_ARGS__)");
        string_id format_id = Sym_format;
        map_put(pln_macro.arg_mapper, format_id, 0);
        pln_macro.is_functional = true;
        pln_macro.is_variadic = true;
        pln_macro.is_valid = true;
        
        string_id pln_id = Sym_pln;
        map_put(preprocessor.macros, pln_id, pln_macro);
    }
    
    {
        // Create global pln macro
        Preprocessor_Macro assert_macro = {};
        assert_macro.source = string_lit("(void) ((expr) || __assert(expr, #expr \" \" __VA_ARGS__, __FILE__, __LINE__))");
        string_id expr_id = Sym_expr;
        map_put(assert_macro.arg_mapper, expr_id, 0);
        assert_macro.is_functional = true;
        assert_macro.is_variadic = true;
        assert_macro.is_valid = true;
        
        string_id assert_id = Sym_assert;
        map_put(preprocessor.macros, assert_id, assert_macro);
    }
#endif
    
    string preprocessed_source = preprocess_file(&preprocessor, 
                                                 file.source, file.abspath, file.extension, file.index);
    
    //pln("Preprocessed % lines", f_s64(preprocessor.preprocessed_lines));
    
    bool flag_print_ast = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_AST));
    bool flag_run_ast_interp = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_RUN_AST_INTERP));
    bool flag_print_bc  = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_BYTECODE));
    bool flag_run_bc_interp  = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_RUN_BYTECODE_INTERP));
    bool flag_print_asm_vreg = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_ASM_VREG));
    bool flag_print_asm = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_ASM));
    
    // TODO(alexander): temp printing source
    //pln("Preprocessed source:\n%", f_string(preprocessed_source));
    //DEBUG_write_entire_file("preprocessed.sq", preprocessed_source.data,
    //(u32) preprocessed_source.count);
    
#if 0
    // Source group debugging
    for_array(preprocessor.source_groups, group, index) {
        pln("group(%): file_index: %, line: %, offset: %, count: %\nSource:", f_int(index), f_uint(group->file_index), f_uint(group->line), f_umm(group->offset), f_umm(group->count));
        
        string group_source = create_string(group->count, preprocessed_source.data + group->offset);
        pln("%\n\n", f_string(group_source));
    }
#endif
    
    if (preprocessor.error_count > 0) {
        pln("\nErrors (%) found during preprocessing, exiting...\n", f_u32(preprocessor.error_count));
        return 1;
    }
    
    // Lexer
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, preprocessed_source, file.abspath, file.index);
    // TODO(alexander): calculate line number!
    
    Parser parser = {};
    parser.source_groups = preprocessor.source_groups;
    parser.tokenizer = &tokenizer;
    Ast_File ast_file = parse_file(&parser);
    
    if (ast_file.error_count > 0) {
        if (flag_print_ast) {
            pln("AST (without types):");
            for_array(ast_file.units, unit, _) {
                print_ast(unit->ast, &tokenizer);
            }
        }
        
        pln("\nErrors found during parsing, exiting...\n");
        return 1;
    }
    
    // Typecheck the AST
    Interp interp = {};
    Type_Context tcx = {};
    
    if (type_check_ast_file(&tcx, &ast_file, &interp) != 0) {
        if (flag_print_ast) {
            pln("AST (not fully typed):");
            for_array(ast_file.units, unit, _) {
                print_ast(unit->ast, &tokenizer);
            }
        }
        
        pln("\nErrors found during type checking, exiting...\n");
        return 1;
    }
    
    if (flag_print_ast) {
        pln("AST:");
        for_array(ast_file.units, unit, _) {
            print_ast(unit->ast, &tokenizer);
        }
    }
    
    if (flag_run_ast_interp) {
        // Interpret the AST
        Type* function_type = load_type_declaration(&tcx, Sym_main, empty_span, true);
        assert(function_type->kind == TypeKind_Function);
        Interp_Value result = interp_function_call(&interp, 0, function_type);
        if (result.modifier == InterpValueMod_Return && is_integer(result.value)) {
            pln("AST interpreter exited with code %\n", f_int((int) result.value.data.signed_int));
        } else {
            pln("AST interpreter exited with code 0\n");
        }
        return 0;
    }
    
    Memory_Arena rdata_arena = {};
    Memory_Arena data_arena = {};
    
    // Start by pushing address lookup table for external libs
    for_map(tcx.import_table.libs, it) {
        for_array(it->value.functions, function, function_index) {
            if (function->type) {
                assert(function->type->kind == TypeKind_Function && 
                       function->type->Function.unit);
                
                Compilation_Unit* cu = function->type->Function.unit;
                
                Ic_Basic_Block* bb_begin = ic_basic_block();
                ic_add(cu, IC_LABEL, bb_begin);
                
                // Library function pointer is replaced by the loader
                void* fn_ptr = arena_push_size(&rdata_arena, 8, 8);
                Intermediate_Code* ic_jump = ic_add(cu, IC_JMP);
                //pln("-> %", f_var(cu->ident));
                ic_jump->src0 = ic_rip_disp32(IC_U64, IcDataArea_Read_Only, &rdata_arena, fn_ptr);
                cu->external_address = ic_jump->src0.data.disp;
            }
        }
        
        arena_push_size(&rdata_arena, 8, 8); // null entry
    }
    
    Ic_Arg_Map* x64_globals = 0;
    
    Compilation_Unit* main_cu = 0;
    for_array(ast_file.units, cu, _2) {
        cu->type_info_packer = &tcx.type_info_packer;
        cu->rdata_arena = &rdata_arena;
        cu->data_arena = &data_arena;
        cu->globals = x64_globals;
        if (cu->ast->kind == Ast_Decl_Stmt) {
            Type* type = cu->ast->type;
            if (type->kind == TypeKind_Function) {
                bool is_main = cu->ident == Sym_main;
                convert_procedure_to_intermediate_code(cu, is_debugger_present && is_main);
                
                if (is_main) {
                    main_cu = cu;
                }
            }
        }
        
        x64_globals = cu->globals;
    }
    assert(main_cu);
    
    // Compute the actual stack displacement for each Ic_Arg
    for_array(ast_file.units, cu, _3) {
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
            
            ic = ic->next;
        }
    }
    
    if (flag_print_bc) {
        pln("\nIntermediate code:");
        String_Builder sb = {};
        for_array(ast_file.units, cu, _4) {
            if (cu->ast->kind != Ast_Decl_Stmt) continue;
            
            if (cu->ast->type->kind == TypeKind_Function) {
                string_builder_push_format(&sb, "\n%:\n", f_type(cu->ast->type));
                
            } else {
                continue;
            }
            
            int bb_index = 0;
            Intermediate_Code* curr = cu->ic_first;
            while (curr) {
                if (curr->opcode == IC_LABEL) {
                    string_builder_push(&sb, "\n");
                }
                
                string_builder_push(&sb, curr, &bb_index);
                if (curr->next) {
                    string_builder_push(&sb, "\n");
                }
                curr = curr->next;
            }
            
            string s = string_builder_to_string_nocopy(&sb);
            pln("%", f_string(s));
            string_builder_free(&sb);
        }
    }
    
    // Convert to X64 machine code
    s64 rip = 0;
    for_array(ast_file.units, cu, _4) {
        rip = convert_to_x64_machine_code(cu->ic_first, cu->stk_usage, 0, 0, rip);
    }
    
    Type* type = main_cu->ast->type;
    if (type && type->kind == TypeKind_Function) {
        type = type->Function.return_type;
    }
    u8* asm_buffer_main = (u8*) asm_buffer + main_cu->bb_first->addr;
    
    // NOTE(Alexander): Build initial PE executable
    Memory_Arena build_arena = {};
    PE_Executable pe_executable = convert_to_pe_executable(&build_arena,
                                                           (u8*) asm_buffer, (u32) rip,
                                                           &tcx.import_table,
                                                           &tcx.type_info_packer,
                                                           &rdata_arena,
                                                           &data_arena,
                                                           asm_buffer_main);
    
    // TODO(Alexander): the PE section_alignment is hardcoded as 0x1000
    for_array(ast_file.units, cu, _5) {
        Intermediate_Code* ic = cu->ic_first;
        
        while (ic) {
            ic->dest = patch_rip_relative_address(&pe_executable, ic->dest);
            ic->src0 = patch_rip_relative_address(&pe_executable, ic->src0);
            ic->src1 = patch_rip_relative_address(&pe_executable, ic->src1);
            ic = ic->next;
        }
    }
    
    global_asm_buffer = (s64) asm_buffer;
    s64 rip2 = 0;
    for_array(ast_file.units, cu, _6) {
        rip2 = convert_to_x64_machine_code(cu->ic_first, cu->stk_usage,
                                           (u8*) asm_buffer, (s64) asm_size, rip2);
    }
    assert(rip == rip2);
    
    
#if 0
    pln("\nX64 Machine Code (% bytes):", f_umm(rip));
    for (int byte_index = 0; byte_index < rip; byte_index++) {
        u8 byte = ((u8*) asm_buffer)[byte_index];
        if (byte > 0xF) {
            printf("%hhX ", byte);
        } else {
            printf("0%hhX ", byte);
        }
        
        if (byte_index % 80 == 79) {
            printf("\n");
        }
    }
    printf("\n\n");
#endif
    
    //asm_make_executable(asm_buffer, asm_size);
    //asm_main* func = (asm_main*) asm_buffer;
    //int jit_exit_code = (int) func();
    //pln("JIT exited with code: %", f_int(jit_exit_code));
    
    // NOTE(Alexander): write the PE executable to file
    File_Handle exe_file = DEBUG_open_file_for_writing("simple.exe");
    write_pe_executable_to_file(exe_file, &pe_executable);
    DEBUG_close_file(exe_file);
    pln("\nWrote executable: simple.exe");
    
    //Read_File_Result exe_data = DEBUG_read_entire_file("simple.exe");
    //pe_dump_executable(create_string(exe_data.contents_size, (u8*) exe_data.contents));
    
#if 0
    // NOTE(Alexander): Run machine code
    asm_make_executable(asm_buffer, rip);
    //DEBUG_add_debug_symbols(&ast_file, (u8*) asm_buffer);
    
    if (working_directory.data) {
        cstring dir = string_to_cstring(working_directory);
        DEBUG_set_current_directory(dir);
        cstring_free(dir);
    }
    
    if (type == t_s32) {
        asm_main* func = (asm_main*) asm_buffer_main;
        int jit_exit_code = (int) func();
        pln("\nJIT exited with code: %", f_int(jit_exit_code));
    } else if (type == t_f32) {
        asm_f32_main* func = (asm_f32_main*) asm_buffer_main;
        f32 jit_exit_code = (f32) func();
        pln("\nJIT exited with code: %", f_float(jit_exit_code));
    } else {
        asm_main* func = (asm_main*) asm_buffer_main;
        func();
        pln("\nJIT exited with code: 0");
    }
#endif
    
    return 0;
#endif
}
