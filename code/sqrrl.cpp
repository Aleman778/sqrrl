#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"

#include "sqrrl_value.cpp"
#include "sqrrl_test.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_preprocessor.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_type_checker.cpp"
#include "sqrrl_interp.cpp"
#include "sqrrl_bytecode_builder.cpp"
#include "sqrrl_bytecode_interp.cpp"
#include "sqrrl_x64_builder.cpp"
#include "sqrrl_x64_insn_def.cpp"
#include "sqrrl_x64_assembler.cpp"
#include "sqrrl_x64_converter.cpp"

typedef int asm_main(void);
typedef f32 asm_f32_main(void);

int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[], void* asm_buffer, umm asm_size,
                    void (*asm_make_executable)(void*, umm), bool is_debugger_present) {
    
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
#if BUILD_DEBUG
        // TODO(Alexander): temporary files for testing
        //filepath = string_lit("../personal/first.sq");
        filepath = string_lit("../examples/backend_test.sq");
        //filepath = string_lit("../examples/raytracer/first.sq");
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
    
    vars_initialize_keywords_and_symbols();
    
    // Read entire source file
    Loaded_Source_File file = read_entire_source_file(filename);
    
    if (!file.is_valid) {
        return -1;
    }
    
    // TODO(Alexander): this is hackish solution, we should rely on OS service to do this
    working_directory = filepath;
    working_directory.count = 0;
    for (int index = 0; index < filepath.count; index++) {
        if (filepath.data[index] == '\\' || filepath.data[index] == '/') {
            working_directory.count = index + 1;
        }
    }
    
    Preprocessor preprocessor = {};
    string preprocessed_source = preprocess_file(&preprocessor, file.source, file.abspath, file.index);
    
    bool flag_print_ast = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_AST));
    bool flag_run_ast_interp = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_RUN_AST_INTERP));
    bool flag_print_bc  = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_BYTECODE));
    bool flag_run_bc_interp  = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_RUN_BYTECODE_INTERP));
    bool flag_print_asm_vreg = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_ASM_VREG));
    bool flag_print_asm = value_to_bool(preprocess_eval_macro(&preprocessor, Sym_PRINT_ASM));
    
    // TODO(alexander): temp printing source
    //pln("Preprocessed source:\n%", f_string(preprocessed_source));
    //DEBUG_write_entire_file("build/preprocessed.sq", preprocessed_source.data,
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
    tokenizer_set_source(&tokenizer, preprocessed_source, file.abspath);
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
    if (type_check_ast_file(&ast_file) != 0) {
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
        Interp interp = {};
        interp_ast_declarations(&interp, ast_file.decls);
        Interp_Value result = interp_function_call(&interp, Sym_main, 0);
        if (result.modifier == InterpValueMod_Return && is_integer(result.value)) {
            pln("AST interpreter exited with code %\n", f_int((int) result.value.data.signed_int));
        } else {
            pln("AST interpreter exited with code 0\n");
        }
    }
    pln("sizeof Ic_Arg: %", f_smm(sizeof(Ic_Arg)));
    
    Compilation_Unit* main_cu = 0;
    for_array(ast_file.units, cu, _) {
        if (cu->ast->kind == Ast_Decl_Stmt) {
            Type* type = cu->ast->type;
            if (type->kind == TypeKind_Function) {
                convert_procedure_to_intermediate_code(cu, is_debugger_present);
                
                if (cu->ident == Sym_main) {
                    main_cu = cu;
                }
            }
        }
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
    
    
    pln("\nIntermediate code:");
    String_Builder sb = {};
    for_array(ast_file.units, cu, _2) {
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
                if (bb_index > 0) {
                    string_builder_push_format(&sb, "\nbb%:\n", f_int(bb_index));
                }
                bb_index++;
                
            } else {
                string_builder_push(&sb, "  ");
                
                if (curr->dest.type) {
                    string_builder_push(&sb, curr->dest);
                    string_builder_push(&sb, " = ");
                }
                
                string_builder_push(&sb, ic_opcode_names[curr->opcode]);
                
                if (curr->src0.type) {
                    string_builder_push(&sb, " ");
                    string_builder_push(&sb, curr->src0);
                    
                    if (curr->src1.type) {
                        string_builder_push(&sb, ", ");
                        string_builder_push(&sb, curr->src1);
                        
                    }
                }
                
                if (curr->next) {
                    string_builder_push(&sb, "\n");
                }
            }
            
            curr = curr->next;
        }
        
        string_builder_push(&sb, "\n");
    }
    
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
    
    // Convert to X64 machine code
    s64 rip = 0;
    for_array(ast_file.units, cu, _4) {
        rip = convert_to_x64_machine_code(cu->ic_first, cu->stk_usage, 0, 0, rip);
    }
    
    s64 rip2 = 0;
    for_array(ast_file.units, cu, _5) {
        rip2 = convert_to_x64_machine_code(cu->ic_first, cu->stk_usage,
                                           (u8*) asm_buffer, (s64) asm_size, rip2);
    }
    assert(rip == rip2);
    
#if 1
    pln("\nX64 Machine Code (% bytes):", f_umm(rip));
    for (int byte_index = 0; byte_index < rip; byte_index++) {
        u8 byte = ((u8*) asm_buffer)[byte_index];
        if (byte > 0xF) {
            printf("%hhX ", byte);
        } else {
            printf("0%hhX ", byte);
        }
        
        if (byte_index % 30 == 29) {
            printf("\n");
        }
    }
    printf("\n\n");
#endif
    
    //asm_make_executable(asm_buffer, asm_size);
    //asm_main* func = (asm_main*) asm_buffer;
    //int jit_exit_code = (int) func();
    //pln("JIT exited with code: %", f_int(jit_exit_code));
    
    Type* type = main_cu->ast->type;
    if (type && type->kind == TypeKind_Function) {
        type = type->Function.return_type;
    }
    
    asm_make_executable(asm_buffer, rip);
    void* asm_buffer_main = (u8*) asm_buffer + main_cu->bb_first->addr;
    
    if (type == t_s32) {
        asm_main* func = (asm_main*) asm_buffer_main;
        int jit_exit_code = (int) func();
        pln("\nJIT exited with code: %", f_int(jit_exit_code));
    } else if (type == t_f32) {
        asm_f32_main* func = (asm_f32_main*) asm_buffer_main;
        f32 jit_exit_code = (f32) func();
        pln("\nJIT exited with code: %", f_float(jit_exit_code));
    } else {
        pln("\nJIT exited with code: 0");
    }
    
    return 0;
}
