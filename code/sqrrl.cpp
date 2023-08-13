#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"

#include "sqrrl_value.cpp"
#include "sqrrl_data.cpp"
#include "sqrrl_types.cpp"
#include "sqrrl_test.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_preprocessor.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_type_checker.cpp"
#include "sqrrl_interp.cpp"
#include "sqrrl_bytecode_builder.cpp"
#include "sqrrl_x64_converter.cpp"
#include "sqrrl_pe_converter.cpp"
#include "sqrrl_pdb_converter.cpp"
#include "sqrrl_wasm_converter.cpp"

typedef int asm_main(void);
typedef f32 asm_f32_main(void);

int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[], void* asm_buffer, umm asm_size,
                    void (*asm_make_executable)(void*, umm), bool is_debugger_present) {
    
    Compiler_Task compiler_task = CompilerTask_Build;//CompilerTask_Run;
    Backend_Type target_backend = Backend_X64;
    
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
    string output_filepath = {};
    
    if (argc > 1) {
#if BUILD_TEST
        if (string_equals(string_lit(argv[1]), string_lit("test"))) {
            if (argc > 2) {
                filepath = string_lit(argv[2]);
            } else {
                filepath = string_lit("../tests/first.sq");
                working_directory = string_lit("../tests/");
            }
            return run_compiler_tests(filepath, asm_buffer, asm_size, asm_make_executable,
                                      is_debugger_present);
        }
#endif
        
        if (argc > 2) {
            if (string_equals(string_lit(argv[1]), string_lit("-wasm"))) {
                target_backend = Backend_WASM;
            } else if (string_equals(string_lit(argv[1]), string_lit("-x64"))) {
                target_backend = Backend_X64;
            }
            
            filepath = string_lit(argv[2]);
        } else {
            filepath = string_lit(argv[1]);
        }
        
        if (argc > 3) {
            if (string_equals(string_lit(argv[2]), string_lit("-output"))) {
                output_filepath = string_lit(argv[3]);
            }
        }
    } else {
#if  BUILD_DEBUG
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
    
    string output_filename = output_filepath;
    if (!output_filename.data) {
        // TODO(Alexander): this is kind of a HACK to extract the name without extension and replacing with new extension
        string name_part = string_view(file.abspath.data + file.filedir.count, 
                                       file.abspath.data + file.abspath.count - file.extension.count - 1);
        output_filename = string_concat(name_part, ".exe");
    }
    
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
    
    {
        string zero = string_lit("0");
        string one = string_lit("1");
        Preprocessor_Macro define_target = {};
        define_target.is_integral = true;
        define_target.is_valid = true;
        string_id ident;
        ident = vars_save_cstring("BUILD_TARGET_X64");
        define_target.integral = target_backend == Backend_X64;
        define_target.source = target_backend == Backend_X64 ? one : zero;
        map_put(preprocessor.macros, ident, define_target);
        
        ident = vars_save_cstring("BUILD_TARGET_WASM");
        define_target.integral = target_backend == Backend_WASM;
        define_target.source = target_backend == Backend_WASM ? one : zero;
        map_put(preprocessor.macros, ident, define_target);
    }
    
    
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
    
    Data_Packer data_packer = {};
    data_packer.rdata_arena.flags |= ArenaPushFlag_Align_From_Zero;
    data_packer.data_arena.flags |= ArenaPushFlag_Align_From_Zero;
    Ic_Arg_Map* x64_globals = 0;
    
    // Typecheck the AST
    Interp interp = {};
    Type_Context tcx = {};
    tcx.target_backend = target_backend;
    tcx.data_packer = &data_packer;
    
    if (type_check_ast_file(&tcx, &ast_file, &interp) != 0) {
        for_array(ast_file.units, cu, _) {
            if (!(cu->ast && cu->ast->kind == Ast_Decl_Stmt)) continue;
            
            if (flag_print_ast || (cu->ast->type->kind == TypeKind_Function &&
                                   cu->ast->type->Function.dump_ast)) {
                print_ast(cu->ast, &tokenizer);
            }
        }
        
        pln("\nErrors found during type checking, exiting...\n");
        return 1;
    }
    
    for_array(ast_file.units, cu, _) {
        if (!(cu->ast && cu->ast->kind == Ast_Decl_Stmt)) continue;
        
        if (flag_print_ast || (cu->ast->type->kind == TypeKind_Function &&
                               cu->ast->type->Function.dump_ast)) {
            print_ast(cu->ast, &tokenizer);
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
    
    Bytecode_Builder bytecode_builder = {};
    bytecode_builder.data_packer = &data_packer;
    bytecode_builder.interp = &interp;
    
    
    if (target_backend == Backend_X64) {
        bytecode_builder.use_absolute_memory = compiler_task == CompilerTask_Run;
        
    } else if (target_backend == Backend_WASM) {
        bytecode_builder.pointer_type = BytecodeType_i32;
    }
    
    // First create functions imported from libraries
    for_map(tcx.import_table.libs, it) {
        for_array(it->value.functions, function, function_index) {
            Type* type = function->type;
            assert(type && type->kind == TypeKind_Function);
            
            Bytecode_Function* func = begin_bytecode_function(&bytecode_builder, type);
            func->is_imported = true;
            func->code_ptr = type->Function.external_address;
            if (type->Function.unit) {
                type->Function.unit->bc_func = func;
            }
            
            Bytecode_Import import = {};
            import.module = it->key;
            import.function = function->name;
            
            // TODO(Alexander): hack this doesn't really belong here
            // but this is needed for now to make this data appear first in the rdata section.
            if (target_backend == Backend_X64 && compiler_task == CompilerTask_Build) {
                // Start by pushing address lookup table for external libs
                // NOTE(Alexander): library function pointer is replaced by the loader
                Exported_Data import_fn = export_size(&data_packer, Read_Data_Section, 8, 8);
                import.rdata_offset = import_fn.relative_ptr;
            }
            
            array_push(bytecode_builder.bytecode.imports, import);
            end_bytecode_function(&bytecode_builder);
        }
        
        if (target_backend == Backend_X64 && compiler_task == CompilerTask_Build) {
            export_size(&data_packer, Read_Data_Section, 8, 8); // null entry
        }
    }
    
    // Build the bytecode
    Compilation_Unit* main_cu = 0;
    for_array(ast_file.units, cu, _2) {
        if (cu->bc_func) continue;
        
        cu->data_packer = &data_packer;
        cu->globals = x64_globals;
        if (cu->ast->kind == Ast_Decl_Stmt) {
            Type* type = cu->ast->type;
            if (type->kind == TypeKind_Function) {
                bool is_main = cu->ident == Sym_main;
                cu->bc_func = convert_function_to_bytecode(&bytecode_builder, cu->ast,
                                                           is_main, is_debugger_present && is_main);
                
                if (is_main) {
                    main_cu = cu;
                }
            }
        }
        
        x64_globals = cu->globals;
    }
    assert(main_cu && "no main function"); // TODO: turn this into an actual error
    
    // Print the bytecode
    String_Builder sb = {};
    for_array(ast_file.units, cu, _3) {
        if (flag_print_bc || (cu->ast && cu->ast->type && 
                              cu->ast->type->kind == TypeKind_Function &&
                              cu->ast->type->Function.dump_bytecode)) {
            string_builder_dump_bytecode(&sb, &bytecode_builder.bytecode, cu->bc_func);
        }
    }
    if (sb.data) {
        string s = string_builder_to_string_nocopy(&sb);
        pln("\nBytecode:\n%", f_string(s));
        string_builder_free(&sb);
    }
    
    switch (target_backend) {
        case Backend_X64: {
            Type* main_func_return_type = main_cu->ast->type;
            if (main_func_return_type && main_func_return_type->kind == TypeKind_Function) {
                main_func_return_type = main_func_return_type->Function.return_type;
            }
            
            Buffer buf = {};
            buf.data = (u8*) asm_buffer;
            buf.size = asm_size;
            
            u8* asm_buffer_main = convert_bytecode_to_x64_machine_code(&bytecode_builder.bytecode, 
                                                                       &buf, &data_packer, 
                                                                       &tcx.import_table, 
                                                                       compiler_task);
            
#if 1
            pln("\nX64 Machine Code (% bytes):", f_umm(buf.curr_used));
            for (int byte_index = 0; byte_index < buf.curr_used; byte_index++) {
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
            
            if (compiler_task == CompilerTask_Run) {
                // NOTE(Alexander): Run machine code
                asm_make_executable(buf.data, buf.curr_used);
                //DEBUG_add_debug_symbols(&ast_file, (u8*) asm_buffer);
                
                fflush(stdout);
                fflush(stderr);
                
                if (working_directory.data) {
                    cstring dir = string_to_cstring(working_directory);
                    DEBUG_set_current_directory(dir);
                    cstring_free(dir);
                }
                
                if (main_func_return_type == t_s32) {
                    asm_main* func = (asm_main*) asm_buffer_main;
                    int jit_exit_code = (int) func();
                    pln("\nJIT exited with code: %", f_int(jit_exit_code));
                } else if (main_func_return_type == t_f32) {
                    asm_f32_main* func = (asm_f32_main*) asm_buffer_main;
                    f32 jit_exit_code = (f32) func();
                    pln("\nJIT exited with code: %", f_float(jit_exit_code));
                } else {
                    asm_main* func = (asm_main*) asm_buffer_main;
                    func();
                    pln("\nJIT exited with code: 0");
                }

                
            } else if (compiler_task == CompilerTask_Build) {
                
#if 0
                // Compute the actual stack displacement for each Ic_Arg
                for_array(ast_file.units, cu, _6) {
                    Intermediate_Code* ic = cu->ic_first;
                    while (ic) {
                        
                        // TODO: robustness we need a better way to know what the instruction data is!
                        if (ic->opcode >= IC_NEG && ic->opcode <= IC_SETNE) {
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
                        
                        ic = ic->next;
                    }
                }
                
                return 0;
                
                // Convert to X64 machine code
                s64 rip = 0;
                for_array(ast_file.units, cu, _7) {
                    unimplemented;
                    //rip = convert_to_x64_machine_code(cu->first, 0, 0, 0, rip);
                }
                
                // NOTE(Alexander): Build initial PE executable
                Memory_Arena build_arena = {};
                PE_Executable pe_executable = convert_to_pe_executable(&build_arena,
                                                                       (u8*) asm_buffer, (u32) rip,
                                                                       &tcx.import_table,
                                                                       &data_packer,
                                                                       asm_buffer_main);
                
                
                // TODO(Alexander): the PE section_alignment is hardcoded as 0x1000
                for_array(ast_file.units, cu, _8) {
                    Intermediate_Code* ic = cu->ic_first;
                    
                    while (ic) {
                        // TODO: we should have instruction type or something?
                        if (ic->opcode >= IC_NEG && ic->opcode <= IC_SETNE) {
                            ic->dest = patch_rip_relative_address(&pe_executable, ic->dest);
                            ic->src0 = patch_rip_relative_address(&pe_executable, ic->src0);
                            ic->src1 = patch_rip_relative_address(&pe_executable, ic->src1);
                        } else if (ic->opcode == IC_JMP_INDIRECT) {
                            ic->src0 = patch_rip_relative_address(&pe_executable, ic->src0);
                        }
                        ic = ic->next;
                    }
                }
                
                global_asm_buffer = (s64) asm_buffer;
                s64 rip2 = 0;
                for_array(ast_file.units, cu, _9) {
                    rip2 = convert_to_x64_machine_code(cu->ic_first, cu->stk_usage,
                                                       (u8*) asm_buffer, 0, (s64) asm_size, rip2);
                }
                assert(rip == rip2);
                
                //asm_make_executable(asm_buffer, asm_size);
                //asm_main* func = (asm_main*) asm_buffer;
                //int jit_exit_code = (int) func();
                //pln("JIT exited with code: %", f_int(jit_exit_code));
                
                // NOTE(Alexander): write the PE executable to file
                cstring exe_filename = string_to_cstring(output_filename);
                File_Handle exe_file = DEBUG_open_file_for_writing(exe_filename);
                cstring_free(exe_filename);
                write_pe_executable_to_file(exe_file, &pe_executable);
                DEBUG_close_file(exe_file);
                pln("\nWrote executable: %", f_string(output_filename));
                
                //Read_File_Result exe_data = DEBUG_read_entire_file("simple.exe");
                //pe_dump_executable(create_string(exe_data.contents_size, (u8*) exe_data.contents));
#endif
            }
        } break;
        
        case Backend_WASM: {
            
            Buffer buffer = {};
            buffer.data = (u8*) asm_buffer;
            buffer.size = asm_size;
            
            convert_to_wasm_module(&bytecode_builder.bytecode, &data_packer, 0, &buffer);
            
            File_Handle wasm_file = DEBUG_open_file_for_writing("simple.wasm");
            DEBUG_write(wasm_file, buffer.data, (u32) buffer.curr_used);
            DEBUG_close_file(wasm_file);
            pln("\nWrote executable: simple.wasm");
            
            pln("\nRunning: `wasm2wat simple.wasm`:");
            fflush(stdout);
            system("wasm2wat simple.wasm");
        } break;
    }
    
    return 0;
}
