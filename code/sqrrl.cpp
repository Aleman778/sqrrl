#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"

#include "sqrrl_value.cpp"
#include "sqrrl_types.cpp"
//#include "sqrrl_test.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_type_checker.cpp"
#include "sqrrl_interp.cpp"
#include "sqrrl_bytecode_builder.cpp"
#include "sqrrl_x64_instructions.cpp"
#include "sqrrl_x64_converter.cpp"
#include "sqrrl_pe_converter.cpp"
#include "sqrrl_pdb_converter.cpp"
#include "sqrrl_wasm_instructions.cpp"
#include "sqrrl_wasm_converter.cpp"

typedef int asm_main(void);
typedef f32 asm_f32_main(void);


struct Parsed_Args {
    string filename;
    string output_filename;
    string working_directory;
    Backend_Type backend;
    Compiler_Task task;
};

Parsed_Args
compiler_parse_args(int argc, char** argv) {
    Parsed_Args result = {};
    result.task = CompilerTask_Build;
    result.backend = Backend_X64;
    
    for (int arg_index = 1; arg_index < argc; arg_index++) {
        string arg = string_lit(argv[arg_index]);
        
        if (string_equals(arg, string_lit("test"))) {
            if (arg_index + 1 < argc) {
                result.filename = string_lit(argv[++arg_index]);
            } else {
                result.filename = string_lit("../tests/first.sq");
                result.working_directory = string_lit("../tests/");
            }
            result.task = CompilerTask_Test;
            
        } else if (string_equals(arg, string_lit("-wasm"))) {
            result.backend = Backend_WASM;
            
        } else if (string_equals(arg, string_lit("-x64"))) {
            result.backend = Backend_X64;
            
        } else if (string_equals(arg, string_lit("-run"))) {
            result.task = CompilerTask_Run;
            
        } else if (string_equals(arg, string_lit("-output"))) {
            if (arg_index + 1 < argc) {
                result.output_filename = string_lit(argv[++arg_index]);
            } else {
                result.task = CompilerTask_Exit;
                pln("error: expected file name after -output");
                return result;
            }
            
        } else {
            if (result.filename.data) {
                result.task = CompilerTask_Exit;
                pln("error: expected only single filename as input");
                return result;
            }
            result.filename = arg;
        }
    }
    
    if (!result.filename.data) {
        result.task = CompilerTask_Exit;
        pln("Usage: sqrrl file.sq");
        return result;
    }
    
    return result;
}

void 
test_exception_handler(u32 exception_code, string message) {
    unimplemented;
};

int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[], void* asm_buffer, umm asm_size,
                    void (*asm_make_executable)(void*, umm), bool is_debugger_present) {
    
    Parsed_Args compiler = compiler_parse_args(argc, argv);
    
    if (compiler.task == CompilerTask_Exit) {
        return 0;
    }
    
    if (compiler.task == CompilerTask_Test) {
        working_directory = compiler.working_directory;
        unimplemented;
        return 0; // TODO(Alexander): add back the test runner
        //return run_compiler_tests(compiler.filename, asm_buffer, asm_size, asm_make_executable,
        //is_debugger_present);
    }
    
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
    
    Data_Packer data_packer = {};
    data_packer.rdata_arena.flags |= ArenaPushFlag_Align_From_Zero;
    data_packer.data_arena.flags |= ArenaPushFlag_Align_From_Zero;
    
    Interp interp = {};
    
    Type_Context tcx = {};
    init_type_context(&tcx, &interp, &data_packer, compiler.backend);
    tcx.scope = &interp.global_scope;
    
    {
        // TODO(Alexander): make it possible to override these values
        interp_put_global(&interp, 
                          vars_save_cstring("OS_WINDOWS"),
                          t_s64,
                          create_signed_int_value(OS_WINDOWS));
        
        interp_put_global(&interp, 
                          vars_save_cstring("OS_LINUX"),
                          t_s64,
                          create_signed_int_value(OS_LINUX));
        
        interp_put_global(&interp,
                          vars_save_cstring("OS_WEB"),
                          t_s64,
                          create_signed_int_value(compiler.backend == Backend_WASM));
        
        interp_put_global(&interp,
                          vars_save_cstring("ARCH_X64"),
                          t_s64,
                          create_signed_int_value(compiler.backend == Backend_X64));
        
        interp_put_global(&interp,
                          vars_save_cstring("ARCH_WASM32"),
                          t_s64,
                          create_signed_int_value(compiler.backend == Backend_WASM));
    }
    
    interp_push_source_file(&interp, compiler.filename);
    if (!compiler.output_filename.data) {
        Source_File* file = interp.source_files[0];
        string name_part = string_view(file->abspath.data + file->filedir.count, 
                                       file->abspath.data + file->abspath.count - file->extension.count - 1);
        // TODO(Alexander): hardcoded .exe
        compiler.output_filename = string_concat(name_part, ".exe");
    }
    
    // TODO(Alexander): this is hackish solution, we should rely on OS service to do this
    {
        working_directory = compiler.filename;
        working_directory.count = 0;
        string filename = compiler.filename;
        for (int index = 0; index < compiler.filename.count; index++) {
            if (filename.data[index] == '\\' || filename.data[index] == '/') {
                working_directory.count = index + 1;
            }
        }
    }
    
    // Parse source files in our module
    Ast_Module ast_module = {};
    Parser parser = {};
    for (int i = 0; i < array_count(interp.source_files); i++) {
        Source_File* file = interp.source_files[i];
        pln("Parsing file `%`...", f_string(file->abspath));
        Ast_File* ast_file = parse_file(&parser, file);
        array_push(ast_module.files, ast_file);
    }
    
    if (parser.error_count > 0) {
        pln("\nErrors found during parsing, exiting...\n");
        return 1;
    }
    
    // Type infer directives
    array(Compilation_Unit)* compilation_units = 0;
    for (int ast_file_index  = 0;
         ast_file_index < array_count(ast_module.files);
         ast_file_index++) {
        Ast_File* ast_file = ast_module.files[ast_file_index];
        
        for (int decl_index = 0;
             decl_index < array_count(ast_file->declarations);
             decl_index++) {
            Ast* decl = ast_file->declarations[decl_index];
            if (is_ast_directive(decl)) {
                Compilation_Unit cu = {};
                cu.ast = decl;
                array_push(compilation_units, cu);
            }
        }
    }
    pln("Before:");
    for_array_it(compilation_units, cu) {
        print_ast(cu->ast);
    }
    
    
    run_type_checker(&tcx, compilation_units);
    
    pln("After:");
    for_array_it(compilation_units, cu) {
        print_ast(cu->ast);
    }
    
    array_free(compilation_units);
    
    // Typecheck the AST
    run_type_checker(&tcx, compilation_units);
    if (tcx.error_count == 0 && !tcx.entry_point) {
        type_error(&tcx, string_lit("`main` function must be defined"), empty_span);
        return 1;
    }
    
    bool flag_dump_ast    = value_to_bool(interp_get_value(&interp, Sym_DUMP_AST));
    bool flag_dump_bc     = value_to_bool(interp_get_value(&interp, Sym_DUMP_BYTECODE));
    bool flag_dump_disasm = value_to_bool(interp_get_value(&interp, Sym_DUMP_DISASM));
    
    if (tcx.error_count != 0) {
        for_array(compilation_units, cu, _) {
            if (!(cu->ast && cu->ast->kind == Ast_Decl_Stmt)) continue;
            
            if (flag_dump_ast || (cu->ast->type->kind == TypeKind_Function &&
                                  cu->ast->type->Function.dump_ast)) {
                print_ast(cu->ast);
            }
        }
        
        pln("\nErrors found during type checking, exiting...\n");
        return 1;
    }
    
    for_array(compilation_units, cu, _) {
        if (!(cu->ast && cu->ast->kind == Ast_Decl_Stmt)) continue;
        
        if (flag_dump_ast || (cu->ast->type->kind == TypeKind_Function &&
                              cu->ast->type->Function.dump_ast)) {
            print_ast(cu->ast);
        }
    }
    
    Bytecode_Builder bytecode_builder = {};
    bytecode_builder.data_packer = &data_packer;
    bytecode_builder.interp = &interp;
    
    // First create functions imported from libraries
    for_map(tcx.import_table.libs, it) {
        
        Exported_Data library = {};
        for_array(it->value.functions, function, function_index) {
            Type* type = function->type;
            assert(type && type->kind == TypeKind_Function);
            
            Bytecode_Function* func = add_bytecode_function(&bytecode_builder, type);
            func->is_imported = true;
            
            Bytecode_Import import = {};
            import.module = it->key;
            import.name = function->name;
            
            
            if (it->value.resolve_at_runtime) {
                if (!library.data) {
                    library = export_struct(&data_packer, Dynamic_Library, Data_Section);
                    int lib_global = add_bytecode_global(&bytecode_builder, library);
                    map_put(bytecode_builder.globals, it->key, lib_global);
                }
                
                Dynamic_Library* dynamic_library = (Dynamic_Library*) library.data;
                dynamic_library->count++;
                
                // Library function pointer is replaced by user at runtime
                Exported_Data import_fn = export_struct(&data_packer, Dynamic_Function, Data_Section);
                int global_func_index = add_bytecode_global(&bytecode_builder, import_fn);
                import.kind = BC_IMPORT_GLOBAL;
                import.global_index = global_func_index;
                
                Dynamic_Function* dynamic_fn = (Dynamic_Function*) import_fn.data;
                func->code_ptr = &dynamic_fn->pointer;
                Exported_Data fn_name = export_string(&data_packer, function->name);
                dynamic_fn->name = fn_name.str;
                push_relocation(&data_packer, add_offset(import_fn, 8), fn_name);
                
                if (!dynamic_library->functions) {
                    dynamic_library->functions = dynamic_fn;
                    push_relocation(&data_packer, library, import_fn);
                }
                
            } else {
                import.kind = BC_IMPORT_FUNC;
                import.func_index = func->type_index;
                
                func->code_ptr = type->Function.external_address;
                function->bc_func_index = func->type_index;
            }
            
            array_push(bytecode_builder.bytecode.imports, import);
        }
    }
    
    for_array(compilation_units, cu, _2) {
        if (!cu->bytecode_function && cu->ast->kind == Ast_Decl_Stmt) {
            Type* type = cu->ast->type;
            if (type->kind == TypeKind_Function) {
                add_bytecode_function(&bytecode_builder, type);
            }
            
        } else if (cu->ast->kind == Ast_Assign_Stmt) {
            Type* type = cu->ast->type;
            string_id ident = ast_unwrap_ident(cu->ast->Assign_Stmt.ident);
            
            if (map_key_exists(bytecode_builder.globals, ident)) {
                type_error(&tcx, string_print("cannot redeclare global `%`", f_var(ident)),
                           cu->ast->span);
                continue;
            }
            
            void* data = interp_get_data_pointer(&interp, ident);
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
    
    // Build the bytecode
    for_array(compilation_units, cu, _3) {
        if (cu->bytecode_function) {
            bool is_main = cu->is_main;
            emit_function(&bytecode_builder, cu->bytecode_function, cu->ast,
                          is_main, is_debugger_present && is_main);
        }
    }
    
    // Build initializer
    emit_initializer_function(&bytecode_builder);
    
    // Validate + optimize
    validate_bytecode(&bytecode_builder.bytecode);
    
    // Print the bytecode
    String_Builder sb = {};
    for_array(compilation_units, cu, _4) {
        if (flag_dump_bc || (cu->ast && cu->ast->type && 
                             cu->ast->type->kind == TypeKind_Function &&
                             cu->ast->type->Function.dump_bytecode)) {
            
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
    
    switch (compiler.backend) {
        case Backend_X64: {
            Buffer buf = {};
            buf.data = (u8*) asm_buffer;
            buf.size = asm_size;
            
            X64_Assembler x64 = convert_bytecode_to_x64_machine_code(&bytecode_builder.bytecode,
                                                                     &buf, &data_packer, 
                                                                     compiler.task == CompilerTask_Run);
            
            if (compiler.task == CompilerTask_Run) {
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
                
                Bytecode* bc = &bytecode_builder.bytecode;
                Bytecode_Function* main_func = bc->functions[bc->entry_func_index];
                u8* code_entry_point = (u8*) main_func->code_ptr;
                if (main_func->ret_count == 1) {
                    Bytecode_Type ret_type = function_ret_types(main_func)->type;
                    
                    if (ret_type.kind == BC_TYPE_INT) {
                        asm_main* func = (asm_main*) code_entry_point;
                        int jit_exit_code = (int) func();
                        pln("\nJIT exited with code: %", f_int(jit_exit_code));
                        
                    } else if (ret_type.kind == BC_TYPE_FLOAT) {
                        asm_f32_main* func = (asm_f32_main*) code_entry_point;
                        f32 jit_exit_code = (f32) func();
                        pln("\nJIT exited with code: %", f_float(jit_exit_code));
                    } 
                } else {
                    asm_main* func = (asm_main*) code_entry_point;
                    func();
                    pln("\nJIT exited with code: 0");
                }
                
            } else if (compiler.task == CompilerTask_Build) {
                
                // NOTE(Alexander): Build initial PE executable
                Bytecode* bc = &bytecode_builder.bytecode;
                u8* entry_point = (u8*) bc->functions[bc->entry_func_index]->code_ptr;
                PE_Executable pe = convert_to_pe_executable(buf.data, (u32) buf.curr_used,
                                                            &tcx.import_table,
                                                            &data_packer,
                                                            entry_point);
                
                // TODO(Alexander): HACK we need Import Address Table in the bytecode imports
                for_map(tcx.import_table.libs, it) {
                    for_array(it->value.functions, function, function_index) {
                        Bytecode_Import* import = &bc->imports[function->bc_func_index];
                        if (import->kind == BC_IMPORT_FUNC) {
                            import->iat_offset = function->relative_ptr;
                        }
                    }
                }
                
                x64_patch_pe_rva(&x64, &pe, &buf);
                
                
                // NOTE(Alexander): write the PE executable to file
                cstring exe_filename = string_to_cstring(compiler.output_filename);
                File_Handle exe_file = DEBUG_open_file_for_writing(exe_filename);
                write_pe_executable_to_file(exe_file, &pe);
                DEBUG_close_file(exe_file);
                pln("\nWrote executable: %", f_string(compiler.output_filename));
                
                //Read_File_Result exe_data = DEBUG_read_entire_file(exe_filename);
                //pe_dump_executable(create_string(exe_data.contents_size, (u8*) exe_data.contents));
                //DEBUG_close_file(exe_file);
                //cstring_free(exe_filename);
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
            
            String_Builder wat_sb = {};
            convert_wasm_binary_to_text(&wat_sb, &bytecode_builder.bytecode, buffer.data, buffer.data + buffer.curr_used);
            pln("%", f_string(string_builder_to_string_nocopy(&wat_sb)));
            string_builder_free(&wat_sb);
            
            pln("\nRunning: `wasm2wat simple.wasm`:");
            fflush(stdout);
            system("wasm2wat simple.wasm");
        } break;
    }
    
    return 0;
}

