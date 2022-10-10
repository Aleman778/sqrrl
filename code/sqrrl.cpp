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

typedef int asm_main(void);

int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[], void* asm_buffer, umm asm_size,
                    void (*asm_make_executable)(void*, umm)) {
    
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
                filepath = string_lit("tests/first.sq");
            }
            return run_compiler_tests(filepath, asm_buffer, asm_size, asm_make_executable);
        }
#endif
        
        filepath = string_lit(argv[1]);
        
    } else {
#if BUILD_DEBUG
        filepath = string_lit("personal/first.sq");
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
            for_map(ast_file.decls, decl) {
                print_ast(decl->value, &tokenizer);
            }
        }
        
        pln("\nErrors found during parsing, exiting...\n");
        return 1;
    }
    
    // Typecheck the AST
    if (type_check_ast_file(&ast_file) != 0) {
        if (flag_print_ast) {
            pln("AST (not fully typed):");
            for_map(ast_file.decls, decl) {
                print_ast(decl->value, &tokenizer);
            }
        }
        
        pln("\nErrors found during type checking, exiting...\n");
        return 1;
    }
    
    if (flag_print_ast) {
        pln("AST:");
        for_map(ast_file.decls, decl) {
            print_ast(decl->value, &tokenizer);
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
    
    {
        // Build bytecode representation of the AST
        Bc_Builder bytecode_builder = {};
        bc_build_from_ast(&bytecode_builder, &ast_file);
        Bytecode* bytecode = &bytecode_builder.code;
        
        if (flag_print_bc) {
            // TODO(Alexander): only counts last memory block
            pln("Bytecode (code) size (% bytes):\n", f_umm(bytecode_builder.code_arena.curr_used));
            
            String_Builder sb = {};
            for_map (bytecode_builder.declarations, it) {
                if (it->key.ident == Kw_global) {
                    continue;
                }
                
                Bc_Decl* decl = &it->value;
                if (decl->kind == BcDecl_Procedure) {
                    Bc_Basic_Block* first_basic_block = get_bc_basic_block(bytecode, decl->first_byte_offset);
                    Bc_Instruction* label = get_first_bc_instruction(first_basic_block);
                    Bc_Register_Mapper register_mapper = {};
                    
                    string_builder_push(&sb, "\n");
                    string_builder_push(&sb, &register_mapper, &label->src0);
                    string_builder_push(&sb, " ");
                    string_builder_push(&sb, vars_load_string(it->key.ident));
                    string_builder_push(&sb, &register_mapper, label->src1.Argument_List, true);
                    string_builder_push(&sb, " {\n");
                    string_builder_push(&sb, &register_mapper, first_basic_block, bytecode);
                    string_builder_push(&sb, "}\n");
                    
                } else if (decl->kind == BcDecl_Data) {
                    string_builder_push(&sb, "%");
                    string_builder_push(&sb, "%");
                    string_builder_push(&sb, it->key);
                    string_builder_push(&sb, " = ");
                    string_builder_push(&sb, &decl->Data.value);
                    string_builder_push(&sb, "\n");
                }
            }
            
            string str = string_builder_to_string_nocopy(&sb);
            pln("%", f_string(str));
            string_builder_free(&sb);
        }
        
        // Interpret the bytecode
        if (flag_run_bc_interp) {
            Bc_Interp interp = {};
            interp.code = &bytecode_builder.code;
            interp.declarations = bytecode_builder.declarations;
            int interp_exit_code = (int) bc_interp_bytecode(&interp).signed_int;
            pln("Bytecode interpreter exited with code: %\n", f_int(interp_exit_code));
        }
        
        // Generate X64 machine code
        X64_Builder x64_builder = {};
        x64_builder.label_indices = bytecode_builder.label_indices;
        x64_builder.bc_register_live_lengths = bytecode_builder.live_lengths;
        x64_builder.bytecode = &bytecode_builder.code;
        x64_builder.next_free_virtual_register = bytecode_builder.next_register;
        
        // Make sure to compile entry point function first
        Bc_Label entry_point_label = { Sym_main, 0 };
        Bc_Decl* main_decl = &map_get(bytecode_builder.declarations, entry_point_label);
        assert(main_decl && main_decl->kind == BcDecl_Procedure);
        
        Bc_Basic_Block* main_block =
            get_bc_basic_block(bytecode, main_decl->first_byte_offset);
        x64_build_procedure(&x64_builder, bytecode, main_block, &main_decl->Procedure);
        //pln("compiling procedure `%`", f_string(vars_load_string(Sym_main)));
        
        for_map (bytecode_builder.declarations, it) {
            if (it->key.ident == Sym_main || it->key.ident == Kw_global) {
                continue;
            }
            
            Bc_Decl* decl = &it->value;
            if (decl->kind == BcDecl_Procedure) {
                // TODO(Alexander): to support procedure overloading we should remove this
                if (it->key.index != 0) continue;
                
                //pln("compiling procedure `%`", f_string(vars_load_string(it->key.ident)));
                Bc_Basic_Block* proc_block =
                    get_bc_basic_block(bytecode, decl->first_byte_offset);
                x64_build_procedure(&x64_builder, bytecode, proc_block, &decl->Procedure);
            } else if (decl->kind == BcDecl_Data) {
                // TODO(Alexander): we need to store the actual value type in the declarations
                x64_build_data_storage(&x64_builder, it->key, decl->Data.value.data, decl->Data.type);
            }
        }
        
        // Print interference graph before register allocation
        string interference_graph = x64_interference_graph_to_graphviz_dot(&x64_builder);
        //pln("\nGraphviz interference graph (before):\n%", f_string(interference_graph));
        DEBUG_write_entire_file("build/rig_before.dot", 
                                interference_graph.data, 
                                (u32) interference_graph.count);
        
        if (flag_print_asm_vreg) {
            // Print the human readable x64 assembly code (before register allocation)
            String_Builder sb = {};
            string_builder_push(&sb, x64_builder.first_basic_block, true);
            
            string str = string_builder_to_string_nocopy(&sb);
            pln("\nX64 Assembly (without register allocation):\n%", f_string(str));
            string_builder_free(&sb);
        }
        
        // Perform register allocation
        x64_perform_register_allocation(&x64_builder);
        
        // Print interference graph before register allocation
        interference_graph = x64_interference_graph_to_graphviz_dot(&x64_builder);
        //pln("\nGraphviz interference graph (after):\n%", f_string(interference_graph));
        DEBUG_write_entire_file("build/rig_after.dot", 
                                interference_graph.data, 
                                (u32) interference_graph.count);
        
        if (flag_print_asm) {
            // Print the human readable x64 assembly code
            String_Builder sb = {};
            string_builder_push(&sb, x64_builder.first_basic_block, false);
            
            string str = string_builder_to_string_nocopy(&sb);
            pln("\nX64 Assembly:\n%", f_string(str));
            string_builder_free(&sb);
        }
        
        // x64 build instruction definitions
        X64_Instruction_Def_Table* x64_instruction_definitions = parse_x86_64_definitions();
        
        // x64 assembler
        X64_Assembler assembler = {};
        assembler.bytes = (u8*) asm_buffer;
        assembler.size = asm_size;
        assembler.rodata_offsets = x64_builder.rodata_offsets;
        
        x64_assemble_to_machine_code(&assembler,
                                     x64_instruction_definitions,
                                     x64_builder.first_basic_block);
        
        assert(assembler.curr_used + x64_builder.rodata_section_arena.curr_used < assembler.size && 
               "asm buf out of memory");
        memcpy(assembler.bytes + assembler.curr_used, 
               x64_builder.rodata_section_arena.base, 
               x64_builder.rodata_section_arena.curr_used);
        
#if 0
        pln("\nX64 Machine Code (% bytes):", f_umm(assembler.curr_used));
        for (int byte_index = 0; byte_index < assembler.curr_used; byte_index++) {
            u8 byte = assembler.bytes[byte_index];
            if (byte > 0xF) {
                printf("%hhX ", byte);
            } else {
                printf("0%hhX ", byte);
            }
            
            if (byte_index % 90 == 89) {
                printf("\n");
            }
        }
#endif
        
        asm_make_executable(asm_buffer, asm_size);
        asm_main* func = (asm_main*) asm_buffer;
        int jit_exit_code = (int) func();
        pln("JIT exited with code: %", f_int(jit_exit_code));
    }
    
    return 0;
}
