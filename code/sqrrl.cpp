#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"
#include "sqrrl_value.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_preprocessor.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_typer.cpp"
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
    
    string filepath;
    
#if BUILD_DEBUG
    if (argc > 1) {
        filepath = string_lit(argv[1]);
    } else {
        //filepath = string_lit("examples/demo3.sq");
        filepath = string_lit("examples/demo4.sq");
        //filepath = string_lit("examples/primes.sq");
        //filepath = string_lit("tests/literals.sq");
        //filepath = string_lit("tests/preprocessor.sq");
    }
#else
    if (argc <= 1) {
        pln("Usage: sqrrl file.sq");
        return 0;
    }
    filepath = string_lit(argv[1]);
#endif
    
    
    // Extract the directory from the filepath and use as working directory
    // TODO(Alexander): this is hackish solution, we should rely on OS service to do this
    //working_directory = filepath;
    //working_directory.count = 0;
    //for (int index = 0; index < filepath.count; index++) {
    //if (filepath.data[index] == '\\' || filepath.data[index] == '/') {
    //working_directory.count = index + 1;
    //}
    //}
    string filename = filepath;
    //filename.data += working_directory.count;
    //filename.count -= working_directory.count;
    
    //pln("working directory: %", f_string(working_directory));
    
    // Setup string interning of variables
    vars_initialize();
    
    // Read entire source file
    Loaded_Source_File file = read_entire_source_file(filename);
    
    if (!file.is_valid) {
        return -1;
    }
    
    Preprocessor preprocessor = {};
    string preprocessed_source = preprocess_file(&preprocessor, file.source, file.filename, file.index);
    
    
    // TODO(alexander): temp printing source
    pln("Preprocessed source:\n%", f_string(preprocessed_source));
    
#if 1
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
    tokenizer_set_source(&tokenizer, preprocessed_source, filepath);
    // TODO(alexander): calculate line number!
    
    Parser parser = {};
    parser.source_groups = preprocessor.source_groups;
    parser.tokenizer = &tokenizer;
    Ast_File ast_file = parse_file(&parser);
    
#if BUILD_MAX_DEBUG
    // NOTE(Alexander): Print the AST
    pln("AST before types:");
    for_map(ast_file.decls, decl) {
        print_ast(decl->value, &tokenizer);
    }
#endif
    
    if (ast_file.error_count > 0) {
        pln("\nErrors found during parsing, exiting...\n");
        return 1;
    }
    
#if 1
    // Typecheck the AST
    if (type_check_ast_file(&ast_file) != 0) {
        pln("\nErrors found during type checking, exiting...\n");
        return 1;
    }
    
    
#if BUILD_MAX_DEBUG
    // NOTE(Alexander): Print the AST
    pln("AST after types:");
    for_map(ast_file.decls, decl) {
        print_ast(decl->value, &tokenizer);
    }
#endif
#endif
#if 1
    {
        // Interpret the AST
        Interp interp = {};
        interp_ast_declarations(&interp, ast_file.decls);
        Interp_Value result = interp_function_call(&interp, Sym_main, 0);
        if (result.modifier == InterpValueMod_Return) {
            if (is_integer(result.value)) {
                pln("Interpreter exited with code %", f_int((int) result.value.data.signed_int));
            } else {
                pln("Interpreter exited with code 0");
            }
        }
    }
#endif
    
#if 1
    {
        // Build bytecode representation of the AST
        Bc_Builder bytecode_builder = {};
        bc_build_from_ast(&bytecode_builder, &ast_file);
        pln("Bytecode size: % bytes", f_umm(bytecode_builder.arena.curr_used)); // TODO(Alexander): only counts last memory block
        
        {
            String_Builder sb = {};
            for_map (bytecode_builder.declarations, it) {
                if (it->value.type == Value_basic_block) {
                    Bc_Basic_Block* curr_block = it->value.data.basic_block;
                    while (curr_block) {
                        Bc_Instruction* curr_insn = curr_block->first;
                        for (int i = 0; i < curr_block->count; i++) {
                            string_builder_push(&sb, curr_insn++);
                            string_builder_push(&sb, "\n");
                        }
                        
                        curr_block = curr_block->next;
                    }
                } else {
                    string_builder_push_format(&sb, "% = memory_alloc %\n\n",
                                               f_string(vars_load_string(it->key.ident)),
                                               f_value(&it->value));
                }
            }
            
            string str = string_builder_to_string_nocopy(&sb);
            pln("%", f_string(str));
            string_builder_free(&sb);
        }
        
        // Interpret the bytecode
        Bc_Interp interp = {};
        interp.declarations = bytecode_builder.declarations;
        int interp_exit_code = (int) bc_interp_bytecode(&interp).signed_int;
        
        // Generate X64 machine code
        X64_Builder x64_builder = {};
        x64_builder.bc_register_live_lengths = bytecode_builder.live_lengths;
        
        // Make sure to compile entry point function first
        Bc_Register entry_point_label = { Sym_main, 0 };
        Value main_decl = map_get(bytecode_builder.declarations, entry_point_label);
        assert(main_decl.type == Value_basic_block);
        
        Bc_Basic_Block* main_block = main_decl.data.basic_block;
        x64_build_function(&x64_builder, main_block);
        pln("compiling function `%`", f_string(vars_load_string(Sym_main)));
        
        for_map (bytecode_builder.declarations, it) {
            if (it->key.ident == Sym_main || it->key.index != 0) {
                continue;
            }
            
            pln("compiling function `%`", f_string(vars_load_string(it->key.ident)));
            
            if (it->value.type == Value_basic_block) {
                Bc_Basic_Block* function_block = it->value.data.basic_block;
                x64_build_function(&x64_builder, function_block);
            } else {
                // TODO(Alexander): we need to store the actual value type in the declarations
                x64_build_data_storage(&x64_builder, it->key, it->value.data, &global_primitive_types[PrimitiveType_int]);
            }
        }
        
        // Print interference graph before register allocation
        string interference_graph = x64_interference_graph_to_graphviz_dot(&x64_builder);
        //pln("\nGraphviz interference graph (before):\n%", f_string(interference_graph));
        DEBUG_write_entire_file("build/rig_before.dot", 
                                interference_graph.data, 
                                (u32) interference_graph.count);
        
        {
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
        
        {
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
        
        // TODO(Alexander): int3 breakpoint for debugging
        //push_u8(&assembler, 0xCC);
        
        x64_assemble_to_machine_code(&assembler,
                                     x64_instruction_definitions,
                                     x64_builder.first_basic_block);
        
        pln("\nX64 Machine Code (% bytes):", f_umm(assembler.curr_used));
        for (int byte_index = 0; byte_index < assembler.curr_used; byte_index++) {
            printf("0x%hhX ", (u8) assembler.bytes[byte_index]);
            if (byte_index % 10 == 9) {
                printf("\n");
            }
        }
        
        pln("\n\nRunning JIT:");
        
        asm_make_executable(asm_buffer, asm_size);
        asm_main* func = (asm_main*) asm_buffer;
        int jit_exit_code = (int) func();
        pln("\n\nInterpreter exited with code: %", f_int(interp_exit_code));
        pln("        JIT exited with code: %", f_int(jit_exit_code));
    }
    
#endif
    
    return 0;
}
