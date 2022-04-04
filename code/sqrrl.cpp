#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"
#include "sqrrl_value.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_preprocessor.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_interp.cpp"
#include "sqrrl_bytecode_builder.cpp"
#include "sqrrl_bytecode_interp.cpp"
#include "sqrrl_x64_builder.cpp"

int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[]) {
    
    {
        // Put dummy file as index 0
        Loaded_Source_File file = {};
        file.filepath = string_lit("invalid");
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
        filepath = string_lit("examples/demo3.sq");
        //filepath = string_lit("tests/literals.sq");
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
    working_directory = filepath;
    working_directory.count = 0;
    for (int index = 0; index < filepath.count; index++) {
        if (filepath.data[index] == '\\' || filepath.data[index] == '/') {
            working_directory.count = index + 1;
        }
    }
    string filename = filepath;
    filename.data += working_directory.count;
    filename.count -= working_directory.count;
    
    pln("working directory: %", f_string(working_directory));
    
    // Setup string interning of variables
    vars_initialize();
    
    // Read entire source file
    Loaded_Source_File file = read_entire_file(filename);
    
    Preprocessor preprocessor = {};
    string preprocessed_source = preprocess_file(&preprocessor, file.source, file.filepath, file.index);
    
#if 0
    // Source group debugging
    for_array(preprocessor.source_groups, group, index) {
        pln("group(%): file_index: %, line: %, offset: %, count: %\nSource:", f_int(index), f_uint(group->file_index), f_uint(group->line), f_umm(group->offset), f_umm(group->count));
        
        string group_source = create_string(group->count, preprocessed_source.data + group->offset);
        pln("%\n\n", f_string(group_source));
    }
#endif
    
    if (preprocessor.error_count > 0) {
        pln("\nErrors found during preprocessing, exiting...\n");
        return 1;
    }
    
    
    // TODO(alexander): temp printing source
    //pln("%", f_string(preprocessed_source));
    
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
    for_map(ast_file.decls, decl) {
        print_ast(decl->value, &tokenizer);
    }
#endif
    
    if (ast_file.error_count > 0) {
        pln("\nErrors found during parsing, exiting...\n");
        return 1;
    }
    
#if 0
    {
        // NOTE(Alexander): Interpret the AST
        Interp interp = {};
        interp_register_primitive_types(&interp);
        DEBUG_interp_register_intrinsics(&interp);
        interp_ast_declarations(&interp, ast_file.decls);
        Interp_Value result = interp_function_call(&interp, Sym_main, 0);
        if (result.modifier == InterpValueMod_Return) {
            if (is_integer(result.value)) {
                pln("Interpreter exited with code %", f_int((int) result.value.signed_int));
            } else {
                pln("Interpreter exited with code 0");
            }
        }
    }
#endif
    
#if 1
    {
        // NOTE(Alexander): Interpret the bytecode
        Bc_Builder bytecode_builder = {};
        Bc_Basic_Block* main_block = bc_build_from_ast(&bytecode_builder, &ast_file);
        
        {
            String_Builder sb = {};
            Bc_Basic_Block* curr_block = main_block;
            while (curr_block) {
                Bc_Instruction* curr_insn = curr_block->first;
                for (int i = 0; i < curr_block->count; i++) {
                    string_builder_push(&sb, curr_insn++);
                    string_builder_push(&sb, "\n");
                }
                
                curr_block = curr_block->next;
            }
            
            string str = string_builder_to_string_nocopy(&sb);
            pln("%", f_string(str));
            string_builder_free(&sb);
        }
        
        // Interpret the bytecode
        Bc_Interp interp = {};
        interp.declarations = bytecode_builder.declarations;
        interp.curr_block = main_block;
        bc_interp_function(&interp, Sym_main);
        
        // Generate X64 machine code
        X64_Builder x64_builder = {};
        x64_build_function(&x64_builder, main_block);
        x64_perform_register_allocation(&x64_builder);
        
        {
            // Print the human readable x64 assembly code
            String_Builder sb = {};
            X64_Instruction* insns = (X64_Instruction*) x64_builder.arena.base;
            
            // TODO(Alexander): won't work for expanded memory arenas
            X64_Basic_Block* curr_block = x64_builder.first_basic_block; 
            for (umm insn_index = 0; insn_index < curr_block->count; insn_index++) {
                X64_Instruction* insn = curr_block->first + insn_index;
                
                if (insn->opcode == X64Opcode_label) {
                    X64_Basic_Block* block = insn->op0.basic_block;
                    if (block->label.ident) {
                        string_builder_push_format(&sb, "%:\n", 
                                                   f_string(vars_load_string(block->label.ident)));
                    } else {
                        string_builder_push_format(&sb, "%:\n",
                                                   vars_load_string(block->label.index));
                    }
                } else {
                    
                    cstring mnemonic = x64_opcode_names[insn->opcode];
                    
                    string_builder_push_format(&sb, "    %", f_cstring(mnemonic));
                    if (insn->op0.kind) {
                        string_builder_push(&sb, " ");
                        string_builder_push(&sb, &insn->op0);
                    }
                    
                    if (insn->op1.kind) {
                        string_builder_push(&sb, ", ");
                        string_builder_push(&sb, &insn->op1);
                    }
                    
                    if (insn->op2.kind) {
                        string_builder_push(&sb, ", ");
                        string_builder_push(&sb, &insn->op2);
                    }
                    string_builder_push(&sb, "\n");
                }
            }
            string str = string_builder_to_string_nocopy(&sb);
            pln("\nX64 Assembly:\n%", f_string(str));
            string_builder_free(&sb);
            
            string interference_graph = x64_interference_graph_to_graphviz_dot(&x64_builder);
            pln("\nGraphviz interference graph:\n%", f_string(interference_graph));
        }
    }
    
#endif
    
    return 0;
}
