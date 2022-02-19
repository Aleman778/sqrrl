#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"
#include "sqrrl_value.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_interp.cpp"


int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[]) {
    string filepath;
    
#if BUILD_DEBUG
    if (argc > 1) {
        filepath = string_lit(argv[1]);
    } else {
        filepath = string_lit("examples/demo_macros.sq");
    }
#else
    if (argc <= 1) {
        pln("Usage: sqrrl <file.sq>");
        return 0;
    }
    filepath = string_lit(argv[1]);
#endif
    
    // Setup string interning of variables
    vars_initialize();
    
    // Read entire source file
    Read_File_Result file = DEBUG_read_entire_file(string_to_cstring(filepath));
    string source = create_string(file.contents_size, (u8*) file.contents);
    
    string preprocessed_source = preprocess_file(source, filepath);
    
    // TODO(alexander): temp printing source
    pln("%", f_string(preprocessed_source));
    
    // Lexer
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, preprocessed_source, filepath);
    // TODO(alexander): calculate line number!
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    Ast_File ast_file = parse_file(&parser);
    
#if BUILD_MAX_DEBUG
    // NOTE(Alexander): Print the AST
    for (int i = 0; i < map_count(ast_file.decls); i++) {
        Ast_Decl_Entry entry = ast_file.decls[i];
        print_ast(entry.value, &tokenizer);
    }
#endif
    
    if (ast_file.error_count > 0) {
        pln("\nErrors found during parsing, exiting...\n");
        return 1;
    }
    
    // NOTE(Alexander): Interpreter pass
    Interp interp = {};
    interp_register_primitive_types(&interp);
    DEBUG_interp_register_intrinsics(&interp);
    interp_ast_declarations(&interp, ast_file.decls);
    Interp_Value result = interp_function_call(&interp, vars_save_cstring("main"), 0);
    if (result.modifier == InterpValueMod_Return) {
        if (is_integer(result.value)) {
            pln("Interpreter exited with code %", f_int((int) result.value.signed_int));
        } else {
            pln("Interpreter exited with code 0");
        }
    }
    
    return 0;
}
