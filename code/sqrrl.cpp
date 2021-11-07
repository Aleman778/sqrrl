#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_parser.cpp"
#include "sqrrl_interp.cpp"

int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[]) {
    string filepath;
    if (argc > 1) {
        filepath = string_lit(argv[1]);
    } else {
        filepath = string_lit("C:/Dev/sqrrl/examples/demo.sq");
    }
    
    // Setup string interning of variables
    vars_initialize();
    
    // Read entire source file
    FILE* file;
    fopen_s(&file, filepath, "rb");
    if (!file) {
        pln("File `%` was not found!", f_string(filepath));
        return -1;
    }
    
    fseek(file, 0, SEEK_END);
    umm file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    string source = (string) malloc(file_size + 5) + 4;
    *((u32*) source - 1) = (u32) file_size;
    fread(source, string_count(source), 1, file);
    fclose(file);
    
    // TODO(alexander): temp printing source
    pln("%", f_string(source));
    
    // Lexer
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    // TODO(alexander): calculate line number!
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    Ast_File ast_file = parse_file(&parser);
    
    if (ast_file.error_count > 0) {
        pln("\nErrors found during parsing, exiting...\n");
        return 0;
    }
    
#if 1
    // NOTE(Alexander): Print the AST
    for (int i = 0; i < map_count(ast_file.decls); i++) {
        Ast_Decl_Entry entry = ast_file.decls[i];
        print_ast(entry.value, &tokenizer);
    }
#endif
    
    // NOTE(Alexander): Interpreter pass
    Interp interp = {};
    interp_register_primitive_types(&interp);
    interp_ast_declarations(&interp, ast_file.decls);
    Interp_Value result = interp_function_call(&interp, vars_save_string("main"), 0);
    if (result.modifier == InterpValueMod_Return) {
        if (is_integer(result.value)) {
            pln("Interpreter exited with code %", f_int((int) result.value.signed_int));
        } else {
            pln("Interpreter exited with code 0");
        }
    }
    
    //int x = 10 + 10;
    //return x > 10 ? 10 : x;
    
    int x = 10 + 10;
    return x > 10 ? 10 : x + 30 > 10 ? 33 : 44;
}
