#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"

#include "sqrrl_basic.cpp"
#include "sqrrl_tokenizer.cpp"
#include "sqrrl_parser.cpp"

int // NOTE(alexander): this is called by the platform layer
compiler_main_entry(int argc, char* argv[]) {
    str filepath;
    if (argc > 1) {
        filepath = str_lit(argv[1]);
    } else {
        filepath = str_lit("examples/demo.sq");
    }
    
    // Setup string interning of variables
    vars_initialize();
    
    // Read entire source file
    FILE* file;
    fopen_s(&file, filepath, "rb");
    if (!file) {
        pln("File `%` was not found!", f_str(filepath));
        return -1;
    }
    
    fseek(file, 0, SEEK_END);
    umm file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    str source = (str) malloc(file_size + 5) + 4;
    *((u32*) source - 1) = (u32) file_size;
    fread(source, str_count(source), 1, file);
    fclose(file);
    
    // TODO(alexander): temp printing source
    pln("%", f_str(source));
    
    // Lexer
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    // TODO(alexander): calculate line number!
    
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    Ast_File ast_file = parse_file(&parser);
    
    
    return 0;
}