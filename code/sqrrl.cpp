#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"
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
    
    // Read entire source file
    FILE* file;
    fopen_s(&file, lit(filepath), "rb");
    if (!file) {
        printf("File `%s` was not found!", lit(filepath));
        return -1;
    }
    
    fseek(file, 0, SEEK_END);
    umm file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    str source;
    source.data = (u8*) malloc(file_size + 1);
    source.count = file_size;
    fread(source.data, source.count, 1, file);
    fclose(file);
    
    // TODO(alexander): temp printing source
    pln("%\n", f_str(source));
    
    // Lexer
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, source, filepath);
    // TODO(alexander): calculate line number!
    
    // TODO(alexander): testing parser
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    
    Token token = next_token(&parser);
    pln("next_token: %\n", f_token_name(token.type));
    pln("next_token: %\n", f_token(token.type));
    token = peek_token(&parser);
    pln("peek_token: %\n", f_token(token.type));
    pln("peek_token: %\n", f_token(token.type));
    token = next_token(&parser);
    pln("next_token: %\n", f_token_name(token.type));
    pln("next_token: %\n", f_token(token.type));
    
    // NOTE(alexander): just a test for a better formatter than printf
    str firstname = str_lit("Alexander");
    str lastname = str_lit("Mennborg");
    int level = 24;
    pln("Welcome % %, you are at level %!\n", f_str(firstname), f_str(lastname), f_int(level));
    
    return 0;
}