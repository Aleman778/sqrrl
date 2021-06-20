#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl.h"

#include "sqrrl_tokenizer.cpp"

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
    
    printf("%s\n", lit(source));
    
    // Lexer
    Tokenizer tokenizer = {};
    tokenizer.start = source.data;
    tokenizer.end = source.data + source.count;
    tokenizer.next = tokenizer.start;
    tokenizer.source = source;
    tokenizer.filepath = filepath;
    // TODO(alexander): calculate line number!
    
    
    
    
    return 0;
    
}