#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sqrrl_types.h"

int main(int argc, char* argv[]) {
    str filename;
    if (argc > 1) {
        filename = str_lit(argv[1]);
    } else {
        filename = str_lit("examples/demo.sq");
    }
    
    // Read entire source file
    FILE* file;
    fopen_s(&file, lit(filename), "rb");
    if (!file) {
        printf("File `%s` was not found!", lit(filename));
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
    
    return 0;
    
}