#ifndef _SQRRL_H_
#define _SQRRL_H_

// TODO(Alexander): keep this if we want to modify the allocations of stb libs 
#if 0
void*
sqrrl_realloc(void* data, size_t size) {
    return realloc(data, size);
}

void
sqrrl_free(void* data) {
    free(data);
}

#define STBDS_REALLOC(c, p, s) sqrrl_realloc(p, s)
#define STBDS_FREE(c, p) sqrrl_free(p)
#endif


enum Backend_Type {
    Backend_X64,
    Backend_WASM,
    
    Backend_Count,
};

enum Compiler_Task {
    CompilerTask_Exit = 0,
    
    CompilerTask_Run,
    CompilerTask_Build,
    CompilerTask_Test,
};


// TODO(alexander): this will in the future be replaced with our own implementation
#define STB_DS_IMPLEMENTATION
#include "stb_ds.h" // TODO(alexander): implement this on our own!

#include "sqrrl_basic.h"
#include "sqrrl_platform.h"
#include "sqrrl_intrinsics.h"
#include "sqrrl_identifier.h"
#include "sqrrl_source_file.h"

#include "lexer.h"
//#include "sqrrl_tokenizer.h"

//#include "sqrrl_bytecode.h"
//#include "sqrrl_value.h"
//#include "sqrrl_data.h"
//#include "sqrrl_types.h"
//#include "sqrrl_ast.h"
//#include "sqrrl_type_checker.h"

#include "ast.h"
#include "parser.h"
#include "typer.h"

//#include "sqrrl_interp.h"
//#include "sqrrl_bytecode_builder.h"
//#include "sqrrl_pe_converter.h"
//#include "sqrrl_x64_converter.h"
//#include "sqrrl_wasm_converter.h"
//#include "sqrrl_test.h"

extern "C" int compiler_main_entry(int argc, char* argv[]);

#endif // _SQRRL_H_
