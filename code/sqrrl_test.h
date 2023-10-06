
#if BUILD_TEST

typedef s32 Test_Execution_Modes;
enum {
    TestExecutionMode_None = 0,
    TestExecutionMode_Interp = bit(0),
    TestExecutionMode_X64_JIT = bit(1),
    TestExecutionMode_WASM = bit(2),
    
    TestExecutionMode_Count = 3,
    TestExecutionMode_All = (TestExecutionMode_Interp |
                             TestExecutionMode_X64_JIT |
                             TestExecutionMode_WASM),
};

const cstring test_execution_names[] = {
    "AST interpreter", "X64 JIT"
};

struct Test_Execution {
    void* context;
    String_Builder output;
    bool failed;
};

struct Test_Result {
    string_id ident;
    
    u32 num_passed;
    u32 num_failed;
    u32 num_skipped;
    u32 num_tests;
    bool fatal_error;
    
    Test_Execution_Modes modes;
    Test_Execution exec[TestExecutionMode_Count];
    
    Compilation_Unit* unit;
    Interp* interp;
};

global bool is_test_mode;
global Test_Result* curr_test;
global Test_Execution* curr_execution;


int run_compiler_tests(string filepath);

#endif
