
#if BUILD_TEST

typedef s32 Test_Execution_Modes;
enum {
    TestExecutionMode_Interp = bit(0),
    TestExecutionMode_Bc_Interp = bit(1),
    TestExecutionMode_X64 = bit(2),
};

struct Test_Result {
    u32 num_passed;
    u32 num_failed;
    u32 num_skipped;
    u32 num_tests;
    
    Ast* ast;
    string_id ident;
    Test_Execution_Modes modes;
};

global bool is_test_mode;
global Test_Result* curr_test;
global Test_Execution_Modes curr_test_exec_mode;

extern "C" void intrinsic_assert(int expr);

int run_compiler_tests(string filepath);

#endif