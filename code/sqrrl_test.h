
#if BUILD_TEST

typedef s32 Test_Execution_Modes;
enum {
    TestExecutionMode_Interp = bit(0),
    TestExecutionMode_X64 = bit(1),
};

struct Test_Result {
    u32 num_passed;
    u32 num_failed;
    u32 num_skipped;
    u32 num_tests;
    
    Compilation_Unit* unit;
    Test_Execution_Modes modes;
};

global bool is_test_mode;
global Test_Result* curr_test;
global Test_Execution_Modes curr_test_exec_mode;


int run_compiler_tests(string filepath);

#endif