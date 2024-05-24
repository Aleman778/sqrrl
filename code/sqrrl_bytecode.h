
enum Opcode : u8 {
    BC_NOOP,
    
    BC_FUNCTION_START, // res_index = func_index
    BC_FUNCTION_END,   // res_index = func_index
    
    BC_RETURN,
    
    BC_LOAD_CONSTANT,
    
    
    BC_COUNT,
};

global const cstring opcode_names[BC_COUNT] = {
    "noop",
    
    "FUNCTION_START",
    "FUNCTION_END",
    
    "LOAD_CONSTANT",
};

enum Bc_Type {
    BC_PTR,
    BC_I32,
    BC_I64,
    BC_F32,
    BC_F64,
};

struct Bc {
    Opcode opcode;
    
    int res_index;
    int a_index;
    int b_index;
    
    union {
        Value constant;
        Ast_Function* function;
    };
};

struct Bc_Bucket {
    Bc_Bucket* next;
    
    int count;
};

#define BC_INSTRUCTION_BUCKET_SIZE ARENA_DEFAULT_BLOCK_SIZE
#define BC_INSTRUCTIONS_PER_BUCKET ((BC_INSTRUCTION_BUCKET_SIZE - sizeof(Bc_Bucket))/sizeof(Bc))

struct Bc_Function {
    int func_index;
};

struct Bc_Module {
    Bc_Bucket* first_bucket;
    
    array(Bc_Type)* register_types;
    //array(Bc_Function)* functions;
    
    int next_func_index;
    
};

