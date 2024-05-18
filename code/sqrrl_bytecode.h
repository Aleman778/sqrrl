
enum Opcode : u8 {
    BYTECODE_NOOP,
    
    BYTECODE_FUNCTION_START, // res_index = func_index
    BYTECODE_FUNCTION_END,   // res_index = func_index
    
    BYTECODE_CONST_I64,
    
};

global const cstring opcode_names[] = {
    "noop"
};

enum BC_Type {
    BYTECODE_PTR,
    BYTECODE_I32,
    BYTECODE_I64,
    BYTECODE_F32,
    BYTECODE_F64,
};

struct BC {
    Opcode opcode;
    
    int res_index;
    int a_index;
    int b_index;
    
    union {
        u64 _u64;
        f32 _f32;
        f64 _f64;
    } constant;
};

struct BC_Function {
    int func_index;
    
};

struct BC_Module {
    array(BC_Function)* functions;
    
};
