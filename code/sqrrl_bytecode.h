
enum Opcode : u8 {
    BC_NOOP,
    
    BC_FUNCTION_START, // res_index = func_index
    BC_FUNCTION_END,   // res_index = func_index
    
    BC_CONST_I64,
    
};

global const cstring opcode_names[] = {
    "noop"
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
    
    Value constant;
};

struct Bc_Function {
    int func_index;
    
    array(Bc_Type)* register_types;
};

struct Bc_Module {
    array(Bc_Function)* functions;
};
