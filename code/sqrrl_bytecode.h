
enum Bytecode_Operator : u8 {
    BC_END_OF_FUNCTION = 0,
    BC_NOOP,
    
    // Control
    BC_DEBUG_BREAK,
    BC_LOOP,
    BC_BLOCK,
    BC_END,
    BC_BRANCH,
    BC_CALL,
    BC_CALL_INDIRECT,
    BC_RETURN,
    
    // Constants
    BC_INT_CONST, // int x := <int-literal>
    BC_F32_CONST, // f32 x := <f32-literal>
    BC_F64_CONST, // f64 x := <f64-literal>
    
    // Pointers
    BC_LOCAL,        // ptr x := locals(size, align)
    BC_GLOBAL,       // ptr x := globals(index)
    BC_FUNCTION,     // ptr x := function(index)
    BC_ARRAY_ACCESS, // ptr x := a[b]
    BC_FIELD_ACCESS, // ptr x := a.b (or (u8*) a + offset(b)
    
    // Memory
    BC_COPY,   // x := y
    BC_STORE,  // *x := y
    BC_LOAD,   // x := *y
    BC_MEMCPY, // memcpy(dest, src, size)
    BC_MEMSET, // memset(dest, val, size)
    
    // Conversions
    BC_TRUNCATE,
    BC_EXTEND,
    BC_INT_TO_FLOAT,
    BC_FLOAT_TO_INT,
    BC_FLOAT_TO_FLOAT,
    BC_REINTERPRET_F2I,
    
    // Unary
    BC_NEG,
    BC_NOT,
    BC_INC,
    BC_DEC,
    
    // Binary
    BC_ADD,
    BC_SUB,
    BC_MUL,
    BC_DIV_S,
    BC_DIV_U,
    BC_MOD_S,
    BC_MOD_U,
    BC_AND,
    BC_OR,
    BC_XOR,
    BC_SHL,
    BC_SAR,
    BC_SHR,
    
    // Comparators
    BC_EQ,
    BC_GT_S,
    BC_GT_U,
    BC_GE_S,
    BC_GE_U,
    BC_LT_U,
    BC_LT_S,
    BC_LE_U,
    BC_LE_S,
    BC_NEQ,
    
    // Intrinsics (x64)
    BC_X64_RDTSC,
    
    // End of function
    BC_EOF
};

bool
bc_is_comparator(Bytecode_Operator op) {
    return op >= BC_EQ && op <= BC_NEQ;
}

global const cstring bc_operator_names[] = {
    /*                   */ "", "noop",
    /* Control:          */ "debug_break", "loop", "block", "end", "branch", "call", "call_indirect",
    /*                   */ "return",
    /* Constants:        */ "i64.const", "f32.const", "f64.const",
    /* Pointers:         */ "ptr.local", "ptr.global", "ptr.function", "ptr.array_access",
    /*                   */ "ptr.field_access",
    /* Memory:           */ "copy", "store", "load", "memcpy", "memset",
    /* Conversions:      */ "truncate", "extend", "int_to_float", "float_to_int", "float_to_float",
    /*                   */ "reinterpret_f2i",
    /* Unary:            */ "neg", "not", "inc", "dec",
    /* Binary:           */ "add", "sub", "mul", "div_s", "div_u", "mod_s", "mod_u", "and", 
    /*                   */ "or", "xor", "shl", "sar", "shr",
    /* Comparators:      */ "eq", "gt_s", "gt_u", "ge_s", "ge_u", "lt_u", "lt_s", "le_u", 
    /*                   */ "le_s", "neq",
    /* intrinsics (x64): */ "x64_rdts"
};

global const cstring bc_type_names[] = {
    "int", "float", "ptr"
};

enum Bytecode_Type_Kind {
    BC_TYPE_INT,
    BC_TYPE_FLOAT,
    BC_TYPE_PTR,
};

enum Byytecode_Type_Flags {
    BC_FLAG_SIGNED = bit(0),
};

struct Bytecode_Type {
    u8 kind;
    u8 flags;
    u8 size;
};

global const Bytecode_Type bc_type_bool = { BC_TYPE_INT, 0, 1 };
#define BC_BOOL bc_type_bool

global const Bytecode_Type bc_type_ptr = { BC_TYPE_PTR, 0, 0 };
#define BC_PTR bc_type_ptr

struct Bytecode_Function_Arg {
    Bytecode_Type type;
    u32 size, align;
};

struct Bytecode_Function {
    int register_count;
    
    union {
        void* code_ptr;
        string_id intrinsic_id;
    };
    
    u32 relative_ptr;
    
    u32 type_index;
    
    u32 insn_count;
    u32 block_count;
    
    s32 arg_count;
    s32 ret_count;
    
    u32 max_caller_arg_count;
    
    s32 first_insn; // relative pointer to first instruction
    
    bool return_as_first_arg;
    bool is_imported;
    bool is_intrinsic;
    
    // followed by array of Bytecode_Function_Arg, function returns followed by its
    // arguments types and lastly instructions
};

inline Bytecode_Type
register_type(Bytecode_Function* func, int register_index) {
    return BC_PTR;
}

inline Bytecode_Function_Arg*
function_ret_types(Bytecode_Function* func) {
    return (Bytecode_Function_Arg*) (func + 1);
}

inline Bytecode_Function_Arg*
function_arg_types(Bytecode_Function* func) {
    return (Bytecode_Function_Arg*) (func + 1) + func->ret_count;
}

struct Bytecode_Import {
    string_id module;
    string_id function;
    
    u32 func_index;
    
    u32 rdata_offset;
};

struct Bytecode_Export {
    string_id function;
    u32 func_index;
};

enum Bytecode_Memory_Kind {
    BC_MEM_READ_ONLY,
    BC_MEM_READ_WRITE,
};

struct Bytecode_Global {
    void* address; // for JIT
    u32 offset;
    u32 size, align;
    Bytecode_Memory_Kind kind;
    // TODO(Alexander): maybe we should add reference count so we
    // can safely exclude the data if we optimized out this
};

struct Bytecode {
    
    array(Bytecode_Import)* imports;
    array(Bytecode_Export)* exports;
    
    array(Bytecode_Function*)* functions;
    array(string_id)* function_names;
    
    array(Bytecode_Global)* globals;
    
    int entry_func_index;
};

global cstring bc_memory_kind_names[] = { 
    "", "rodata", "data"
};


#define Bytecode_Instruction_Base \
Bytecode_Operator opcode; \
Bytecode_Type type; \
s32 next_insn; \

// Base structure, can be pointer casted to any of the other instruction types below
struct Bytecode_Instruction {
    Bytecode_Instruction_Base;
};
global Bytecode_Instruction bc_end_of_function = {};

struct Bytecode_Const_Int {
    Bytecode_Instruction_Base;
    
    int res_index;
    s64 val;
};

struct Bytecode_Const_F32 {
    Bytecode_Instruction_Base;
    
    int res_index;
    f32 val;
};

struct Bytecode_Const_F64 {
    Bytecode_Instruction_Base;
    
    int res_index;
    f64 val;
};

struct Bytecode_Result {
    Bytecode_Instruction_Base;
    
    int res_index;
};

struct Bytecode_Unary {
    Bytecode_Instruction_Base;
    
    int res_index;
    int a_index;
};

struct Bytecode_Binary {
    Bytecode_Instruction_Base;
    
    int res_index;
    int a_index;
    int b_index;
};

struct Bytecode_Assign {
    Bytecode_Instruction_Base;
    
    int dest_index;
    int src_index;
};

struct Bytecode_Local {
    Bytecode_Instruction_Base;
    
    int res_index;
    s32 size, align;
};

struct Bytecode_Field_Access {
    Bytecode_Instruction_Base;
    
    int res_index;
    int base;
    s32 offset;
};

struct Bytecode_Array_Access {
    Bytecode_Instruction_Base;
    
    int res_index;
    int base;
    int index;
    s32 stride;
};

struct Bytecode_Memcpy {
    Bytecode_Instruction_Base;
    
    int dest_index;
    int src_index;
    int size;
};

struct Bytecode_Memset {
    Bytecode_Instruction_Base;
    
    int dest_index;
    int value;
    int size;
};

struct Bytecode_Call {
    Bytecode_Instruction_Base;
    
    u32 func_index;
    // argument operands followed by return operands
};

inline int*
bc_call_args(Bytecode_Call* call) {
    return (int*) (call + 1);
}

struct Bytecode_Call_Indirect {
    Bytecode_Instruction_Base;
    
    int func_ptr_index;
    s32 ret_count;
    s32 arg_count;
    // argument operands followed by return operands
};

inline int*
bc_call_args(Bytecode_Call_Indirect* call) {
    return (int*) (call + 1);
}

struct Bytecode_Block {
    Bytecode_Instruction_Base;
    
    u32 label_index;
    u32 end_insn;
};

struct Bytecode_Branch {
    Bytecode_Instruction_Base;
    
    int cond;
    
    u32 label_index;
    // TODO(Alexander): maybe have a true and false label?
};

inline Bytecode_Instruction*
iter_bytecode_instructions(Bytecode_Function* func, Bytecode_Instruction* iter) {
    if (!iter) {
        if (!func->first_insn) {
            return &bc_end_of_function;
        }
        
        iter = (Bytecode_Instruction*) ((u8*) func + func->first_insn);
        return iter;
    }
    
    if (!iter->next_insn) {
        return &bc_end_of_function;
    }
    
    iter = (Bytecode_Instruction*) ((u8*) iter + iter->next_insn);
    return iter;
}

#define for_bc_insn(func, insn) \
for (Bytecode_Instruction* insn = iter_bytecode_instructions(func, 0); \
insn->opcode; \
insn = iter_bytecode_instructions(func, insn))