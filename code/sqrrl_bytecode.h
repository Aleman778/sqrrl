
enum Bytecode_Operator {
    BC_NOOP = 0,
    
    // Control
    BC_DEBUG_BREAK,
    BC_LOOP,
    BC_BLOCK,
    BC_END,
    BC_BRANCH,
    BC_CALL,
    BC_RETURN,
    
    // Constants
    BC_INT_CONST,
    BC_F32_CONST,
    BC_F64_CONST,
    
    // Pointers
    BC_LOCAL, // (int size, int align) -> void*
    BC_GLOBAL,
    BC_ARRAY_INDEX,
    BC_STRUCT_FIELD,
    
    // Memory
    BC_STORE,
    BC_LOAD,
    BC_MEMCPY,
    BC_MEMSET,
    
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
    BC_ADDR_OF,
    BC_DEREF,
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
    BC_X64_RDTSC
};

bool
bc_is_comparator(Bytecode_Operator op) {
    return op >= BC_EQ && op <= BC_NEQ;
}

global const cstring bc_operator_names[] = {
    /*                   */ "noop",
    /* Control:          */ "debug_break", "loop", "block", "end", "branch", "call", "return",
    /* Constants:        */ "i64.const", "f32.const", "f64.const",
    /* Pointers:         */ "ptr.local", "ptr.global", "ptr.array_index", "ptr.struct_field",
    /* Memory:           */ "store", "load", "memcpy", "memset",
    /* Conversions:      */ "truncate", "extend", "int_to_float", "float_to_int", "float_to_float",
    /*                   */ "reinterpret_f2i",
    /* Unary:            */ "neg", "not", "inc", "dec",
    /* Binary:           */ "addr_of", "deref", "add", "sub", "mul", "div_s", "div_u", 
    /*                   */ "mod_s", "mod_u", "and", "or", "xor", "shl", "sar", "shr",
    /* Comparators:      */ "eq", "gt_s", "gt_u", "ge_s", "ge_u", "lt_u", "lt_s", "le_u", "le_s", "neq",
    /* intrinsics (x64): */ "x64_rdts"
};

global const cstring bc_type_names[] = {
    "", "i32", "i64", "f32", "f64"
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
    s32 size; // for BC_TYPE_PTR this is size of value adjusted for alignment
};

struct Bytecode_Function_Arg {
    Bytecode_Type type;
    u32 size, align;
};

struct Bytecode_Function {
    array(Bytecode_Type)* register_types;
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
    
    u32 first_insn; // relative pointer to first instruction
    
    bool return_as_first_arg;
    bool is_imported;
    bool is_intrinsic;
    
    // followed by array of Bytecode_Function_Arg, function returns followed by its
    // arguments types and lastly instructions
};

inline Bytecode_Function_Arg*
function_ret_types(Bytecode_Function* func) {
    return (Bytecode_Function_Arg*) (func + 1);
}

inline Bytecode_Function_Arg*
function_arg_types(Bytecode_Function* func) {
    return (Bytecode_Function_Arg*) (func + 1) + func->ret_count;
}

inline Bytecode_Type
register_type(Bytecode_Function* func, int register_index) {
    return func->register_types[register_index];
}

struct Bytecode_Import {
    string_id module;
    string_id function;
    
    u32 rdata_offset;
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
};

struct Bytecode {
    array(Bytecode_Import)* imports;
    
    array(Bytecode_Function*)* functions;
    array(string_id)* function_names;
    
    array(Bytecode_Global)* globals;
    
    int entry_func_index;
};

global cstring bc_memory_kind_names[] = { 
    "", "rodata", "data"
};


enum Bytecode_Instruction_Kind {
    BytecodeInstructionKind_None,
    
    BytecodeInstructionKind_Base,
    BytecodeInstructionKind_Alloca,
    BytecodeInstructionKind_Unary,
    BytecodeInstructionKind_Binary,
    BytecodeInstructionKind_Call,
    BytecodeInstructionKind_Block,
    BytecodeInstructionKind_Branch,
    BytecodeInstructionKind_Memory,
};

#define Bytecode_Instruction_Base \
Bytecode_Operator opcode; \
Bytecode_Instruction_Kind kind; \
u32 next_insn; \
cstring comment

// NOTE(Alexander): this is just a base structure, most are extended
struct Bytecode_Base {
    Bytecode_Instruction_Base;
};

// NOTE(Alexander): alias for Bytecode_Base
struct Bytecode_Instruction {
    Bytecode_Instruction_Base;
};

global Bytecode_Instruction bc_empty = {};

struct Bytecode_Binary {
    Bytecode_Instruction_Base;
    
    int res_index;
    union {
        struct {
            int arg0_index;
            int arg1_index;
        };
        s64 const_i64;
        f32 const_f32;
        f64 const_f64;
    };
};

struct Bytecode_Call {
    Bytecode_Instruction_Base;
    
    u32 func_index;
    // argument operands followed by return operands
};

int*
bc_call_args(Bytecode_Call* call) {
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
            return &bc_empty;
        }
        
        iter = (Bytecode_Instruction*) ((u8*) func + func->first_insn);
        return iter;
    }
    
    if (!iter->next_insn) {
        return &bc_empty;
    }
    
    iter = (Bytecode_Instruction*) ((u8*) iter + iter->next_insn);
    return iter;
}

#define for_bc_insn(func, insn) \
for (Bytecode_Instruction* insn = iter_bytecode_instructions(func, 0); \
insn->kind; \
insn = iter_bytecode_instructions(func, insn))