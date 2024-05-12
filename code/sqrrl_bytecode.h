
enum Bytecode_Operator : u8 {
    BC_END_OF_FUNCTION = 0,
    BC_NOOP,
    
    BC_DEBUG_BREAK,
    BC_DROP,
    
    // Control flow
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
    BC_PROCEDURE,    // ptr x := procedure(index)
    BC_ARRAY_ACCESS, // ptr x := a[b]
    BC_FIELD_ACCESS, // ptr x := a.b (or (u8*) a + offset(b)
    
    // Memory
    BC_COPY,   // res  = src
    BC_STORE,  // *res = src
    BC_LOAD,   // res = *src
    BC_LEA,    // res = &src
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
};

bool
bc_is_comparator(Bytecode_Operator op) {
    return op >= BC_EQ && op <= BC_NEQ;
}

global const cstring bc_operator_names[] = {
    /*                   */ "", "noop", "debug_break", "drop",
    /* Control flow:     */ "loop", "block", "end", "branch", "call", "call_indirect",
    /*                   */ "return",
    /* Constants:        */ "const", "const", "const",
    /* Pointers:         */ "local", "global", "procedure", "array_access",
    /*                   */ "field_access",
    /* Memory:           */ "copy", "store", "load", "lea", "memcpy", "memset",
    /* Conversions:      */ "truncate", "extend", "int_to_float", "float_to_int", "float_to_float",
    /*                   */ "reinterpret_f2i",
    /* Unary:            */ "neg", "not", "inc", "dec",
    /* Binary:           */ "add", "sub", "mul", "div_s", "div_u", "mod_s", "mod_u", "and", 
    /*                   */ "or", "xor", "shl", "sar", "shr",
    /* Comparators:      */ "eq", "gt_s", "gt_u", "ge_s", "ge_u", "lt_u", "lt_s", "le_u", 
    /*                   */ "le_s", "neq",
    /* intrinsics (x64): */ "x64_rdts"
};

enum Bytecode_Type {
    BC_TYPE_PTR,
    BC_TYPE_S32,
    BC_TYPE_S64,
    BC_TYPE_F32,
    BC_TYPE_F64
};

inline Bytecode_Type
to_bytecode_type(Type_Storage kind) {
    switch (kind) {
        case TYPE_BOOL:
        case TYPE_S8:
        case TYPE_S16:
        case TYPE_S32:
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32: return BC_S32;
        
        case TYPE_S64:
        case TYPE_U64: return BC_S64;
        
        case TYPE_F32: return BC_F32;
        
        case TYPE_F64: return BC_F64;
        
        case TYPE_STRING:
        case TYPE_CSTRING:
        case TYPE_STRUCT:
        case TYPE_UNION:
        case TYPE_ARRAY_FIXED:
        case TYPE_ARRAY_RESIZABLE:
        case TYPE_ARRAY_VIEW: 
        case TYPE_PROCEDURE: return BC_PTR;
        default: unimplemented;
    }
}

struct Bytecode_Procedure {
    u32 proc_index;
    
    int register_count;
    int first_insn;
    
    Ast_Procedure* proc;
};

enum Bytecode_Import_Kind {
    BC_IMPORT_NONE,
    BC_IMPORT_FUNC,
    BC_IMPORT_GLOBAL,
};

struct Bytecode_Import {
    string_id module;
    string_id name;
    
    Bytecode_Import_Kind kind;
    union {
        u32 proc_index;
        u32 global_index;
    };
    
    u32 iat_offset;
};

struct Bytecode_Export {
    string_id procedure;
    u32 proc_index;
};

enum Bytecode_Memory_Kind {
    BC_MEM_READ_ONLY,
    BC_MEM_READ_WRITE,
};

struct Bytecode_Global {
    Ast* initializer;
    void* jit_address;
    u32 offset;
    u32 size, align;
    Bytecode_Memory_Kind kind;
    // TODO(Alexander): maybe we should add reference count so we
    // can safely exclude the data if we optimized out this
};

struct Bytecode_Module {
    
    array(Bytecode_Import)* imports;
    array(Bytecode_Export)* exports;
    
    array(Bytecode_Procedure*)* procedures;
    array(string_id)* procedure_names;
    
    array(Bytecode_Global)* globals;
    
    int entry_proc_index;
};

#define Bytecode_Instruction_Base \
Bytecode_Operator opcode; \
Bytecode_Type type; \
s32 next_insn; \
cstring comment;

// Base structure, can be pointer casted to any of the other instruction types below
struct Bytecode_Instruction {
    Bytecode_Instruction_Base;
};
global Bytecode_Instruction bc_end_of_procedure = {};

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
    
    u32 proc_index;
    s32 arg_count;
    // argument operands followed by return operands
};

inline int*
bc_call_args(Bytecode_Call* call) {
    return (int*) (call + 1);
}

struct Bytecode_Call_Indirect {
    Bytecode_Instruction_Base;
    
    int proc_ptr_index;
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
iter_bytecode_instructions(Bytecode_Procedure* proc, Bytecode_Instruction* iter) {
    if (!iter) {
        if (!proc->first_insn) {
            return &bc_end_of_procedure;
        }
        
        iter = (Bytecode_Instruction*) ((u8*) proc + proc->first_insn);
        return iter;
    }
    
    if (!iter->next_insn) {
        return &bc_end_of_procedure;
    }
    
    iter = (Bytecode_Instruction*) ((u8*) iter + iter->next_insn);
    return iter;
}

#define for_bc_insn(proc, insn) \
for (Bytecode_Instruction* insn = iter_bytecode_instructions(proc, 0); \
insn->opcode; \
insn = iter_bytecode_instructions(proc, insn))