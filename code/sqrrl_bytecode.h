
struct Bytecode_Function;

struct Bytecode_Import;

struct Bytecode {
    array(Bytecode_Import)* imports;
    
    array(Bytecode_Function*)* functions;
    array(string_id)* function_names;
    
    int entry_func_index;
};

struct Bytecode_Import {
    string_id module;
    string_id function;
    
    u32 rdata_offset;
};

#define DEF_BYTECODE_OPERATORS \
OP(NOOP) \
OP(DEBUG_BREAK) \
OP(CONST) \
OP(ALLOCA) \
OP(LOAD) \
OP(STORE) \
OP(LOAD_FUNCTION_PTR) \
OP(CALL) \
OP(RETURN) \
OP(BLOCK) \
OP(LOOP) \
OP(END) \
OP(BRANCH) \
OP(MOV) \
OP(MOV_8) \
OP(MOV_16) \
OP(MOV_32) \
OP(WRAP_I64) \
OP(MEMORY_COPY) \
OP(MEMORY_SET) \
OP(EXTEND) \
OP(CONVERT_S32) \
OP(CONVERT_U32) \
OP(CONVERT_S64) \
OP(CONVERT_U64) \
OP(CONVERT_F32_S) \
OP(CONVERT_F32_U) \
OP(CONVERT_F64_S) \
OP(CONVERT_F64_U) \
OP(CONVERT_F2F) \
OP(REINTERPRET_F2I) \
OP(NEG) \
OP(NOT) \
OP(INC) \
OP(DEC) \
OP(ADDR_OF) \
OP(DEREF) \
OP(ADD) \
OP(SUB) \
OP(MUL) \
OP(DIV_S) \
OP(DIV_U) \
OP(MOD_S) \
OP(MOD_U) \
OP(AND) \
OP(OR) \
OP(XOR) \
OP(SHL) \
OP(SAR) \
OP(SHR) \
OP(EQ) \
OP(GT_S) \
OP(GT_U) \
OP(GE_S) \
OP(GE_U) \
OP(LT_U) \
OP(LT_S) \
OP(LE_U) \
OP(LE_S) \
OP(NEQ) \
OP(RDTSC)


enum Bytecode_Operator {
#define OP(name) BC_##name,
    DEF_BYTECODE_OPERATORS
#undef OP
};

global const cstring bc_opcode_names[] = {
#define OP(name) #name,
    DEF_BYTECODE_OPERATORS
#undef OP
};

global const cstring bc_type_names[] = {
    "", "i32", "i64", "f32", "f64"
};

enum {
    BC_FLAG_8BIT   = bit(0),
    BC_FLAG_16BIT  = bit(1),
    BC_FLAG_32BIT  = bit(2),
    BC_FLAG_64BIT  = bit(3),
    BC_FLAG_BOOL   = bit(4),
    BC_FLAG_FLOAT  = bit(5),
    BC_FLAG_SIGNED = bit(6),
};
typedef u8 Bytecode_Flags;

#define BC_SIZE_MASK (BC_FLAG_8BIT | BC_FLAG_16BIT | BC_FLAG_32BIT | BC_FLAG_64BIT)
#define BC_SIZE_PLUS_SIGNED_MASK (BC_FLAG_8BIT | BC_FLAG_16BIT | BC_FLAG_32BIT | BC_FLAG_64BIT | BC_FLAG_SIGNED)

enum Bytecode_Type {
    BytecodeType_i32,
    BytecodeType_i64,
    BytecodeType_f32,
    BytecodeType_f64,
};

//struct Bytecode_Type {
//Bytecode_Flags flags;
//};

struct Bytecode_Function_Arg {
    Bytecode_Type type;
    u32 size, align;
};

struct Bytecode_Function {
    array(Bytecode_Flags)* register_types;
    int register_count;
    
    union {
        void* code_ptr;
        string_id intrinsic_id;
    };
    
    u32 relative_ptr;
    
    u32 type_index;
    
    u32 insn_count;
    u32 block_count;
    
    u32 arg_count;
    u32 ret_count;
    
    u32 max_caller_arg_count;
    
    u32 first_insn; // relative pointer to first instruction
    
    bool is_imported;
    bool is_intrinsic;
    
    // followed by array of Bytecode_Function_Arg, function arguments then return types and lastly instructions
};

inline Bytecode_Flags
register_type(Bytecode_Function* func, int register_index) {
    return func->register_types[register_index];
}


enum Bytecode_Operand_Kind {
    BytecodeOperand_empty,
    
    BytecodeOperand_const,
    BytecodeOperand_register,
    BytecodeOperand_memory,
};

enum Bytecode_Memory_Kind {
    BytecodeMemory_absolute,
    
    BytecodeMemory_read_only,
    BytecodeMemory_read_write,
};

global cstring bc_memory_kind_names[] = { 
    "", "rodata", "data"
};


// TODO: maybe we want to ecode the kind and type as part of the instruction?
struct Bytecode_Operand {
    Bytecode_Operand_Kind kind;
    union {
        s32 const_i32;
        s64 const_i64;
        f32 const_f32;
        f64 const_f64;
        
        u32 register_index;
        
        struct {
            Bytecode_Memory_Kind memory_kind;
            union {
                s32 memory_offset;
                
                // TODO(Alexander): hack!!! we need a way to remove this later (needed for X64 JIT)
                void* memory_absolute;
            };
        };
    };
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
Bytecode_Type type; \
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

struct Bytecode_Alloca {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand dest;
    u32 size, align;
};

struct Bytecode_Unary {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand first;
};

#define bc_unary_first(insn) (((Bytecode_Unary*) insn)->first)

struct Bytecode_Binary {
    Bytecode_Instruction_Base;
    
    int res_index;
    int arg0_index;
    int arg1_index;
    
    s64 const_i64;
    
    
    Bytecode_Operand first;
    Bytecode_Operand second;
};

#define bc_binary_first(insn) (((Bytecode_Binary*) insn)->first)
#define bc_binary_second(insn) (((Bytecode_Binary*) insn)->second)

struct Bytecode_Call {
    Bytecode_Instruction_Base;
    
    u32 func_index;
    // argument operands followed by return operands
};

struct Bytecode_Block {
    Bytecode_Instruction_Base;
    
    u32 label_index;
    u32 end_insn;
};

struct Bytecode_Branch {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand cond;
    
    u32 label_index;
    // TODO(Alexander): maybe have a true and false label?
};

struct Bytecode_Memory {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand dest;
    union {
        Bytecode_Operand src;
        u8 value;
    };
    s32 size;
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