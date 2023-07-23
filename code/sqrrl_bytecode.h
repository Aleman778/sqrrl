
struct Bytecode_Function;

struct Bytecode {
    
    array(Bytecode_Function*)* functions;
    array(string_id)* function_names;
    
    int entry_func_index;
};

#define DEF_BYTECODE_OPERATORS \
OP(NOOP) \
OP(DEBUG_BREAK) \
OP(LOAD_FUNCTION_PTR) \
OP(CALL) \
OP(RETURN) \
OP(BLOCK) \
OP(BRANCH) \
OP(MOV) \
OP(MOV_8) \
OP(MOV_16) \
OP(MOV_32) \
OP(WRAP_I64) \
OP(EXTEND_S8) \
OP(EXTEND_S16) \
OP(EXTEND_S32) \
OP(EXTEND_U8) \
OP(EXTEND_U16) \
OP(EXTEND_U32) \
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
OP(NEQ)


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

enum Bytecode_Type : u8 {
    BytecodeType_void,
    BytecodeType_i32,
    BytecodeType_i64,
    BytecodeType_f32,
    BytecodeType_f64
};

global const cstring bc_type_names[] = {
    "", "i32", "i64", "f32", "f64"
};

struct Stack_Entry {
    u32 size;
    u32 align;
    Bytecode_Type type;
};

struct Bytecode_Function {
    array(u32)* labels;
    array(u32)* register_lifetimes;
    array(Stack_Entry)* stack;
    
    // Used by backends
    union {
        u8* x64_machine_code_ptr;
        struct {
            u32 blocks;
            u32 loops;
        } wasm;
    };
    
    u32 relative_ptr;
    u32 type_index;
    u32 insn_count;
    
    u32 arg_count;
    u32 ret_count;
    
    u32 first_insn; // relative pointer to first instruction
    
    // followed by Bytecode_Type, function arguments then return types and lastly instructions
};

enum Bytecode_Operand_Kind {
    BytecodeOperand_empty,
    
    BytecodeOperand_const_i32,
    BytecodeOperand_const_i64,
    BytecodeOperand_const_f32,
    BytecodeOperand_const_f64,
    BytecodeOperand_register,
    BytecodeOperand_stack,
    BytecodeOperand_memory,
};

const Bytecode_Operand_Kind bc_type_to_const_operand[] = {
    BytecodeOperand_empty,
    BytecodeOperand_const_i32,
    BytecodeOperand_const_i64,
    BytecodeOperand_const_f32,
    BytecodeOperand_const_f64,
};

inline Bytecode_Operand_Kind
get_const_operand_from_type(Bytecode_Type type) {
    return bc_type_to_const_operand[type];
}

enum Bytecode_Memory_Kind {
    BytecodeMemory_read_only,
    BytecodeMemory_read_write,
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
            u32 stack_index;
            s32 stack_offset;
        };
        
        struct {
            u32 memory_offset;
            Bytecode_Memory_Kind memory_kind;
        };
    };
};

enum Bytecode_Instruction_Kind {
    BytecodeInstructionKind_None,
    
    BytecodeInstructionKind_Base,
    BytecodeInstructionKind_Unary,
    BytecodeInstructionKind_Binary,
    BytecodeInstructionKind_Call,
    BytecodeInstructionKind_Block,
    BytecodeInstructionKind_Branch,
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

struct Bytecode_Unary {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand first;
};

struct Bytecode_Binary {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand first;
    Bytecode_Operand second;
};

struct Bytecode_Call {
    Bytecode_Instruction_Base;
    
    u32 func_index;
    // argument operands followed by return operands
};

struct Bytecode_Block {
    Bytecode_Instruction_Base;
    
    u32 label_index;
    
    // Used by backends
    union {
        u8* x64_machine_code_ptr;
        struct {
            u32 blocks;
            u32 loops;
        } wasm;
    };
};

struct Bytecode_Branch {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand cond;
    
    u32 label_index;
    // TODO(Alexander): maybe have a true and false label?
};

#define bc_unary_first(insn) (((Bytecode_Unary*) insn)->first)
#define bc_binary_first(insn) (((Bytecode_Binary*) insn)->first)
#define bc_binary_second(insn) (((Bytecode_Binary*) insn)->second)

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