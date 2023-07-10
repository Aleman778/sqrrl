
struct Bytecode_Function;

struct Bytecode {
    
    array(Bytecode_Function*)* functions;
};

#define DEF_BYTECODE_OPERATORS \
OP(NOOP) \
OP(DEBUG_BREAK) \
OP(LOAD_FUNCTION_PTR) \
OP(RETURN) \
OP(BLOCK) \
OP(LOAD) \
OP(STORE) \
OP(ADD) \
OP(SUB) \


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

enum Bytecode_Type {
    BytecodeType_void,
    BytecodeType_i32,
    BytecodeType_i64,
    BytecodeType_f32,
    BytecodeType_f64
};

struct Bytecode_Function {
    u32 type_index;
    
    u32 ret_count;
    u32 arg_count;
    // followed by Bytecode_Type, returns, args and lastly instructions
    
    u32 first_insn; // relative pointer to first basic block
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
    Bytecode_Type type;
    union {
        s32 const_i32;
        s64 const_i64;
        f32 const_f32;
        f64 const_f64;
        
        u32 register_index;
        
        struct {
            u32 stack_index;
            u32 stack_offset;
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
    BytecodeInstructionKind_Block,
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

struct Bytecode_Unary {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand first;
};

struct Bytecode_Binary {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand first;
    Bytecode_Operand second;
};

// NOTE(Alexander): this is basically a label instruction
struct Bytecode_Block {
    Bytecode_Instruction_Base;
    
    u32 label_index;
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
