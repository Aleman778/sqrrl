
struct Bytecode_Function;

struct Bytecode {
    
    array(Bytecode_Function*)* functions;
};

#define DEF_BYTECODE_OPERATORS \
OP(NOOP) \
OP(DEBUG_BREAK) \
OP(LABEL) \
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
    
    u32 insn_count;
    u32 first_insn; // relative pointer to first instruction
};

struct Bytecode_Operand {
    
};

enum Bytecode_Instruction_Kind {
    BytecodeInstructionKind_None,
    
    BytecodeInstructionKind_Base,
    BytecodeInstructionKind_Binary,
};

#define Bytecode_Instruction_Base \
Bytecode_Operator opcode; \
Bytecode_Instruction_Kind kind; \
u32 next_insn; \
cstring comment

// NOTE(Alexander): this is just a base structure, most are extended
struct Bytecode_Instruction {
    Bytecode_Instruction_Base;
};

global Bytecode_Instruction bc_empty = {};

struct Binary_Instruction {
    Bytecode_Instruction_Base;
    
    Bytecode_Operand first;
    Bytecode_Operand second;
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
    
    iter = (Bytecode_Instruction*) ((u8*) iter + iter->next_insn);
    return iter;
}
