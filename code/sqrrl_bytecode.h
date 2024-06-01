
enum Opcode : u8 {
    BC_NOOP,
    
    BC_BEGIN_FUNCTION, // res_index = func_index
    BC_END_FUNCTION,   // res_index = func_index
    
    BC_NEG,
    
    BC_ADD,
    BC_SUB,
    BC_MUL,
    BC_DIV_U,
    BC_DIV_S,
    
    BC_RETURN,
    
    BC_LOAD_CONSTANT,
    
    
    BC_COUNT,
};

global const cstring opcode_names[BC_COUNT] = {
    "NOOP",
    
    "FUNCTION_START",
    "FUNCTION_END",
    
    "NEG",
    
    "ADD",
    "SUB",
    "MUL",
    "DIV_U",
    "DIV_S",
    
    "RETURN",
    
    "LOAD_CONSTANT",
};

inline Opcode
operator_to_opcode(Operator_Kind op, Ast_Type* type) {
    bool is_unsigned = type->flags & TYPE_FLAG_UNSIGNED;
    switch (op) {
        case OP_NEG: return BC_NEG;
        case OP_ADD: return BC_ADD;
        case OP_SUB: return BC_SUB;
        case OP_MUL: return BC_MUL;
        case OP_DIV: return is_unsigned ? BC_DIV_U : BC_DIV_S;
        default: assert(0 && "invalid operator");
    }
    return BC_NOOP;
}

enum Bc_Type {
    BC_PTR,
    BC_I32,
    BC_I64,
    BC_F32,
    BC_F64,
};

struct Bc_Operand {
    
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


struct Bc_Iterator {
    Bc* inst;
    int inst_index;
    
    Bc_Bucket* inst_bucket;
};

inline Bc_Iterator
bc_begin_iterator(Bc_Bucket* bucket) {
    Bc_Iterator result = {};
    if (bucket) {
        result.inst_bucket = bucket;
        result.inst = (Bc*) (bucket + 1);
    }
    
    return result;
}

bool
bc_iterator_next(Bc_Iterator* it) {
    Bc_Bucket* bucket = it->inst_bucket;
    if (!bucket) return false;
    
    it->inst++; it->inst_index++;
    if (it->inst_index >= bucket->count) {
        *it = bc_begin_iterator(bucket->next);
        if (!bucket) return false;
    }
    
    return true;
}

#define for_bc_inst(module, it) \
for (Bc_Iterator it = bc_begin_iterator((module).first_bucket); \
it.inst_bucket; \
bc_iterator_next(&it))



struct Bc_Function {
    int func_index;
};

struct Bc_Module {
    Bc_Bucket* first_bucket;
    
    array(Bc_Type)* register_types;
    //array(Bc_Function)* functions;
    
    int next_func_index;
    
};

