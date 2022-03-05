
enum Bc_Opcode {
    Op_Noop,
    Op_Stack_Alloc,
    Op_Store,
    Op_Load,
    Op_Add,
    Op_Return,
};

enum Bc_Type_Kind {
    BcTypeKind_None,
    
#define PRIMITIVE(symbol,...) BcTypeKind_##symbol,
    DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
    
#define PRIMITIVE(symbol,...) BcTypeKind_ptr_##symbol,
    DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
    
    BcTypeKind_Aggregate,
};

struct Bc_Type {
    Bc_Type_Kind kind;
};

enum Bc_Operand_Kind {
    BcOperand_None,
    BcOperand_Register,
    BcOperand_Const,
    BcOperand_Type
};

struct Bc_Register {
    string_id ident;
    u32 index;
};

struct Bc_Operand {
    Bc_Operand_Kind kind;
    Bc_Type type;
    union {
        Bc_Register Register;
        Value Const;
    };
};

struct Bc_Instruction {
    Bc_Opcode opcode;
    Bc_Operand dest;
    Bc_Operand src0;
    Bc_Operand src1;
};

struct Bc_Basic_Block {
    array(Bc_Instruction)* instructions;
};

bool
string_builder_push(String_Builder* sb, Bc_Type* type) {
    switch (type->kind) {
        case BcTypeKind_None: {
            return false;
        } break;
        
        
#define PRIMITIVE(symbol,...) case BcTypeKind_##symbol: string_builder_push(sb, #symbol); break;
        DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
        
#define PRIMITIVE(symbol,...) case BcTypeKind_ptr_##symbol: { \
string_builder_push(sb, "*"); \
string_builder_push(sb, #symbol); \
} break;
        DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
        
        // TODO(Alexander): aggregate types etc.
    }
    
    return true;
}

bool
string_builder_push(String_Builder* sb, Bc_Operand* operand) {
    switch (operand->kind) {
        case BcOperand_None: return false;
        
        case BcOperand_Register: {
            if (string_builder_push(sb, &operand->type)) {
                string_builder_push(sb, " ");
            }
            string_builder_push(sb, "%");
            
            if (operand->Register.ident > 0) {
                string_builder_push(sb, vars_load_string(operand->Register.ident));
            }
            string_builder_push_format(sb, "%", f_u32(operand->Register.index));
        } break;
        
        case BcOperand_Const: {
            string_builder_push(sb, &operand->Const);
        } break;
        
        case BcOperand_Type: {
            string_builder_push(sb, &operand->type);
        } break;
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb, Bc_Instruction* insn, u32 spacing = 0) {
    if (string_builder_push(sb, &insn->dest)) {
        string_builder_push(sb, " = ");
    }
    
    switch (insn->opcode) {
        case Op_Stack_Alloc: string_builder_push(sb, "stack_alloc "); break;
        case Op_Store: string_builder_push(sb, "store "); break;
        case Op_Load: string_builder_push(sb, "load "); break;
        case Op_Add: string_builder_push(sb, "add "); break;
        case Op_Return: string_builder_push(sb, "return "); break;
    }
    
    if (string_builder_push(sb, &insn->src0)) {
        string_builder_push(sb, ", ");
    }
    
    string_builder_push(sb, &insn->src1);
}