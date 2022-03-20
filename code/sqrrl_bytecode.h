
// BC(name, num_operands)
#define DEF_BYTECODES \
BC(noop, 0) \
BC(stack_alloc, 2) \
BC(load, 2) \
BC(store, 2) \
BC(neg, 2) \
BC(not, 2) \
BC(mul, 3) \
BC(div, 3) \
BC(mod, 3) \
BC(add, 3) \
BC(sub, 3) \
BC(shl, 3) \
BC(shr, 3) \
BC(lt, 3) \
BC(le, 3) \
BC(gt, 3) \
BC(ge, 3) \
BC(eq, 3) \
BC(neq, 3) \
BC(and, 3) \
BC(or, 3) \
BC(xor, 3) \
BC(land, 3) \
BC(lor, 3) \
BC(cmpeq, 3) \
BC(cmpneq, 3) \
BC(cmple, 3) \
BC(cmplt, 3) \
BC(cmpge, 3) \
BC(cmpgt, 3) \
BC(branch, 3) \
BC(label, 1) \
BC(param, 1) \
BC(call, 1) \
BC(ret, 1)

enum Bc_Opcode {
#define BC(name, ...) Bytecode_##name,
    DEF_BYTECODES
#undef BC
};

global cstring bytecode_opcode_names[] = {
#define BC(name, ...) #name,
    DEF_BYTECODES
#undef BC
};

global u32 bytecode_num_operands[] = {
#define BC(_, num_operands) num_operands,
    DEF_BYTECODES
#undef BC
};

enum Bc_Type_Kind {
    BcTypeKind_None,
    
    BcTypeKind_s1,
    BcTypeKind_s8,
    BcTypeKind_s16,
    BcTypeKind_s32,
    BcTypeKind_s64,
    
    BcTypeKind_u8,
    BcTypeKind_u16,
    BcTypeKind_u32,
    BcTypeKind_u64,
    
    BcTypeKind_f32,
    BcTypeKind_f64,
    
    BcTypeKind_Aggregate,
};

struct Bc_Type {
    Bc_Type_Kind kind;
    u32 ptr_depth;
};

enum Bc_Operand_Kind {
    BcOperand_None,
    BcOperand_Register,
    BcOperand_Const,
    BcOperand_Basic_Block,
    BcOperand_Type
};

struct Bc_Register {
    string_id ident;
    u32 index;
};

struct Bc_Const {
    Value value;
};

// NOTE(Alexander): forward declare
struct Bc_Instruction;

struct Bc_Basic_Block {
    Bc_Register label;
    Bc_Instruction* first;
    umm count;
    Bc_Basic_Block* next;
};

struct Bc_Operand {
    Bc_Operand_Kind kind;
    Bc_Type type;
    union {
        Bc_Register Register;
        Bc_Const Const;
        Bc_Basic_Block* Basic_Block;
    };
};

struct Bc_Instruction {
    Bc_Opcode opcode;
    Bc_Operand dest;
    Bc_Operand src0;
    Bc_Operand src1;
};

bool
string_builder_push(String_Builder* sb, Bc_Type* type) {
    switch (type->kind) {
        case BcTypeKind_None: return false;
        
        case BcTypeKind_s1: string_builder_push(sb, "s1"); break;
        case BcTypeKind_s8: string_builder_push(sb, "s8"); break;
        case BcTypeKind_s16: string_builder_push(sb, "s16"); break;
        case BcTypeKind_s32: string_builder_push(sb, "s32"); break;
        case BcTypeKind_s64: string_builder_push(sb, "s64"); break;
        
        case BcTypeKind_u8: string_builder_push(sb, "u8"); break;
        case BcTypeKind_u16: string_builder_push(sb, "u16"); break;
        case BcTypeKind_u32: string_builder_push(sb, "u32"); break;
        case BcTypeKind_u64: string_builder_push(sb, "u64"); break;
    }
    
    
    for (u32 i = 0; i < type->ptr_depth; i++) {
        string_builder_push(sb, "*");
    }
    
    return true;
}

bool
string_builder_push(String_Builder* sb, Bc_Operand* operand, bool show_type = true) {
    switch (operand->kind) {
        case BcOperand_None: return false;
        
        case BcOperand_Register: {
            if (show_type && string_builder_push(sb, &operand->type)) {
                string_builder_push(sb, " ");
            }
            string_builder_push(sb, "%");
            
            if (operand->Register.ident > 0) {
                string_builder_push(sb, vars_load_string(operand->Register.ident));
            }
            string_builder_push_format(sb, "%", f_u32(operand->Register.index));
        } break;
        
        case BcOperand_Const: {
            string_builder_push(sb, &operand->Const.value);
        } break;
        
        case BcOperand_Type: {
            if (show_type) string_builder_push(sb, &operand->type);
        } break;
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb, Bc_Instruction* insn) {
    if (insn->opcode == Bytecode_label) {
        Bc_Basic_Block* block = insn->dest.Basic_Block;
        if (block->label.ident > 0) {
            string_builder_push_format(sb, "%:", f_string(vars_load_string(block->label.ident)));
        } else {
            string_builder_push_format(sb, "%:", f_u32(block->label.index));
        }
    } else {
        string_builder_push(sb, "    ");
        if (string_builder_push(sb, &insn->dest, false)) {
            string_builder_push(sb, " = ");
        }
        
        string_builder_push(sb, bytecode_opcode_names[insn->opcode]);
        
        if (insn->src0.kind) {
            string_builder_push(sb, " ");
        }
        
        if (string_builder_push(sb, &insn->src0)) {
            if (insn->src1.kind) {
                string_builder_push(sb, ", ");
            }
        }
        
        string_builder_push(sb, &insn->src1);
    }
}