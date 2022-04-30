
// BC(name, num_operands)
#define DEF_BYTECODES \
BC(noop, 0) \
BC(stack_alloc, 2) \
BC(load, 2) \
BC(store, 2) \
BC(assign, 2) \
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
BC(cmpeq, 3) \
BC(cmpneq, 3) \
BC(cmple, 3) \
BC(cmplt, 3) \
BC(cmpge, 3) \
BC(cmpgt, 3) \
BC(branch, 3) \
BC(label, 1) \
BC(truncate, 3) \
BC(sign_extend, 3) \
BC(zero_extend, 3) \
BC(cast_fp_to_sint, 3) \
BC(cast_fp_to_uint, 3) \
BC(cast_sint_to_fp, 3) \
BC(cast_uint_to_fp, 3) \
BC(fp_extend, 3) \
BC(fp_truncate, 3) \
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

inline bool
is_bc_type_floating(Bc_Type_Kind kind) {
    return kind == BcTypeKind_f32 || kind == BcTypeKind_f64;
}

inline bool
is_bc_type_sint(Bc_Type_Kind kind) {
    return kind >= BcTypeKind_s1 && kind <= BcTypeKind_s64;
}

inline s32
bc_type_to_bitsize(Bc_Type_Kind kind) {
    switch (kind) {
        case BcTypeKind_s1: return 1;
        case BcTypeKind_s8: return 8;
        case BcTypeKind_s16: return 16;
        case BcTypeKind_s32: return 32;
        case BcTypeKind_s64: return 64;
        
        case BcTypeKind_u8: return 8;
        case BcTypeKind_u16: return 16;
        case BcTypeKind_u32: return 32;
        case BcTypeKind_u64: return 64;
        
        case BcTypeKind_f32: return 32;
        case BcTypeKind_f64: return 64;
    }
    
    return 0;
}

Value_Type
bc_type_to_value_type(Bc_Type_Kind kind) {
    switch (kind) {
        case BcTypeKind_s1:
        case BcTypeKind_s8:
        case BcTypeKind_s16:
        case BcTypeKind_s32:
        case BcTypeKind_s64: return Value_signed_int;
        
        case BcTypeKind_u8:
        case BcTypeKind_u16:
        case BcTypeKind_u32:
        case BcTypeKind_u64: return Value_unsigned_int;
        
        case BcTypeKind_f32:
        case BcTypeKind_f64: return Value_floating;
        
        default: {
            assert(0 && "unsupprted bytecode type");
        } break;
    }
    
    return Value_void;
}

Primitive_Type_Kind
bc_type_to_primitive_type_kind(Bc_Type_Kind kind) {
    switch (kind) {
        case BcTypeKind_s1:  return PrimitiveTypeKind_bool;
        case BcTypeKind_s8:  return PrimitiveTypeKind_s8;
        case BcTypeKind_s16: return PrimitiveTypeKind_s16;
        case BcTypeKind_s32: return PrimitiveTypeKind_s32;
        case BcTypeKind_s64: return PrimitiveTypeKind_s64;
        
        case BcTypeKind_u8:  return PrimitiveTypeKind_u8;
        case BcTypeKind_u16: return PrimitiveTypeKind_u16;
        case BcTypeKind_u32: return PrimitiveTypeKind_u32;
        case BcTypeKind_u64: return PrimitiveTypeKind_u64;
        
        case BcTypeKind_f32: return PrimitiveTypeKind_f32;
        case BcTypeKind_f64: return PrimitiveTypeKind_f64;
        
        default: {
            assert(0 && "invalid bytecode primitive type");
        } break;
    }
    
    return PrimitiveTypeKind_void;
}

inline bool
is_bc_type_uint(Bc_Type_Kind kind) {
    return kind >= BcTypeKind_u8 && kind <= BcTypeKind_u64;
}

struct Bc_Type {
    Bc_Type_Kind kind;
    u32 ptr_depth;
};

enum Bc_Operand_Kind {
    BcOperand_None,
    BcOperand_Register,
    BcOperand_Value,
    BcOperand_Basic_Block,
    BcOperand_Type
};

struct Bc_Register {
    string_id ident;
    u32 index;
};

// NOTE(Alexander): forward declare
struct Bc_Instruction;

struct Bc_Basic_Block {
    Bc_Register label;
    Bc_Instruction* first;
    umm count;
    Bc_Basic_Block* next;
};

#define for_bc_basic_block(first_block, it, it_index, code) { \
Bc_Basic_Block* it_block = first_block; \
umm it_index = 0; \
\
while (it_block) { \
while (it_index < it_block->count) { \
Bc_Instruction* it = it_block->first + it_index; \
code; \
it_index++; \
} \
it_block = it_block->next; \
it_index = 0; \
} \
}

struct Bc_Operand {
    Bc_Operand_Kind kind;
    Bc_Type type;
    union {
        Bc_Register Register;
        Value_Data Value;
        Bc_Basic_Block* Basic_Block;
    };
};

struct Bc_Instruction {
    Bc_Opcode opcode;
    Bc_Operand dest;
    Bc_Operand src0;
    Bc_Operand src1;
};

typedef map(Bc_Register, Value_Data) Bc_Label_To_Value_Table;

bool
string_builder_push(String_Builder* sb, Bc_Type type) {
    switch (type.kind) {
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
        
        case BcTypeKind_f32: string_builder_push(sb, "f32"); break;
        case BcTypeKind_f64: string_builder_push(sb, "f64"); break;
    }
    
    
    for (u32 i = 0; i < type.ptr_depth; i++) {
        string_builder_push(sb, "*");
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb, Bc_Register reg) {
    if (reg.index == 0) {
        string_builder_push_format(sb, "%", f_string(vars_load_string(reg.ident)));
    } else {
        string_builder_push_format(sb, "%", f_u32(reg.index));
    }
}

void
string_builder_push(String_Builder* sb, Value_Data value, Bc_Type type) {
    if (type.ptr_depth == 0) {
        switch (type.kind) {
            case BcTypeKind_s1:  
            case BcTypeKind_s8:  
            case BcTypeKind_s16: 
            case BcTypeKind_s32: 
            case BcTypeKind_s64: {
                string_builder_push_format(sb, "%", f_s64(value.signed_int));
            } break;
            
            case BcTypeKind_u8:  
            case BcTypeKind_u16: 
            case BcTypeKind_u32: 
            case BcTypeKind_u64: {
                string_builder_push_format(sb, "%", f_u64(value.unsigned_int));
            } break;
            
            case BcTypeKind_f32: 
            case BcTypeKind_f64: {
                string_builder_push_format(sb, "%", f_float(value.floating));
            } break;
        }
    } else {
        string_builder_push_format(sb, "%", f_u64_HEX(value.unsigned_int));
    }
}

bool
string_builder_push(String_Builder* sb, Bc_Operand* operand, bool show_type = true) {
    switch (operand->kind) {
        case BcOperand_None: return false;
        
        case BcOperand_Register: {
            if (show_type && string_builder_push(sb, operand->type)) {
                string_builder_push(sb, " ");
            }
            string_builder_push(sb, "%");
            string_builder_push(sb, operand->Register);
        } break;
        
        case BcOperand_Value: {
            string_builder_push(sb, operand->Value, operand->type);
        } break;
        
        case BcOperand_Type: {
            if (show_type) string_builder_push(sb, operand->type);
        } break;
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb, Bc_Instruction* insn) {
    if (insn->opcode == Bytecode_label) {
        Bc_Basic_Block* block = insn->dest.Basic_Block;
        string_builder_push(sb, block->label);
        string_builder_push(sb, ":");
    } else {
        bool is_opcode_assign = insn->opcode == Bytecode_branch;
        
        string_builder_push(sb, "    ");
        
        if (!is_opcode_assign) {
            if (string_builder_push(sb, &insn->dest, false)) {
                string_builder_push(sb, " = ");
            }
        }
        
        string_builder_push(sb, bytecode_opcode_names[insn->opcode]);
        
        if (is_opcode_assign) {
            string_builder_push(sb, " ");
            if (string_builder_push(sb, &insn->dest)) {
                if (insn->src1.kind) {
                    string_builder_push(sb, ",");
                }
            }
        }
        
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