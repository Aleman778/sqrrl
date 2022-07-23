
// BC(name, num_operands)
#define DEF_BYTECODES \
BC(noop, 0) \
BC(stack_alloc, 2) \
BC(memory_alloc, 2) \
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

inline bool
bc_opcode_is_conditional(Bc_Opcode opcode) {
    return opcode >= Bytecode_cmpeq && opcode <= Bytecode_cmpgt;
}

// NOTE(Alexander): forward declare
struct Bc_Instruction;

enum Bc_Decl_Kind {
    BcDecl_Global,
    BcDecl_Function,
    BcDecl_Basic_Block,
};

struct Bc_Decl {
    Bc_Decl_Kind kind;
    union {
        Bc_Instruction* Global;
        
    };
};

enum Bc_Type_Kind {
    BcType_None,
    
    BcType_s1,
    BcType_s8,
    BcType_s16,
    BcType_s32,
    BcType_s64,
    
    BcType_u8,
    BcType_u16,
    BcType_u32,
    BcType_u64,
    
    BcType_f32,
    BcType_f64,
    
    BcType_Aggregate,
};

struct Bc_Type {
    Bc_Type_Kind kind;
    Type* aggregate;
    u32 ptr_depth;
};


inline bool
is_bc_type_floating(Bc_Type_Kind kind) {
    return kind == BcType_f32 || kind == BcType_f64;
}

inline bool
is_bc_type_sint(Bc_Type_Kind kind) {
    return kind >= BcType_s1 && kind <= BcType_s64;
}

inline s32
bc_type_to_bitsize(Bc_Type_Kind kind) {
    switch (kind) {
        case BcType_s1: return 1;
        case BcType_s8: return 8;
        case BcType_s16: return 16;
        case BcType_s32: return 32;
        case BcType_s64: return 64;
        
        case BcType_u8: return 8;
        case BcType_u16: return 16;
        case BcType_u32: return 32;
        case BcType_u64: return 64;
        
        case BcType_f32: return 32;
        case BcType_f64: return 64;
    }
    
    return 0;
}

inline s32
bc_type_to_size(Bc_Type type) {
    if (type.ptr_depth > 0) {
        return 8; // arch dep!
    }
    
    switch (type.kind) {
        case BcType_s1: return 1;
        case BcType_s8: return 1;
        case BcType_s16: return 2;
        case BcType_s32: return 4;
        case BcType_s64: return 8;
        
        case BcType_u8: return 1;
        case BcType_u16: return 2;
        case BcType_u32: return 4;
        case BcType_u64: return 8;
        
        case BcType_f32: return 4;
        case BcType_f64: return 8;
        
        case BcType_Aggregate: {
            return type.aggregate->cached_size;
        } break;
    }
    
    return 0;
}

inline s32
bc_type_to_align(Bc_Type type) {
    if (type.ptr_depth > 0) {
        return 8; // arch dep!
    }
    
    switch (type.kind) {
        case BcType_s1: return 1;
        case BcType_s8: return 1;
        case BcType_s16: return 2;
        case BcType_s32: return 4;
        case BcType_s64: return 8;
        
        case BcType_u8: return 1;
        case BcType_u16: return 2;
        case BcType_u32: return 4;
        case BcType_u64: return 8;
        
        case BcType_f32: return 4;
        case BcType_f64: return 8;
        
        case BcType_Aggregate: {
            return type.aggregate->cached_align;
        } break;
    }
    
    return 0;
}

Value_Type
bc_type_to_value_type(Bc_Type_Kind kind) {
    switch (kind) {
        case BcType_s1:
        case BcType_s8:
        case BcType_s16:
        case BcType_s32:
        case BcType_s64: return Value_signed_int;
        
        case BcType_u8:
        case BcType_u16:
        case BcType_u32:
        case BcType_u64: return Value_unsigned_int;
        
        case BcType_f32:
        case BcType_f64: return Value_floating;
        
        default: {
            assert(0 && "unsupprted bytecode type");
        } break;
    }
    
    return Value_void;
}

Primitive_Type_Kind
bc_type_to_primitive_type_kind(Bc_Type_Kind kind) {
    switch (kind) {
        case BcType_s1:  return PrimitiveType_bool;
        case BcType_s8:  return PrimitiveType_s8;
        case BcType_s16: return PrimitiveType_s16;
        case BcType_s32: return PrimitiveType_s32;
        case BcType_s64: return PrimitiveType_s64;
        
        case BcType_u8:  return PrimitiveType_u8;
        case BcType_u16: return PrimitiveType_u16;
        case BcType_u32: return PrimitiveType_u32;
        case BcType_u64: return PrimitiveType_u64;
        
        case BcType_f32: return PrimitiveType_f32;
        case BcType_f64: return PrimitiveType_f64;
        
        default: {
            assert(0 && "invalid bytecode primitive type");
        } break;
    }
    
    return PrimitiveType_void;
}

inline bool
is_bc_type_uint(Bc_Type_Kind kind) {
    return kind >= BcType_u8 && kind <= BcType_u64;
}

global const Bc_Type bc_type_s1 = { BcType_s1, 0 };

enum Bc_Operand_Kind {
    BcOperand_None,
    BcOperand_Void, // do nothing
    BcOperand_Register,
    BcOperand_Value,
    BcOperand_Basic_Block,
    BcOperand_Argument_List,
    BcOperand_Type
};

struct Bc_Register {
    string_id ident;
    u32 index;
};

struct Bc_Basic_Block {
    Bc_Register label;
    Bc_Instruction* first;
    umm count;
    Bc_Basic_Block* next;
    //array(u32)* args;
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
        array(Bc_Operand)* Argument_List;
    };
};

struct Bc_Instruction {
    Bc_Opcode opcode;
    union {
        struct {
            Bc_Operand op0;
            Bc_Operand op1;
            Bc_Operand op2;
        };
        struct {
            Bc_Operand dest;
            Bc_Operand src0;
            Bc_Operand src1;
        };
        struct {
            Bc_Operand cond;
            Bc_Operand true_block;
            Bc_Operand false_block;
        };
        struct {
            Bc_Operand ret;
            Bc_Operand unused0;
            Bc_Operand unused1;
        };
    };
};

typedef map(Bc_Register, Value) Bc_Label_To_Value_Table;

bool
string_builder_push(String_Builder* sb, Bc_Type type) {
    switch (type.kind) {
        case BcType_None: return false;
        
        case BcType_s1: string_builder_push(sb, "s1"); break;
        case BcType_s8: string_builder_push(sb, "s8"); break;
        case BcType_s16: string_builder_push(sb, "s16"); break;
        case BcType_s32: string_builder_push(sb, "s32"); break;
        case BcType_s64: string_builder_push(sb, "s64"); break;
        
        case BcType_u8: string_builder_push(sb, "u8"); break;
        case BcType_u16: string_builder_push(sb, "u16"); break;
        case BcType_u32: string_builder_push(sb, "u32"); break;
        case BcType_u64: string_builder_push(sb, "u64"); break;
        
        case BcType_f32: string_builder_push(sb, "f32"); break;
        case BcType_f64: string_builder_push(sb, "f64"); break;
    }
    
    
    for (u32 i = 0; i < type.ptr_depth; i++) {
        string_builder_push(sb, "*");
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb, Bc_Register reg, bool is_label = false) {
    if (reg.index == 0) {
        string_builder_push_format(sb, "%", f_string(vars_load_string(reg.ident)));
    } else {
        if (is_label) {
            string_builder_push_format(sb, "%", f_string(vars_load_string(reg.ident)));
        }
        
        string_builder_push_format(sb, "%", f_u32(reg.index));
    }
}

void
string_builder_push(String_Builder* sb, Value_Data value, Bc_Type type) {
    if (type.ptr_depth == 0) {
        switch (type.kind) {
            case BcType_s1:  
            case BcType_s8:  
            case BcType_s16: 
            case BcType_s32: 
            case BcType_s64: {
                string_builder_push_format(sb, "%", f_s64(value.signed_int));
            } break;
            
            case BcType_u8:  
            case BcType_u16: 
            case BcType_u32: 
            case BcType_u64: {
                string_builder_push_format(sb, "%", f_u64(value.unsigned_int));
            } break;
            
            case BcType_f32: 
            case BcType_f64: {
                string_builder_push_format(sb, "%", f_float(value.floating));
            } break;
            
            case BcType_Aggregate: {
                assert(type.aggregate);
                switch (type.aggregate->kind) {
                    case Type_String: {
                        string_builder_push_format(sb, "\"%\"", f_string(value.str));
                    } break;
                }
            } break;
        }
    } else {
        string_builder_push_format(sb, "%", f_u64_HEX(value.unsigned_int));
    }
}

bool
string_builder_push(String_Builder* sb, Bc_Operand* operand, bool show_type = true) {
    switch (operand->kind) {
        case BcOperand_None:
        case BcOperand_Void: return false;
        
        case BcOperand_Basic_Block: {
            string_builder_push(sb, "%");
            string_builder_push(sb, operand->Basic_Block->label, true);
        } break;
        
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
        
        case BcOperand_Argument_List: {
            string_builder_push(sb, "[");
            if (operand->Argument_List) {
                for_array_v(operand->Argument_List, arg, arg_index) {
                    string_builder_push(sb, &arg);
                    if (arg_index < array_count(operand->Argument_List) - 1) {
                        string_builder_push(sb, ", ");
                    }
                }
            }
            string_builder_push(sb, "]");
        } break;
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb, Bc_Instruction* insn) {
    if (insn->opcode == Bytecode_label) {
        Bc_Basic_Block* block = insn->dest.Basic_Block;
        string_builder_push(sb, block->label, true);
        string_builder_push(sb, ":");
    } else {
        bool is_opcode_assign = (insn->opcode == Bytecode_branch ||
                                 insn->opcode == Bytecode_ret);
        
        string_builder_push(sb, "    ");
        
        if (!is_opcode_assign) {
            if (string_builder_push(sb, &insn->dest, true)) {
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