
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

enum Bc_Type_Kind {
    BcType_void,
    
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

inline Bc_Type
create_bc_type(Bc_Type_Kind type_kind) {
    Bc_Type result = {};
    result.kind = type_kind;
    return result;
}

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

inline bool
is_bc_type_uint(Bc_Type_Kind kind) {
    return kind >= BcType_u8 && kind <= BcType_u64;
}

global const Bc_Type bc_type_s1 = { BcType_s1, 0 };

enum Bc_Operand_Kind {
    BcOperand_None,
    BcOperand_Void, // do nothing
    BcOperand_Register,
    BcOperand_Int,
    BcOperand_String,
    BcOperand_Float,
    BcOperand_Label,
    BcOperand_Argument_List,
    BcOperand_Type
};

inline bool
is_bc_operand_value(Bc_Operand_Kind kind) {
    return (kind == BcOperand_Int || kind == BcOperand_Float );
}

// NOTE(Alexander): forward declare
struct Bc_Instruction;

typedef u64 Bc_Register;

struct Bc_Label {
    string_id ident;
    u32 index;
};

struct Bc_Basic_Block {
    Bc_Label label;
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

// NOTE(Alexander): forward declare
struct Bc_Argument;

struct Bc_Operand {
    Bc_Operand_Kind kind;
    union {
        Bc_Register Register;
        Bc_Label Label;
        s64 Signed_Int;
        u64 Unsigned_Int;
        f64 Float;
        Memory_String String;
        array(Bc_Argument)* Argument_List;
        Bc_Type Type;
    };
};

struct Bc_Argument {
    Bc_Type type;
    Bc_Operand src;
};

struct Bc_Instruction {
    Bc_Opcode opcode;
    Bc_Type dest_type;
    Bc_Operand dest;
    Bc_Operand src0;
    Bc_Operand src1;
};

enum Bc_Decl_Kind {
    BcDecl_Data,
    BcDecl_Basic_Block,
    BcDecl_Procedure,
};

struct Bc_Decl {
    Bc_Decl_Kind kind;
    umm first_byte_offset;
    union {
        struct {
            Bc_Type type;
            Value_Data value;
        } Data;
        
        struct {
            Bc_Register first_register;
            Bc_Register first_return_reg;
            Bc_Register first_arg_reg;
        } Procedure;
    };
};


bool
string_builder_push(String_Builder* sb, Bc_Type type) {
    switch (type.kind) {
        case BcType_void: return false;
        
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
        
        case BcType_Aggregate: {
            assert(type.aggregate);
            if (type.aggregate->kind == Type_Function) {
                string_builder_push(sb, vars_load_string(type.aggregate->Function.ident));
            } else {
                string_builder_push(sb, type.aggregate);
            }
        } break;
    }
    
    
    for (u32 i = 0; i < type.ptr_depth; i++) {
        string_builder_push(sb, "*");
    }
    
    return true;
}

inline void
string_builder_push(String_Builder* sb, Bc_Label label) {
    assert(label.ident > 0);
    
    string_builder_push_format(sb, "%", f_string(vars_load_string(label.ident)));
    
    if (label.index > 0) {
        string_builder_push_format(sb, "%", f_u32(label.index));
    }
}

// NOTE(Alexander): forward declare below
void string_builder_push(String_Builder* sb, array(Bc_Argument)* argument_list, bool show_types=false);

bool
string_builder_push(String_Builder* sb, Bc_Operand* operand, Bc_Type type={}) {
    switch (operand->kind) {
        case BcOperand_None:
        case BcOperand_Void: return false;
        
        case BcOperand_Label: {
            string_builder_push(sb, "%");
            string_builder_push(sb, operand->Label);
        } break;
        
        case BcOperand_Register: {
            string_builder_push_format(sb, "r%", f_u64(operand->Register));
        } break;
        
        case BcOperand_Int: {
            string_builder_push_format(sb, "%", f_s64(operand->Signed_Int));
        } break;
        
        case BcOperand_Float: {
            string_builder_push_format(sb, "%", f_u64(operand->Float));
        } break;
        
        case BcOperand_Type: {
            string_builder_push(sb, operand->Type);
        } break;
        
        case BcOperand_Argument_List: {
            string_builder_push(sb, operand->Argument_List);
        } break;
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb, array(Bc_Argument)* argument_list, bool show_types) {
    string_builder_push(sb, "(");
    if (argument_list) {
        for_array(argument_list, arg, arg_index) {
            if (show_types) {
                string_builder_push(sb, arg->type);
                string_builder_push(sb, " ");
            }
            string_builder_push(sb, &arg->src);
            if (arg_index < array_count(argument_list) - 1) {
                string_builder_push(sb, ", ");
            }
        }
    }
    string_builder_push(sb, ")");
}

void
string_builder_push(String_Builder* sb, Bc_Instruction* insn) {
    if (insn->opcode == Bytecode_label) {
        string_builder_push(sb, "  ");
        string_builder_push(sb, insn->dest.Label);
        string_builder_push(sb, ":");
    } else {
        bool is_opcode_assign = !(insn->opcode == Bytecode_store ||
                                  insn->opcode == Bytecode_ret);
        
        string_builder_push(sb, "    ");
        
        bool has_assignment = false;
        if (is_opcode_assign) {
            has_assignment = string_builder_push(sb, insn->dest_type);
        }
        
        if (has_assignment) {
            string_builder_push(sb, " ");
            string_builder_push(sb, &insn->dest);
            string_builder_push(sb, " = ");
        }
        
        string_builder_push(sb, bytecode_opcode_names[insn->opcode]);
        
        if (!has_assignment && insn->dest.kind != BcOperand_None) {
            string_builder_push(sb, " ");
            string_builder_push(sb, &insn->dest);
        }
        
        if (insn->src0.kind) {
            if (!has_assignment && insn->dest.kind != BcOperand_None) {
                string_builder_push(sb, ",");
            }
            
            string_builder_push(sb, " ");
        }
        
        if (string_builder_push(sb, &insn->src0)) {
            if (insn->src1.kind && insn->opcode != Bytecode_call) {
                string_builder_push(sb, ", ");
            }
        }
        
        string_builder_push(sb, &insn->src1);
    }
}

void
string_builder_push(String_Builder* sb, Bc_Basic_Block* block) {
    Bc_Basic_Block* curr_block = block;
    while (curr_block) {
        Bc_Instruction* curr_insn = curr_block->first;
        for (int i = 0; i < curr_block->count; i++) {
            string_builder_push(sb, curr_insn++);
            string_builder_push(sb, "\n");
        }
        
        curr_block = curr_block->next;
    }
}