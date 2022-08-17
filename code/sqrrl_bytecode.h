
// BC(name, num_operands)
#define DEF_BYTECODES \
BC(noop) \
BC(stack_alloc) \
BC(memory_alloc) \
BC(copy) \
BC(copy_from_ref) \
BC(copy_from_deref) \
BC(copy_to_deref) \
BC(neg) \
BC(not) \
BC(mul) \
BC(div) \
BC(mod) \
BC(add) \
BC(sub) \
BC(shl) \
BC(shr) \
BC(lt) \
BC(le) \
BC(gt) \
BC(ge) \
BC(eq) \
BC(neq) \
BC(and) \
BC(or) \
BC(xor) \
BC(cmpeq) \
BC(cmpneq) \
BC(cmple) \
BC(cmplt) \
BC(cmpge) \
BC(cmpgt) \
BC(goto) \
BC(branch) \
BC(label) \
BC(truncate) \
BC(sign_extend) \
BC(zero_extend) \
BC(float_to_sint) \
BC(float_to_uint) \
BC(sint_to_float) \
BC(uint_to_float) \
BC(float_extend) \
BC(float_truncate) \
BC(call) \
BC(ret)

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
bc_type_to_bitsize(Bc_Type type) {
    if (type.ptr_depth) {
        return 64; // TODO(Alexander): arch dep!
    }
    
    switch (type.kind) {
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
        return 8; // TODO(Alexander): arch dep!
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
    BcOperand_Memory,
    BcOperand_Stack,
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
    smm instruction_count;
    smm next_byte_offset;
};

#define get_first_bc_instruction(block) \
(Bc_Instruction*) ((block) + 1)

#define for_bc_basic_block(bytecode, first_block, it, it_index, code) { \
Bc_Basic_Block* it_block = first_block; \
smm it_index = 0; \
\
while (it_block) { \
while (it_index < it_block->instruction_count) { \
Bc_Instruction* it = get_first_bc_instruction(it_block) + it_index; \
code; \
it_index++; \
} \
it_block = get_bc_basic_block(bytecode, it_block->next_byte_offset); \
it_index = 0; \
} \
}

struct Bytecode {
    array(u8*)* blocks;
    umm block_size;
};

inline Bc_Basic_Block*
get_bc_basic_block(Bytecode* code, smm offset) {
    if (offset < 0) return 0;
    
    assert(code->block_size > 0);
    // TODO(Alexander): maybe we can improve the % and / by using block sizes powers of two
    smm block_index = offset / code->block_size;
    smm block_offset = offset % code->block_size;
    assert(block_index < array_count(code->blocks));
    u8* block = code->blocks[block_index];
    //pln("get_first_bc_basic_block, %: % -> %", f_umm(offset), f_umm(block_index), f_umm(block_offset));
    //pln("num_blocks = %", f_umm(array_count(code->blocks)));
    return (Bc_Basic_Block*) (block + block_offset);
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
        Value_Data Const;
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
        
        case BcOperand_Memory:
        case BcOperand_Stack: {
            string_builder_push_format(sb, "[r%]", f_u64(operand->Register));
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
        bool is_opcode_assign = !(insn->opcode == Bytecode_copy ||
                                  insn->opcode == Bytecode_copy_from_ref ||
                                  insn->opcode == Bytecode_copy_from_deref ||
                                  insn->opcode == Bytecode_copy_to_deref ||
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
string_builder_push(String_Builder* sb, Bc_Basic_Block* block, Bytecode* code) {
    Bc_Basic_Block* curr_block = block;
    while (curr_block) {
        Bc_Instruction* curr_insn = (Bc_Instruction*) (curr_block + 1);
        for (int i = 0; i < curr_block->instruction_count; i++) {
            string_builder_push(sb, curr_insn++);
            string_builder_push(sb, "\n");
        }
        
        curr_block = get_bc_basic_block(code, curr_block->next_byte_offset);
    }
}