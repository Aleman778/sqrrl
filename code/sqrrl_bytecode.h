
// BC(name)
#define DEF_BYTECODES \
BC(noop) \
BC(stack_alloc) \
BC(memory_alloc) \
BC(copy) \
BC(load) \
BC(load_address) \
BC(store) \
BC(memset) \
BC(memcpy) \
BC(field) \
BC(index) \
BC(neg) \
BC(not) \
BC(add) \
BC(sub) \
BC(mul) \
BC(div) \
BC(mod) \
BC(fadd) \
BC(fsub) \
BC(fmul) \
BC(fdiv) \
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
#define BC(name) Bytecode_##name,
    DEF_BYTECODES
#undef BC
};

global const cstring bytecode_opcode_names[] = {
#define BC(name) #name,
    DEF_BYTECODES
#undef BC
};

global const Bc_Opcode binary_op_to_bc_opcode_table[] = {
#define BINOP(name, op, prec, assoc, is_comparator, bc_mnemonic) Bytecode_##bc_mnemonic,
    DEF_BINARY_OPS
#undef BINOP
};

inline bool
bc_opcode_is_conditional(Bc_Opcode opcode) {
    return opcode >= Bytecode_cmpeq && opcode <= Bytecode_cmpgt;
}

typedef Type* Bc_Type;

inline Bc_Type
create_basic_type(Basic_Type type_kind) {
    return &basic_type_definitions[type_kind];
}

inline bool
is_basic_type_floating(Type* type) {
    assert(type->kind == TypeKind_Basic);
    u32 flags = type->Basic.flags;
    return is_bitflag_set(flags, BasicFlag_Floating);
}

inline bool
is_basic_type_uint(Type* type) {
    assert(type->kind == TypeKind_Basic);
    u32 flags = type->Basic.flags;
    return (is_bitflag_set(flags, BasicFlag_Integer) &&
            is_bitflag_set(flags, BasicFlag_Unsigned));
}

inline bool
is_basic_type_sint(Type* type) {
    assert(type->kind == TypeKind_Basic);
    u32 flags = type->Basic.flags;
    return (is_bitflag_set(flags, BasicFlag_Integer) &&
            !is_bitflag_set(flags, BasicFlag_Unsigned));
}

Value_Type
bc_type_to_value_type(Basic_Type basic_type) {
    u32 flags = basic_type_definitions[basic_type].Basic.flags;
    return value_type_from_basic_flags(flags);
}

inline s32
bc_type_to_bitsize(Bc_Type type) {
    return type->size * 8;
}

inline s32
bc_type_to_size(Bc_Type type) {
    return type->size;
}

inline s32
bc_type_to_align(Bc_Type type) {
    return type->align;
}

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


struct Bc_Procedure_Info {
    Bc_Register first_register;
    Bc_Register first_return_reg;
    Bc_Register first_arg_reg;
};

struct Bc_Decl {
    Bc_Decl_Kind kind;
    umm first_byte_offset;
    union {
        struct {
            Bc_Type type;
            Value value;
        } Data;
        
        Bc_Procedure_Info Procedure;
    };
};

inline void
string_builder_push(String_Builder* sb, Bc_Label label) {
    assert(label.ident > 0);
    
    string_builder_push_format(sb, "%", f_string(vars_load_string(label.ident)));
    
    if (label.index > 0) {
        string_builder_push_format(sb, "%", f_u32(label.index));
    }
}

struct Bc_Register_Mapper {
    map(Bc_Register, u64)* table;
    u64 next_register;
    
    ~Bc_Register_Mapper() {
        map_free(table);
    }
};

u64
bc_map_register(Bc_Register_Mapper* mapper, Bc_Register reg) {
#if 0
    u64 result;
    smm index = map_get_index(mapper->table, reg);
    if (index == -1) {
        result = mapper->next_register++;
        map_put(mapper->table, reg, result);
    } else {
        result = mapper->table[index].value;
    }
    return result;
#else
    return reg;
#endif
}


// NOTE(Alexander): forward declare below
void string_builder_push(String_Builder* sb, 
                         Bc_Register_Mapper* mapper,
                         array(Bc_Argument)* argument_list, 
                         bool show_types=false);

bool
string_builder_push(String_Builder* sb, 
                    Bc_Register_Mapper* mapper, 
                    Bc_Operand* operand, 
                    Bc_Type type={}) {
    
    switch (operand->kind) {
        case BcOperand_None:
        case BcOperand_Void: return false;
        
        case BcOperand_Label: {
            string_builder_push(sb, "%");
            string_builder_push(sb, operand->Label);
        } break;
        
        case BcOperand_Register: {
            u64 reg = bc_map_register(mapper, operand->Register);
            string_builder_push_format(sb, "r%", f_u64(reg));
        } break;
        
        case BcOperand_Memory:
        case BcOperand_Stack: {
            u64 reg = bc_map_register(mapper, operand->Register);
            if (type) {
                string_builder_push_format(sb, "r%", f_u64(reg));
            } else {
                string_builder_push_format(sb, "[r%]", f_u64(reg));
            }
        } break;
        
        case BcOperand_Int: {
            string_builder_push_format(sb, "%", f_s64(operand->Signed_Int));
        } break;
        
        case BcOperand_Float: {
            string_builder_push_format(sb, "%", f_float(operand->Float));
        } break;
        
        case BcOperand_String: {
            string_builder_push_format(sb, "\"%\"", f_mstring(operand->String));
        } break;
        
        case BcOperand_Type: {
            string_builder_push(sb, operand->Type);
        } break;
        
        case BcOperand_Argument_List: {
            string_builder_push(sb, mapper, operand->Argument_List);
        } break;
    }
    
    return true;
}

void
string_builder_push(String_Builder* sb,
                    Bc_Register_Mapper* mapper,
                    array(Bc_Argument)* argument_list, 
                    bool show_types) {
    
    string_builder_push(sb, "(");
    if (argument_list) {
        for_array(argument_list, arg, arg_index) {
            if (show_types) {
                string_builder_push(sb, arg->type);
                string_builder_push(sb, " ");
            }
            string_builder_push(sb, mapper, &arg->src);
            if (arg_index < array_count(argument_list) - 1) {
                string_builder_push(sb, ", ");
            }
        }
    }
    string_builder_push(sb, ")");
}

void
string_builder_push(String_Builder* sb, Bc_Register_Mapper* mapper, Bc_Instruction* insn) {
    if (insn->opcode == Bytecode_label) {
        string_builder_push(sb, "\n  ");
        string_builder_push(sb, insn->dest.Label);
        string_builder_push(sb, ":");
    } else {
        bool is_opcode_assign = !(insn->opcode == Bytecode_copy ||
                                  insn->opcode == Bytecode_store ||
                                  insn->opcode == Bytecode_memcpy ||
                                  insn->opcode == Bytecode_memset ||
                                  insn->opcode == Bytecode_ret);
        
        string_builder_push(sb, "    ");
        
        bool has_assignment = false;
        if (is_opcode_assign && insn->dest_type) {
            string_builder_push(sb, insn->dest_type);
            if (insn->dest.kind == BcOperand_Stack || insn->dest.kind == BcOperand_Memory) {
                string_builder_push(sb, "*");
            }
            has_assignment = true;
        }
        
        if (has_assignment) {
            string_builder_push(sb, " ");
            string_builder_push(sb, mapper, &insn->dest, insn->dest_type);
            string_builder_push(sb, " = ");
        }
        
        string_builder_push(sb, bytecode_opcode_names[insn->opcode]);
        
        if (!has_assignment && insn->dest.kind != BcOperand_None) {
            string_builder_push(sb, " ");
            string_builder_push(sb, mapper, &insn->dest);
        }
        
        if (insn->src0.kind) {
            if (!has_assignment && insn->dest.kind != BcOperand_None) {
                string_builder_push(sb, ",");
            }
            
            string_builder_push(sb, " ");
        }
        
        if (string_builder_push(sb, mapper, &insn->src0)) {
            if (insn->src1.kind && insn->opcode != Bytecode_call) {
                string_builder_push(sb, ", ");
            }
        }
        
        string_builder_push(sb, mapper, &insn->src1);
    }
}

void
string_builder_push(String_Builder* sb, 
                    Bc_Register_Mapper* mapper, 
                    Bc_Basic_Block* block, 
                    Bytecode* code) {
    
    Bc_Basic_Block* curr_block = block;
    while (curr_block) {
        Bc_Instruction* curr_insn = (Bc_Instruction*) (curr_block + 1);
        for (int i = 0; i < curr_block->instruction_count; i++) {
            string_builder_push(sb, mapper, curr_insn++);
            string_builder_push(sb, "\n");
        }
        
        curr_block = get_bc_basic_block(code, curr_block->next_byte_offset);
    }
}