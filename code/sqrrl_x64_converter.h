
Ic_Opcode x64_intrin_rdtsc = IC_RDTSC;

enum X64_Reg: u8 {
    X64_RAX,
    X64_RCX,
    X64_RDX,
    X64_RBX,
    X64_RSP,
    X64_RBP,
    X64_RSI,
    X64_RDI,
    X64_R8,
    X64_R9,
    X64_R10,
    X64_R11,
    X64_R12,
    X64_R13,
    X64_R14,
    X64_R15,
    
    X64_XMM0,
    X64_XMM1,
    X64_XMM2,
    X64_XMM3,
    X64_XMM4,
    X64_XMM5,
    X64_XMM6,
    X64_XMM7,
    X64_XMM8,
    X64_XMM9,
    X64_XMM10,
    X64_XMM11,
    X64_XMM12,
    X64_XMM13,
    X64_XMM14,
    X64_XMM15,
    
    X64_RIP,
    
    X64_REG_COUNT,
};

global const cstring register_names[] {
    "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
    "R8",  "R9",  "R10", "R11", "R12", "R13", "R14", "R15",
    "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", 
    "XMM6", "XMM7", "XMM8", "XMM9", "XMM10", "XMM11", 
    "XMM12", "XMM13", "XMM14", "XMM15",
};

struct X64_Register {
    u32 virtual_index;
    Ic_Raw_Type raw_type;
    bool is_allocated;
};

struct X64_Block {
    u32 label_index;
    bool is_loop;
};

struct X64_Function {
    u8* code;
    u8** labels;
};

struct X64_Jump_Patch {
    u8* origin;
    u8** target;
};

struct X64_Data_Patch {
    u8* origin;
    Exported_Data data;
};

struct X64_Assembler {
    Bytecode* bytecode;
    u32* stack;
    
    Data_Packer* data_packer;
    
    X64_Function* curr_function;
    X64_Function* functions;
    array(X64_Block)* block_stack;
    u32 label_index;
    
    u32 curr_bytecode_insn_index;
    
    X64_Register registers[X64_REG_COUNT];
    
    array(X64_Jump_Patch)* jump_patches;
    array(X64_Data_Patch)* data_patches;
    
    
    s64 current_stack_displacement_for_bytecode_registers;
    s64 stack_displacement_for_caller_arguments;
    
    s64 read_write_data_offset;
    s64 read_only_data_offset;
    
    bool use_absolute_ptrs;
};


inline s64
register_displacement(X64_Assembler* x64, int register_index) {
    // Each register is 8 bytes from RSP
    return x64->current_stack_displacement_for_bytecode_registers + register_index * 8;
}


global const X64_Reg int_arg_registers_ccall_windows[] {
    X64_RCX, X64_RDX, X64_R8, X64_R9
};

global const X64_Reg float_arg_registers_ccall_windows[] {
    X64_XMM0, X64_XMM1, X64_XMM2, X64_XMM3
};


#define X64_OP_SIZE_PREFIX 0x66

#define REX_PATTERN 0x40
#define REX_W bit(3)
#define REX_R bit(2)
#define REX_X bit(1)
#define REX_B bit(0)

#define MODRM_DIRECT 0xC0
#define MODRM_INDIRECT_DISP8 0x40
#define MODRM_INDIRECT_DISP32 0x80

global const u32 x64_setcc_opcodes[] = {
    0xC0940F, 0xC0950F, 0xC0970F, 0xC0930F, 0xC0920F,
    0xC0960F, 0xC09F0F, 0xC09D0F, 0xC09C0F, 0xC09E0F
};

global const u16 x64_jcc_opcodes[] = {
    0x840F, 0x8F0F, 0x870F, 0x8D0F, 0x830F, 0x820F, 0x8C0F, 0x860F, 0x8E0F, 0x850F
};

//inline void
//x64_rex(Buffer* buf, u8 flags) {
//push_u8(buf, REX_PATTERN | flags);
//}

inline void
x64_rex(Buffer* buf, u8 flags, u8 reg=0, u8 rm=0) {
    push_u8(buf, REX_PATTERN | flags | ((reg&8)>>1) | (rm&8)>>3);
}

inline void
x64_sib(Buffer* buf, u8 scale, u8 index, u8 base) {
    push_u8(buf, scale << 6 | index << 3 | base);
}

inline void
x64_rip_relative(Buffer* buf, s64 r, s64 data) {
    push_u8(buf, ((u8) (r&7)<<3) | (u8) X64_RBP);
    
    u8* x64_machine_code_ptr = buf->data + buf->curr_used;
    s64 disp = data - (s64) x64_machine_code_ptr - 4;
    
    push_u32(buf, (u32) disp);
}

inline void
x64_jump_address(X64_Assembler* x64, Buffer* buf, u8** target) {
    if (*target) {
        push_u32(buf, (u32) (*target - (buf->data + buf->curr_used + 4)));
    } else {
        X64_Jump_Patch patch = {};
        patch.origin = buf->data + buf->curr_used;
        patch.target = target;
        array_push(x64->jump_patches, patch);
        push_u32(buf, 0);
    }
}

inline void
x64_jump_address_for_label(X64_Assembler* x64, Buffer* buf, Bytecode_Function* func, u32 label_index) {
    if (label_index > 0) {
        label_index = x64->block_stack[label_index - 1].label_index;
    }
    
    x64_jump_address(x64, buf, &x64->curr_function->labels[label_index]);
}

inline void
x64_modrm(Buffer* buf, u8 reg, u8 rm, s64 disp, s64 rip) {
    reg = reg % 16; 
    rm = rm % 16;
    
    if (disp < S8_MIN || disp > S8_MAX) {
        push_u8(buf, MODRM_INDIRECT_DISP32 | (( reg&7)<<3) |  rm&7);
        if (rm == X64_RSP) {
            push_u8(buf,  (rm << 3) |  rm);
        }
        push_u32(buf, (u32) disp);
    } else {
        push_u8(buf, MODRM_INDIRECT_DISP8 | (( reg&7)<<3) |  rm&7);
        if (rm == X64_RSP) {
            push_u8(buf,  (rm << 3) |  rm);
        }
        push_u8(buf, (u8) disp);
    }
}

inline void
x64_modrm_direct(Buffer* buf, u8 reg, u8 rm) {
    push_u8(buf, MODRM_DIRECT | ((reg&7)<<3) | (rm&7));
}

inline void
x64_modrm_exported_data(X64_Assembler* x64, Buffer* buf, X64_Reg reg, Exported_Data data) {
    push_u8(buf, ((u8) (reg&7)<<3) | (u8) X64_RBP);
    if (x64->use_absolute_ptrs) {
        s64 disp = (s64) data.data - (s64) (buf->data + buf->curr_used + 4);
        push_u32(buf, (u32) disp);
    } else {
        X64_Data_Patch patch = {};
        patch.origin = buf->data + buf->curr_used;
        patch.data = data;
        array_push(x64->data_patches, patch);
        push_u32(buf, 0);
    }
}

#if 0
inline void
x64_spill_register(X64_Assembler* x64, Buffer* buf, X64_Reg reg) {
    if (x64->registers[reg].is_allocated) {
        Ic_Raw_Type rt = x64->registers[reg].raw_type;
        Ic_Arg stk = {};
        stk.type = IC_STK | rt;
        stk.reg = X64_RSP;
        //stk.disp = x64->curr_function->stack_locals;
        //x64->curr_function->stack_locals += 8; // TODO(Alexander): proper stack allocation (use size and alignment)
        
        if (rt & IC_FLOAT) {
            //x64_fmov(buf, stk.type, stk.reg, stk.disp, IC_REG | rt, reg, 0, 0);
        } else {
            //x64_mov(buf, stk.type, stk.reg, stk.disp, IC_REG | rt, reg, 0, 0);
        }
        x64->registers[reg].is_allocated = false;
        
        u32 virtual_index = x64->registers[reg].virtual_index;
        //x64->virtual_registers[virtual_index] = stk;
        unimplemented;
    }
}

inline Ic_Arg
x64_alloc_register(X64_Assembler* x64, Buffer* buf, u32 virtual_index, X64_Reg reg, Ic_Raw_Type raw_type) {
    x64_spill_register(x64, buf, reg);
    
    Ic_Arg result = {};
    result.type = IC_REG | raw_type;
    result.reg = (u8) reg;
    
    x64->registers[reg].is_allocated = true;
    x64->registers[reg].virtual_index = virtual_index;
    x64->registers[reg].raw_type = raw_type;
    
    //x64->virtual_registers[virtual_index] = result;
    unimplemented;
    return result;
}
#endif
void convert_bytecode_function_to_x64_machine_code(X64_Assembler* x64,
                                                   Bytecode_Function* func,
                                                   Buffer* buf);
void convert_bytecode_insn_to_x64_machine_code(X64_Assembler* x64, 
                                               Buffer* buf,
                                               Bytecode_Function* func,
                                               Bytecode_Instruction* insn);

#if 0
inline void
x64_unary(Buffer* buf, Ic_Type t, s64 r, s64 d, u8 opcode, u8 reg_field, s64 rip);

inline void
x64_binary(Buffer* buf,
           Ic_Type t1, s64 r1, s64 d1, 
           Ic_Type t2, s64 r2, s64 d2,
           u8 reg_field, u8 opcode, s64 rip);
#endif

inline Ic_Arg
ic_stk(Ic_Raw_Type raw_type, s64 disp, Ic_Stk_Area area=IcStkArea_None, u8 reg=X64_RSP) {
    Ic_Arg result = {};
    result.type = raw_type + IC_STK;
    result.reg = reg;
    result.stk.area = area;
    result.stk.disp = safe_truncate_s64(disp);
    return result;
}


#if 0
struct X64_Arg_Copy {
    Type* type;
    union {
        Ast* expr;
        string_id ident;
    };
    Ic_Arg dest;
    Ic_Arg src;
};

inline Ic_Arg
ic_reg(Ic_Raw_Type t=IC_S32, u8 reg=0) {
    // TODO(Alexander): how to handle register allocation
    Ic_Arg result = {};
    result.reg = reg;
    result.type = t + IC_REG; // TODO(Alexander): hardcoded type
    return result;
}

inline s64
compute_stk_displacement(Compilation_Unit* cu, Ic_Arg arg) {
    //assert(arg.stk_entry_index < array_count(cu->stk_entries));
    //Ic_Stk_Entry stk = cu->stk_entries[arg.stk_entry_index];
    switch (arg.stk.area) {
        case IcStkArea_None:
        case IcStkArea_Args: return arg.stk.disp;
        case IcStkArea_Locals: return arg.stk.disp + cu->stk_args;
        case IcStkArea_Caller_Args: return arg.stk.disp + cu->stk_args + cu->stk_locals;
        
        default: assert(0 && "invalid stack allocation");
    }
    
    return 0;
}

inline Ic_Arg
ic_stk_offset(Ic_Raw_Type raw_type, Ic_Arg stk, s64 disp) {
    Ic_Arg result = stk;
    result.type = raw_type + IC_STK;
    result.stk.disp += safe_truncate_s64(disp);
    return result;
}

inline Ic_Arg
ic_push_local(Compilation_Unit* cu, Type* type, string_id ident=0) {
    assert(type->size != 0 && "bad size");
    assert(type->align != 0 && "bad align");
    
    s64 disp = align_forward(cu->stk_locals, type->align);
    cu->stk_locals = disp + type->size;
    //pln("push: disp = % (%) size = %, align = %", f_s64(disp), f_var(ident), f_int(type->size), f_int(type->align));
    
    Ic_Arg result = ic_stk(convert_type_to_raw_type(type), disp, IcStkArea_Locals);
    if (ident) {
        map_put(cu->locals, ident, result);
    }
    return result;
}

inline Ic_Arg
ic_imm(Ic_Raw_Type t, s64 d) {
    Ic_Arg result = {};
    result.type = t + IC_DISP;
    result.disp = d;
    return result;
}


inline Ic_Arg
ic_rip_disp32(Compilation_Unit* cu, Ic_Raw_Type t, Ic_Data_Area data_area, void* data, u32 relative_ptr) {
    Ic_Arg result = {};
    result.type = t + IC_RIP_DISP32;
    result.reg = X64_RIP;
    if (cu->use_absolute_ptrs) {
        result.disp = (s64) data;
    } else {
        result.data.disp = relative_ptr;
        result.data.area = data_area;
    }
    
    return result;
}

inline bool
x64_is_scalar_type(int size) {
    return size == 1 || size == 2 || size == 4 || size == 8;
}

enum X64_Jump_Opcode {
    X64_JMP,
    X64_JA,
    X64_JAE,
    X64_JB,
    X64_JBE,
    X64_JC,
    X64_JE,
    X64_JG,
    X64_JGE,
    X64_JL,
    X64_JLE,
    X64_JNA,
    X64_JNAE,
    X64_JNB,
    X64_JNBE,
    X64_JNC,
    X64_JNE,
    X64_JNG,
    X64_JNGE,
    X64_JNL,
    X64_JNLE,
    X64_JNO,
    X64_JNP,
    X64_JNS,
    X64_JNZ,
    X64_JO,
    X64_JP,
    X64_JPE,
    X64_JPO,
    X64_JS,
    X64_JMP_COUNT
};

global const X64_Jump_Opcode ic_jmp_to_x64_jmp[] = {
#define IC(name) X64_JMP,
#define IC_JMP(name) X64_##name,
    DEF_IC_OPCODES
#undef IC_JMP
#undef IC
};

global const u8 x64_jmp_opcodes[] = {
    // Short jump
    0xEB, 0x77, 0x73, 0x72, 0x76, 0x72, 0x74, 0x7F, 0x7D, 0x7C, 0x7E, 0x76,
    0x72, 0x73, 0x77, 0x73, 0x75, 0x7E, 0x7C, 0x7D, 0x7F, 0x71, 0x7B, 0x79,
    0x75, 0x70, 0x7A, 0x7A, 0x7B, 0x78,
    
    // Long jump
    0xE9, 0x0F, 0x87, 0x0F, 0x83, 0x0F, 0x82, 0x0F, 0x86, 0x0F, 0x82, 0x0F,
    0x84, 0x0F, 0x8F, 0x0F, 0x8D, 0x0F, 0x8C, 0x0F, 0x8E, 0x0F, 0x86, 0x0F,
    0x82, 0x0F, 0x83, 0x0F, 0x87, 0x0F, 0x83, 0x0F, 0x85, 0x0F, 0x8E, 0x0F,
    0x8C, 0x0F, 0x8D, 0x0F, 0x8F, 0x0F, 0x81, 0x0F, 0x8B, 0x0F, 0x89, 0x0F,
    0x85, 0x0F, 0x80, 0x0F, 0x8A, 0x0F, 0x8A, 0x0F, 0x8B, 0x0F, 0x88,
};

inline Intermediate_Code*
ic_add_orphan(Compilation_Unit* cu, Ic_Opcode opcode = IC_NOOP, smm size=sizeof(Intermediate_Code)) {
    // TODO(Alexander): temporary bump allocation for now
    Intermediate_Code* result = (Intermediate_Code*) calloc(1, size);
    result->opcode = opcode;
    return result;
}

#define ic_add(cu, opcode) ic_add_insn(cu, opcode, __FILE__ ":" S2(__LINE__))
#define ic_label(cu, bb) ic_label_insn(cu, bb, __FILE__ ":" S2(__LINE__))
#define ic_jump(cu, opcode, target_bb) ic_jump_insn(cu, opcode, target_bb, __FILE__ ":" S2(__LINE__))
#define ic_call(cu) (Ic_Call*) ic_add_insn(cu, IC_CALL, __FILE__ ":" S2(__LINE__), sizeof(Ic_Call))


internal void
ic_add_to_basic_block(Compilation_Unit* cu, Intermediate_Code* ic, Ic_Basic_Block* bb) {
    if (!bb->ic_first) {
        bb->ic_first = ic;
    }
    if (!cu->ic_first) {
        cu->ic_first = ic;
    }
    
    if (bb->ic_last) {
        bb->ic_last->next = ic;
    }
    bb->ic_last = ic;
    cu->ic_last = ic;
}

void
ic_label_insn(Compilation_Unit* cu, Ic_Basic_Block* bb, cstring comment=0) {
    Intermediate_Code* result = ic_add_orphan(cu, IC_LABEL);
    result->data = bb;
    
    if (!cu->bb_first) {
        cu->bb_first = bb;
    }
    
    if (cu->bb_last) {
        cu->bb_last->next = bb;
    }
    
    bb->index = cu->bb_index++;
    bb->ic_last = cu->ic_last;
    cu->bb_last = bb;
    
    ic_add_to_basic_block(cu, result, bb);
    
    result->comment = comment;
}

Intermediate_Code*
ic_add_insn(Compilation_Unit* cu,
            Ic_Opcode opcode = IC_NOOP, 
            cstring comment=0,
            smm size=sizeof(Intermediate_Code)) {
    
    Intermediate_Code* result = ic_add_orphan(cu, opcode, size);
    
    Ic_Basic_Block* bb = cu->bb_last;
    assert(bb && "missing label");
    ic_add_to_basic_block(cu, result, bb);
    
    result->comment = comment;
    
    return result;
}

void
ic_jump_insn(Compilation_Unit* cu, Ic_Opcode opcode,
             Ic_Basic_Block* target_bb, cstring comment=0) {
    
    assert(opcode >= IC_JMP && opcode <= IC_JNE);
    Ic_Jump* ic = (Ic_Jump*) ic_add_insn(cu, opcode, comment, sizeof(Ic_Jump));
    ic->target = target_bb;
}

#define ic_mov(cu, dest, src) _ic_mov(cu, dest, src, __FILE__ ":" S2(__LINE__))

inline void
_ic_mov(Compilation_Unit* cu, Ic_Arg dest, Ic_Arg src, cstring comment=0) {
    //assert(dest.raw_type == src.raw_type);
    Ic_Opcode opcode = (dest.type & IC_FLOAT) ? IC_FMOV : IC_MOV;
    if (opcode == IC_MOV && dest.type & IC_REG && src.type & (IC_T8 | IC_T16)) {
        if (dest.raw_type & (IC_T8 | IC_T16)) {
            // NOTE(Alexander): make sure dest is >= 32 bits size
            dest.raw_type = (dest.raw_type & IC_RT_SIZE_MASK) | IC_T32;
        }
        
        if (src.type & IC_DISP) {
            src.raw_type = dest.raw_type;
        } else {
            opcode = (dest.type & IC_SINT) ? IC_MOVSX : IC_MOVZX;
        }
    }
    
    Intermediate_Code* ic = ic_add_insn(cu, opcode, comment);
    ic->src0 = dest;
    ic->src1 = src;
}

inline Ic_Arg
ic_reg_mov(Compilation_Unit* cu, u8 dest_reg, Ic_Arg src) {
    Ic_Arg result = ic_reg(src.raw_type, dest_reg);
    if ((src.type & IC_REG && src.reg != dest_reg) || (src.type & (IC_DISP_STK_RIP))) {
        ic_mov(cu, result, src);
    }
    return result;
}

#define ic_lea(cu, dest, src, ...) _ic_lea(cu, dest, src, __FILE__ ":" S2(__LINE__))

inline void
_ic_lea(Compilation_Unit* cu, Ic_Arg dest, Ic_Arg src, cstring comment=0, u8 tmp_reg=X64_RAX) {
    Ic_Arg tmp = {};
    if (dest.type & IC_STK_RIP) {
        tmp = dest;
        dest = ic_reg(dest.type & IC_RT_MASK, tmp_reg);
    }
    Intermediate_Code* ic = ic_add_insn(cu, IC_LEA, comment);
    ic->src0 = dest;
    ic->src1 = src;
    
    if (tmp.type & IC_STK_RIP) {
        _ic_mov(cu, tmp, dest, comment);
        dest = tmp;
    }
}

Ic_Arg convert_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr);

void convert_procedure_to_intermediate_code(Compilation_Unit* cu, bool insert_debug_break);
s64 convert_to_x64_machine_code(Intermediate_Code* ic, s64 stack_usage, u8* buffer, s64 buffer_offset, s64 buf_size, s64 rip);

void string_builder_push(String_Builder* sb, Ic_Arg arg);
void string_builder_push(String_Builder* sb, Intermediate_Code* ic, int bb_index=0);
void print_intermediate_code(Intermediate_Code* value);
#endif