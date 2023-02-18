

enum {
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
    X64_RIP,
    
    X64_XMM0 = 0,
    X64_XMM1 = 1,
    X64_XMM2 = 2,
    X64_XMM3 = 3,
    X64_XMM4 = 4,
    X64_XMM5 = 5,
    X64_XMM6 = 6,
    X64_XMM7 = 7,
    X64_XMM8 = 8,
    X64_XMM9 = 9,
    X64_XMM10 = 10,
    X64_XMM11 = 11,
    X64_XMM12 = 12,
    X64_XMM13 = 13,
    X64_XMM14 = 14,
    X64_XMM15 = 15,
};
typedef u8 X64_Reg;

global const cstring int_register_names[] {
    "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
    "R8",  "R9",  "R10", "R11", "R12", "R13", "R14", "R15"
};

global const cstring float_register_names[] {
    "XMM0", "XMM1", "XMM2", "XMM3", "XMM4", "XMM5", 
    "XMM6", "XMM7", "XMM8", "XMM9", "XMM10", "XMM11", 
    "XMM12", "XMM13", "XMM14", "XMM15",
};

global const X64_Reg int_arg_registers_ccall_windows[] {
    X64_RCX, X64_RDX, X64_R8, X64_R9
};

global const X64_Reg float_arg_registers_ccall_windows[] {
    X64_XMM0, X64_XMM1, X64_XMM2, X64_XMM3
};

struct X64_Arg_Copy {
    Type* type;
    union {
        Ast* expr;
        string_id ident;
    };
    Ic_Arg dest;
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
ic_stk(Ic_Raw_Type raw_type, s64 disp, Ic_Stk_Area area=IcStkArea_None, u8 reg=X64_RSP) {
    Ic_Arg result = {};
    result.type = raw_type + IC_STK;
    result.reg = reg;
    result.stk.area = area;
    result.stk.disp = safe_truncate_s64(disp);
    return result;
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
    s64 disp = align_forward(cu->stk_locals, type->align);
    cu->stk_locals = disp + type->size;
    
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
ic_rip_disp32(Ic_Raw_Type t, void* data) {
    Ic_Arg result = {};
    result.type = t + IC_RIP_DISP32;
    result.reg = X64_RIP;
    result.disp = (s64) data;
    return result;
}

inline bool
x64_is_scalar_type(int size) {
    return size == 1 || size == 2 || size == 4 || size == 8;
}

inline int
intrin_index_of_first_set_bit(u32 value) {
    unsigned long result = 0;
    // TODO(Alexander): MSVC intrinsics, make compiler agnostic
    if (_BitScanForward(&result, value)) {
        return result;
    }
    return -1;
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
ic_add_orphan(Compilation_Unit* cu, Ic_Opcode opcode = IC_NOOP, void* data=0) {
    Intermediate_Code* result = (Intermediate_Code*) calloc(1, sizeof(Intermediate_Code));
    result->opcode = opcode;
    result->data = data;
    return result;
}

#define S1(x) #x
#define S2(x) S1(x)
#define ic_add(cu, opcode, ...) _ic_add(cu, opcode, __FILE__ ":" S2(__LINE__), __VA_ARGS__)

Intermediate_Code*
_ic_add(Compilation_Unit* cu, Ic_Opcode opcode = IC_NOOP, cstring comment=0, void* data=0) {
    // TODO(Alexander): temporary bump allocation for now
    Intermediate_Code* result = ic_add_orphan(cu, opcode, data);
    
    Ic_Basic_Block* bb;
    
    if (opcode == IC_LABEL) {
        bb = (Ic_Basic_Block*) data;
        if (!cu->bb_first) {
            cu->bb_first = bb;
        }
        
        if (cu->bb_last) {
            cu->bb_last->next = bb;
        }
        
        bb->ic_last = cu->ic_last;
        cu->bb_last = bb;
    } else {
        bb = cu->bb_last;
        
        if (!bb) {
            unimplemented;
            //bb = ic_basic_block();
            //cu->bb_first = bb;
            //cu->bb_last = bb;
        }
    }
    
    if (!bb->ic_first) {
        bb->ic_first = result;
    }
    if (!cu->ic_first) {
        cu->ic_first = result;
    }
    
    if (bb->ic_last) {
        bb->ic_last->next = result;
    }
    bb->ic_last = result;
    cu->ic_last = result;
    
    result->comment = comment;
    
    return result;
}

#define ic_mov(cu, dest, src) _ic_mov(cu, dest, src, __FILE__ ":" S2(__LINE__))

inline void
_ic_mov(Compilation_Unit* cu, Ic_Arg dest, Ic_Arg src, cstring comment=0) {
    //assert(dest.raw_type == src.raw_type);
    Intermediate_Code* ic = _ic_add(cu, (dest.type & IC_FLOAT) ? IC_FMOV : IC_MOV, comment);
    ic->src0 = dest;
    ic->src1 = src;
}

inline Ic_Arg
ic_reg_mov(Compilation_Unit* cu, u8 dest_reg, Ic_Arg src) {
    Ic_Arg result = ic_reg(src.raw_type, dest_reg);
    if ((src.type & IC_REG && src.reg != dest_reg) || (src.type & (IC_STK | IC_DISP))) {
        ic_mov(cu, result, src);
    }
    return result;
}

#define ic_lea(cu, dest, src, ...) _ic_lea(cu, dest, src, __FILE__ ":" S2(__LINE__))

inline void
_ic_lea(Compilation_Unit* cu, Ic_Arg dest, Ic_Arg src, cstring comment=0, u8 tmp_reg=X64_RAX) {
    Ic_Arg tmp = {};
    if (dest.type & IC_STK) {
        tmp = dest;
        dest = ic_reg(dest.type & IC_RT_MASK, tmp_reg);
    }
    Intermediate_Code* ic = _ic_add(cu, IC_LEA, comment);
    ic->src0 = dest;
    ic->src1 = src;
    
    if (tmp.type & IC_STK) {
        _ic_mov(cu, tmp, dest, comment);
        dest = tmp;
    }
}

#define REX_PATTERN 0x40
#define REX_FLAG_W bit(3)
#define REX_FLAG_R bit(2)
#define REX_FLAG_X bit(1)
#define REX_FLAG_B bit(0)
#define REX_FLAG_64_BIT REX_FLAG_W

inline void
x64_rex(Intermediate_Code* ic, u8 flags) {
    ic_u8(ic, REX_PATTERN | flags);
}

#define MODRM_DIRECT 0xC0
#define MODRM_INDIRECT_DISP8 0x40
#define MODRM_INDIRECT_DISP32 0x80

inline void x64_mov(Intermediate_Code* ic, 
                    Ic_Type t1, s64 r1, s64 d1, 
                    Ic_Type t2, s64 r2, s64 d2, s64 rip);

inline void x64_add(Intermediate_Code* ic, 
                    Ic_Type t1, s64 r1, s64 d1, 
                    Ic_Type t2, s64 r2, s64 d2, 
                    Ic_Type t3, s64 r3, s64 d3,
                    u8 reg_field, u8 opcode, s64 rip);

Ic_Arg convert_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr);

void convert_procedure_to_intermediate_code(Compilation_Unit* cu, bool insert_debug_break);
s64 convert_to_x64_machine_code(Intermediate_Code* ic, s64 stack_usage, u8* buf, s64 buf_size, s64 rip);
