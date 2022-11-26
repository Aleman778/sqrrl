
#define DEF_IC_OPCODES \
IC(NOOP) \
IC(PRLG) \
IC(EPLG) \
IC(DEBUG_BREAK) \
IC(LABEL) \
IC(CALL) \
IC(ADD) \
IC(SUB) \
IC(MUL) \
IC(DIV) \
IC(MOD) \
IC(MOV) \
IC(CAST_F2S) \
IC(CAST_S2F) \
IC(REINTERP_F2S) \
IC(MOVSX) \
IC(MOVZX) \
IC(MEMCPY) \
IC(MEMSET) \
IC(LEA) \
IC(CMP) \
IC(FMOV) \
IC(FADD) \
IC(FSUB) \
IC(FMUL) \
IC(FDIV) \
IC(FMOD) \
IC(FCMP) \
IC(SETG) \
IC(SETNG) \
IC(SETL) \
IC(SETNL) \
IC_JMP(JMP) \
IC_JMP(JG) \
IC_JMP(JNG) \
IC_JMP(JL) \
IC_JMP(JNL) \
IC_JMP(JE) \
IC_JMP(JNE) \

enum Ic_Opcode {
#define IC(name) IC_##name,
#define IC_JMP(name) IC_##name,
    DEF_IC_OPCODES
#undef IC_JMP
#undef IC
};

global const cstring ic_opcode_names[] = {
#define IC(name) #name,
#define IC_JMP(name) #name,
    DEF_IC_OPCODES
#undef IC_JMP
#undef IC
};


enum {
    IC_Void = 0,
    IC_T8 = bit(0),
    IC_T16 = bit(1),
    IC_T32 = bit(2),
    IC_T64 = bit(3),
    // reserved = bit(4)
    IC_UINT = bit(5),
    IC_SINT = bit(6),
    IC_FLOAT = bit(7),
    IC_RT_MASK = 0xFF,
};

// raw type flags e.g. u32 =  IC_UINT | IC_T32
#define IC_S8  (IC_SINT  | IC_T8)
#define IC_U8  (IC_UINT  | IC_T8)
#define IC_S16 (IC_SINT  | IC_T16)
#define IC_U16 (IC_UINT  | IC_T16)
#define IC_S32 (IC_SINT  | IC_T32)
#define IC_U32 (IC_UINT  | IC_T32)
#define IC_S64 (IC_SINT  | IC_T64)
#define IC_U64 (IC_UINT  | IC_T64)
#define IC_F32 (IC_FLOAT | IC_T32)
#define IC_F64 (IC_FLOAT | IC_T64)

typedef u8 Ic_Raw_Type;

inline s32
sizeof_raw_type(Ic_Raw_Type rt) {
    return rt & 0xF;
}

inline Ic_Raw_Type
basic_type_to_raw_type(Basic_Type basic) {
    switch (basic) {
        case Basic_s8:  return IC_S8;
        case Basic_u8:  return IC_U8;
        case Basic_s16: return IC_S16;
        case Basic_u16: return IC_U16;
        case Basic_s32: return IC_S32;
        case Basic_u32: return IC_U32;
        case Basic_s64: return IC_S64;
        case Basic_u64: return IC_U64;
        case Basic_f32: return IC_F32;
        case Basic_f64: return IC_F64;
        case Basic_string:
        case Basic_cstring: return IC_T64;
        default: unimplemented;
    }
    return IC_Void;
}

inline Ic_Raw_Type
convert_type_to_raw_type(Type* type) {
    Ic_Raw_Type raw_type = IC_T64;
    if (type->kind == TypeKind_Basic) {
        raw_type = basic_type_to_raw_type(type->Basic.kind);
    }
    return raw_type;
}

enum {
    IC_IMM = bit(8),
    IC_REG = bit(9),
    IC_STK = bit(10),
};
typedef u8 Ic_Type_Flags;

#define IC_TF_MASK 0xFF00

typedef s16 Ic_Type;

enum Ic_Stk_Area {
    IcStkArea_None,
    IcStkArea_Caller_Args,
    IcStkArea_Args,
    IcStkArea_Locals,
};

struct Ic_Stk_Entry {
    s64 disp;
    Ic_Stk_Area area;
};

struct Ic_Arg {
    union {
        Ic_Type type;
        struct {
            Ic_Raw_Type raw_type;
            Ic_Type_Flags type_flags;
        };
    };
    u8 reg;
    union {
        s64 disp; // alt. imm
        
        // unresolved stk disp
        struct {
            s32 disp;
            s32 area; // Ic_Stk_Area
        } stk;
    };
};


struct Intermediate_Code {
    Intermediate_Code* next;
    void* data;
    
    Ic_Opcode opcode;
    Ic_Arg dest, src0, src1;
    
    u8 count;
    u8 code[31];
};

inline void
ic_u8(Intermediate_Code* ic, u8 b) {
    assert(ic->count < fixed_array_count(ic->code));
    ic->code[ic->count++] = b;
}

inline void
ic_u16(Intermediate_Code* ic, u16 w) {
    assert(ic->count + 2 < fixed_array_count(ic->code));
    *((u16*) (ic->code + ic->count)) = w;
    ic->count += 2;
}

inline void
ic_u32(Intermediate_Code* ic, u32 dw) {
    assert(ic->count + 4 < fixed_array_count(ic->code));
    *((u32*) (ic->code + ic->count)) = dw;
    ic->count += 4;
}

inline void
ic_u64(Intermediate_Code* ic, u64 qw) {
    assert(ic->count + 8 < fixed_array_count(ic->code));
    *((u64*) (ic->code + ic->count)) = qw;
    ic->count += 8;
}

struct Ic_Basic_Block {
    Ic_Basic_Block *next;
    Intermediate_Code *ic_first, *ic_last;
    
    s64 addr;
};

struct Ic_Data {
    Ic_Data *next;
    
    
};

#define IC_INVALID_ADDR S64_MIN

inline Ic_Basic_Block*
ic_basic_block() {
    // TODO(Alexander): temporary bump allocation for now
    // TODO(Alexander): maybe give it a unique name for debugging
    Ic_Basic_Block* result = (Ic_Basic_Block*) calloc(1, sizeof(Ic_Basic_Block));
    result->addr = IC_INVALID_ADDR;
    return result;
}
