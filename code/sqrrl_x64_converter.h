
void test_x64_converter(int argc, char* argv[], void* asm_buffer, umm asm_size,
                        void (*asm_make_executable)(void*, umm), bool is_debugger_present);

enum {
    IC_Void,
    IC_S8,
    IC_U8,
    IC_S16,
    IC_U16,
    IC_S32,
    IC_U32,
    IC_S64,
    IC_U64,
    IC_F32,
    IC_F64,
    IC_MASK= 0xFF,
};
typedef u8 Ic_Raw_Type;

enum {
    IC_IMM = bit(8),
    IC_REG = bit(9),
    IC_STK = bit(10),
};
typedef u8 Ic_Type_Flags;

#define IC_TF_MASK 0xFF00

typedef s16 Ic_Type;

struct Ic_Arg {
    union {
        Ic_Type type;
        struct {
            Ic_Raw_Type raw;
            Ic_Type_Flags flags;
        };
    };
    u8 reg;
    s64 disp; // alt. imm
};

#define DEF_IC_OPCODES \
IC(NOOP) \
IC(PRLG) \
IC(EPLG) \
IC(LABEL) \
IC(ADD) \
IC(DIV) \
IC(MOD) \
IC(MOV) \
IC(CMP) \
IC(JNG)

enum Ic_Opcode {
#define IC(name) IC_##name,
    DEF_IC_OPCODES
#undef IC
};

global const cstring ic_opcode_names[] = {
#define IC(name) #name,
    DEF_IC_OPCODES
#undef IC
};


struct Intermediate_Code {
    Intermediate_Code* next, *last;
    
    Ic_Opcode opcode;
    Ic_Arg dest, src0, src1;
    
    u8 count;
    u8 code[20];
};

inline void
ic_u8(Intermediate_Code* ic, u8 b) {
    assert(ic->count < fixed_array_count(ic->code));
    ic->code[ic->count++] = b;
}

inline void
ic_u16(Intermediate_Code* ic, u16 w) {
    assert(ic->count < fixed_array_count(ic->code));
    *((u16*) (ic->code + ic->count)) = w;
}

inline void
ic_u32(Intermediate_Code* ic, u32 dw) {
    assert(ic->count < fixed_array_count(ic->code));
    *((u32*) (ic->code + ic->count)) = dw;
    ic->count += 4;
}

inline void
ic_u64(Intermediate_Code* ic, u64 qw) {
    assert(ic->count < fixed_array_count(ic->code));
    *((u64*) (ic->code + ic->count)) = qw;
    ic->count += 4;
}


#define MODRM_DIRECT 0xC0
#define MODRM_INDIRECT_DISP8 0x40
#define MODRM_INDIRECT_DISP32 0x80


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
    X64_R15
};

global const cstring int_register_names[] {
    "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI",
    "R8",  "R9",  "R10", "R11", "R12", "R13", "R14", "R15"
};

inline void x64_mov(Intermediate_Code* ic, 
                    Ic_Type t1, s64 r1, s64 d1, 
                    Ic_Type t2, s64 r2, s64 d2);

inline void x64_add(Intermediate_Code* ic, 
                    Ic_Type t1, s64 r1, s64 d1, 
                    Ic_Type t2, s64 r2, s64 d2, 
                    Ic_Type t3, s64 r3, s64 d3);
