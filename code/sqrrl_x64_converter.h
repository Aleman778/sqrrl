
void test_x64_converter(int argc, char* argv[], void* asm_buffer, umm asm_size,
                        void (*asm_make_executable)(void*, umm), bool is_debugger_present);




enum {
    IC_IMM = bit(1),
    IC_REG = bit(2),
    IC_STACK = bit(3),
};
typedef u8 IC_Type_Flags;

enum {
    IC_S32;
    IC_S64;
};
typedef u8 IC_Type_Kind;

union Ic_Type {
    struct {
        IC_Type_Kind kind;
        IC_Type_Flags mods;
    };
    u16 packed;
};

struct Ic_Arg {
    Ic_Type type;
    u8 reg;
    s64 disp; // alt. imm
};


enum Ic_Opcode {
    IC_NOOP,
    IC_PRLG,
    IC_EPLG,
    IC_ADD,
    IC_DIV,
    IC_MOD,
    IC_MOV,
};

struct Intermediate_Code {
    Intermediate_Code* next, *last;
    
    Ic_Opcode opcode;
    Ic_Arg dest, src0, src1;
    
    u8 count;
    u8 code[20];
};


#define MODRM_DIRECT 0xC0
#define MODRM_INDIRECT_DISP8 0x40
#define MODRM_INDIRECT_DISP32 0x80


inline void x64_mov(Intermediate_Code* ic, 
                    Ic_Type t1, s64 r1, s64 d1, 
                    Ic_Type t2, s64 r2, s64 d2);

inline void x64_add(Intermediate_Code* ic, 
                    Ic_Type t1, s64 r1, s64 d1, 
                    Ic_Type t2, s64 r2, s64 d2, 
                    Ic_Type t3, s64 r3, s64 d3);
