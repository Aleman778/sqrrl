
enum X64_Opcode {
    X64Opcode_ADD,
    X64Opcode_MOV,
    X64Opcode_PUSH,
    X64Opcode_POP,
    X64Opcode_RET,
    
    X64Opcode_LABEL, // pseudo opcode
};

enum X64_Register_Type {
    X64RegisterType_GPR, // General purpose register
    X64RegisterType_FPU, // Floating point unit
    X64RegisterType_MMX, // First version of SIMD
    X64RegisterType_SSE, // Streaming SIMD Extension
    X64RegisterType_AVX, // Advanced Vector Extension
};

// X64_GPR_BYTE(index, qword, dword, word, byte_low, byte_high)
// X64_GPR(index, qword, dword, word, byte_low)
// X64_MMX_FPU(index, mmx, stx)

// X64_REGISTER(mnemonic, size, type)
#define DEF_X64_REGISTERS \
X64_REGISTER(unallocated_r8,   1, GPR) \
X64_REGISTER(unallocated_r16,   2, GPR) \
X64_REGISTER(unallocated_r32,  4, GPR) \
X64_REGISTER(unallocated_r64,  8, GPR) \
X64_REGISTER(unallocated_stx,  10, GPR) \
X64_REGISTER(unallocated_mmx,  8, GPR) \
\
X64_REGISTER(ah,   1, GPR) \
X64_REGISTER(al,   1, GPR) \
X64_REGISTER(ax,   2, GPR) \
X64_REGISTER(eax,  4, GPR) \
X64_REGISTER(rax,  8, GPR) \
\
X64_REGISTER(bh,   1, GPR) \
X64_REGISTER(bl,   1, GPR) \
X64_REGISTER(bx,   2, GPR) \
X64_REGISTER(ebx,  4, GPR) \
X64_REGISTER(rbx,  8, GPR) \
\
X64_REGISTER(ch,   1, GPR) \
X64_REGISTER(cl,   1, GPR) \
X64_REGISTER(cx,   2, GPR) \
X64_REGISTER(ecx,  4, GPR) \
X64_REGISTER(rcx,  8, GPR) \
\
X64_REGISTER(dh,   1, GPR) \
X64_REGISTER(dl,   1, GPR) \
X64_REGISTER(dx,   2, GPR) \
X64_REGISTER(edx,  4, GPR) \
X64_REGISTER(rdx,  8, GPR) \
\
X64_REGISTER(sil,  1, GPR) \
X64_REGISTER(si,   2, GPR) \
X64_REGISTER(esi,  4, GPR) \
X64_REGISTER(rsi,  8, GPR) \
\
X64_REGISTER(dil,  1, GPR) \
X64_REGISTER(di,   2, GPR) \
X64_REGISTER(edi,  4, GPR) \
X64_REGISTER(rdi,  8, GPR) \
\
X64_REGISTER(dil,  1, GPR) \
X64_REGISTER(di,   2, GPR) \
X64_REGISTER(edi,  4, GPR) \
X64_REGISTER(rdi,  8, GPR) \
\
X64_REGISTER(bpl,  1, GPR) \
X64_REGISTER(bp,   2, GPR) \
X64_REGISTER(ebp,  4, GPR) \
X64_REGISTER(rbp,  8, GPR) \
\
X64_REGISTER(spl,  1, GPR) \
X64_REGISTER(sp,   2, GPR) \
X64_REGISTER(esp,  4, GPR) \
X64_REGISTER(rsp,  8, GPR) \
\
X64_REGISTER(r8b,  1, GPR) \
X64_REGISTER(r8w,  2, GPR) \
X64_REGISTER(r8d,  4, GPR) \
X64_REGISTER(r8,   8, GPR) \
\
X64_REGISTER(r9b,  1, GPR) \
X64_REGISTER(r9w,  2, GPR) \
X64_REGISTER(r9d,  4, GPR) \
X64_REGISTER(r9,   8, GPR) \
\
X64_REGISTER(r10b,  1, GPR) \
X64_REGISTER(r10w,  2, GPR) \
X64_REGISTER(r10d,  4, GPR) \
X64_REGISTER(r10,   8, GPR) \
\
X64_REGISTER(r11b,  1, GPR) \
X64_REGISTER(r11w,  2, GPR) \
X64_REGISTER(r11d,  4, GPR) \
X64_REGISTER(r11,   8, GPR) \
\
X64_REGISTER(r12b,  1, GPR) \
X64_REGISTER(r12w,  2, GPR) \
X64_REGISTER(r12d,  4, GPR) \
X64_REGISTER(r12,   8, GPR) \
\
X64_REGISTER(r13b,  1, GPR) \
X64_REGISTER(r13w,  2, GPR) \
X64_REGISTER(r13d,  4, GPR) \
X64_REGISTER(r13,   8, GPR) \
\
X64_REGISTER(r14b,  1, GPR) \
X64_REGISTER(r14w,  2, GPR) \
X64_REGISTER(r14d,  4, GPR) \
X64_REGISTER(r14,   8, GPR) \
\
X64_REGISTER(r15b,  1, GPR) \
X64_REGISTER(r15w,  2, GPR) \
X64_REGISTER(r15d,  4, GPR) \
X64_REGISTER(r15,   8, GPR) \
\
X64_REGISTER(st0, 10, FPU) \
X64_REGISTER(st1, 10, FPU) \
X64_REGISTER(st2, 10, FPU) \
X64_REGISTER(st3, 10, FPU) \
X64_REGISTER(st4, 10, FPU) \
X64_REGISTER(st5, 10, FPU) \
X64_REGISTER(st6, 10, FPU) \
X64_REGISTER(st7, 10, FPU) \
\
X64_REGISTER(mm0, 8, MMX) \
X64_REGISTER(mm1, 8, MMX) \
X64_REGISTER(mm2, 8, MMX) \
X64_REGISTER(mm3, 8, MMX) \
X64_REGISTER(mm4, 8, MMX) \
X64_REGISTER(mm5, 8, MMX) \
X64_REGISTER(mm6, 8, MMX) \
X64_REGISTER(mm7, 8, MMX)
// TODO(Alexander): add more registers

global int const x64_num_physical_registers = 24; // counting rax and eax as the same

enum X64_Register {
#define X64_REGISTER(mnemonic, ...) X64Register_##mnemonic,
    DEF_X64_REGISTERS
#undef X64_REGISTER
};

global int register_size_table[] = {
#define X64_REGISTER(mnemonic, size, ...) size,
    DEF_X64_REGISTERS
#undef X64_REGISTER
};

bool
register_is_gpr(X64_Register) {
    return reg >= X64Register_ah && reg <= X64Register_r15;
}

enum X64_Operand_Kind {
    X64Operand_None,
    X64Operand_r8,
    X64Operand_r16,
    X64Operand_r32,
    X64Operand_r64,
    X64Operand_m8,
    X64Operand_m16,
    X64Operand_m32,
    X64Operand_m64,
    X64Operand_rm8,
    X64Operand_rm16,
    X64Operand_rm32,
    X64Operand_rm64,
    X64Operand_imm8,
    X64Operand_imm16,
    X64Operand_imm32,
    X64Operand_imm64,
    X64Operand_st,
    X64Operand_mm,
    X64Operand_xmm,
    X64Operand_ymm,
    X64Operand_zmm,
    X64Operand_rel8,
    X64Operand_rel32,
};

struct X64_Operand {
    X64_Operand_Kind kind;
    union {
        X64_Register reg;
        s8  imm8;
        s16 imm16;
        s32 imm32;
        s64 imm64;
    };
};

struct X64_Instruction {
    X64_Opcode opcode;
    union {
        struct {
            X64_Operand op0;
            X64_Operand op1;
            X64_Operand op2;
        };
        X64_Operand operands[3];
    };
};

union X64_Instruction_Index {
    struct {
        u8 opcode;
        u8 op0;
        u8 op1;
        u8 op2;
    };
    u32 packed;
};


struct X64_Encoding {
    u8 prefix;
    u8 modrm;
    u8 sib;
};