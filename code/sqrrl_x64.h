
enum X64_Opcode {
    X64Opcode_ADD,
    X64Opcode_MOV,
};

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
// X64_GPR_BYTE(index, qword, dword, word, byte_low, byte_high)
// X64_GPR(index, qword, dword, word, byte_low)
// X64_MMX_FPU(index, mmx, stx)
#define DEF_X64_REGISTERS \
X64_GPR_BYTE(0, rax, eax, ax, al, ah) \
X64_GPR_BYTE(1, rbx, ebx, bx, bl, bh) \
X64_GPR_BYTE(2, rcx, ecx, cx, cl, ch) \
X64_GPR_BYTE(3, rdx, edx, dx, dl, dh) \
X64_GPR(4,  rsi, esi,  si,   sil) \
X64_GPR(5,  rdi, edi,  di,   dil) \
X64_GPR(6,  rbp, ebp,  bp,   bpl) \
X64_GPR(7,  rsp, esp,  sp,   spl) \
X64_GPR(8,  r8,  r8d,  r8w,  r8b) \
X64_GPR(9,  r9,  r9d,  r9w,  r9b) \
X64_GPR(10, r10, r10d, r10w, r10b) \
X64_GPR(11, r11, r11d, r11w, r11b) \
X64_GPR(12, r12, r12d, r12w, r12b) \
X64_GPR(13, r13, r13d, r13w, r13b) \
X64_GPR(14, r14, r14d, r14w, r14b) \
X64_GPR(15, r15, r15d, r15w, r15b) \
X64_MMX_FPU(16, mm0, st0) \
X64_MMX_FPU(17, mm1, st1) \
X64_MMX_FPU(18, mm2, st2) \
X64_MMX_FPU(19, mm3, st3) \
X64_MMX_FPU(20, mm4, st4) \
X64_MMX_FPU(21, mm5, st5) \
X64_MMX_FPU(22, mm6, st6) \
X64_MMX_FPU(23, mm7, st7) \
X64_AVX_SSE(24, zmm0,  ymm0,  xmm0) \
X64_AVX_SSE(25, zmm1,  ymm1,  xmm1) \
X64_AVX_SSE(26, zmm2,  ymm2,  xmm2) \
X64_AVX_SSE(27, zmm3,  ymm3,  xmm3) \
X64_AVX_SSE(28, zmm4,  ymm4,  xmm4) \
X64_AVX_SSE(29, zmm5,  ymm5,  xmm5) \
X64_AVX_SSE(30, zmm6,  ymm6,  xmm6) \
X64_AVX_SSE(31, zmm7,  ymm7,  xmm7) \
X64_AVX_SSE(32, zmm8,  ymm8,  xmm8) \
X64_AVX_SSE(33, zmm9,  ymm9,  xmm9) \
X64_AVX_SSE(34, zmm10, ymm10, xmm10) \
X64_AVX_SSE(35, zmm11, ymm11, xmm11) \
X64_AVX_SSE(36, zmm12, ymm12, xmm12) \
X64_AVX_SSE(37, zmm13, ymm13, xmm13) \
X64_AVX_SSE(38, zmm14, ymm14, xmm14) \
X64_AVX_SSE(39, zmm15, ymm15, xmm15) \
X64_AVX(40, zmm16) \
X64_AVX(41, zmm17) \
X64_AVX(42, zmm18) \
X64_AVX(43, zmm19) \
X64_AVX(44, zmm20) \
X64_AVX(45, zmm21) \
X64_AVX(46, zmm22) \
X64_AVX(47, zmm23) \
X64_AVX(48, zmm24) \
X64_AVX(49, zmm25) \
X64_AVX(50, zmm26) \
X64_AVX(51, zmm27) \
X64_AVX(52, zmm28) \
X64_AVX(53, zmm29) \
X64_AVX(54, zmm30) \
X64_AVX(55, zmm31)

global int const x64_num_physical_registers = 24; // counting rax and eax as the same

enum X64_Register {
#define X64_GPR_BYTE(index, qword, dword, word, byte_low, byte_high) X64_##qword, X64_##dword, X64_##word, X64_##byte_low, X64_##byte_high,
#define X64_GPR(index, qword, dword, word, byte_low) X64_##qword, X64_##dword, X64_##word, X64_##byte_low,
#define X64_MMX_FPU(index, mmx, fpu) X64_##mmx, X64_##fpu,
#define X64_AVX_SSE(index, zmm, ymm, xmm) X64_##zmm, X64_##ymm, X64_##xmm,
#define X64_AVX(index, zmm) X64_##zmm,
    DEF_X64_REGISTERS
#undef X64_GPR_BYTE
#undef X64_GPR
#undef X64_MMX_FPU
#undef X64_AVX_SSE
#undef X64_AVX
};

// TODO(Alexander): is this really needed?
int x64_register_to_index[] = {
#define X64_GPR_BYTE(index, ...) index, index, index, index, index,
#define X64_GPR(index, ...) index, index, index, index,
#define X64_MMX_FPU(index, ...) index, index,
#define X64_AVX_SSE(index, ...) index, index, index,
#define X64_AVX(index, ...) index,
    DEF_X64_REGISTERS
#undef X64_GPR_BYTE
#undef X64_GPR
#undef X64_MMX_FPU
#undef X64_AVX_SSE
#undef X64_AVX
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

struct X64_Encoding {
    
};

struct X64_Instruction {
    X64_Opcode opcode;
    union {
        struct {
            X64_Operand dest;
            X64_Operand src0;
            X64_Operand src1;
        };
        X64_Operand operands[3];
    };
};


