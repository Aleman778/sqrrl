
#define DEF_X64_OPCODES \
X64_OPCODE(invalid, INVALID) \
X64_OPCODE(noop, NOOP) \
X64_OPCODE(int3, INT3) \
X64_OPCODE(neg, NEG) \
X64_OPCODE(not, NOT) \
X64_OPCODE(and, AND) \
X64_OPCODE(or, OR) \
X64_OPCODE(xor, XOR) \
X64_OPCODE(add, ADD) \
X64_OPCODE(sub, SUB) \
X64_OPCODE(mul, MUL) \
X64_OPCODE(imul, IMUL) \
X64_OPCODE(idiv, IDIV) \
X64_OPCODE(cwd, CWD) \
X64_OPCODE_ALIAS(cwd, cwq, CWQ) \
X64_OPCODE_ALIAS(cwd, cwo, CWO) \
X64_OPCODE(lea, LEA) \
X64_OPCODE(mov, MOV) \
X64_OPCODE(movsx, MOVSX) \
X64_OPCODE(movzx, MOVZX) \
X64_OPCODE(push, PUSH) \
X64_OPCODE(pop, POP) \
X64_OPCODE(cmp, CMP) \
X64_OPCODE(test, TEST) \
X64_OPCODE(jmp, JMP) \
X64_OPCODE(ja, JA) \
X64_OPCODE(jae, JAE) \
X64_OPCODE(jb, JB) \
X64_OPCODE(jbe, JBE) \
X64_OPCODE(jc, JC) \
X64_OPCODE(jcxz, JCXZ) \
X64_OPCODE(jecxz, JECXZ) \
X64_OPCODE(jrcxz, JRCXZ) \
X64_OPCODE(je, JE) \
X64_OPCODE(jg, JG) \
X64_OPCODE(jge, JGE) \
X64_OPCODE(jl, JL) \
X64_OPCODE(jle, JLE) \
X64_OPCODE(jna, JNA) \
X64_OPCODE(jnae, JNAE) \
X64_OPCODE(jnb, JNB) \
X64_OPCODE(jnbe, JNBE) \
X64_OPCODE(jnc, JNC) \
X64_OPCODE(jne, JNE) \
X64_OPCODE(jng, JNG) \
X64_OPCODE(jnge, JNGE) \
X64_OPCODE(jnl, JNL) \
X64_OPCODE(jnle, JNLE) \
X64_OPCODE(jno, JNO) \
X64_OPCODE(jnp, JNP) \
X64_OPCODE(jns, JNS) \
X64_OPCODE(jnz, JNZ) \
X64_OPCODE(jo, JO) \
X64_OPCODE(jp, JP) \
X64_OPCODE(jpe, JPE) \
X64_OPCODE(jpo, JPO) \
X64_OPCODE(js, JS) \
X64_OPCODE(jz, JZ) \
X64_OPCODE(seta, SETA) \
X64_OPCODE(setae, SETAE) \
X64_OPCODE(setb, SETB) \
X64_OPCODE(setbe, SETBE) \
X64_OPCODE(setc, SETC) \
X64_OPCODE(sete, SETE) \
X64_OPCODE(setg, SETG) \
X64_OPCODE(setge, SETGE) \
X64_OPCODE(setl, SETL) \
X64_OPCODE(setle, SETLE) \
X64_OPCODE(setna, SETNA) \
X64_OPCODE(setnae, SETNAE) \
X64_OPCODE(setnb, SETNB) \
X64_OPCODE(setnbe, SETNBE) \
X64_OPCODE(setnc, SETNC) \
X64_OPCODE(setne, SETNE) \
X64_OPCODE(setng, SETNG) \
X64_OPCODE(setnge, SETNGE) \
X64_OPCODE(setnl, SETNL) \
X64_OPCODE(setnle, SETNLE) \
X64_OPCODE(setno, SETNO) \
X64_OPCODE(setnp, SETNP) \
X64_OPCODE(setns, SETNS) \
X64_OPCODE(setnz, SETNZ) \
X64_OPCODE(seto, SETO) \
X64_OPCODE(setp, SETP) \
X64_OPCODE(setpe, SETPE) \
X64_OPCODE(setpo, SETPO) \
X64_OPCODE(sets, SETS) \
X64_OPCODE(setz, SETZ) \
X64_OPCODE(call, CALL) \
X64_OPCODE(ret, RET) \
X64_OPCODE(label, LABEL) \
X64_DIRECTIVE(db, DB) \
X64_DIRECTIVE(dw, DW) \
X64_DIRECTIVE(dd, DD) \
X64_DIRECTIVE(dq, DQ)
// NOTE(Alexander): label is not a real opcode

enum X64_Opcode {
#define X64_OPCODE(mnemonic,...) X64Opcode_##mnemonic,
#define X64_OPCODE_ALIAS(alias, mnemonic,...) X64Opcode_##mnemonic = X64Opcode_##alias,
#define X64_DIRECTIVE(mnemonic, ...) X64Directive_##mnemonic,
    DEF_X64_OPCODES
#undef X64_DIRECTIVE
#undef X64_OPCODE_ALIAS
#undef X64_OPCODE
};


global const cstring x64_opcode_names[] = {
#define X64_OPCODE(mnemonic,...) #mnemonic,
#define X64_OPCODE_ALIAS(...)
#define X64_DIRECTIVE(mnemonic, ...) #mnemonic,
    DEF_X64_OPCODES
#undef X64_DIRECTIVE
#undef X64_OPCODE_ALIAS
#undef X64_OPCODE
};

inline X64_Opcode
x64_opcode_invert_jump_condition(X64_Opcode opcode) {
    switch (opcode) {
        
        case X64Opcode_ja: return X64Opcode_jna;
        case X64Opcode_jna: return X64Opcode_ja;
        
        case X64Opcode_jae: return X64Opcode_jnae;
        case X64Opcode_jnae: return X64Opcode_jae;
        
        case X64Opcode_jb: return X64Opcode_jnb;
        case X64Opcode_jnb: return X64Opcode_jb;
        
        case X64Opcode_jbe: return X64Opcode_jnbe;
        case X64Opcode_jnbe: return X64Opcode_jbe;
        
        case X64Opcode_jc: return X64Opcode_jnc;
        case X64Opcode_jnc: return X64Opcode_jc;
        
        case X64Opcode_je: return X64Opcode_jne;
        case X64Opcode_jne: return X64Opcode_je;
        
        case X64Opcode_jg: return X64Opcode_jng;
        case X64Opcode_jng: return X64Opcode_jg;
        
        case X64Opcode_jge: return X64Opcode_jnge;
        case X64Opcode_jnge: return X64Opcode_jge;
        
        case X64Opcode_jl: return X64Opcode_jnl;
        case X64Opcode_jnl: return X64Opcode_jl;
        
        case X64Opcode_jle: return X64Opcode_jnle;
        case X64Opcode_jnle: return X64Opcode_jle;
        
        default: assert(0 && "not invertable conditional jump opcode");
    }
    
    return opcode;
}


#define DEF_X64_OPERANDS \
X64_OP(None) \
X64_OP(r8) \
X64_OP(r16) \
X64_OP(r32) \
X64_OP(r64) \
X64_OP(m8) \
X64_OP(m16) \
X64_OP(m32) \
X64_OP(m64) \
X64_OP(imm8) \
X64_OP(imm16) \
X64_OP(imm32) \
X64_OP(imm64) \
X64_OP(st) \
X64_OP(mm) \
X64_OP(xmm) \
X64_OP(ymm) \
X64_OP(zmm) \
X64_OP(rel8) \
X64_OP(rel32) \
X64_OP(jump_target) \
X64_OP(data_target) \
X64_OP(basic_block)

enum X64_Operand_Kind {
#define X64_OP(op) X64Operand_##op,
    DEF_X64_OPERANDS
#undef X64_OP
};


enum X64_Register_Type {
    X64RegisterType_GPR, // General purpose register
    X64RegisterType_FPU, // Floating point unit
    X64RegisterType_MMX, // First version of SIMD
    X64RegisterType_SSE, // Streaming SIMD Extension
    X64RegisterType_AVX, // Advanced Vector Extension
};

// X64_REGISTER(mnemonic, size, id, opcode, operand_kind, type)
#define DEF_X64_REGISTERS \
X64_REGISTER(unallocated_r8,   1,  -1, r8, GPR) \
X64_REGISTER(unallocated_r16,  2,  -1, r16, GPR) \
X64_REGISTER(unallocated_r32,  4,  -1, r32, GPR) \
X64_REGISTER(unallocated_r64,  8,  -1, r64, GPR) \
X64_REGISTER(unallocated_stx,  10, -1, st, GPR) \
X64_REGISTER(unallocated_mmx,  8,  -1, mm, GPR) \
\
X64_REGISTER(ah,   1, 20, r8, GPR) \
X64_REGISTER(al,   1, 0, r8,  GPR) \
X64_REGISTER(ax,   2, 0, r16, GPR) \
X64_REGISTER(eax,  4, 0, r32, GPR) \
X64_REGISTER(rax,  8, 0, r64, GPR) \
\
X64_REGISTER(bh,   1, 23, r8, GPR) \
X64_REGISTER(bl,   1, 3, r8,  GPR) \
X64_REGISTER(bx,   2, 3, r16, GPR) \
X64_REGISTER(ebx,  4, 3, r32, GPR) \
X64_REGISTER(rbx,  8, 3, r64, GPR) \
\
X64_REGISTER(ch,   1, 21, r8, GPR) \
X64_REGISTER(cl,   1, 1, r8,  GPR) \
X64_REGISTER(cx,   2, 1, r16, GPR)\
X64_REGISTER(ecx,  4, 1, r32, GPR) \
X64_REGISTER(rcx,  8, 1, r64, GPR) \
\
X64_REGISTER(dh,   1, 22, r8, GPR) \
X64_REGISTER(dl,   1, 2, r8,  GPR) \
X64_REGISTER(dx,   2, 2, r16, GPR) \
X64_REGISTER(edx,  4, 2, r32, GPR) \
X64_REGISTER(rdx,  8, 2, r64, GPR) \
\
X64_REGISTER(sil,  1, 6, r8,  GPR) \
X64_REGISTER(si,   2, 6, r16, GPR) \
X64_REGISTER(esi,  4, 6, r32, GPR) \
X64_REGISTER(rsi,  8, 6, r64, GPR) \
\
X64_REGISTER(dil,  1, 7, r8,  GPR) \
X64_REGISTER(di,   2, 7, r16, GPR) \
X64_REGISTER(edi,  4, 7, r32, GPR) \
X64_REGISTER(rdi,  8, 7, r64, GPR) \
\
X64_REGISTER(bpl,  1, 5, r8,  GPR) \
X64_REGISTER(bp,   2, 5, r16, GPR) \
X64_REGISTER(ebp,  4, 5, r32, GPR) \
X64_REGISTER(rbp,  8, 5, r64, GPR) \
\
X64_REGISTER(spl,  1, 4, r8,  GPR) \
X64_REGISTER(sp,   2, 4, r16, GPR) \
X64_REGISTER(esp,  4, 4, r32, GPR) \
X64_REGISTER(rsp,  8, 4, r64, GPR) \
\
X64_REGISTER(r8b,  1, 8, r8,  GPR) \
X64_REGISTER(r8w,  2, 8, r16, GPR)\
X64_REGISTER(r8d,  4, 8, r32, GPR) \
X64_REGISTER(r8,   8, 8, r64, GPR) \
\
X64_REGISTER(r9b,  1, 9, r8,  GPR) \
X64_REGISTER(r9w,  2, 9, r16, GPR) \
X64_REGISTER(r9d,  4, 9, r32, GPR) \
X64_REGISTER(r9,   8, 9, r64, GPR) \
\
X64_REGISTER(r10b,  1, 10, r8,  GPR) \
X64_REGISTER(r10w,  2, 10, r16, GPR)\
X64_REGISTER(r10d,  4, 10, r32, GPR) \
X64_REGISTER(r10,   8, 10, r64, GPR) \
\
X64_REGISTER(r11b,  1, 11, r8,  GPR) \
X64_REGISTER(r11w,  2, 11, r16, GPR) \
X64_REGISTER(r11d,  4, 11, r32, GPR) \
X64_REGISTER(r11,   8, 11, r64, GPR) \
\
X64_REGISTER(r12b,  1, 12, r8,  GPR) \
X64_REGISTER(r12w,  2, 12, r16, GPR)\
X64_REGISTER(r12d,  4, 12, r32, GPR) \
X64_REGISTER(r12,   8, 12, r64, GPR) \
\
X64_REGISTER(r13b,  1, 13, r8,  GPR) \
X64_REGISTER(r13w,  2, 13, r16, GPR) \
X64_REGISTER(r13d,  4, 13, r32, GPR) \
X64_REGISTER(r13,   8, 13, r64, GPR) \
\
X64_REGISTER(r14b,  1, 14, r8,  GPR) \
X64_REGISTER(r14w,  2, 14, r16, GPR) \
X64_REGISTER(r14d,  4, 14, r32, GPR) \
X64_REGISTER(r14,   8, 14, r64, GPR) \
\
X64_REGISTER(r15b,  1, 15, r8,  GPR) \
X64_REGISTER(r15w,  2, 15, r16, GPR) \
X64_REGISTER(r15d,  4, 15, r32, GPR) \
X64_REGISTER(r15,   8, 15, r64, GPR) \
\
X64_REGISTER(ip,    2, 25, r16, GPR) \
X64_REGISTER(eip,   4, 25, r32, GPR) \
X64_REGISTER(rip,   8, 25, r64, GPR) \
\
X64_REGISTER(st0, 10, 0, st, FPU) \
X64_REGISTER(st1, 10, 1, st, FPU) \
X64_REGISTER(st2, 10, 2, st, FPU) \
X64_REGISTER(st3, 10, 3, st, FPU) \
X64_REGISTER(st4, 10, 4, st, FPU) \
X64_REGISTER(st5, 10, 5, st, FPU) \
X64_REGISTER(st6, 10, 6, st, FPU) \
X64_REGISTER(st7, 10, 7, st, FPU) \
\
X64_REGISTER(mm0, 8, 0, mm, MMX) \
X64_REGISTER(mm1, 8, 1, mm, MMX) \
X64_REGISTER(mm2, 8, 2, mm, MMX) \
X64_REGISTER(mm3, 8, 3, mm, MMX) \
X64_REGISTER(mm4, 8, 4, mm, MMX) \
X64_REGISTER(mm5, 8, 5, mm, MMX) \
X64_REGISTER(mm6, 8, 6, mm, MMX) \
X64_REGISTER(mm7, 8, 7, mm, MMX)
// TODO(Alexander): add more registers

global int const x64_num_physical_registers = 24; // counting rax and eax as the same

enum X64_Register {
#define X64_REGISTER(mnemonic, ...) X64Register_##mnemonic,
    DEF_X64_REGISTERS
#undef X64_REGISTER
};

global int x64_register_size_table[] = {
#define X64_REGISTER(mnemonic, size, ...) size,
    DEF_X64_REGISTERS
#undef X64_REGISTER
};

global int x64_register_id_table[] = {
#define X64_REGISTER(mnemonic, size, id, ...) id,
    DEF_X64_REGISTERS
#undef X64_REGISTER
};

global X64_Operand_Kind x64_register_operand_table[] = {
#define X64_REGISTER(mnemonic, size, id, operand_kind, ...) X64Operand_##operand_kind,
    DEF_X64_REGISTERS
#undef X64_REGISTER
};

global cstring x64_register_name_table[] = {
#define X64_REGISTER(mnemonic, ...) #mnemonic,
    DEF_X64_REGISTERS
#undef X64_REGISTER
};

global X64_Register x64_gpr_register_table[] = {
    X64Register_rax, X64Register_rcx, X64Register_rdx, X64Register_rbx,
    X64Register_rsi, X64Register_rdi, X64Register_r8,  X64Register_r9,
    X64Register_r10, X64Register_r11, X64Register_r12, X64Register_r13,
    X64Register_r14, X64Register_r15
};

bool
register_is_gpr(X64_Register reg) {
    return reg >= X64Register_ah && reg <= X64Register_r15;
}

// NOTE(Alexander): forward declare
struct X64_Basic_Block;

typedef Bc_Label X64_Label;

struct X64_Operand {
    X64_Operand_Kind kind;
    union {
        X64_Register reg;
        s8  imm8;
        s16 imm16;
        s32 imm32;
        s64 imm64;
        X64_Basic_Block* basic_block;
        X64_Label jump_target;
        u64 virtual_register;
    };
    union {
        s8 disp8;
        s16 disp16;
        s32 disp32;
        s64 disp64;
    };
    b32 is_allocated;
};

inline bool
operand_is_register(X64_Operand_Kind kind) {
    return kind >= X64Operand_r8 && kind <= X64Operand_r64;
}

inline bool
operand_is_memory(X64_Operand_Kind kind) {
    return (kind >= X64Operand_m8  && kind <= X64Operand_m64);
}

inline bool
operand_is_immediate(X64_Operand_Kind kind) {
    return (kind >= X64Operand_imm8 && kind <= X64Operand_imm64);
}

X64_Operand_Kind
x64_get_register_kind(Bc_Type type) {
    if (type.ptr_depth > 0) {
        return X64Operand_r64;
    }
    
    switch (type.kind) {
        case BcType_s1:
        case BcType_s8:
        case BcType_u8: return X64Operand_r8;
        
        case BcType_s16:
        case BcType_u16: return X64Operand_r16;
        
        case BcType_s32:
        case BcType_u32: return X64Operand_r32;
        
        case BcType_s64:
        case BcType_u64: return X64Operand_r64;
        
        case BcType_f32:
        case BcType_f64: return X64Operand_mm;
        
        case BcType_Aggregate: return X64Operand_r64;
        
        default: assert(0 && "invalid bytecode type kind");
    }
    
    return X64Operand_None;
}

X64_Operand_Kind
x64_get_memory_kind(Bc_Type type) {
    if (type.ptr_depth > 0) {
        return X64Operand_m64;
    }
    
    switch (type.kind) {
        case BcType_s1:
        case BcType_s8:
        case BcType_u8: return X64Operand_m8;
        
        case BcType_s16:
        case BcType_u16: return X64Operand_m16;
        
        case BcType_s32:
        case BcType_u32: return X64Operand_m32;
        
        case BcType_s64:
        case BcType_u64: return X64Operand_m64;
        
        // TODO(Alexander): floating point
        default: assert(0 && "invalid bytecode type kind");
    }
    
    return X64Operand_None;
}

X64_Operand_Kind
x64_get_immediate_kind(Bc_Type type) {
    if (type.ptr_depth > 0) {
        return X64Operand_imm64;
    }
    
    switch (type.kind) {
        case BcType_s1:
        case BcType_s8:
        case BcType_u8: return X64Operand_imm8;
        
        case BcType_s16:
        case BcType_u16: return X64Operand_imm16;
        
        case BcType_s32:
        case BcType_u32: return X64Operand_imm32;
        
        case BcType_s64:
        case BcType_u64: return X64Operand_imm64;
        
        // TODO(Alexander): floating point
        default: assert(0 && "invalid bytecode type kind");
    }
    
    return X64Operand_None;
}

X64_Operand
x64_operand_type_cast(X64_Operand source, Bc_Type dest_type) {
    if (operand_is_register(source.kind))  {
        source.kind = x64_get_register_kind(dest_type);
    } else if (operand_is_memory(source.kind)) {
        source.kind = x64_get_memory_kind(dest_type);
    } else if  (operand_is_immediate(source.kind)) {
        source.kind = x64_get_immediate_kind(dest_type);
    }
    return source;
}


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
#if BUILD_DEBUG
    cstring comment; // NOTE(Alexander): only for debug purposes
#endif
};

struct X64_Machine_Code {
    u8* bytes;
    umm size;
    umm offset;
};

struct X64_Basic_Block {
    X64_Label label;
    X64_Instruction* first;
    umm count;
    X64_Basic_Block* next;
    X64_Machine_Code code;
};

#define for_x64_basic_block(first_block, it, it_index, code) { \
X64_Basic_Block* it_block = first_block; \
umm it_index = 0; \
\
while (it_block) { \
while (it_index < it_block->count) { \
X64_Instruction* it = it_block->first + it_index; \
code; \
it_index++; \
} \
it_block = it_block->next; \
it_index = 0; \
} \
}

union X64_Instruction_Index {
    struct {
        u8 opcode;
        u8 op0;
        u8 op1;
        u8 op2;
    };
    u32 packed;
};


typedef u8 ModRM_Mod;
enum {
    ModRM_indirect        = (u8) 0b00 << 6,
    ModRM_indirect_disp8  = (u8) 0b01 << 6,
    ModRM_indirect_disp32 = (u8) 0b10 << 6,
    ModRM_direct          = (u8) 0b11 << 6,
    ModRM_not_used        = (u8) 0b11111111,
};

struct X64_Encoding {
    bool use_prefix;
    bool use_0f_prefix;
    bool use_rex_prefix;
    bool use_rex_w;
    bool use_opcode_addend;
    
    u8 prefix;
    
    u8 primary_opcode;
    u8 secondary_opcode;
    u8 opcode_addend;
    
    u8 modrm_mod;
    u8 modrm_reg;
    u8 modrm_rm;
    
    u8 imm_size;
    u8 imm_op;
    
    bool is_valid;
};


struct X64_Int_Register {
    u64 virtual_register;
};

typedef map(X64_Instruction_Index, X64_Encoding) X64_Instruction_Def_Table;


union X64_Register_Index {
    struct {
        u8 register_id;
        u8 operand_kind;
    };
    u16 packed;
};

X64_Register
convert_register_to_specific_kind(X64_Register reg, X64_Operand_Kind kind) {
    X64_Operand_Kind reg_kind = x64_register_operand_table[reg];
    if (reg_kind == kind) {
        return reg;
    }
    
#define rindex(id, operand_kind) (((u16) id << 8) | (u16) X64Operand_##operand_kind)
    
    u16 index = (u16) x64_register_id_table[reg] << 8 | (u16) kind;
    
    switch (index) {
#define X64_REGISTER(mnemonic, size, id, opreand_kind, ...) case rindex(id, opreand_kind): \
return X64Register_##mnemonic;
        DEF_X64_REGISTERS
#undef X64_REGISTER
        default: {
            switch (kind) {
                case X64Operand_r8: return X64Register_unallocated_r8;
                case X64Operand_r16: return X64Register_unallocated_r16;
                case X64Operand_r32: return X64Register_unallocated_r32;
                case X64Operand_r64: return X64Register_unallocated_r64;
            }
        } break;
    }
    
    return reg;
}

void
string_builder_push(String_Builder* sb, X64_Operand* operand, bool show_virtual_registers=false) {
    switch (operand->kind) {
        case X64Operand_r8:
        case X64Operand_r16:
        case X64Operand_r32:
        case X64Operand_r64: {
            if (operand->is_allocated) {
                X64_Register actual_reg = convert_register_to_specific_kind(operand->reg, operand->kind);
                string_builder_push_format(sb, "%", f_cstring(x64_register_name_table[actual_reg]));
            } else {
                if (show_virtual_registers) {
                    string_builder_push_format(sb, "r%", f_u32(operand->virtual_register));
                } else {
                    string_builder_push(sb, "???");
                }
            }
        } break;
        
        case X64Operand_m8:
        case X64Operand_m16:
        case X64Operand_m32:
        case X64Operand_m64: {
            switch (operand->kind) {
                case X64Operand_m8:  string_builder_push(sb, "byte ptr ");  break;
                case X64Operand_m16: string_builder_push(sb, "word ptr ");  break;
                case X64Operand_m32: string_builder_push(sb, "dword ptr "); break;
                case X64Operand_m64: string_builder_push(sb, "qword ptr "); break;
            }
            
            if (operand->is_allocated) {
                X64_Operand_Kind reg_kind = X64Operand_r64;
                X64_Register actual_reg = convert_register_to_specific_kind(operand->reg, reg_kind);
                string_builder_push_format(sb, "[%", f_cstring(x64_register_name_table[actual_reg]));
            } else {
                string_builder_push_format(sb, "[r%", f_u32(operand->virtual_register));
            }
            
            if (operand->disp64 > 0) {
                string_builder_push_format(sb, " + %", f_u64(operand->disp64));
            } else if (operand->disp64 < 0) {
                string_builder_push_format(sb, " - %", f_u64(-operand->disp64));
            }
            
            string_builder_push(sb, "]");
        } break;
        
        case X64Operand_imm8: {
            string_builder_push_format(sb, "%", f_s64(operand->imm8));
        } break;
        
        case X64Operand_imm16: {
            string_builder_push_format(sb, "%", f_s64(operand->imm16));
        } break;
        
        case X64Operand_imm32: {
            string_builder_push_format(sb, "%", f_s64(operand->imm32));
        } break;
        
        case X64Operand_imm64: {
            string_builder_push_format(sb, "%", f_s64(operand->imm64));
        } break;
        
        case X64Operand_jump_target: {
            string_builder_push_format(sb, "%", f_string(vars_load_string(operand->jump_target.ident)));
            if (operand->jump_target.index > 0) {
                string_builder_push_format(sb, "%", f_u32(operand->jump_target.index));
            }
        } break;
        
        case X64Operand_data_target: {
            string_builder_push_format(sb, "[%", f_string(vars_load_string(operand->jump_target.ident)));
            if (operand->jump_target.index > 0) {
                string_builder_push_format(sb, "%]", f_u32(operand->jump_target.index));
            } else {
                string_builder_push(sb, "]");
            }
        } break;
        
    }
}

void
string_builder_push(String_Builder* sb, X64_Instruction* insn, bool show_virtual_registers=false) {
    if (insn->opcode == X64Opcode_label) {
        X64_Basic_Block* block = insn->op0.basic_block;
        if (block->label.ident) {
            if (block->label.index > 0) {
                string_builder_push_format(sb, "%", 
                                           f_string(vars_load_string(block->label.ident)));
                string_builder_push_format(sb, "%:", f_u32(block->label.index));
            } else {
                string_builder_push_format(sb, "%:",
                                           f_string(vars_load_string(block->label.ident)));
            }
        } else {
            string_builder_push_format(sb, "%:", f_u32(block->label.index));
        }
    } else {
        
        cstring mnemonic = x64_opcode_names[insn->opcode];
        
        string_builder_push_format(sb, "    %", f_cstring(mnemonic));
        if (insn->op0.kind) {
            string_builder_push(sb, " ");
            string_builder_push(sb, &insn->op0, show_virtual_registers);
        }
        
        if (insn->op1.kind) {
            string_builder_push(sb, ", ");
            string_builder_push(sb, &insn->op1, show_virtual_registers);
        }
        
        if (insn->op2.kind) {
            string_builder_push(sb, ", ");
            string_builder_push(sb, &insn->op2, show_virtual_registers);
        }
        
#if BUILD_DEBUG
        if (insn->opcode != X64Opcode_invalid && insn->comment) {
            const s32 comment_offset = 50;
            
            // Find line length by going back to previous newline character
            u32 line_length = 0;
            u8* curr = sb->data + sb->curr_used;
            while (line_length++ < comment_offset && *curr-- != '\n');
            
            // Add spaces to make line length at least 30 characters long
            if (line_length < comment_offset) {
                for (int i = line_length; i < comment_offset; i++) string_builder_push(sb, " ");
            }
            
            string_builder_push_format(sb, " // %", f_cstring(insn->comment));
        }
#endif
    }
    
}

void
string_builder_push(String_Builder* sb, 
                    X64_Basic_Block* first_block,
                    bool show_virtual_registers=false) {
    
    X64_Basic_Block* curr_block = first_block;
    while (curr_block) {
        for (umm insn_index = 0; insn_index < curr_block->count; insn_index++) {
            X64_Instruction* insn = curr_block->first + insn_index;
            string_builder_push(sb, insn, show_virtual_registers);
            string_builder_push(sb, "\n");
        }
        
        string_builder_push(sb, "\n");
        curr_block = curr_block->next;
    }
}
