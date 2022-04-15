
#define DEF_X64_OPCODES \
X64_OPCODE(invalid, INVALID) \
X64_OPCODE(add, ADD) \
X64_OPCODE(sub, SUB) \
X64_OPCODE(mul, MUL) \
X64_OPCODE(imul, IMUL) \
X64_OPCODE(idiv, IDIV) \
X64_OPCODE(cwd, CWD) \
X64_OPCODE(mov, MOV) \
X64_OPCODE(push, PUSH) \
X64_OPCODE(pop, POP) \
X64_OPCODE(ret, RET) \
X64_OPCODE(label, LABEL) \
X64_OPCODE_ALIAS(cwd, cwq, CWQ) \
X64_OPCODE_ALIAS(cwd, cwo, CWO)
// NOTE(Alexander): label is not a real opcode

enum X64_Opcode {
#define X64_OPCODE(mnemonic,...) X64Opcode_##mnemonic,
#define X64_OPCODE_ALIAS(alias, mnemonic,...) X64Opcode_##mnemonic = X64Opcode_##alias,
    DEF_X64_OPCODES
#undef X64_OPCODE_ALIAS
#undef X64_OPCODE
};


global const cstring x64_opcode_names[] = {
#define X64_OPCODE(mnemonic,...) #mnemonic,
#define X64_OPCODE_ALIAS(...)
    DEF_X64_OPCODES
#undef X64_OPCODE_ALIAS
#undef X64_OPCODE
};


enum X64_Register_Type {
    X64RegisterType_GPR, // General purpose register
    X64RegisterType_FPU, // Floating point unit
    X64RegisterType_MMX, // First version of SIMD
    X64RegisterType_SSE, // Streaming SIMD Extension
    X64RegisterType_AVX, // Advanced Vector Extension
};

// X64_REGISTER(mnemonic, size, id, type)
#define DEF_X64_REGISTERS \
X64_REGISTER(unallocated_r8,   1,  0, GPR) \
X64_REGISTER(unallocated_r16,  2,  0, GPR) \
X64_REGISTER(unallocated_r32,  4,  0, GPR) \
X64_REGISTER(unallocated_r64,  8,  0, GPR) \
X64_REGISTER(unallocated_stx,  10, 0, GPR) \
X64_REGISTER(unallocated_mmx,  8,  0, GPR) \
\
X64_REGISTER(ah,   1, 4, GPR) \
X64_REGISTER(al,   1, 0, GPR) \
X64_REGISTER(ax,   2, 0, GPR) \
X64_REGISTER(eax,  4, 0, GPR) \
X64_REGISTER(rax,  8, 0, GPR) \
\
X64_REGISTER(bh,   1, 7, GPR) \
X64_REGISTER(bl,   1, 3, GPR) \
X64_REGISTER(bx,   2, 3, GPR) \
X64_REGISTER(ebx,  4, 3, GPR) \
X64_REGISTER(rbx,  8, 3, GPR) \
\
X64_REGISTER(ch,   1, 5, GPR) \
X64_REGISTER(cl,   1, 1, GPR) \
X64_REGISTER(cx,   2, 1, GPR) \
X64_REGISTER(ecx,  4, 1, GPR) \
X64_REGISTER(rcx,  8, 1, GPR) \
\
X64_REGISTER(dh,   1, 6, GPR) \
X64_REGISTER(dl,   1, 2, GPR) \
X64_REGISTER(dx,   2, 2, GPR) \
X64_REGISTER(edx,  4, 2, GPR) \
X64_REGISTER(rdx,  8, 2, GPR) \
\
X64_REGISTER(sil,  1, 6, GPR) \
X64_REGISTER(si,   2, 6, GPR) \
X64_REGISTER(esi,  4, 6, GPR) \
X64_REGISTER(rsi,  8, 6, GPR) \
\
X64_REGISTER(dil,  1, 7, GPR) \
X64_REGISTER(di,   2, 7, GPR) \
X64_REGISTER(edi,  4, 7, GPR) \
X64_REGISTER(rdi,  8, 7, GPR) \
\
X64_REGISTER(bpl,  1, 5, GPR) \
X64_REGISTER(bp,   2, 5, GPR) \
X64_REGISTER(ebp,  4, 5, GPR) \
X64_REGISTER(rbp,  8, 5, GPR) \
\
X64_REGISTER(spl,  1, 4, GPR) \
X64_REGISTER(sp,   2, 4, GPR) \
X64_REGISTER(esp,  4, 4, GPR) \
X64_REGISTER(rsp,  8, 4, GPR) \
\
X64_REGISTER(r8b,  1, 8, GPR) \
X64_REGISTER(r8w,  2, 8, GPR) \
X64_REGISTER(r8d,  4, 8, GPR) \
X64_REGISTER(r8,   8, 8, GPR) \
\
X64_REGISTER(r9b,  1, 9, GPR) \
X64_REGISTER(r9w,  2, 9, GPR) \
X64_REGISTER(r9d,  4, 9, GPR) \
X64_REGISTER(r9,   8, 9, GPR) \
\
X64_REGISTER(r10b,  1, 10, GPR) \
X64_REGISTER(r10w,  2, 10, GPR) \
X64_REGISTER(r10d,  4, 10, GPR) \
X64_REGISTER(r10,   8, 10, GPR) \
\
X64_REGISTER(r11b,  1, 11, GPR) \
X64_REGISTER(r11w,  2, 11, GPR) \
X64_REGISTER(r11d,  4, 11, GPR) \
X64_REGISTER(r11,   8, 11, GPR) \
\
X64_REGISTER(r12b,  1, 12, GPR) \
X64_REGISTER(r12w,  2, 12, GPR) \
X64_REGISTER(r12d,  4, 12, GPR) \
X64_REGISTER(r12,   8, 12, GPR) \
\
X64_REGISTER(r13b,  1, 13, GPR) \
X64_REGISTER(r13w,  2, 13, GPR) \
X64_REGISTER(r13d,  4, 13, GPR) \
X64_REGISTER(r13,   8, 13, GPR) \
\
X64_REGISTER(r14b,  1, 14, GPR) \
X64_REGISTER(r14w,  2, 14, GPR) \
X64_REGISTER(r14d,  4, 14, GPR) \
X64_REGISTER(r14,   8, 14, GPR) \
\
X64_REGISTER(r15b,  1, 15, GPR) \
X64_REGISTER(r15w,  2, 15, GPR) \
X64_REGISTER(r15d,  4, 15, GPR) \
X64_REGISTER(r15,   8, 15, GPR) \
\
X64_REGISTER(st0, 10, 0, FPU) \
X64_REGISTER(st1, 10, 1, FPU) \
X64_REGISTER(st2, 10, 2, FPU) \
X64_REGISTER(st3, 10, 3, FPU) \
X64_REGISTER(st4, 10, 4, FPU) \
X64_REGISTER(st5, 10, 5, FPU) \
X64_REGISTER(st6, 10, 6, FPU) \
X64_REGISTER(st7, 10, 7, FPU) \
\
X64_REGISTER(mm0, 8, 0, MMX) \
X64_REGISTER(mm1, 8, 1, MMX) \
X64_REGISTER(mm2, 8, 2, MMX) \
X64_REGISTER(mm3, 8, 3, MMX) \
X64_REGISTER(mm4, 8, 4, MMX) \
X64_REGISTER(mm5, 8, 5, MMX) \
X64_REGISTER(mm6, 8, 6, MMX) \
X64_REGISTER(mm7, 8, 7, MMX)
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
X64_OP(basic_block)

enum X64_Operand_Kind {
#define X64_OP(op) X64Operand_##op,
    DEF_X64_OPERANDS
#undef X64_OP
};

// NOTE(Alexander): forward declare
struct X64_Basic_Block;

struct X64_Operand {
    X64_Operand_Kind kind;
    union {
        X64_Register reg;
        s8  imm8;
        s16 imm16;
        s32 imm32;
        s64 imm64;
        X64_Basic_Block* basic_block;
        u32 virtual_register;
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
    return ((kind >= X64Operand_r8  && kind <= X64Operand_r64) ||
            (kind >= X64Operand_m8 && kind <= X64Operand_m64));
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
};

struct X64_Basic_Block {
    Bc_Register label;
    X64_Instruction* first;
    umm count;
    X64_Basic_Block* next;
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

typedef map(X64_Instruction_Index, X64_Encoding) X64_Instruction_Def_Table;

void
string_builder_push(String_Builder* sb, X64_Operand* operand, bool show_virtual_registers=false) {
    switch (operand->kind) {
        case X64Operand_r8:
        case X64Operand_r16:
        case X64Operand_r32:
        case X64Operand_r64: {
            if (operand->is_allocated) {
                string_builder_push_format(sb, "%", f_cstring(x64_register_name_table[operand->reg]));
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
            if (operand->is_allocated) {
                string_builder_push_format(sb, "[%", f_cstring(x64_register_name_table[operand->reg]));
            } else {
                string_builder_push_format(sb, "[r%", f_u32(operand->virtual_register));
            }
            
            if (operand->disp64 > 0) {
                string_builder_push_format(sb, " + %", f_u32(operand->disp64));
            } else if (operand->disp64 < 0) {
                string_builder_push_format(sb, " - %", f_u32(-operand->disp64));
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
        
    }
}

void
string_builder_push(String_Builder* sb, 
                    X64_Basic_Block* first_block,
                    bool show_virtual_registers=false) {
    
    X64_Basic_Block* curr_block = first_block;
    for (umm insn_index = 0; insn_index < curr_block->count; insn_index++) {
        X64_Instruction* insn = curr_block->first + insn_index;
        
        if (insn->opcode == X64Opcode_label) {
            X64_Basic_Block* block = insn->op0.basic_block;
            if (block->label.ident) {
                string_builder_push_format(sb, "%:\n", 
                                           f_string(vars_load_string(block->label.ident)));
            } else {
                string_builder_push_format(sb, "%:\n",
                                           vars_load_string(block->label.index));
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
            string_builder_push(sb, "\n");
        }
    }
}
