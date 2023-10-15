
inline void
x64_zero(Buffer* buf, X64_Reg reg) {
    // REX.W + 33 /r 	XOR r64, r/m64 	RM
    x64_rex(buf, REX_W, reg, reg);
    push_u8(buf, 0x33);
    x64_modrm_direct(buf, reg, reg);
}

inline void
x64_move_rax_u64(Buffer* buf, u64 disp) {
    // REX.W + B8+ rd io 	MOV r64, imm64 	OI
    x64_rex(buf, REX_W);
    push_u8(buf, 0xB8);
    push_u64(buf, disp);
}

inline void
x64_move_immediate_to_register(Buffer* buf, X64_Reg dest, s32 immediate) { 
    // REX.W + C7 /0 id 	MOV r/m64, imm32
    x64_rex(buf, REX_W, 0, dest);
    push_u8(buf, 0xC7);
    x64_modrm_direct(buf, 0, dest);
    push_u32(buf, (u32) immediate);
}

inline void
x64_move8_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src) {
    // 88 /r 	MOV r/m8, r8 	MR
    if (src&8) x64_rex(buf, 0, src);
    push_u8(buf, 0x88);
    x64_modrm(buf, src, dest, disp, 0);
}

inline void
x64_move16_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src) {
    // 89 /r 	MOV r/m16, r16 	MR 	
    push_u8(buf, X64_OP_SIZE_PREFIX);
    if (src&8) x64_rex(buf, 0, src);
    push_u8(buf, 0x89);
    x64_modrm(buf, src, dest, disp, 0);
}

inline void
x64_move32_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src) {
    // 89 /r 	MOV r/m32, r32 	MR
    if (src&8) x64_rex(buf, 0, src);
    push_u8(buf, 0x89);
    x64_modrm(buf, src, dest, disp, 0);
}

inline void
x64_move_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src) {
    // 89 /r 	MOV r/m64,r64 	MR
    x64_rex(buf, REX_W, src);
    push_u8(buf, 0x89);
    x64_modrm(buf, src, dest, disp, 0);
}

inline void
x64_move_memory_to_register(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp) {
    // REX.W + 8B /r 	MOV r64, r/m64 	RM
    x64_rex(buf, REX_W, dest);
    push_u8(buf, 0x8B);
    x64_modrm(buf, dest, src, disp, 0);
}

inline u32*
x64_move_memory_to_register_disp(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp) {
    x64_rex(buf, REX_W, dest);
    push_u8(buf, 0x8B);
    push_u8(buf, MODRM_INDIRECT_DISP32 | ((dest & 7) << 3) | (src & 7));
    push_u8(buf, 0x24); // SIB for RSP
    u32* result = (u32*) (buf->data + buf->curr_used);
    push_u32(buf, (u32) disp);
    return result;
}

void
x64_move_extend(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp, int size, bool is_signed) {
    if (is_signed) {
        switch (size) {
            case 1: {
                // REX.W + 0F BE /r 	MOVSX r64, r/m8 	RM
                x64_rex(buf, REX_W, dest);
                push_u16(buf, 0xBE0F);
            } break;
            
            case 2: {
                // 0F BF /r 	MOVSX r32, r/m16 	RM
                push_u8(buf, X64_OP_SIZE_PREFIX);
                x64_rex(buf, REX_W, dest);
                push_u16(buf, 0xBF0F);
            } break;
            
            case 4: {
                // REX.W + 63 /r 	MOVSXD r64, r/m32 	RM
                x64_rex(buf, REX_W, dest);
                push_u8(buf, 0x63);
            } break;
            
            default: {
                x64_rex(buf, REX_W, dest);
                push_u8(buf, 0x8B);
            } break;
        }
    } else {
        switch (size) {
            case 1: {
                // REX.W + 0F B6 /r 	MOVZX r64, r/m81 	RM
                x64_rex(buf, REX_W, dest);
                push_u16(buf, 0xB60F);
            } break;
            
            case 2: {
                // REX.W + 0F B6 /r 	MOVZX r64, r/m81 	RM
                x64_rex(buf, REX_W, dest);
                push_u16(buf, 0xB70F);
            } break;
            
            case 4: {
                // 8B /r 	MOV r32, r/m32 (this will clear the upper 32-bits)
                x64_rex(buf, 0, dest);
                push_u8(buf, 0x8B);
            } break;
            
            default: {
                //REX.W + 8B /r 	MOV r64, r/m64 	RM
                x64_rex(buf, REX_W, dest);
                push_u8(buf, 0x8B);
            } break;
        }
    }
    
    x64_modrm(buf, dest, src, disp, 0);
}

inline void
x64_lea(Buffer* buf, X64_Reg a, X64_Reg b, s64 disp) {
    // REX.W + 8D /r 	LEA r64,m 	RM
    x64_rex(buf, REX_W, a, b);
    push_u8(buf, 0x8D);
    x64_modrm(buf, a, b, disp, 0);
}

void
x64_move_slot_to_register(X64_Assembler* x64, Buffer* buf, X64_Reg dest, int register_index) {
    X64_Slot src = get_slot(x64, register_index);
    switch (src.type) {
        case X64_SLOT_RSP_DISP32: {
            if (src.is_value) {
                // TODO: if arg1 is const we can completely optimize this without any instructions
                x64_lea(buf, dest, X64_RSP, register_displacement(x64, register_index));
            } else {
                x64_move_memory_to_register(buf, dest, X64_RSP, register_displacement(x64, register_index));
            }
        } break;
        
        default: unimplemented;
    }
}

inline void
x64_inc(Buffer* buf, X64_Reg reg) {
    // REX.W + FF /0 	INC r/m64 	M
    x64_rex(buf, REX_W, 0, reg);
    push_u8(buf, 0xFF);
    x64_modrm_direct(buf, 0, reg);
}

inline void
x64_dec(Buffer* buf, X64_Reg reg) {
    // REX.W + FF /1 	DEC r/m64 	M
    x64_rex(buf, REX_W, 1, reg);
    push_u8(buf, 0xFF);
    x64_modrm_direct(buf, 1, reg);
}

inline void
x64_not(Buffer* buf, X64_Reg reg) {
    // F7 /2 	NOT r/m32 	M
    x64_rex(buf, REX_W, 2, reg);
    push_u8(buf, 0xF7);
    x64_modrm_direct(buf, 2, reg);
}

inline void
x64_neg(Buffer* buf, X64_Reg reg) {
    // F7 /3 	NEG r/m32 	M
    x64_rex(buf, REX_W, 3, reg);
    push_u8(buf, 0xF7);
    x64_modrm_direct(buf, 3, reg);
}

inline void
x64_add64(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + 03 /r 	ADD r64, r/m64 	RM
    x64_rex(buf, REX_W, a, b);
    push_u8(buf, 0x03);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_add64_immediate(Buffer* buf, X64_Reg a, s32 imm) {
    // REX.W + 81 /0 id 	ADD r/m64, imm32 	MI
    x64_rex(buf, REX_W, 0, a);
    push_u8(buf, 0x81);
    x64_modrm_direct(buf, 0, a);
    push_u32(buf, imm);
}

inline void
x64_sub64(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + 2B /r 	SUB r64, r/m64 	RM
    x64_rex(buf, REX_W, a, b);
    push_u8(buf, 0x2B);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_mul64(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + 0F AF /r 	IMUL r64, r/m64 	RM
    x64_rex(buf, REX_W, a, b);
    push_u16(buf, 0xAF0F);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_mul64_immediate(Buffer* buf, X64_Reg a, X64_Reg b, s32 imm) {
    // REX.W + 69 /r id 	IMUL r64, r/m64, imm32 	RMI 	Valid
    x64_rex(buf, REX_W, a, b);
    push_u8(buf, 0x69);
    x64_modrm_direct(buf, a, b);
    push_u32(buf, imm);
}

inline void
x64_div64(Buffer* buf, X64_Reg a, X64_Reg b, bool is_signed) {
    assert(a == X64_RAX);
    
    // REX.W + 99 	CQO 	ZO
    x64_rex(buf, REX_W);
    push_u8(buf, 0x99);
    
    // REX.W + F7 /6 	DIV r/m64 	M 	Valid
    x64_rex(buf, REX_W, 0, b);
    push_u8(buf, 0xF7);
    x64_modrm_direct(buf, is_signed ? 7 : 6, b);
}

inline void
x64_cmp64(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + 3B /r 	CMP r64, r/m64 	RM
    x64_rex(buf, REX_W, a, b);
    push_u8(buf, 0x3B);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_move_float_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src, int size) {
    // F3 0F 11 /r MOVSS xmm2/m32, xmm1
    // F2 0F 11 /r MOVSD xmm1/m64, xmm2
    push_u24(buf, (size == 8) ? 0x110FF2 : 0x110FF3);
    x64_modrm(buf, src, dest, disp, 0);
}

inline void
x64_move_memory_to_float_register(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp, int size) {
    // F3 0F 10 /r MOVSS xmm1, m32
    // F2 0F 10 /r MOVSD xmm1, m64
    push_u24(buf, (size == 8) ? 0x100FF2 : 0x100FF3);
    x64_modrm(buf, dest, src, disp, 0);
}

inline void
x64_move_const_to_float_register(X64_Assembler* x64, Buffer* buf, X64_Reg dest, Exported_Data data, int size) {
    // F3 0F 10 /r MOVSS xmm1, m32
    // F2 0F 10 /r MOVSD xmm1, m64
    push_u24(buf, (size == 8) ? 0x100FF2 : 0x100FF3);
    x64_modrm_exported_data(x64, buf, dest, data);
}

inline void
x64_addss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 58 /r ADDSS xmm1, xmm2/m32
    // F2 0F 58 /r ADDSD xmm1, xmm2/m64
    push_u24(buf, (size == 8) ? 0x580FF2 : 0x580FF3);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_subss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 5C /r SUBSS xmm1, xmm2/m32
    // F2 0F 5C /r SUBSD xmm1, xmm2/m64
    push_u24(buf, (size == 8) ? 0x5C0FF2 : 0x5C0FF3);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_mulss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 59 /r MULSS xmm1,xmm2/m32 	A
    // F2 0F 59 /r MULSD xmm1,xmm2/m64 	A
    push_u24(buf, (size == 8) ? 0x590FF2 : 0x590FF3);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_divss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 5E /r DIVSS xmm1, xmm2/m32 	A
    // F2 0F 5E /r DIVSD xmm1, xmm2/m64 	A
    push_u24(buf, (size == 8) ? 0x5E0FF2 : 0x5E0FF3);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_xorps(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // NP 0F 57 /r XORPS xmm1, xmm2/m128 	A
    // 66 0F 57 /r XORPD xmm1, xmm2/m128 	A
    if (size == 8) {
        push_u24(buf, 0x570F66);
    } else {
        push_u16(buf, 0x570F);
    }
    x64_modrm_direct(buf, a, b);
}

inline void
x64_convert_float_to_int(Buffer* buf, X64_Reg dest, X64_Reg src, int size) {
    // F3 REX.W 0F 2C /r CVTTSS2SI r64, xmm1/m32
    // F2 REX.W 0F 2C /r CVTTSD2SI r64, xmm1/m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    x64_rex(buf, REX_W);
    push_u16(buf, 0x2C0F);
    x64_modrm_direct(buf, dest, src);
}

inline void
x64_convert_int_to_float(Buffer* buf, X64_Reg dest, X64_Reg src, int size) {
    // F3 REX.W 0F 2A /r CVTSI2SS xmm1, r/m64
    // F2 REX.W 0F 2A /r CVTSI2SD xmm1, r/m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    x64_rex(buf, REX_W);
    push_u16(buf, 0x2A0F);
    x64_modrm_direct(buf, dest, src);
}

inline void
x64_rep_movsb(Buffer* buf, X64_Reg dest, X64_Reg src, X64_Reg count) {
    assert(dest == X64_RDI);
    assert(src == X64_RSI);
    assert(count == X64_RCX);
    
    // F3 A4 	REP MOVS m8, m8 	ZO
    // F3 REX.W A4 	REP MOVS m8, m8 	ZO
    push_u24(buf, 0xA448F3);
}


inline void
x64_rep_stosb(Buffer* buf, X64_Reg dest, X64_Reg byte, X64_Reg count) {
    assert(dest == X64_RDI);
    assert(byte == X64_RAX);
    assert(count == X64_RCX);
    
    // F3 REX.W AA 	REP STOS m8 	ZO 	Valid
    push_u24(buf, 0xAA48F3);
}

#if 0
void
x64_mov(Buffer* buf,
        Ic_Type t1, s64 r1, s64 d1,
        Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_DISP) {
                if (d2 == 0) {
                    x64_zero(buf, (X64_Reg) r1);
                } else if (t1 & IC_T64 || d2 < S32_MIN || d2 > S32_MAX) {
                    // REX.W + B8+ rd io 	MOV r64, imm64 	OI
                    x64_rex(buf, REX_W | (r1&8)>>3);
                    push_u8(buf, 0xB8+((u8)r1&7));
                    push_u64(buf, (u64) d2);
                } else {
                    // B8+ rd id 	MOV r32, imm32 	OI
                    if (r1&8) {
                        x64_rex(buf, REX_B);
                    }
                    push_u8(buf, 0xB8+((u8)r1&7));
                    push_u32(buf, (u32) d2);
                }
                
            } else if (t2 & (IC_REG | IC_STK | IC_RIP_DISP32)) {
                if (t1 & IC_T64 || r1&8 || r2&8) {
                    x64_rex(buf, (t1&IC_T64) | ((u8) r1&8)>>1 | ((u8)r2&8)>>3);
                }
                // 8B /r 	MOV r32,r/m32 	RM
                push_u8(buf, t1 & IC_T8 ? 0x8A : 0x8B);
                x64_modrm(buf, t2,  (X64_Reg) r1, (X64_Reg) r2, d2, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        case IC_STK:
        case IC_RIP_DISP32: {
            if (t2 & IC_DISP) {
                if (d2 < S32_MIN || d2 > S32_MAX) {
                    x64_move_rax_u64(buf, d2);
                    x64_mov(buf, t1, r1, d1, IC_T64 + IC_REG, X64_RAX, 0, rip);
                } else {
                    // C7 /0 id 	MOV r/m32, imm32 	MI
                    if (t1 & IC_T16) {
                        push_u8(buf, X64_OP_SIZE_PREFIX);
                    }
                    
                    if (t1 & IC_T64) {
                        x64_rex(buf, REX_W);
                    }
                    
                    s64 disp_size = t1 & IC_T8 ? 1 : 4;
                    push_u8(buf, t1 & IC_T8 ? 0xC6 : 0xC7);
                    if (t1 & IC_RIP_DISP32)  {
                        x64_modrm(buf, t1, d1, 0, r1, rip + disp_size);
                    } else {
                        x64_modrm(buf, t1, d1, 0, r1, rip + disp_size);
                    }
                    if (t1 & IC_T8) {
                        push_u8(buf, (u8) d2);
                    } else if (t1 & IC_T16) {
                        push_u16(buf, (u16) d2);
                    } else {
                        push_u32(buf, (u32) d2);
                    }
                }
            } else if (t2 & IC_REG) {
                if (t1 & IC_T16) {
                    push_u8(buf, X64_OP_SIZE_PREFIX);
                }
                
                // 89 /r 	MOV r/m32,r32 	MR
                if ((t2 & IC_T64) || (r2 & 8)) {
                    x64_rex(buf, t2&IC_T64|((u8) r2&8)>>1);
                }
                push_u8(buf, t1 & IC_T8 ? 0x88 : 0x89);
                x64_modrm(buf, t1, d1, r2&7, r1, rip);
            } else if (t2 & (IC_STK_RIP)) {
                // NOTE(Alexander): x64 doesn't allow MOV STK, STK, move to tmp reg
                // TODO(Alexander): reg hardcoded RAX
                s64 tmpr = X64_RAX;
                if (r2 == tmpr || r1 == tmpr) {
                    tmpr = X64_RCX;
                }
                x64_mov(buf, IC_REG + (t2 & IC_RT_MASK), tmpr, 0, t2, r2, d2, rip);
                x64_mov(buf, t1, r1, d1, IC_REG + (t1 & IC_RT_MASK), tmpr, 0, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

void
x64_movzx(Buffer* buf,
          Ic_Type t1, s64 r1, s64 d1,
          Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    if (t2 & IC_T8) {
        if (r1 & 8) {
            x64_rex(buf, REX_R);
        }
        push_u16(buf, 0xB60F);
        x64_modrm(buf, t2, d2, r1, r2, rip);
        
    } else if (t2 & IC_T16) {
        if (r1 & 8) {
            x64_rex(buf, REX_R);
        }
        push_u16(buf, 0xB70F);
        x64_modrm(buf, t2, d2, r1, r2, rip);
        
    } else {
        assert(t1 & IC_T64 && t2 & IC_T32);
        t1 = (t1 & IC_TF_MASK) | (t2 & IC_RT_MASK);
        
        x64_mov(buf,
                t1, r1, d1,
                t2, r2, d2, rip);
    }
}

void
x64_movsx(Buffer* buf,
          Ic_Type t1, s64 r1, s64 d1,
          Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    if (t1 & IC_T16) {
        push_u8(buf, X64_OP_SIZE_PREFIX);
    }
    
    if (t1 & IC_T64) {
        x64_rex(buf, REX_W);
    }
    
    switch (t2 & IC_RT_SIZE_MASK) {
        case IC_T8: {
            assert(t1 & (IC_T64 | IC_T32 | IC_T16));
            // 0F BE /r 	MOVSX r16, r/m8 	RM
            // 0F BE /r 	MOVSX r32, r/m8 	RM
            // REX.W + 0F BE /r 	MOVSX r64, r/m8 	RM
            push_u16(buf, 0xBE0F);
        } break;
        
        case IC_T16: {
            assert(t1 & (IC_T64 | IC_T32));
            // 0F BF /r 	MOVSX r32, r/m16 	RM
            push_u16(buf, 0xBF0F);
        } break;
        
        case IC_T32: {
            assert(t1 & IC_T64);
            // REX.W + 63 /r 	MOVSXD r64, r/m32 	RM
            push_u8(buf, 0x63);
        } break;
    }
    x64_modrm(buf, t2, d2, r1, r2, rip);
}

void
x64_fmov(Buffer* buf,
         Ic_Type t1, s64 r1, s64 d1,
         Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    assert(t1 & IC_FLOAT);
    
    if ((t1 & IC_STK_RIP) && (t2 & IC_STK_RIP)) {
        Ic_Type tmpt = IC_REG + (t2 & IC_RT_MASK);
        s64 tmpr = X64_XMM0;
        x64_fmov(buf, tmpt, tmpr, 0, t2, r2, d2, rip);
        t2 = tmpt;
        r2 = tmpr;
        d2 = 0;
    }
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            // F3 0F 10 /r MOVSS xmm1, m32 or
            // F2 0F 10 /r MOVSD xmm1, m64
            push_u8(buf, (t1 & IC_T32) ? 0xF3 : 0xF2);
            push_u8(buf, 0x0F);
            push_u8(buf, 0x10);
            x64_modrm(buf, t2, d2, r1, r2, rip);
        } break;
        
        case IC_STK:
        case IC_RIP_DISP32: {
            assert(t2 & IC_REG);
            
            // F3 0F 11 /r MOVSS xmm2/m32, xmm1 or
            // F2 0F 11 /r MOVSD xmm1/m64, xmm2
            push_u8(buf, (t1 & IC_T32) ? 0xF3 : 0xF2);
            push_u8(buf, 0x0F);
            push_u8(buf, 0x11);
            x64_modrm(buf, t1, d1, r2, r1, rip);
        } break;
        
        default: unimplemented;
    }
    
}
#endif

#if 0
inline void
x64_mul(X64_Assembler* x64, Buffer* buf, Bytecode_Binary* binary, s64 rip) {
    Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, binary->first, binary->type);
    Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, binary->second, binary->type);
    
    assert(first.type & IC_REG); // TODO(Alexander): we can allow stack/ memory later
    
    if (second.type & IC_DISP) {
        // 69 /r id 	IMUL r32, r/m32, imm32 	RMI
        if (first.type & IC_T64) {
            x64_rex(buf, REX_W);
        }
        
        push_u8(buf, 0x69);
        x64_modrm(buf, first.type, first.disp, first.reg, first.reg, rip + 4);
        assert(second.disp >= S32_MIN && second.disp <= S32_MAX && "cannot fit in imm32");
        push_u32(buf, (u32) second.disp);
    } else {
        Ic_Type t1 = first.type;
        s64 r1 = first.reg;
        s64 d1 = first.disp;
        
        Ic_Type t2 = second.type;
        s64 r2 = second.reg;
        s64 d2 = second.disp;
        
        if (t1 & IC_STK_RIP) {
            // NOTE(Alexander): swap the order of mul
            if (t2 & IC_STK || r2 != first.reg) {
                x64_mov(buf, first.type, first.reg, first.disp, t2, r2, d2, rip);
            }
            t2 = t1;
            r2 = r1;
            d2 = d1;
            
            t1 = first.type;
            r1 = first.reg;
            d1 = first.disp;
        }
        
        // 0F AF /r 	IMUL r32, r/m32
        push_u16(buf, 0xAF0F);
        x64_modrm(buf, t2, d2, r1, r2, rip);
    }
}

inline void
x64_div(X64_Assembler* x64, Buffer* buf, Bytecode_Binary* binary, bool remainder, s64 rip) {
    Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, binary->first, binary->type);
    Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, binary->second, binary->type);
    x64_spill_register(x64, buf, X64_RCX);
    
    // Make sure first is in RAX
    if (!(first.type & IC_REG && first.reg == X64_RAX)) {
        if (binary->first.kind == BytecodeOperand_register) {
            x64_alloc_register(x64, buf, binary->first.register_index, X64_RAX, first.raw_type);
        }  else {
            x64_spill_register(x64, buf, X64_RAX);
        }
        x64_mov(buf, first.raw_type + IC_REG, X64_RAX, 0, first.type, first.reg, first.disp, rip);
    }
    x64_spill_register(x64, buf, X64_RDX); // RAX expands into RDX so make sure it's free
    
    // Make sure second is in RCX
    if (!(second.type & IC_REG && second.reg == X64_RCX)) {
        if (binary->second.kind == BytecodeOperand_register) {
            x64_alloc_register(x64, buf, binary->second.register_index, X64_RCX, second.raw_type);
        } else {
            x64_spill_register(x64, buf, X64_RCX);
        }
        x64_mov(buf, second.raw_type + IC_REG, X64_RCX, 0, second.type, second.reg, second.disp, rip);
    }
    
    // CDQ
    if (first.type & IC_T64) {
        x64_rex(buf, REX_W);
    }
    if (first.type & IC_SINT) {
        push_u8(buf, 0x99);
    } else {
        x64_zero(buf, X64_RDX);
    }
    
    // F7 /6  	DIV r/m32 	M
    // F7 /7 	IDIV r/m32 	M
    if (first.type & IC_T64) {
        x64_rex(buf, REX_W);
    }
    push_u8(buf, 0xF7);
    push_u8(buf, ((first.type & IC_UINT) ? 0xF0 : 0xF8) | (u8) X64_RCX);
    
    if (remainder) {
        x64_mov(buf, IC_REG, X64_RAX, 0, IC_REG, X64_RDX, 0, rip);
    }
}

inline void
x64_float_binary(X64_Assembler* x64, Buffer* buf, Bytecode_Binary* binary, u8 opcode, s64 rip, s64 prefix_opcode) {
    Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, binary->first, binary->type);
    Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, binary->second, binary->type);
    
    Ic_Type t1 = first.type, t2 = second.type;
    s64 r1 = first.reg, r2 = second.reg;
    s64 d1 = first.disp, d2 = second.disp;
    assert(t1 & IC_FLOAT);
    // NOTE(Alexander): assumes destination to be a register
    //assert(first.type & IC_REG);
    
    // Make sure first argument is a register
    if (t1 & IC_DISP_STK_RIP) {
        X64_Reg tmpr = X64_XMM5;
        if (binary->first.kind == BytecodeOperand_register) {
            x64_alloc_register(x64, buf, binary->first.register_index, tmpr, t1 & IC_RT_MASK);
        } else {
            x64_spill_register(x64, buf, tmpr);
        }
        x64_fmov(buf, IC_REG | (t1 & IC_RT_MASK), tmpr, 0, t1, r1, d1, rip);
        t1 = IC_REG | (t1 & IC_RT_MASK);
        r1 = X64_XMM5;
        d1 = 0;
    }
    
    // F3 0F 5E /r DIVSS xmm1, xmm2/m32
    // F2 0F 5E /r DIVSD xmm1, xmm2/m64
    if (prefix_opcode == -1) {
        prefix_opcode = (t1 & IC_T32) ? 0xF3 : 0xF2;
    }
    if (prefix_opcode >= 0) {
        push_u8(buf, (u8) prefix_opcode);
    }
    push_u8(buf, 0x0F);
    push_u8(buf, opcode);
    x64_modrm(buf, t2, d2, r1, r2, rip);
    
    //if (!(first.type == t1 && first.reg == r1)) {
    // TODO(Alexander): check displacement too, if both are STK
    //x64_fmov(buf, first.type, first.reg, first.disp, t1, r1, d1, rip);
    //}
}

// TODO(Alexander): we can simplify these conversion functions into a single function
inline void
x64_convert_float_type(Buffer* buf, 
                       Ic_Type t1, s64 r1, s64 d1, 
                       Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    assert(t1 & IC_REG);
    assert(t2 & (IC_REG | IC_STK | IC_RIP_DISP32));
    
    // F2 0F 5A /r CVTSD2SS xmm1, xmm2/m64
    // F3 0F 5A /r CVTSS2SD xmm1, xmm2/m32
    push_u8(buf, t2 & IC_T64 ? 0xF2 : 0xF3);
    push_u16(buf, 0x5A0F);
    x64_modrm(buf, t2, d2, r1, r2, rip);
}

inline void
x64_convert_int_to_float_type(Buffer* buf, 
                              Ic_Type t1, s64 r1, s64 d1, 
                              Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    assert(t1 & IC_REG);
    assert(t2 & (IC_REG | IC_STK | IC_RIP_DISP32));
    
    // F3 0F 2A /r CVTSI2SS xmm1, r/m32
    // F2 0F 2A /r CVTSI2SD xmm1, r32/m32
    push_u8(buf, t1 & IC_T64 ? 0xF2 : 0xF3);
    if (t2 & IC_T64) {
        x64_rex(buf, REX_W);
    }
    push_u16(buf, 0x2A0F);
    x64_modrm(buf, t2, d2, r1, r2, rip);
}

inline void
x64_lea(Buffer* buf, X64_Reg r1, X64_Reg r2, s64 d2, s64 rip) {
    // REX.W + 8D /r 	LEA r64,m 	RM
    x64_rex(buf, REX_W, r1);
    push_u8(buf, 0x8D);
    
    if (r2 == X64_RIP) {
        push_u8(buf, (u8) (r1&7)<<3 | (u8) X64_RBP);
        push_u32(buf, (u32) (d2 - ((s64) (buf->data + buf->curr_used) + 4)));
    } else {
        x64_modrm(buf, IC_STK, d2, r1&7, r2, rip);
    }
}

inline void
x64_string_op(X64_Assembler* x64, Buffer* buf,
              Ic_Type destt, s64 destr, s64 destd, 
              Ic_Type srct, s64 srcr, s64 srcd, 
              s64 count, u16 opcode, s64 rip, s64 src_int_reg) {
    x64_spill_register(x64, buf, X64_RCX);
    
    // Move RCX bytes from [RSI] to [RDI].
    x64_mov(buf, IC_S64 + IC_REG, X64_RCX, 0, IC_S64 + IC_DISP, 0, count, rip);
    
    // Move destination (RDI)
    x64_spill_register(x64, buf, X64_RDI);
    //if (destt & IC_REG) {
    //x64_mov(buf, IC_S64 + IC_REG, X64_RDI, 0, IC_S64 + IC_STK, X64_RSP, destd, rip);
    //} else {
    x64_lea(buf, X64_RDI, destr, destd, rip);
    //}
    
    // Move source (RSI)
    //if (srct & (IC_UINT | IC_SINT)) {
    //x64_spill_register(x64, buf, (X64_Reg) src_int_reg);
    //x64_mov(buf, IC_S64 + IC_REG, src_int_reg, 0, srct, srcr, srcd, rip);
    //} else if (srct & IC_T64) {
    //if (srct & IC_DISP) {
    //x64_lea(buf, X64_RSI, X64_RIP, disp, rip);
    x64_spill_register(x64, buf, X64_RSI);
    if (srct & IC_REG) {
        x64_mov(buf, IC_S64 + IC_REG, X64_RSI, 0, srct, srcr, srcd, rip);
    } else {
        x64_lea(buf, X64_RSI, srcr, srcd, rip);
    }
    //} else {
    //unimplemented;
    //}
    
    // e.g. F3 A4 	REP MOVS m8, m8 	ZO
    push_u16(buf, opcode);
}


inline void
x64_shr(X64_Assembler* x64, Buffer* buf, Bytecode_Binary* binary, u8 reg_field, s64 rip) {
    Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, binary->first, binary->type);
    Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, binary->second, binary->type);
    
    if (second.type & IC_DISP) {
        // C1 /7 ib 	SAR r/m32, imm8 	MI (signed)
        // C1 /5 ib 	SHR r/m32, imm8 	MI (unsigned)
        push_u8(buf, 0xC1);
        x64_modrm(buf, first.type, first.disp, reg_field, first.reg, rip);
        push_u8(buf, (u8) second.disp);
    } else {
        if (!(second.type & IC_REG && second.reg == X64_RCX)) {
            x64_mov(buf, 
                    first.type, first.reg, first.disp, 
                    second.type, second.reg, second.disp, rip);
        }
        
        // D3 /7 	SAR r/m32, CL 	MC (signed)
        // D3 /5 	SHR r/m16, CL     MC (unsigned)
        push_u8(buf, 0xD3);
        x64_modrm(buf, first.type, first.disp, reg_field, first.reg, rip);
    }
}
#endif
