
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
    if (src&8 || dest&8) x64_rex(buf, 0, src, dest);
    push_u8(buf, 0x88);
    x64_modrm(buf, src, dest, disp);
}

inline void
x64_move16_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src) {
    // 89 /r 	MOV r/m16, r16 	MR 	
    push_u8(buf, X64_OP_SIZE_PREFIX);
    if (src&8 || dest&8) x64_rex(buf, 0, src, dest);
    push_u8(buf, 0x89);
    x64_modrm(buf, src, dest, disp);
}

inline void
x64_move32_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src) {
    // 89 /r 	MOV r/m32, r32 	MR
    if (src&8 || dest&8) x64_rex(buf, 0, src, dest);
    push_u8(buf, 0x89);
    x64_modrm(buf, src, dest, disp);
}

inline void
x64_move_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src) {
    // 89 /r 	MOV r/m64,r64 	MR
    x64_rex(buf, REX_W, src, dest);
    push_u8(buf, 0x89);
    x64_modrm(buf, src, dest, disp);
}

inline void
x64_move_memory_to_register(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp) {
    // REX.W + 8B /r 	MOV r64, r/m64 	RM
    x64_rex(buf, REX_W, dest, src);
    push_u8(buf, 0x8B);
    x64_modrm(buf, dest, src, disp);
}

inline void
x64_move_register_to_register(Buffer* buf, X64_Reg dest, X64_Reg src) {
    if (dest == src) return;
    
    // 89 /r 	MOV r/m64,r64 	MR
    x64_rex(buf, REX_W, src, dest);
    push_u8(buf, 0x89);
    x64_modrm_direct(buf, src, dest);
}

inline s32*
x64_move_memory_to_register_disp(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp) {
    // REX.W + 8B /r 	MOV r64, r/m64 	RM
    x64_rex(buf, REX_W, dest, src);
    push_u8(buf, 0x8B);
    push_u8(buf, MODRM_INDIRECT_DISP32 | ((dest & 7) << 3) | (src & 7));
    push_u8(buf, 0x24); // SIB for RSP
    s32* result = (s32*) (buf->data + buf->curr_used);
    push_u32(buf, (u32) disp);
    return result;
}

inline void
x64_lea(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp) {
    // REX.W + 8D /r 	LEA r64,m 	RM
    x64_rex(buf, REX_W, dest, src);
    push_u8(buf, 0x8D);
    x64_modrm(buf, dest, src, disp);
}

inline void
x64_lea_sib(Buffer* buf, X64_Reg dest, u8 scale, X64_Reg index, X64_Reg base, s64 disp) {
    // REX.W + 8D /r 	LEA r64,m 	RM
    // LEA index [base_reg + (index * scale) + base_disp]
    x64_rex(buf, REX_W, dest, base, index);
    push_u8(buf, 0x8D);
    x64_modrm_sib(buf, dest, scale, index, base, disp);
    
}

void
x64_move_extend_opcode(Buffer* buf, X64_Reg dest, X64_Reg src, int size, bool is_signed) {
    if (is_signed) {
        switch (size) {
            case 1: {
                // REX.W + 0F BE /r 	MOVSX r64, r/m8 	RM
                x64_rex(buf, REX_W, dest, src);
                push_u16(buf, 0xBE0F);
            } break;
            
            case 2: {
                // 0F BF /r 	MOVSX r32, r/m16 	RM
                push_u8(buf, X64_OP_SIZE_PREFIX);
                x64_rex(buf, REX_W, dest, src);
                push_u16(buf, 0xBF0F);
            } break;
            
            case 4: {
                // REX.W + 63 /r 	MOVSXD r64, r/m32 	RM
                x64_rex(buf, REX_W, dest, src);
                push_u8(buf, 0x63);
            } break;
            
            default: {
                x64_rex(buf, REX_W, dest, src);
                push_u8(buf, 0x8B);
            } break;
        }
    } else {
        switch (size) {
            case 1: {
                // REX.W + 0F B6 /r 	MOVZX r64, r/m81 	RM
                x64_rex(buf, REX_W, dest, src);
                push_u16(buf, 0xB60F);
            } break;
            
            case 2: {
                // REX.W + 0F B6 /r 	MOVZX r64, r/m81 	RM
                x64_rex(buf, REX_W, dest, src);
                push_u16(buf, 0xB70F);
            } break;
            
            case 4: {
                // 8B /r 	MOV r32, r/m32 (this will clear the upper 32-bits)
                x64_rex(buf, 0, dest, src);
                push_u8(buf, 0x8B);
            } break;
            
            default: {
                //REX.W + 8B /r 	MOV r64, r/m64 	RM
                x64_rex(buf, REX_W, dest, src);
                push_u8(buf, 0x8B);
            } break;
        }
    }
}

void
x64_move_extend_memory_to_register(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp, 
                                   int size, bool is_signed) {
    x64_move_extend_opcode(buf, dest, src, size, is_signed);
    x64_modrm(buf, dest, src, disp);
}

void
x64_move_extend_register_to_register(Buffer* buf, X64_Reg dest, X64_Reg src,
                                     int size, bool is_signed) {
    x64_move_extend_opcode(buf, dest, src, size, is_signed);
    x64_modrm_direct(buf, dest, src);
}

X64_Reg
x64_move_slot_to_register(X64_Assembler* x64, Buffer* buf, X64_Reg dest, int src_index) {
    X64_Slot src = get_slot(x64, src_index);
    switch (src.kind) {
        case X64_SLOT_RSP_DISP32: {
            x64_lea(buf, dest, X64_RSP, src.disp);
        } break;
        
        case X64_SLOT_SPILL: {
            x64_move_extend_memory_to_register(buf, dest, X64_RSP, src.disp, src.type.size,
                                               src.type.flags & BC_FLAG_SIGNED);
        } break;
        
        case X64_SLOT_REG: {
            //if (src.reg != dest) {
            x64_move_extend_register_to_register(buf, dest, src.reg, src.type.size,
                                                 src.type.flags & BC_FLAG_SIGNED);
            //}
        } break;
        
        case X64_SLOT_IMM32: {
            x64_move_immediate_to_register(buf, dest, src.imm32);
        } break;
        
        default: {
            pln("x64_move_slot_to_register - src_index = r%", f_int(src_index));
            unimplemented;
        } break;
    }
    
    return dest;
}

void
x64_move_register_to_slot(X64_Assembler* x64, Buffer* buf, int dest_index, X64_Reg src) {
    X64_Slot dest = get_slot(x64, dest_index);
    switch (dest.kind) {
        case X64_SLOT_SPILL:
        case X64_SLOT_RSP_DISP32: {
            x64_move_register_to_memory(buf, X64_RSP, dest.disp, src);
        } break;
        
        case X64_SLOT_REG: {
            x64_move_register_to_register(buf, dest.reg, src);
        } break;
        
        default: {
            pln("x64_move_register_to_slot - dest_index = r%", f_int(dest_index));
            unimplemented;
        } break;
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
x64_or(Buffer* buf, X64_Reg reg) {
    // REX.W + F7 /2 	OR r/m64 	M
    x64_rex(buf, REX_W, 2, reg);
    push_u8(buf, 0xF7);
    x64_modrm_direct(buf, 2, reg);
}

inline void
x64_not(Buffer* buf, X64_Reg reg) {
    // REX.W + F7 /2 	NOT r/m64 	M
    x64_rex(buf, REX_W, 2, reg);
    push_u8(buf, 0xF7);
    x64_modrm_direct(buf, 2, reg);
}

inline void
x64_neg(Buffer* buf, X64_Reg reg) {
    // REX.W + F7 /3 	NEG r/m64 	M
    x64_rex(buf, REX_W, 3, reg);
    push_u8(buf, 0xF7);
    x64_modrm_direct(buf, 3, reg);
}

inline void
x64_shl(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + D3 /4 	SHL r/m64, CL 	MC
    assert(b == X64_RCX && "shl only supports CL on b");
    x64_rex(buf, REX_W, 4, a);
    push_u8(buf, 0xD3);
    x64_modrm_direct(buf, 4, a);
}

inline void
x64_shr(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + D3 /5 	SHR r/m64, CL 	MC
    assert(b == X64_RCX && "shr only supports CL on b");
    x64_rex(buf, REX_W, 5, a);
    push_u8(buf, 0xD3);
    x64_modrm_direct(buf, 5, a);
}

inline void
x64_sar(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + D3 /7 	SAR r/m64, CL 	MC
    assert(b == X64_RCX && "sar only supports CL on b");
    x64_rex(buf, REX_W, 7, a);
    push_u8(buf, 0xD3);
    x64_modrm_direct(buf, 7, a);
}

inline void
x64_and64(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + 23 /r 	AND r64, r/m64 	RM
    x64_rex(buf, REX_W, a, b);
    push_u8(buf, 0x23);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_or64(Buffer* buf, X64_Reg a, X64_Reg b) {
    // REX.W + 0B /r 	OR r64, r/m64 	RM
    x64_rex(buf, REX_W, a, b);
    push_u8(buf, 0x0B);
    x64_modrm_direct(buf, a, b);
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
x64_sub64_immediate(Buffer* buf, X64_Reg a, s32 imm) {
    // REX.W + 81 /5 id 	SUB r/m64, imm32 	MI
    x64_rex(buf, REX_W, 5, a);
    push_u8(buf, 0x81);
    x64_modrm_direct(buf, 5, a);
    push_u32(buf, imm);
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
    
    if (is_signed) {
        // REX.W + 99 	CQO 	ZO
        x64_rex(buf, REX_W);
        push_u8(buf, 0x99);
    } else {
        x64_zero(buf, X64_RDX);
    }
    
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


global const u16 x64_setcc_opcodes[] = {
    // BC_EQ, BC_GT_S,  BC_GT_U,  BC_GE_S,  BC_GE_U,
    0x940F, 0x9F0F, 0x970F, 0x9D0F, 0x930F,
    // BC_LT_U, BC_LT_S, BC_LE_U, BC_LE_S,  BC_NEQ,
    0x920F, 0x9C0F, 0x960F, 0x9E0F, 0x950F
};

inline void
x64_setcc(Buffer* buf, Bytecode_Operator opcode, X64_Reg dest) {
    // setcc 
    push_u16(buf, x64_setcc_opcodes[opcode - BC_EQ]); 
    x64_modrm_direct(buf, 0, dest);
}


inline void
x64_move_float_register_to_memory(Buffer* buf, X64_Reg dest, s64 disp, X64_Reg src, int size) {
    // F3 0F 11 /r MOVSS xmm2/m32, xmm1
    // F2 0F 11 /r MOVSD xmm1/m64, xmm2
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (src&8 || dest&8) x64_rex(buf, 0, src, dest);
    push_u16(buf, 0x110F);
    x64_modrm(buf, src, dest, disp);
}

inline void
x64_move_float_register_to_register(Buffer* buf, X64_Reg dest, X64_Reg src, int size) {
    if (dest == src) return;
    
    // F3 0F 11 /r MOVSS xmm2/m32, xmm1
    // F2 0F 11 /r MOVSD xmm1/m64, xmm2
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (src&8 || dest&8) x64_rex(buf, 0, src, dest);
    push_u16(buf, 0x110F);
    x64_modrm_direct(buf, src, dest);
}

inline void
x64_move_memory_to_float_register(Buffer* buf, X64_Reg dest, X64_Reg src, s64 disp, int size) {
    // F3 0F 10 /r MOVSS xmm1, m32
    // F2 0F 10 /r MOVSD xmm1, m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (src&8 || dest&8) x64_rex(buf, 0, dest, src);
    push_u16(buf, 0x100F);
    x64_modrm(buf, dest, src, disp);
}

inline void
x64_move_const_to_float_register(X64_Assembler* x64, Buffer* buf, X64_Reg dest, Exported_Data data, int size) {
    // F3 0F 10 /r MOVSS xmm1, m32
    // F2 0F 10 /r MOVSD xmm1, m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (dest&8) x64_rex(buf, 0, dest);
    push_u16(buf, 0x100F);
    x64_modrm_exported_data(x64, buf, dest, data);
}

inline void
x64_move_slot_to_float_register(X64_Assembler* x64, Buffer* buf, X64_Reg dest, int src_index) {
    X64_Slot src = get_slot(x64, src_index);
    switch (src.kind) {
        case X64_SLOT_SPILL:
        case X64_SLOT_RSP_DISP32: {
            x64_move_memory_to_float_register(buf, dest, X64_RSP, src.disp, src.type.size);
        } break;
        
        case X64_SLOT_REG: {
            x64_move_float_register_to_register(buf, dest, src.reg, src.type.size);
        } break;
        
        default: verify_not_reached();
    }
}

inline void
x64_move_float_register_to_slot(X64_Assembler* x64, Buffer* buf, int dest_index, X64_Reg src) {
    X64_Slot dest = get_slot(x64, dest_index);
    switch (dest.kind) {
        case X64_SLOT_SPILL:
        case X64_SLOT_RSP_DISP32: {
            x64_move_float_register_to_memory(buf, X64_RSP, dest.disp, src, dest.type.size);
        } break;
        
        case X64_SLOT_REG: {
            x64_move_float_register_to_register(buf, dest.reg, src, dest.type.size);
        } break;
        
        default: verify_not_reached();
    }
}

inline void
x64_addss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 58 /r ADDSS xmm1, xmm2/m32
    // F2 0F 58 /r ADDSD xmm1, xmm2/m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (a&8 || b&8) x64_rex(buf, 0, b, a);
    push_u16(buf,0x580F);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_subss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 5C /r SUBSS xmm1, xmm2/m32
    // F2 0F 5C /r SUBSD xmm1, xmm2/m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (a&8 || b&8) x64_rex(buf, 0, b, a);
    push_u16(buf, 0x5C0F);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_mulss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 59 /r MULSS xmm1,xmm2/m32 	A
    // F2 0F 59 /r MULSD xmm1,xmm2/m64 	A
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (a&8 || b&8) x64_rex(buf, 0, b, a);
    push_u16(buf, 0x590F);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_divss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // F3 0F 5E /r DIVSS xmm1, xmm2/m32 	A
    // F2 0F 5E /r DIVSD xmm1, xmm2/m64 	A
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    if (a&8 || b&8) x64_rex(buf, 0, b, a);
    push_u16(buf, 0x5E0F);
    x64_modrm_direct(buf, a, b);
}

inline void
x64_ucomiss(Buffer* buf, X64_Reg a, X64_Reg b, int size) {
    // NP 0F 2E /r UCOMISS xmm1, xmm2/m32 	A
    // 66 0F 2E /r UCOMISD xmm1, xmm2/m64 	A
    if (size == 8) {
        push_u24(buf, 0x2E0F66);
    } else {
        push_u16(buf, 0x2E0F);
    }
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
    assert(src & 0x10);
    // F3 REX.W 0F 2C /r CVTTSS2SI r64, xmm1/m32
    // F2 REX.W 0F 2C /r CVTTSD2SI r64, xmm1/m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    x64_rex(buf, REX_W, dest, src);
    push_u16(buf, 0x2C0F);
    x64_modrm_direct(buf, dest, src);
}

inline void
x64_convert_int_to_float(Buffer* buf, X64_Reg dest, X64_Reg src, int size) {
    assert(dest & 0x10);
    // F3 REX.W 0F 2A /r CVTSI2SS xmm1, r/m64
    // F2 REX.W 0F 2A /r CVTSI2SD xmm1, r/m64
    push_u8(buf, (size == 8) ? 0xF2 : 0xF3);
    x64_rex(buf, REX_W, dest, src);
    push_u16(buf, 0x2A0F);
    x64_modrm_direct(buf, dest, src);
}

inline void
x64_convert_float_to_float(Buffer* buf, X64_Reg dest, X64_Reg src, int src_size) {
    assert(src & 0x10);
    assert(dest & 0x10);
    // F3 0F 5A /r CVTSS2SD xmm1, xmm2/m32
    // F2 0F 5A /r CVTSD2SS xmm1, xmm2/m64
    push_u8(buf, (src_size == 8) ? 0xF2 : 0xF3);
    if (src&8 || dest&8) x64_rex(buf, 0, dest, src);
    push_u16(buf, 0x5A0F);
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
