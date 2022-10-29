
Intermediate_Code*
create_ic(Ic_Opcode opcode = IC_NOOP) {
    // TODO(Alexander): temporary bump allocation for now
    Intermediate_Code* result = (Intermediate_Code*) calloc(1, sizeof(Intermediate_Code));
    result->opcode = opcode;
    result->last = result;
    return result;
}

Ic_Arg
convert_to_ic_arg(Ast* node) {
    Ic_Arg result = {};
    
    switch (node->kind) {
        case Ast_Value: {
            if (is_integer(node->Value.value)) {
                result.type = IC_IMM;
                result.disp = value_to_s64(node->Value.value);
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Ident: {
            result.type = IC_STACK;
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

Ic_Arg
ic_reg() {
    // TODO(Alexander): how to handle register allocation
    Ic_Arg result = {};
    result.reg = 0;
    result.type = IC_REG;
    return result;
}



Intermediate_Code*
convert_to_intermediate_code(Ast* node) {
    Intermediate_Code* result = 0;
    
    switch (node->kind) {
        case Ast_Binary_Expr: {
            result = create_ic();
            result->dest = ic_reg();
            result->src0 = convert_to_ic_arg(node->Binary_Expr.first);
            result->src1 = convert_to_ic_arg(node->Binary_Expr.second);
            
            switch (node->Binary_Expr.op) {
                case BinaryOp_Add: result->opcode = IC_ADD; break;
                case BinaryOp_Divide: result->opcode = IC_DIV; break;
                case BinaryOp_Modulo: result->opcode = IC_MOD; break;
                default: unimplemented;
            }
            
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

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

inline void
x64_add(Intermediate_Code* ic, 
        Ic_Type t1, s64 r1, s64 d1, 
        Ic_Type t2, s64 r2, s64 d2, 
        Ic_Type t3, s64 r3, s64 d3) {
    
    if (t1 == IC_REG) {
        x64_mov(ic, t1, r1, d1, t2, r2, d2);
    } else {
        unimplemented;
    }
    
    switch (t1) {
        case IC_REG: {
            if (t3 == IC_IMM) {
                // 81 /0 id 	ADD r/m32, imm32 	MI
                ic_u8(ic, 0x81);
                ic_u8(ic, 0xC0 | ((u8) r1<<3) | (u8) r3);
                ic_u32(ic, (u32) d3);
                
            } else if (t3 == IC_REG) {
                // 03 /r 	ADD r32, r/m32 	RM
                ic_u8(ic, 0x03);
                ic_u8(ic, 0xC0 | ((u8) r1<<3) | (u8) r3);
            } else {
                unimplemented;
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}


inline void
x64_mov(Intermediate_Code* ic,
        Ic_Type t1, s64 r1, s64 d1,
        Ic_Type t2, s64 r2, s64 d2) {
    
    switch (t1) {
        case IC_REG: {
            
            if (t2 == IC_IMM) {
                // B8+ rd id 	MOV r32, imm32 	OI
                ic_u8(ic, 0xB8+(u8)r1);
                ic_u32(ic, (u32) d2);
                
            } else if (t2 == IC_REG || t2 == IC_STACK) {
                // 8B /r 	MOV r32,r/m32 	RM
                ic_u8(ic, 0x8B);
                
                u8 mod = (t2 == IC_STACK) ? MODRM_DIRECT : MODRM_INDIRECT_DISP32;
                if (t2 == IC_STACK) {
                    if (d2 < S8_MIN || d2 > S8_MAX) {
                        ic_u8(ic, MODRM_INDIRECT_DISP32 | ((u8) r1<<3) | X64_RBP);
                        ic_u32(ic, (u32) d2);
                    } else {
                        ic_u8(ic, MODRM_INDIRECT_DISP8 | ((u8) r1<<3) | X64_RBP);
                        ic_u8(ic, (u8) d2);
                    }
                } else {
                    ic_u8(ic, MODRM_DIRECT | ((u8) r1<<3) | (u8) r2);
                }
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

inline void
x64_div(Intermediate_Code* ic, bool remainder) {
    
    x64_mov(ic, IC_REG, X64_RAX, 0, ic->src0.type, ic->src0.reg, ic->src0.disp);
    x64_mov(ic, IC_REG, X64_RCX, 0, ic->src1.type, ic->src1.reg, ic->src1.disp);
    ic_u8(ic, 0x99); // CDQ
    
    // F7 /6 	DIV r/m32 	M
    ic_u8(ic, 0xF7);
    ic_u8(ic, 0xF0 | (u8) X64_RCX);
    
#if 0
    // TODO(Alexander): check for signedness
    // F7 /7 	IDIV r/m32 	M
    ic_u8(ic, 0xF7);
    ic_u8(ic, 0xF8 | (u8) ic->src1.reg);
#endif
    
    s64 dest_reg = remainder ? X64_RDX : X64_RAX;
    x64_mov(ic, ic->dest.type, ic->dest.reg, ic->dest.disp, IC_REG, dest_reg, 0);
}

void
convert_to_x64_machine_code(Intermediate_Code* ic) {
    
    ic->count = 0;
    switch (ic->opcode) {
        case IC_PRLG: {
            s64 t = IC_REG << 8 | IC_S64;
            x64_mov(ic, t, X64_RBP, 0, t, X64_RSP, 0);
        } break;
        
        case IC_EPLG: {
            
        } break;
        
        case IC_ADD: {
            x64_add(ic, 
                    ic->dest.type, ic->dest.reg, ic->dest.disp, 
                    ic->src0.type, ic->src0.reg, ic->src0.disp, 
                    ic->src1.type, ic->src1.reg, ic->src1.disp);
        } break;
        
        case IC_MOV: {
            x64_mov(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, ic->src1.type, ic->src1.reg, ic->src1.disp);
        } break;
        
        case IC_DIV: {
            x64_div(ic, false);
        } break;
        
        case IC_MOD: {
            x64_div(ic, true);
        } break;
        
        default: unimplemented;
    }
}



void
test_x64_converter(int srcc, char* srcv[], void* asm_buffer, umm asm_size,
                   void (*asm_make_executable)(void*, umm), bool is_debugger_present) {
    
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, string_lit("x + 10"), string_lit("test"));
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    //Ast_File ast_file = parse_file(&parser);
    
    Ast* expr = parse_expression(&parser);
    
    print_ast(expr, &tokenizer);
    
    Intermediate_Code* ic_begin = create_ic(IC_PRLG);
    {
        Intermediate_Code* ic = convert_to_intermediate_code(expr);
        Intermediate_Code* ic_end = create_ic(IC_EPLG);
        
        ic_begin->next = ic;
        ic_begin->last = ic_end;
        ic->next = ic_end;
        ic->last = ic_end;
        ic_end->last = ic_end;
    }
    
    umm asm_curr_used = 0;
    if (is_debugger_present) {
        *((u8*) asm_buffer) = 0xCC;
        asm_curr_used = 1;
    }
    
    Intermediate_Code* curr = ic_begin;
    while (curr) {
        convert_to_x64_machine_code(curr);
        assert((asm_curr_used + curr->count) < asm_size);
        memcpy((u8*) asm_buffer + asm_curr_used, curr->code, curr->count);
        asm_curr_used += curr->count;
        
        curr = curr->next;
    }
    
    pln("\nX64 Machine Code (% bytes):", f_umm(asm_curr_used));
    for (int byte_index = 0; byte_index < asm_curr_used; byte_index++) {
        u8 byte = ((u8*) asm_buffer)[byte_index];
        if (byte > 0xF) {
            printf("%hhX ", byte);
        } else {
            printf("0%hhX ", byte);
        }
        
        if (byte_index % 90 == 89) {
            printf("\n");
        }
    }
    
    asm_make_executable(asm_buffer, asm_curr_used);
    asm_main* func = (asm_main*) asm_buffer;
    int jit_exit_code = (int) func();
    pln("JIT exited with code: %", f_int(jit_exit_code));
}
