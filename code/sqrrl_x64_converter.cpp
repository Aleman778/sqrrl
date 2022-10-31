
inline Intermediate_Code*
create_ic(Ic_Opcode opcode = IC_NOOP) {
    // TODO(Alexander): temporary bump allocation for now
    Intermediate_Code* result = (Intermediate_Code*) calloc(1, sizeof(Intermediate_Code));
    result->opcode = opcode;
    result->last = result;
    return result;
}

inline Ic_Arg
ic_reg() {
    // TODO(Alexander): how to handle register allocation
    Ic_Arg result = {};
    result.reg = 0;
    result.type = IC_S32 + IC_REG; // TODO(Alexander): hardcoded type
    return result;
}

// TODO(Alexander): this is temporary
map(string_id, s64)* stack_displacements;
s64 curr_displacement;

Ic_Arg
convert_to_ic_arg(Ast* node) {
    Ic_Arg result = {};
    
    switch (node->kind) {
        case Ast_Value: {
            if (is_integer(node->Value.value)) {
                result.type = IC_S32 + IC_IMM;
                result.disp = value_to_s64(node->Value.value);
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Ident: {
            string_id ident = ast_unwrap_ident(node);
            assert(map_key_exists(stack_displacements, ident));
            
            result.type = IC_S32 + IC_STK;
            result.reg = X64_RBP; // TODO(Alexander): RBP hardcoded for now
            result.disp = map_get(stack_displacements, ident);
        } break;
        
        default: unimplemented;
    }
    
    return result;
}


Intermediate_Code*
convert_to_intermediate_code(Ast* node) {
    Intermediate_Code* result = 0;
    
    switch (node->kind) {
        case Ast_Ident: {
            result = create_ic(IC_MOV);
            result->dest = ic_reg();
            result->src0 = convert_to_ic_arg(node);
        } break;
        
        case Ast_Value: {
            result = create_ic(IC_MOV);
            result->dest = ic_reg();
            result->src0 = convert_to_ic_arg(node);
        } break;
        
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
        
        case Ast_Assign_Stmt: {
            string_id ident = ast_unwrap_ident(node->Assign_Stmt.ident);
            
            curr_displacement -= 4; // TODO(Alexander): hardcoded s32 for now
            map_put(stack_displacements, ident, curr_displacement);
            
            if (!is_ast_none(node->Assign_Stmt.expr)) {
                result = convert_to_intermediate_code(node->Assign_Stmt.expr);
                result->dest = convert_to_ic_arg(node->Assign_Stmt.ident);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            result = convert_to_intermediate_code(node->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            Intermediate_Code* ic = 0;
            for_compound(node->Block_Stmt.stmts, it) {
                Intermediate_Code* ic_next = convert_to_intermediate_code(it);
                if (ic) {
                    ic->next = ic_next;
                }
                ic = ic_next;
                
                if (!result) { 
                    result = ic;
                }
                result->last = ic;
            }
        } break;
        
        case Ast_If_Stmt: {
            
            
            
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

#define REX_PATTERN 0x40
#define REX_W bit(3)
#define REX_R bit(2)
#define REX_X bit(1)
#define REX_B bit(0)

void
x64_rex(Intermediate_Code* ic, Ic_Type t, s64 r, s64 rm) {
    bool use_rex = false;
    u8 rex = REX_PATTERN;
    if (t & (IC_S64 | IC_U32)) {
        use_rex = true;
        rex |= REX_W;
    }
    
    // TODO(Alexander): check for this regisetr swap (from intel manual):
    // "When any REX prefix is used, SPL, BPL, SIL and DIL are used. 
    // Otherwise, without any REX prefix AH, CH, DH and BH are used."
    
    if (r & 8) {
        use_rex = true;
        rex |= REX_R;
    }
    
    // TODO(Alexander): TODO: REX_X;
    
    if (rm & 8) {
        use_rex = true;
        rex |= REX_B;
    }
    
    if (use_rex) {
        ic_u8(ic, rex);
    }
}

inline void
x64_add(Intermediate_Code* ic, 
        Ic_Type t1, s64 r1, s64 d1, 
        Ic_Type t2, s64 r2, s64 d2, 
        Ic_Type t3, s64 r3, s64 d3) {
    
    if (t1 & IC_REG) {
        x64_mov(ic, t1, r1, d1, t2, r2, d2);
    } else {
        unimplemented;
    }
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t3 & IC_IMM) {
                // 81 /0 id 	ADD r/m32, imm32 	MI
                ic_u8(ic, 0x81);
                ic_u8(ic, 0xC0 | (u8) r1);
                ic_u32(ic, (u32) d3);
                
            } else if (t3 & IC_REG) {
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

void
x64_modrm(Intermediate_Code* ic, Ic_Type t, s64 d, s64 r, s64 rm=X64_RBP) {
    if (t & IC_STK) {
        if (d < S8_MIN || d > S8_MAX) {
            ic_u8(ic, MODRM_INDIRECT_DISP32 | ((u8) r<<3) | (u8) rm);
            ic_u32(ic, (u32) d);
        } else {
            ic_u8(ic, MODRM_INDIRECT_DISP8 | ((u8) r<<3) | (u8) rm);
            ic_u8(ic, (u8) d);
        }
    } else {
        ic_u8(ic, MODRM_DIRECT | ((u8) r<<3) | (u8) rm);
    }
}

void
x64_mov(Intermediate_Code* ic,
        Ic_Type t1, s64 r1, s64 d1,
        Ic_Type t2, s64 r2, s64 d2) {
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_IMM) {
                // B8+ rd id 	MOV r32, imm32 	OI
                x64_rex(ic, t1, r1, 0);
                ic_u8(ic, 0xB8+(u8)r1);
                ic_u32(ic, (u32) d2);
                
            } else if (t2 & (IC_REG | IC_STK)) {
                // 8B /r 	MOV r32,r/m32 	RM
                x64_rex(ic, t1, r1, r2);
                ic_u8(ic, 0x8B);
                x64_modrm(ic, t2, d2, r1, r2);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        case IC_STK: {
            if (t2 & IC_IMM) {
                // C7 /0 id 	MOV r/m32, imm32 	MI
                x64_rex(ic, t1, 0, r1);
                ic_u8(ic, 0xC7);
                x64_modrm(ic, t1, d1, 0);
                ic_u32(ic, (u32) d2);
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

inline void
x64_div(Intermediate_Code* ic, bool remainder) {
    
    x64_mov(ic, ic->src0.raw + IC_REG, X64_RAX, 0, ic->src0.type, ic->src0.reg, ic->src0.disp);
    x64_mov(ic, ic->src1.raw + IC_REG, X64_RCX, 0, ic->src1.type, ic->src1.reg, ic->src1.disp);
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
            Ic_Type t = IC_REG + IC_S64;
            x64_mov(ic, t, X64_RBP, 0, t, X64_RSP, 0);
            
            // REX.W + 81 /5 id 	SUB r/m64, imm32 	MI
            ic_u8(ic, REX_PATTERN | REX_W);
            ic_u8(ic, 0x81);
            x64_modrm(ic, t, 0, 5, X64_RBP);
            ic_u32(ic, (u32) -curr_displacement);
        } break;
        
        case IC_EPLG: {
            ic_u8(ic, 0xC3);
        } break;
        
        case IC_ADD: {
            x64_add(ic, 
                    ic->dest.type, ic->dest.reg, ic->dest.disp, 
                    ic->src0.type, ic->src0.reg, ic->src0.disp, 
                    ic->src1.type, ic->src1.reg, ic->src1.disp);
        } break;
        
        case IC_MOV: {
            x64_mov(ic, 
                    ic->dest.type, ic->dest.reg, ic->dest.disp, 
                    ic->src0.type, ic->src0.reg, ic->src0.disp);
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

internal void
string_builder_push(String_Builder* sb, Ic_Arg arg) {
    // TODO(Alexander): make use of the type
    switch (arg.type & IC_TF_MASK) {
        case IC_IMM: {
            string_builder_push_format(sb, "%", f_s64(arg.disp));
        } break;
        
        case IC_REG: {
            if (arg.reg < fixed_array_count(int_register_names)) {
                string_builder_push(sb, int_register_names[arg.reg]);
            }
        } break;
        
        case IC_STK: {
            if (arg.disp != 0) {
                string_builder_push_format(sb, "[RBP % %]", 
                                           f_char(arg.disp > 0 ? '+' : '-'), 
                                           f_s64(arg.disp > 0 ? arg.disp : -arg.disp));
            } else {
                string_builder_push(sb, "[RBP]");
            }
        } break;
    }
}

void
test_x64_converter(int srcc, char* srcv[], void* asm_buffer, umm asm_size,
                   void (*asm_make_executable)(void*, umm), bool is_debugger_present) {
    
    vars_initialize_keywords_and_symbols();
    
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, string_lit("{ int x = 20; x + 10; }"), string_lit("test"));
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    //Ast_File ast_file = parse_file(&parser);
    
    Ast* stmt = parse_block_statement(&parser);
    
    print_ast(stmt, &tokenizer);
    
    Intermediate_Code* ic_begin = create_ic(IC_PRLG);
    {
        Intermediate_Code* ic = convert_to_intermediate_code(stmt);
        Intermediate_Code* ic_end = create_ic(IC_EPLG);
        
        ic_begin->next = ic;
        ic_begin->last = ic_end;
        ic->last->next = ic_end;
        ic->last = ic_end;
        ic_end->last = ic_end;
    }
    
    umm asm_curr_used = 0;
    if (is_debugger_present) {
        *((u8*) asm_buffer) = 0xCC;
        asm_curr_used = 1;
    }
    
    {
        Intermediate_Code* curr = ic_begin;
        while (curr) {
            convert_to_x64_machine_code(curr);
            assert((asm_curr_used + curr->count) < asm_size);
            memcpy((u8*) asm_buffer + asm_curr_used, curr->code, curr->count);
            asm_curr_used += curr->count;
            
            curr = curr->next;
        }
    }
    
    pln("\nIntermediate code:");
    
    {
        String_Builder sb = {};
        
        Intermediate_Code* curr = ic_begin;
        while (curr) {
            if (curr->opcode != IC_LABEL) {
                string_builder_push(&sb, "  ");
            }
            
            if (curr->dest.type) {
                string_builder_push(&sb, curr->dest);
                string_builder_push(&sb, " = ");
            }
            
            string_builder_push(&sb, ic_opcode_names[curr->opcode]);
            
            if (curr->src0.type) {
                string_builder_push(&sb, " ");
                string_builder_push(&sb, curr->src0);
                
                if (curr->src1.type) {
                    string_builder_push(&sb, ", ");
                    string_builder_push(&sb, curr->src0);
                    
                }
            }
            
            curr = curr->next;
            
            if (curr) {
                string_builder_push(&sb, "\n");
            }
        }
        
        string s = string_builder_to_string_nocopy(&sb);
        pln("%", f_string(s));
        
        string_builder_free(&sb);
    }
    
    pln("\nX64 Machine Code (% bytes):", f_umm(asm_curr_used));
    for (int byte_index = 0; byte_index < asm_curr_used; byte_index++) {
        u8 byte = ((u8*) asm_buffer)[byte_index];
        if (byte > 0xF) {
            printf("%hhX ", byte);
        } else {
            printf("0%hhX ", byte);
        }
        
        if (byte_index % 16 == 15 || byte_index == asm_curr_used - 1) {
            printf("\n");
        }
    }
    
    asm_make_executable(asm_buffer, asm_curr_used);
    asm_main* func = (asm_main*) asm_buffer;
    int jit_exit_code = (int) func();
    pln("\nJIT exited with code: %", f_int(jit_exit_code));
}
