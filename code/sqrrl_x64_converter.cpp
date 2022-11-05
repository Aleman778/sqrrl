
Ic_Arg
convert_to_intermediate_code(Comp_Unit* cu, Ast* node) {
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
            assert(map_key_exists(cu->stack_displacements, ident));
            
            result.type = IC_S32 + IC_STK;
            result.reg = X64_RBP; // TODO(Alexander): RBP hardcoded for now
            result.disp = map_get(cu->stack_displacements, ident);
        } break;
        
        case Ast_Binary_Expr: {
            Ic_Arg src0 = convert_to_intermediate_code(cu, node->Binary_Expr.first);
            Ic_Arg src1 = convert_to_intermediate_code(cu, node->Binary_Expr.second);
            
            Intermediate_Code* ic = ic_add(cu);
            ic->dest = create_ic_reg();
            ic->src0 = src0;
            ic->src1 = src1;
            result = ic->dest;
            
            Binary_Op op = node->Binary_Expr.op;
            if (binary_is_comparator_table[op]) {
                result.type_raw = IC_S8;
                ic->opcode = IC_CMP;
                ic->dest = result;
                
                ic = ic_add(cu);
                ic->dest = result;
            }
            
            switch (op) {
                case BinaryOp_Assign: {
                    ic->opcode = IC_MOV;
                    ic->dest.type = 0;
                } break;
                case BinaryOp_Add: ic->opcode = IC_ADD; break;
                case BinaryOp_Divide: ic->opcode = IC_DIV; break;
                case BinaryOp_Modulo: ic->opcode = IC_MOD; break;
                case BinaryOp_Greater_Than: ic->opcode = IC_SETG; break;
                default: unimplemented;
            }
            
        } break;
        
        case Ast_Assign_Stmt: {
            string_id ident = ast_unwrap_ident(node->Assign_Stmt.ident);
            
            cu->stack_curr_used -= 4; // TODO(Alexander): hardcoded s32 for now
            map_put(cu->stack_displacements, ident, cu->stack_curr_used);
            
            if (!is_ast_none(node->Assign_Stmt.expr)) {
                Intermediate_Code* ic = ic_add(cu, IC_MOV);
                ic->src0 = convert_to_intermediate_code(cu, node->Assign_Stmt.ident);
                ic->src1 = convert_to_intermediate_code(cu, node->Assign_Stmt.expr);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            result = convert_to_intermediate_code(cu, node->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(node->Block_Stmt.stmts, it) {
                convert_to_intermediate_code(cu, it);
            }
        } break;
        
        case Ast_If_Stmt: {
            Ic_Basic_Block* bb_else = ic_basic_block();
            Ic_Arg cond = convert_to_intermediate_code(cu, node->If_Stmt.cond);
            Intermediate_Code* ic_jump = cu->ic_last;
            if (ic_jump && ic_is_setcc(ic_jump->opcode)) {
                switch (ic_jump->opcode) {
                    case IC_SETG: ic_jump->opcode = IC_JNG; break;
                    default: unimplemented;
                }
                ic_jump->data = bb_else;
            } else {
                Intermediate_Code* cmp = ic_add(cu, IC_CMP, bb_else);
                cmp->src0 = cond;
                cmp->src1.type = IC_S8 + IC_IMM;
                ic_jump = ic_add(cu, IC_JE);
            }
            
            convert_to_intermediate_code(cu, node->If_Stmt.then_block);
            
            if (is_valid_ast(node->If_Stmt.else_block)) {
                Ic_Basic_Block* bb_exit = ic_basic_block();
                ic_add(cu, IC_JMP, bb_exit);
                ic_add(cu, IC_LABEL, bb_else);
                convert_to_intermediate_code(cu, node->If_Stmt.else_block);
                ic_add(cu, IC_LABEL, bb_exit);
            } else {
                ic_add(cu, IC_LABEL, bb_else);
            }
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

#define REX_PATTERN 0x40
#define REX_FLAG_W bit(3)
#define REX_FLAG_R bit(2)
#define REX_FLAG_X bit(1)
#define REX_FLAG_B bit(0)
#define REX_FLAG_64_BIT REX_FLAG_W

void
x64_rex(Intermediate_Code* ic, u8 flags) {
    ic_u8(ic, REX_PATTERN | flags);
}


#if 0
void
x64_rex(Intermediate_Code* ic, Ic_Type t, s64 r, s64 rm) {
    bool use_rex = false;
    u8 rex = REX_PATTERN;
    if (t & (IC_S64 | IC_U64)) {
        use_rex = true;
        rex |= REX_FLAG_W;
    }
    
    // TODO(Alexander): check for this regisetr swap (from intel manual):
    // "When any REX prefix is used, SPL, BPL, SIL and DIL are used. 
    // Otherwise, without any REX prefix AH, CH, DH and BH are used."
    
    if (r & 8) {
        assert(0);
        use_rex = true;
        rex |= REX_FLAG_R;
    }
    
    // TODO(Alexander): TODO: REX_FLAG_X;
    
    if (rm & 8) {
        assert(0);
        use_rex = true;
        rex |= REX_FLAG_B;
    }
    
    if (use_rex) {
        ic_u8(ic, rex);
    }
}
#endif

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

inline void
x64_binary(Intermediate_Code* ic,
           Ic_Type t1, s64 r1, s64 d1, 
           Ic_Type t2, s64 r2, s64 d2,
           s8 reg_field, s8 opcode) {
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_IMM) {
                assert(t1 & (IC_S32 | IC_U32));
                
                // 81 /0 id 	ADD r/m32, imm32 	MI
                ic_u8(ic, 0x81);
                ic_u8(ic, 0xC0 | (reg_field << 3) | (u8) r1);
                ic_u32(ic, (u32) d2);
                
            } else if (t2 & IC_REG) {
                assert(t1 & (IC_S32 | IC_U32));
                
                // 03 /r 	ADD r32, r/m32 	RM
                ic_u8(ic, opcode + 3);
                ic_u8(ic, 0xC0 | ((u8) r1<<3) | (u8) r2);
            } else {
                unimplemented;
            }
        } break;
        
        case IC_STK: {
            if (t2 & IC_IMM) {
                assert(t1 & (IC_S32 | IC_U32));
                
                // 81 /0 id 	ADD r/m32, imm32 	MI
                ic_u8(ic, 0x81);
                //ic_u8(ic, 0xC7);
                x64_modrm(ic, t1, d1, reg_field);
                ic_u32(ic, (u32) d2);
            } else {
                unimplemented;
            }
        } break;
        
        default: assert(0 && "invalid instruction");
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
    
    x64_binary(ic, t1, r1, d1, t3, r3, d3, 0, 0);
}

void
x64_mov(Intermediate_Code* ic,
        Ic_Type t1, s64 r1, s64 d1,
        Ic_Type t2, s64 r2, s64 d2) {
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_IMM) {
                // B8+ rd id 	MOV r32, imm32 	OI
                ic_u8(ic, 0xB8+(u8)r1);
                ic_u32(ic, (u32) d2);
                
            } else if (t2 & (IC_REG | IC_STK)) {
                // 8B /r 	MOV r32,r/m32 	RM
                if (t1 & (IC_S64 | IC_U64)) {
                    x64_rex(ic, REX_FLAG_64_BIT);
                }
                ic_u8(ic, 0x8B);
                x64_modrm(ic, t2, d2, r1, r2);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        case IC_STK: {
            if (t2 & IC_IMM) {
                // C7 /0 id 	MOV r/m32, imm32 	MI
                ic_u8(ic, 0xC7);
                x64_modrm(ic, t1, d1, 0);
                ic_u32(ic, (u32) d2);
            } else if (t2 & IC_REG) {
                // 89 /r 	MOV r/m32,r32 	MR
                ic_u8(ic, 0x89);
                x64_modrm(ic, t1, d1, r2);
                
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

inline void
x64_div(Intermediate_Code* ic, bool remainder) {
    
    x64_mov(ic, ic->src0.type_raw + IC_REG, X64_RAX, 0, ic->src0.type, ic->src0.reg, ic->src0.disp);
    x64_mov(ic, ic->src1.type_raw + IC_REG, X64_RCX, 0, ic->src1.type, ic->src1.reg, ic->src1.disp);
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
x64_encode_relative_jump(Intermediate_Code* ic, s64 rip) {
    Ic_Basic_Block* bb = (Ic_Basic_Block*) ic->data;
    if (bb->addr != IC_INVALID_ADDR) {
        // TODO(Alexander): add support for short jumps
        assert(bb->addr >= S32_MIN && bb->addr <= S32_MAX && "cannot fit addr in rel32");
        ic_u32(ic, (s32) (bb->addr - rip - ic->count - 4));
    } else {
        ic_u32(ic, (s32) 0);
    }
}

s64
convert_to_x64_machine_code(Intermediate_Code* ic, s64 stack_usage, u8* buf, s64 buf_size) {
    
    s64 rip = 0;
    
    while (ic) {
        ic->count = 0;
        
        switch (ic->opcode) {
            case IC_NOOP: break;
            
            case IC_PRLG: {
                Ic_Type t = IC_REG + IC_S64;
                x64_mov(ic, t, X64_RBP, 0, t, X64_RSP, 0);
                
                // REX.W + 81 /5 id 	SUB r/m64, imm32 	MI
                ic_u8(ic, REX_PATTERN | REX_FLAG_64_BIT);
                ic_u8(ic, 0x81);
                x64_modrm(ic, t, 0, 5, X64_RBP);
                ic_u32(ic, (u32) -stack_usage);
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
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp);
            } break;
            
            case IC_DIV: {
                x64_div(ic, false);
            } break;
            
            case IC_MOD: {
                x64_div(ic, true);
            } break;
            
            case IC_CMP: {
                x64_binary(ic,
                           ic->src0.type, ic->src0.reg, ic->src0.disp,
                           ic->src1.type, ic->src1.reg, ic->src1.disp, 7, 0x38);
            } break;
            
            case IC_JNG: {
                // TODO(Alexander): short jumps
                ic_u8(ic, 0x0F);
                ic_u8(ic, 0x8E);
                x64_encode_relative_jump(ic, rip);
            } break;
            
            case IC_JMP: {
                // TODO(Alexander): short jumps (and indirect jump?)
                ic_u8(ic, 0xE9);
                x64_encode_relative_jump(ic, rip);
            } break;
            
            case IC_DEBUG_BREAK: {
                ic_u8(ic, 0xCC);
            } break;
            
            case IC_LABEL: {
                ((Ic_Basic_Block*) ic->data)->addr = rip;
            } break;
            
            default: unimplemented;
        }
        
        if (buf) {
            assert(rip < buf_size);
            memcpy(buf + rip, ic->code, ic->count);
        }
        
        rip += ic->count;
        ic = ic->next;
    }
    
    return rip;
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

int
test_x64_converter(int srcc, char* srcv[], void* asm_buffer, umm asm_size,
                   void (*asm_make_executable)(void*, umm), bool is_debugger_present) {
    
    vars_initialize_keywords_and_symbols();
    
    string filepath = string_lit("../examples/backend_test.sq");
    
    // TODO(Alexander): this is hardcoded for now
    t_string->size = sizeof(string);
    t_string->align = alignof(string);
    t_cstring->size = sizeof(cstring);
    t_cstring->align = alignof(cstring);
    
    // Read entire source file
    Loaded_Source_File file = read_entire_source_file(filepath);
    
    if (!file.is_valid) {
        return -1;
    }
    
    Tokenizer tokenizer = {};
    tokenizer_set_source(&tokenizer, file.source, string_lit("test"));
    
    Parser parser = {};
    parser.tokenizer = &tokenizer;
    //Ast_File ast_file = parse_file(&parser);
    
    Ast* stmt = parse_block_statement(&parser);
    
    print_ast(stmt, &tokenizer);
    
    Comp_Unit cu = {};
    
    if (is_debugger_present) {
        ic_add(&cu, IC_DEBUG_BREAK);
    }
    
    ic_add(&cu, IC_PRLG);
    convert_to_intermediate_code(&cu, stmt);
    ic_add(&cu, IC_EPLG);
    
    s64 rip = convert_to_x64_machine_code(cu.ic_first, cu.stack_curr_used, 0, 0);
    s64 rip2 = convert_to_x64_machine_code(cu.ic_first, cu.stack_curr_used, (u8*) asm_buffer, (s64) asm_size);
    assert(rip == rip2);
    
    pln("\nIntermediate code:");
    
    {
        String_Builder sb = {};
        
        int bb_index = 0;
        Intermediate_Code* curr = cu.ic_first;
        while (curr) {
            
            if (curr->opcode == IC_LABEL) {
                string_builder_push_format(&sb, "\nbb%:",
                                           f_int(bb_index++));
            } else {
                string_builder_push(&sb, "  ");
                
                
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
                        string_builder_push(&sb, curr->src1);
                        
                    }
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
    
    pln("\nX64 Machine Code (% bytes):", f_umm(rip));
    for (int byte_index = 0; byte_index < rip; byte_index++) {
        u8 byte = ((u8*) asm_buffer)[byte_index];
        if (byte > 0xF) {
            printf("%hhX ", byte);
        } else {
            printf("0%hhX ", byte);
        }
        
        if (byte_index % 16 == 15 || byte_index == rip - 1) {
            printf("\n");
        }
    }
    
    asm_make_executable(asm_buffer, rip);
    asm_main* func = (asm_main*) asm_buffer;
    int jit_exit_code = (int) func();
    pln("\nJIT exited with code: %", f_int(jit_exit_code));
    
    return 0;
}
