
Ic_Raw_Type
convert_type_to_raw_type(Type* type) {
    Ic_Raw_Type raw_type = IC_S32;
    if (type->kind == TypeKind_Basic) {
        raw_type = basic_type_to_raw_type(type->Basic.kind);
    } else if (type->kind == TypeKind_Pointer) {
        raw_type = IC_U64;
    }
    return raw_type;
}

Ic_Arg
convert_expr_to_intermediate_code(Comp_Unit* cu, Ast* expr) {
    Ic_Arg result = {};
    
    switch (expr->kind) {
        case Ast_Value: {
            if (is_integer(expr->Value.value)) {
                Ic_Raw_Type raw_type = convert_type_to_raw_type(expr->type);
                result.type = raw_type + IC_IMM;
                result.disp = value_to_s64(expr->Value.value);
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Ident: {
            string_id ident = ast_unwrap_ident(expr);
            assert(map_key_exists(cu->stack_displacements, ident));
            
            assert(expr->type);
            Type* type = expr->type;
            
            Ic_Raw_Type raw_type = convert_type_to_raw_type(type);
            s64 disp = map_get(cu->stack_displacements, ident);
            result = ic_stk(raw_type, disp);
        } break;
        
        case Ast_Paren_Expr: {
            result = convert_expr_to_intermediate_code(cu, expr->Paren_Expr.expr);
        } break;
        
        case Ast_Array_Expr: {
            Type* type = expr->type;
            smm capacity = type->Array.capacity;
            assert(capacity > 0);
            
            type = type->Array.type;
            
            // TODO(Alexander): temporary use Memory_Arena
            smm size = capacity*type->size;
            void* data = malloc(size);
            s64 disp = align_forward(cu->stack_curr_used, type->align) + type->size*capacity;
            
            Intermediate_Code* ic = ic_add(cu, IC_MEMCPY, data);
            ic->dest = ic_stk(0, -disp);
            ic->src0 = ic_imm(0, size);
            result = ic->dest;
            
            u8* curr = (u8*) data;
            for_compound(expr->Array_Expr.elements, e) {
                assert(e->kind == Ast_Value);
                value_store_in_memory(e->type, curr, e->Value.value.data);
                curr += type->size;
            }
            
            pln("%, ", f_int(*((u64*) data)), f_int(*((u64*) data + 1)), f_int(*((u64*) data + 2)));
            
            cu->stack_curr_used = disp;
        } break;
        
        case Ast_Index_Expr: {
            Type* type = expr->type;
            
            result = convert_expr_to_intermediate_code(cu, expr->Index_Expr.array);
            Ic_Arg index = convert_expr_to_intermediate_code(cu, expr->Index_Expr.index);
            if (result.type & IC_STK) {
                if (index.type & IC_IMM) {
                    result.disp += type->size * index.disp;
                }
            }
        } break;
        
        case Ast_Binary_Expr: {
            Ic_Arg src0 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.first);
            Ic_Arg src1 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.second);
            assert(src0.raw_type == src1.raw_type);
            
            Intermediate_Code* ic = ic_add(cu);
            ic->dest = ic_reg(src0.raw_type);
            ic->src0 = src0;
            ic->src1 = src1;
            result = ic->dest;
            
            Binary_Op op = expr->Binary_Expr.op;
            if (binary_is_comparator_table[op]) {
                result.raw_type = IC_S8;
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
        
        case Ast_Cast_Expr: {
            assert(expr->Cast_Expr.expr);
            Type* dest = expr->type;
            Type* src = expr->Cast_Expr.expr->type;
            
            Ic_Arg src_arg = convert_expr_to_intermediate_code(cu, expr->Cast_Expr.expr);
            result = src_arg;
            
            pln("cast from % to %", f_type(src), f_type(dest));
            
            if (dest->kind == TypeKind_Basic && src->kind == TypeKind_Basic) {
                Ic_Raw_Type dest_raw_type = basic_type_to_raw_type(dest->Basic.kind);
                
                if (dest->Basic.flags & BasicFlag_Integer && src->Basic.flags & BasicFlag_Integer) {
                    if (dest->size > src->size) {
                        Intermediate_Code* ic = ic_add(cu, 
                                                       dest->Basic.flags & BasicFlag_Unsigned ? 
                                                       IC_MOVZX : IC_MOVSX);
                        ic->src0 = ic_reg(dest_raw_type);
                        ic->src1 = src_arg;
                        result = ic->src0;
                        
                    } else if (dest->size < src->size) {
                        
                    }
                } else {
                    unimplemented;
                }
            } else {
                unimplemented;
            }
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

void
convert_stmt_to_intermediate_code(Comp_Unit* cu, Ast* stmt) {
    switch (stmt->kind) {
        case Ast_Expr_Stmt: {
            convert_expr_to_intermediate_code(cu, stmt->Expr_Stmt);
        } break;
        
        case Ast_Assign_Stmt: {
            Type* type = stmt->type;
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            
            
            Ic_Arg src = {};
            if (!is_ast_none(stmt->Assign_Stmt.expr)) {
                src = convert_expr_to_intermediate_code(cu, stmt->Assign_Stmt.expr);
            }
            
            s64 disp;
            if (type->kind == TypeKind_Array) {
                // TODO(Alexander): this is in-place for now, normally it would be a ptr
                assert(type->Array.capacity > 0);
                smm capacity = type->Array.capacity;
                
                if (src.type & IC_STK) {
                    map_put(cu->stack_displacements, ident, src.disp);
                    break;
                }
                
                type = type->Array.type;
                disp = align_forward(cu->stack_curr_used, type->align) + type->size;
                cu->stack_curr_used = disp + (capacity - 1)*type->size;
            } else {
                disp = align_forward(cu->stack_curr_used, type->align) + type->size;
                cu->stack_curr_used = disp;
            }
            
            map_put(cu->stack_displacements, ident, -disp);
            
            if (!is_ast_none(stmt->Assign_Stmt.expr)) {
                Ic_Arg dest = convert_expr_to_intermediate_code(cu, stmt->Assign_Stmt.ident);
                Intermediate_Code* ic = ic_add(cu, IC_MOV);
                ic->src0 = dest;
                ic->src1 = src;
            }
            
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                convert_stmt_to_intermediate_code(cu, it);
            }
        } break;
        
        case Ast_If_Stmt: {
            Ic_Basic_Block* bb_else = ic_basic_block();
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->If_Stmt.cond);
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
            
            convert_stmt_to_intermediate_code(cu, stmt->If_Stmt.then_block);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                Ic_Basic_Block* bb_exit = ic_basic_block();
                ic_add(cu, IC_JMP, bb_exit);
                ic_add(cu, IC_LABEL, bb_else);
                convert_stmt_to_intermediate_code(cu, stmt->If_Stmt.else_block);
                ic_add(cu, IC_LABEL, bb_exit);
            } else {
                ic_add(cu, IC_LABEL, bb_else);
            }
        } break;
        
        case Ast_Return_Stmt: {
            // TODO(Alexander): this is platform/architecture specific
            Ic_Arg result = convert_expr_to_intermediate_code(cu, stmt->Return_Stmt.expr);
            Intermediate_Code* ic = ic_add(cu, IC_MOV);
            ic->src0 = ic_reg();
            ic->src1 = result;
        } break;
        
        default: unimplemented;
    }
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
                if (d2 == 0) {
                    return;
                }
                
                if (d2 > U32_MAX) {
                    unimplemented;
                }
                
                if (t1 & IC_T64) {
                    x64_rex(ic, REX_FLAG_64_BIT);
                }
                
                // 81 /0 id 	ADD r/m32, imm32 	MI
                ic_u8(ic, 0x81);
                ic_u8(ic, 0xC0 | (reg_field << 3) | (u8) r1);
                ic_u32(ic, (u32) d2);
                
            } else if (t2 & (IC_REG | IC_STK)) {
                if (t1 & IC_T64) {
                    x64_rex(ic, REX_FLAG_64_BIT);
                }
                
                // 03 /r 	ADD r32, r/m32 	RM
                ic_u8(ic, opcode + 3);
                x64_modrm(ic, t2, d2, r1, r2);
            } else {
                unimplemented;
            }
        } break;
        
        case IC_STK: {
            if (t2 & IC_IMM) {
                assert((t1 & IC_T64) == 0);
                
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
        if (!(t2 & IC_REG)) {
            x64_mov(ic, t1, r1, d1, t2, r2, d2);
        }
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
                if (t1 & IC_T64) {
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
            } else if (t2 & IC_STK) {
                // NOTE(Alexander): x64 doesn't allow MOV STK, STK, move to tmp reg
                // TODO(Alexander): reg hardcoded RAX
                x64_mov(ic, IC_REG + (t2 & IC_RT_MASK), X64_RAX, 0, t2, r2, d2);
                x64_mov(ic, t1, r1, d1, IC_REG + (t1 & IC_RT_MASK), X64_RAX, 0);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

inline void
x64_div(Intermediate_Code* ic, bool remainder) {
    
    x64_mov(ic, ic->src0.raw_type + IC_REG, X64_RAX, 0, ic->src0.type, ic->src0.reg, ic->src0.disp);
    x64_mov(ic, ic->src1.raw_type + IC_REG, X64_RCX, 0, ic->src1.type, ic->src1.reg, ic->src1.disp);
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
x64_lea(Intermediate_Code* ic, s64 r1, s64 r2, s64 d2) {
    // REX.W + 8D /r 	LEA r64,m 	RM
    
    x64_rex(ic, REX_FLAG_W);
    ic_u8(ic, 0x8D);
    
    if (r2 == X64_RIP) {
        ic_u8(ic, (u8) r1<<3 | (u8) X64_RBP);
        ic_u32(ic, (u32) d2);
    } else {
        x64_modrm(ic, IC_STK, d2, r1);
    }
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
                ic_u32(ic, (u32) stack_usage);
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
            
            case IC_DIV: {
                x64_div(ic, false);
            } break;
            
            case IC_MOD: {
                x64_div(ic, true);
            } break;
            
            case IC_MOV:
            case IC_MOVZX: {
                x64_mov(ic,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp);
            } break;
            
            case IC_MOVSX: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, d1 = ic->src0.disp, r2 = ic->src1.reg, d2 = ic->src1.disp;
                
                // REX.W + 63 /r 	MOVSXD r64, r/m32 	RM
                x64_rex(ic, REX_FLAG_W);
                ic_u8(ic, 0x63);
                x64_modrm(ic, t2, d2, r1, r2);
            } break;
            
            case IC_MEMCPY: {
                // Move RCX bytes from [RSI] to [RDI].
                x64_mov(ic, IC_S64 + IC_REG, X64_RCX, 0, ic->src0.type, ic->src0.reg, ic->src0.disp);
                x64_lea(ic, X64_RDI, X64_RBP, ic->dest.disp);
                
                // TODO(Alexander): this is a hack using RIP-relative pointer, why +16?
                s64 disp = (s64) ic->data - ((s64) buf + rip + 16);
                x64_lea(ic, X64_RSI, X64_RIP, disp);
                
                // F3 A4 	REP MOVS m8, m8 	ZO
                ic_u8(ic, 0xF3);
                ic_u8(ic, 0xA4);
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
    
    
    Type_Context tcx = {};
    tcx.return_type = t_s32;
    Type* type = type_infer_statement(&tcx, stmt, true);
    if (type) {
        type_check_statement(&tcx, stmt);
    } else {
        pln("Ast: (partially typed):");
        print_ast(stmt, &tokenizer);
        return -1;
    }
    
    
    if (tcx.error_count > 0) {
        pln("Ast: (partially typed):");
        print_ast(stmt, &tokenizer);
        
        pln("\nExiting due to errors...\n");
        return -1;
    }
    
    
    print_ast(stmt, &tokenizer);
    
    // Codegen begins here
    Comp_Unit cu = {};
    
    if (is_debugger_present) {
        ic_add(&cu, IC_DEBUG_BREAK);
    }
    
    ic_add(&cu, IC_PRLG);
    convert_stmt_to_intermediate_code(&cu, stmt);
    ic_add(&cu, IC_EPLG);
    
    s64 rip = convert_to_x64_machine_code(cu.ic_first, cu.stack_curr_used, 0, 0);
    s64 rip2 = convert_to_x64_machine_code(cu.ic_first, cu.stack_curr_used, 
                                           (u8*) asm_buffer, (s64) asm_size);
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
