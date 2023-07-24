


#if 0
#define convert_assign_to_intermediate_code(cu, type, dest, src, store_inplace) \
_convert_assign_to_intermediate_code(cu, type, dest, src, store_inplace, __FILE__ ":" S2(__LINE__))

void
_convert_assign_to_intermediate_code(Compilation_Unit* cu, Type* type, Ic_Arg dest, Ic_Arg src,
                                     bool store_inplace, cstring comment=0) {
    
    
    if (type->size > 8) {
        if (store_inplace) {
            assert(dest.type & IC_STK_RIP);
            
            if (src.type) {
                Intermediate_Code* ic = ic_add_insn(cu, IC_MEMCPY, comment);
                ic->dest = dest;
                ic->src0 = src;
                ic->src1 = ic_imm(0, type->size);
                
            } else {
                Intermediate_Code* ic = ic_add_insn(cu, IC_MEMSET, comment);
                ic->dest = dest;
                ic->src0 = ic_imm(IC_U8, 0);
                ic->src1 = ic_imm(0, type->size);
            }
        } else {
            assert(dest.type != 0);
            _ic_lea(cu, dest, src, comment);
        }
    } else {
        if (src.type) {
            if (type->kind == TypeKind_Basic && type->Basic.kind == Basic_cstring && src.type & IC_RIP_DISP32) {
                ic_lea(cu, dest, src);
                
            } else {
                _ic_mov(cu, dest, src, comment);
            }
        } else {
            if (dest.type & IC_FLOAT) {
                Intermediate_Code* ic = ic_add_insn(cu, IC_FXOR, comment);
                ic->dest = dest;
                ic->src0 = ic_reg(dest.raw_type, X64_XMM0);
                ic->src1 = ic_reg(dest.raw_type, X64_XMM0);
            } else {
                _ic_mov(cu, dest, ic_imm(dest.raw_type, 0), comment);
            }
        }
    }
}

internal Ic_Arg
ic_inc(Compilation_Unit* cu, Type* type, Ic_Arg src, Ic_Opcode inc_opcode, Ic_Opcode add_opcode, bool post_increment) {
    s64 elem_size = 1;
    if (type->kind == TypeKind_Pointer) {
        elem_size = type->Pointer->size;
    }
    
    Ic_Arg result = src;
    result = src;
    if ((src.type & IC_REG) == 0) {
        result = ic_reg(src.raw_type, X64_RAX);
        ic_mov(cu, result, src);
    }
    
    Ic_Arg post = result;
    if (post_increment) {
        post = ic_reg(result.raw_type, X64_RCX);
        ic_mov(cu, post, result);
    }
    
    if (elem_size == 1) {
        Intermediate_Code* ic = ic_add(cu, inc_opcode);
        ic->src0 = post;
    } else {
        Intermediate_Code* ic = ic_add(cu, add_opcode);
        ic->dest = post;
        ic->src0 = post;
        ic->src1 = ic_imm(IC_S32, elem_size);
    }
    ic_mov(cu, src, post);
    
    return result;
}

Ic_Arg
ic_clobber_register(Compilation_Unit* cu, Intermediate_Code* ic_first, Type* prev_type, Ic_Arg prev_result, Ic_Arg new_result) {
    
    if (prev_result.type & (IC_REG | IC_STK) && prev_result.reg != X64_RSP) {
        
        bool clobbered = false;
        Intermediate_Code* ic_curr = ic_first;
        while (new_result.type && ic_curr) {
            if (ic_curr->opcode == IC_CALL) {
                clobbered = true;
                break;
            }
            
            if (ic_curr->opcode == IC_DIV) {
                clobbered = ((prev_result.raw_type & IC_FLOAT) == 0 && 
                             (prev_result.reg == X64_RAX || prev_result.reg == cu->data_reg));
                if (clobbered) {
                    break;
                }
            }
            
            if (new_result.type & (IC_REG | IC_STK) &&
                prev_result.reg == new_result.reg &&
                (prev_result.raw_type & IC_FLOAT) == (new_result.raw_type & IC_FLOAT)) {
                clobbered = true;
                break;
            }
            
            new_result.type = 0;
            
            ic_curr = ic_curr->next;
            if (ic_curr) {
                new_result = (ic_curr->opcode == IC_MOV ||
                              ic_curr->opcode == IC_LEA ||
                              ic_curr->opcode == IC_FMOV ||
                              ic_curr->opcode == IC_MOVZX ||
                              ic_curr->opcode == IC_MOVSX ||
                              ic_curr->opcode == IC_CAST_F2S ||
                              ic_curr->opcode == IC_CAST_S2F ||
                              ic_curr->opcode == IC_CAST_F2F) ? ic_curr->src0 : ic_curr->dest;
            }
        }
        
        if (clobbered) {
            Ic_Arg clobber = ic_push_local(cu, prev_type); // TODO(Alexander): temp allocation
            
            //Intermediate_Code* mov_clobber = ic_add_orphan(cu, (prev_result.type & IC_FLOAT) ? IC_FMOV : IC_MOV);
            //mov_clobber->src0 = clobber;
            //mov_clobber->src1 = prev_result;
            //mov_clobber->comment = "clobbered";
            
            Intermediate_Code* ic_last = cu->ic_last;
            Intermediate_Code* ic_second = ic_first->next;
            // TODO(Alexander): is there a way to SPEED this up by not storing in_place???
            convert_assign_to_intermediate_code(cu, prev_type, clobber, prev_result, true);
            Intermediate_Code* ic_clobber_first = ic_last->next;
            Intermediate_Code* ic_clobber_second = cu->ic_last;
            
            
            // Update links
            ic_first->next = ic_clobber_first;
            ic_clobber_second->next = ic_second;
            
            if (ic_second) {
                cu->ic_last = ic_last;
            }
            
            Ic_Basic_Block* bb = cu->bb_last;
            if (bb) {
                bb->ic_last = cu->ic_last;
            }
            
            return clobber;
        }
    }
    
    return prev_result;
}

Ic_Arg
convert_function_call_to_intermediate_code(Compilation_Unit* cu, 
                                           Type* function_type,
                                           array(Ast*)* args,
                                           Ast* call_ident,
                                           Ast* var_args=0) {
    
    
    Ic_Arg result = {};
    
    assert(function_type && function_type->kind == TypeKind_Function);
    Type_Function* t_func = &function_type->Function;
    
    if (t_func->intrinsic) {
        
        if (t_func->intrinsic == &interp_intrinsic_debug_break) {
            ic_add(cu, IC_DEBUG_BREAK);
            return result;
        } else if (t_func->intrinsic == &x64_intrin_rdtsc) {
            ic_add(cu, x64_intrin_rdtsc);
            return ic_reg(IC_U64, X64_RAX);
        }
    }
    
    s64 stk_args = 0;
    array(X64_Arg_Copy)* copy_args = 0;
    
    // Setup return type
    if (t_func->return_type) {
        Type* type = t_func->return_type;
        Ic_Raw_Type rt = convert_type_to_raw_type(type);
        
        if (type->size > 8) {
            // NOTE(Alexander): if return argument cannot fit in RAX/ XMM0 then
            //                  pass a pointer to it as the first argument
            s64 disp = stk_args;
            stk_args += 8;
            // TODO(Alexander): we should use argument stack but this has to be allocated last
            //Ic_Arg src = ic_stk(rt, disp, IcStkArea_Args);
            
            X64_Arg_Copy copy = {};
            copy.type = t_func->return_type;
            array_push(copy_args, copy);
        } else {
            result = ic_reg(rt, X64_RAX); // NOTE: RAX == 0 && XMM0 == 0
        }
    }
    
    {
        // Setup arguments
        int arg_index = 0;
        {
            for_array_v(args, arg, _) {
                {
                    Ic_Raw_Type rt = convert_type_to_raw_type(arg->type);
                    s64 disp = stk_args;
                    stk_args += 8;
                    Ic_Arg dest = ic_stk(rt, disp, IcStkArea_Args);
                    
                    X64_Arg_Copy copy = {};
                    copy.type = arg->type;
                    copy.expr = arg;
                    copy.dest = dest;
                    
                    bool precompute_arg = false;
                    
                    
                    if (arg->kind == Ast_Call_Expr) {
                        precompute_arg = true;
                    }
                    
                    if (arg->kind == Ast_Binary_Expr && (arg->Binary_Expr.overload || 
                                                         arg->Binary_Expr.op == Op_Divide)) {
                        precompute_arg = true;
                    }
                    
                    if (arg->kind == Ast_Unary_Expr && arg->Unary_Expr.overload) {
                        precompute_arg = true;
                    }
                    
                    if (precompute_arg) {
                        // TODO(Alexander): we can also have call within binary expr etc. e.g. bar(1 + foo())
                        // NOTE(Alexander): unroll inner function calls and evaulate them before the
                        //                  rest of the arguments.
                        Ic_Arg src = convert_expr_to_intermediate_code(cu, arg);
                        Ic_Arg src_dest = ic_push_local(cu, arg->type);
                        convert_assign_to_intermediate_code(cu, arg->type, src_dest, src, true);
                        copy.src = src_dest;
                    }
                    
                    array_push(copy_args, copy);
                    arg_index++;
                }
                
                // Var args
                if (var_args && t_func->is_variadic && arg_index == array_count(t_func->arg_types) - 1) {
                    Type* var_args_type = t_func->arg_types[array_count(t_func->arg_types) - 1];
                    
                    // Move var args store to the next argument
                    Ic_Raw_Type rt = convert_type_to_raw_type(var_args_type);
                    s64 disp = stk_args;
                    stk_args += 8;
                    Ic_Arg dest = ic_stk(rt, disp, IcStkArea_Args);
                    
                    // Store var_args.data pointer 
                    Ic_Arg var_arg_data = ic_stk(rt, disp + 8, IcStkArea_Args);
                    Ic_Arg var_arg_storage = convert_expr_to_intermediate_code(cu, var_args);
                    ic_lea(cu, var_arg_storage, var_arg_data);
                    
                    X64_Arg_Copy copy = {};
                    copy.type = var_args_type;
                    copy.dest = dest;
                    copy.src = var_arg_storage;
                    
                    array_push(copy_args, copy);
                    arg_index++;
                }
            }
        }
        
        if (arg_index < array_count(t_func->arg_types)) {
            for_array_v(t_func->default_args, arg, index) {
                int default_arg_index = t_func->first_default_arg_index + index;
                if (default_arg_index >= arg_index) {
                    
                    Type* type = arg->type;
                    Ic_Raw_Type rt = convert_type_to_raw_type(arg->type);
                    s64 disp = stk_args;
                    stk_args += 8;
                    Ic_Arg dest = ic_stk(rt, disp, IcStkArea_Args);
                    
                    X64_Arg_Copy copy = {};
                    copy.type = arg->type;
                    copy.expr = arg;
                    copy.dest = dest;
                    array_push(copy_args, copy);
                }
            }
            
        }
    }
    
    // Store arguments according to the windows calling convention
    for_array_reverse(copy_args, arg, arg_index) {
        if (arg_index == 0) {
            // TODO(Alexander): temporary hack to prevent RDX from getting overwritten
            cu->data_reg = X64_RCX;
        }
        
        
        Ic_Arg src;
        if (arg->src.type) {
            src = arg->src;
        } else{
            if (arg->expr) {
                src = convert_expr_to_intermediate_code(cu, arg->expr);
            } else {
                src = ic_push_local(cu, arg->type);
                arg->dest = src;
                result = src;
            }
        }
        
        Ic_Arg dest;
        if (arg_index < 4) {
            if (src.type & IC_FLOAT) {
                dest = ic_reg(src.raw_type, float_arg_registers_ccall_windows[arg_index]);
            } else {
                dest = ic_reg(src.raw_type, int_arg_registers_ccall_windows[arg_index]);
            }
        } else {
            dest = arg->dest;
        }
        
        convert_assign_to_intermediate_code(cu, arg->type, dest, src, false);
        
        if (t_func->is_variadic && arg_index < 4) {
            
            if (src.type & IC_FLOAT) {
                Intermediate_Code* ic = ic_add(cu, IC_REINTERP_F2S);
                ic->src0 = ic_reg(IC_S64, int_arg_registers_ccall_windows[arg_index]);
                ic->src1 = dest;
            }
            
            if (arg_index >= array_count(t_func->arg_types)) {
                ic_mov(cu, arg->dest, dest);
            }
        }
    }
    
    // TODO(Alexander): RESET temporary hack to prevent RDX from getting overwritten
    cu->data_reg = X64_RDX;
    
    cu->stk_args = max(cu->stk_args, stk_args);
    // NOTE(Alexander): make sure to allocate space for HOME registers
    // even if they aren't used!
    cu->stk_args = max(cu->stk_args, 4*8);
    
    Ic_Arg ptr = {};
    
    if (!t_func->unit) {
        if (t_func->intrinsic) {
            ptr = ic_imm(IC_S64, (s64) t_func->intrinsic);
        } else if (call_ident) {
            Ic_Arg addr = convert_expr_to_intermediate_code(cu, call_ident);
            if (addr.type & IC_DISP_STK_RIP) {
                ptr = ic_reg(addr.raw_type, X64_RAX);
                ic_mov(cu, ptr, addr);
            }
        }
    }
    
    Ic_Call* ic = ic_call(cu);
    ic->func = t_func->unit;
    ic->ptr = ptr;
    
    return result;
}

Ic_Arg
convert_binary_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr, Ic_Arg prev_result={}) {
    assert(expr && expr->kind == Ast_Binary_Expr && "not a binary expression");
    
    if (expr->Binary_Expr.overload) {
        
        array(Ast*)* args = 0;
        array_push(args, expr->Binary_Expr.first);
        array_push(args, expr->Binary_Expr.second);
        
        return convert_function_call_to_intermediate_code(cu, expr->Binary_Expr.overload, args, 0);
    }
    
    Ic_Arg result;
    
    Operator op = expr->Binary_Expr.op;
    
    if (op == Op_Logical_Or) {
        Intermediate_Code* test_lhs;
        Intermediate_Code* test_rhs;
        Ic_Basic_Block* bb_true = ic_basic_block();
        Ic_Basic_Block* bb_exit = ic_basic_block();
        
        Ic_Arg src0 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.first);
        result = ic_reg_mov(cu, X64_RAX, src0);
        test_lhs = ic_add(cu, IC_CMP);
        test_lhs->src0 = result;
        test_lhs->src1 = ic_imm(result.raw_type, 0);
        ic_jump(cu, IC_JNE, bb_true);
        
        Ic_Arg src1 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.second);
        ic_reg_mov(cu, X64_RAX, src1);
        test_rhs = ic_add(cu, IC_CMP);
        test_rhs->src0 = result;
        test_rhs->src1 = ic_imm(result.raw_type, 0);
        ic_jump(cu, IC_JNE, bb_true);
        
        // false
        ic_mov(cu, result, ic_imm(result.raw_type, 0));
        ic_jump(cu, IC_JMP, bb_exit);
        
        // true
        ic_label(cu, bb_true);
        ic_mov(cu, result, ic_imm(result.raw_type, 1));
        ic_jump(cu, IC_JMP, bb_exit);
        ic_label(cu, bb_exit);
        
        assert(src0.raw_type == src1.raw_type);
        
    } else if (op == Op_Logical_And) {
        Intermediate_Code* test_lhs, *test_rhs;
        Ic_Basic_Block* bb_false = ic_basic_block();
        Ic_Basic_Block* bb_exit = ic_basic_block();
        
        Ic_Arg src0 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.first);
        result = ic_reg_mov(cu, X64_RAX, src0);
        test_lhs = ic_add(cu, IC_CMP);
        test_lhs->src0 = result;
        test_lhs->src1 = ic_imm(result.raw_type, 0);
        ic_jump(cu, IC_JE, bb_false);
        
        Ic_Arg src1 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.second);
        ic_reg_mov(cu, X64_RAX, src1);
        test_rhs = ic_add(cu, IC_CMP);
        test_rhs->src0 = result;
        test_rhs->src1 = ic_imm(result.raw_type, 0);
        ic_jump(cu, IC_JE, bb_false);
        
        // true
        ic_mov(cu, result, ic_imm(result.raw_type, 1));
        ic_jump(cu, IC_JMP, bb_exit);
        
        // false
        ic_label(cu, bb_false);
        ic_mov(cu, result, ic_imm(result.raw_type, 0));
        ic_jump(cu, IC_JMP, bb_exit);
        ic_label(cu, bb_exit);
        
        //#if BUILD_DEBUG
        //if (src0.raw_type != src1.raw_type) {
        //pln("Detected incompatible types - AST:%", f_ast(expr));
        //assert(0 && src0.raw_type == src1.raw_type);
        //}
        //#endif
        
    } else {
        //Ic_Arg clobber;
        Ic_Arg src0, src1;
        
        if (op == Op_Assign) {
            src0 = {};
        } else {
            Ast* first = expr->Binary_Expr.first;
            if (first->kind == Ast_Binary_Expr) { 
                src0 = convert_binary_expr_to_intermediate_code(cu, first, prev_result);
            } else {
                src0 = convert_expr_to_intermediate_code(cu, first);
            }
        }
        Intermediate_Code* ic_src0 = cu->ic_last;
        
        {
            Ast* second = expr->Binary_Expr.second;
            if (second->kind == Ast_Binary_Expr) { 
                src1 = convert_binary_expr_to_intermediate_code(cu, second, prev_result);
            } else {
                src1 = convert_expr_to_intermediate_code(cu, second);
            }
        }
        
        Type* first_type = expr->Binary_Expr.first->type;
        if (first_type->kind == TypeKind_Pointer && (op == Op_Add || 
                                                     op == Op_Subtract ||
                                                     op == Op_Add_Assign || 
                                                     op == Op_Subtract_Assign)) {
            // Pointer arithmetic
            //pln("Pointer arithmetic AST:\n%", f_ast(expr));
            int sizeof_elem = first_type->Pointer->size;
            if (sizeof_elem != 1) {
                Intermediate_Code* ic = ic_add(cu, IC_MUL);
                ic->dest = ic_reg(IC_S64);
                ic->src0 = src1;
                ic->src1 = ic_imm(IC_S64, sizeof_elem);
                if (is_power_of_two(sizeof_elem)) {
                    ic->opcode = IC_SHL;
                    ic->src1.disp = (int) sqrt(ic->src1.disp);
                }
                
                src1 = ic->dest;
                src1.raw_type = src0.raw_type;
            }
        } 
        
        src0 = ic_clobber_register(cu, ic_src0, expr->Binary_Expr.second->type, src0, src1);
        
        //#if BUILD_DEBUG
        //if (src0.raw_type != 0 && src0.raw_type != src1.raw_type) {
        //pln("AST:\n%", f_ast(expr));
        //assert(0);
        //}
        //#endif
        
        bool isFloat = src0.type & IC_FLOAT;
        
        bool is_assign = operator_is_assign(op);
        if (is_assign) {
            switch (op) {
                case Op_Assign: op = Op_None; break;
                case Op_Add_Assign: op = Op_Add; break;
                case Op_Subtract_Assign: op = Op_Subtract; break;
                case Op_Multiply_Assign: op = Op_Multiply; break;
                case Op_Divide_Assign: op = Op_Divide; break;
                case Op_Modulo_Assign: op = Op_Modulo; break;
                case Op_Bitwise_And_Assign: op = Op_Bitwise_And; break;
                case Op_Bitwise_Or_Assign: op = Op_Bitwise_Or; break;
                case Op_Bitwise_Xor_Assign: op = Op_Bitwise_Xor; break;
                case Op_Shift_Left_Assign: op = Op_Shift_Left; break;
                case Op_Shift_Right_Assign: op = Op_Shift_Right; break;
                
                default: assert(0 && "invalid assign op");
            }
        }
        
        
        if (op == Op_None) {
            result = src1;
        } else {
            // Make sure STK operand size is 32- or 64-bit for src0 and src1
            if (src0.type & IC_STK && src0.type & (IC_T8 | IC_T16)) {
                Ic_Arg tmpt = ic_reg((src0.type & ~(IC_T8 | IC_T16)) | IC_T32, X64_RCX);
                ic_mov(cu, tmpt, src0);
                src0 = tmpt;
            }
            if (src1.type & IC_STK && src1.type & (IC_T8 | IC_T16)) {
                Ic_Arg tmpt = ic_reg((src1.type & ~(IC_T8 | IC_T16)) | IC_T32, X64_RAX);
                ic_mov(cu, tmpt, src1);
                src1 = tmpt;
            }
            
            Intermediate_Code* ic = ic_add(cu, IC_NOOP);
            ic->dest = ic_reg(src0.raw_type);
            ic->src0 = src0;
            ic->src1 = src1;
            result = ic->dest;
            result.raw_type = convert_type_to_raw_type(expr->type);
            
            if (operator_is_comparator(op)) {
                result.raw_type = IC_U8;
                ic->opcode = isFloat ? IC_FCMP : IC_CMP;
                ic->dest = result;
                
                ic = ic_add(cu, IC_NOOP);
                ic->dest = result;
            }
            
            bool isUnsignedIntOrFloat = src0.type & (IC_FLOAT | IC_UINT);
            switch (op) {
                case Op_Add: ic->opcode = isFloat ? IC_FADD : IC_ADD; break;
                case Op_Subtract: ic->opcode = isFloat ? IC_FSUB : IC_SUB; break;
                case Op_Multiply: ic->opcode = isFloat ? IC_FMUL : IC_MUL; break;
                case Op_Divide: ic->opcode = isFloat ? IC_FDIV : IC_DIV; break;
                case Op_Modulo: ic->opcode = isFloat ? IC_FMOD : IC_MOD; break;
                case Op_Equals: ic->opcode = IC_SETE; break;
                case Op_Bitwise_And: ic->opcode = IC_AND; break;
                case Op_Bitwise_Or: ic->opcode = IC_OR; break;
                case Op_Bitwise_Xor: ic->opcode = IC_XOR; break;
                case Op_Not_Equals: ic->opcode = IC_SETNE; break;
                case Op_Shift_Left: ic->opcode = IC_SHL; break;
                case Op_Shift_Right: ic->opcode = IC_SHR; break;
                case Op_Greater_Than: ic->opcode = isUnsignedIntOrFloat ? IC_SETA : IC_SETG; break;
                case Op_Greater_Equals: ic->opcode = isUnsignedIntOrFloat ? IC_SETAE : IC_SETGE; break;
                case Op_Less_Than: ic->opcode = isUnsignedIntOrFloat ? IC_SETB : IC_SETL; break;
                case Op_Less_Equals: ic->opcode = isUnsignedIntOrFloat ? IC_SETBE : IC_SETLE; break;
                case Op_None: break;
                
                default: unimplemented;
            }
        }
        
        if (is_assign) {
            Intermediate_Code* ic_curr = cu->ic_last;
            src0 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.first);
            src1 = result;
            
#if BUILD_DEBUG
            if (!(src0.raw_type == src1.raw_type || src1.raw_type == 0)) {
                pln("AST:\n%", f_ast(expr));
                assert(0);
            }
#endif
            
            src1 = ic_clobber_register(cu, ic_curr, expr->Binary_Expr.first->type, src1, src0);
            
            
            //if (src0.type & IC_DISP) {
            //pln("%", f_ast(expr));
            //}
            
            convert_assign_to_intermediate_code(cu, expr->type, src0, src1, true);
        }
    }
    
    return result;
}

inline void
convert_ic_to_conditional_jump(Compilation_Unit* cu, Intermediate_Code* code, Ic_Arg cond, Ic_Basic_Block* false_target) {
    if (code && ic_is_setcc(code->opcode)) {
        // NOTE(Alexander): we always invert the jump conditions for convenience
        switch (code->opcode) {
            case IC_SETE: code->opcode = IC_JNE; break;
            case IC_SETNE: code->opcode = IC_JE; break;
            case IC_SETG: code->opcode = IC_JLE; break;
            case IC_SETGE: code->opcode = IC_JL; break;
            case IC_SETL: code->opcode = IC_JGE; break;
            case IC_SETLE: code->opcode = IC_JG; break;
            case IC_SETA: code->opcode = IC_JBE; break;
            case IC_SETAE: code->opcode = IC_JB; break;
            case IC_SETB: code->opcode = IC_JAE; break;
            case IC_SETBE: code->opcode = IC_JA; break;
            
            default: unimplemented;
        }
        code->data = false_target;
    } else {
        if (cond.type == 0) {
            __debugbreak();
        }
        
        Intermediate_Code* cmp = ic_add(cu, IC_CMP);
        cmp->src0 = cond;
        cmp->src1 = ic_imm(IC_S8, 0);
        ic_jump(cu, IC_JE, false_target);
    }
}

Ic_Arg
convert_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr) {
    Ic_Arg result = {};
    
    switch (expr->kind) {
        case Ast_None: {
        } break;
        
        
        case Ast_Value: {
            // TODO(Alexander): for floating we need to allocate it on rdata section
            //void* data = arena_push_size(&cu->data_packer->rdata_arena, type->size, type->align);
            //u32 relative_ptr = (u32) arena_relative_pointer(&cu->data_packer->rdata_arena, data);
            
        } break;
        
        case Ast_Ident: {
            assert(expr->type);
            Type* type = expr->type;
            
            string_id ident = ast_unwrap_ident(expr);
            if (map_key_exists(cu->locals, ident)) {
                result = map_get(cu->locals, ident);
                
            } else {
                if (type->kind == TypeKind_Function && type->Function.unit) {
                    
                    Intermediate_Code* ic = ic_add(cu, IC_LPROC);
                    ic->data = type->Function.unit;
                    ic->dest = ic_reg(IC_T64);
                    result = ic->dest;
                    
                } else {
                    // Global variable
                    if (map_key_exists(cu->globals, ident)) {
                        result = map_get(cu->globals, ident);
                        
                    } else {
                        
                        Ic_Raw_Type raw_type = convert_type_to_raw_type(type);
                        void* data = get_interp_value_pointer(cu->interp, ident);
                        if (!data) {
                            Type_Context tcx = {};
                            type_error(&tcx, string_print("compiler bug: value of `%` is void", f_var(ident)), expr->span);
                            assert(0);
                        }
                        
                        void* dest = arena_push_size(&cu->data_packer->data_arena, type->size, type->align);
                        u32 relative_ptr = (u32) arena_relative_pointer(&cu->data_packer->data_arena, dest);
                        memcpy(dest, data, type->size);
                        
                        result = ic_rip_disp32(cu, raw_type, IcDataArea_Globals, dest, relative_ptr);
                        map_put(cu->globals, ident, result);
                    }
                }
            }
        } break;
        
        case Ast_Paren_Expr: {
            result = convert_expr_to_intermediate_code(cu, expr->Paren_Expr.expr);
        } break;
        
        case Ast_Aggregate_Expr: {
            Type* type = expr->type;
            
            // TODO(Alexander): OPTIMIZATION - we should store this in read only data!!!
            // The non-constant fields shouldn't be set here!!!
            
            Exported_Data base = {};
            if (is_valid_ast(expr->Aggregate_Expr.elements->Compound.node)) {
                base = export_size(cu->data_packer, Data_Section, type->size, type->align);
                convert_aggregate_literal_to_memory(expr, base.data);
            }
            
            
            if (base.data) {
                result = ic_rip_disp32(cu, IC_T64, IcDataArea_Type_Info, 
                                       base.data, base.relative_ptr);
                
                // Write to non-constant fields
                Ic_Arg result_dest = {};
                int field_index = (int) expr->Aggregate_Expr.first_index;
                for_compound(expr->Aggregate_Expr.elements, field) {
                    assert(field->kind == Ast_Argument);
                    
                    string_id ident = try_unwrap_ident(field->Argument.ident);
                    Ast* assign = field->Argument.assign;
                    
                    Struct_Field_Info info = {};
                    if (type->kind == TypeKind_Struct || type->kind == TypeKind_Union)  {
                        if (ident) {
                            info = get_field_info(&type->Struct_Like, ident);
                        } else {
                            info = get_field_info_by_index(&type->Struct_Like, field_index);
                        }
                    } else if (type->kind == TypeKind_Array) {
                        info.type = type->Array.type;
                        info.offset = field_index * info.type->size;
                    } else {
                        compiler_bug("unexpected aggregate type");
                    }
                    
                    // TODO(Alexander): make it possible to store dynamic things
                    
                    if (assign->kind == Ast_Value && assign->Value.type == Value_string) {
                        string_id str_id = vars_save_string(assign->Value.data.str);
                        Exported_Data exported = export_string(cu->data_packer, str_id);
                        push_relocation(cu->data_packer, add_offset(base, (int) info.offset), exported);
                        *((string*) ((u8*) base.data + info.offset)) = exported.str;
                        //pln("export: % (%)", f_string(assign->Value.data.str), f_u32(result.data.disp + (u32) info.offset));
                    }
                    
                    
                    if (assign->kind != Ast_Value && 
                        assign->kind != Ast_Aggregate_Expr) {
                        //pln("Adding non-constant field: %", f_ast(assign));
                        
                        Ic_Arg src = convert_expr_to_intermediate_code(cu, assign);
                        Ic_Arg dest = result;
                        dest.raw_type = src.raw_type;
                        dest.data.disp += (u32) info.offset;
                        ic_mov(cu, dest, src);
                    }
                    
                    field_index++;
                }
            }
        } break;
        
        case Ast_Field_Expr: {
            Type* type = expr->Field_Expr.var->type;
            string_id ident = ast_unwrap_ident(expr->Field_Expr.field);
            
            if (type->kind == TypeKind_Enum) {
                Ic_Raw_Type raw_type = convert_type_to_raw_type(type->Enum.type);
                Value value = map_get(type->Enum.values, ident);
                assert(!is_void(value));
                result = ic_imm(raw_type, value.data.signed_int);
                
            } else {
                result = convert_expr_to_intermediate_code(cu, expr->Field_Expr.var);
                assert(result.type & (IC_STK_RIP));
                
                if (type->kind == TypeKind_Pointer) {
                    Ic_Arg tmp = ic_reg(IC_T64, cu->data_reg);
                    ic_mov(cu, tmp, result);
                    result = ic_stk(tmp.raw_type, 0, IcStkArea_None, tmp.reg);
                    
                    type = type->Pointer;
                }
                
                if (type->kind == TypeKind_Struct || type->kind == TypeKind_Union) {
                    Struct_Field_Info info = get_field_info(&type->Struct_Like, ident);
                    result.data.disp += (u32) info.offset;
                    result.raw_type = convert_type_to_raw_type(expr->type);
                    
                    
                } else if (type->kind == TypeKind_Array || type == t_string) {
                    // TODO(Alexander): this has hardcoded sizes and types for now
                    switch (ident) {
                        case Sym_data: {
                            result.raw_type = IC_T64;
                            if (type->Array.is_inplace) {
                                Ic_Arg tmp = ic_reg(result.raw_type, X64_RAX);
                                ic_lea(cu, tmp, result);
                                result = tmp;
                            }
                        } break;
                        
                        case Sym_count: {
                            result.raw_type = result.type & IC_STK ? IC_S64 : IC_T64;
                            result.disp += 8;
                        } break;
                        
                        case Sym_capacity: {
                            result.raw_type = result.type & IC_STK ? IC_S64 : IC_T64;
                            result.disp += 16;
                        } break;
                        
                        
                        default: {
                            assert(0 && "invalid field");
                        } break;
                    }
                } else {
                    assert(0 && "invalid type for field expr");
                }
            }
        } break;
        
        case Ast_Index_Expr: {
            Type* type = expr->type;
            Type* index_type = expr->Index_Expr.index->type;
            Type* array_type = expr->Index_Expr.array->type;
            
            Ic_Arg index = convert_expr_to_intermediate_code(cu, expr->Index_Expr.index);
            if (!(index.type & IC_REG && index.reg == X64_RAX)) {
                Ic_Arg tmp = ic_reg(convert_type_to_raw_type(index_type), X64_RAX);
                ic_mov(cu, tmp, index);
                index = tmp;
            }
            
            if (type->size > 1) {
                Intermediate_Code* mul_ic = ic_add(cu, IC_MUL);
                mul_ic->dest = index;
                mul_ic->src0 = index;
                mul_ic->src1 = ic_imm(IC_S64, type->size);
            }
            
            Intermediate_Code* ic_curr = cu->ic_last;
            Ic_Arg arr = convert_expr_to_intermediate_code(cu, expr->Index_Expr.array);
            if (!(arr.type & IC_REG && arr.reg == cu->data_reg)) {
                Ic_Arg tmp = ic_reg(convert_type_to_raw_type(array_type), cu->data_reg);
                
                if (array_type->kind == TypeKind_Array && array_type->Array.is_inplace) {
                    ic_lea(cu, tmp, arr);
                } else {
                    ic_mov(cu, tmp, arr);
                }
                arr = tmp;
            }
            index = ic_clobber_register(cu, ic_curr, array_type, index, arr);
            
            Intermediate_Code* add_ic = ic_add(cu, IC_ADD);
            add_ic->dest = arr;
            add_ic->src0 = arr;
            add_ic->src1 = index;
            
            result = ic_stk(convert_type_to_raw_type(type), 0, IcStkArea_None, arr.reg);
        } break;
        
        case Ast_Unary_Expr: {
            if (expr->Unary_Expr.overload) {
                
                array(Ast*)* args = 0;
                array_push(args, expr->Unary_Expr.first);
                
                return convert_function_call_to_intermediate_code(cu, expr->Unary_Expr.overload, args, 0);
            }
            
            Ic_Arg src = convert_expr_to_intermediate_code(cu, expr->Unary_Expr.first);
            Ic_Raw_Type rt = convert_type_to_raw_type(expr->type);
            Operator op = expr->Unary_Expr.op;
            
            switch (op) {
                case Op_Negate: {
                    assert(src.type & IC_STK | IC_REG);
                    
                    if (src.type & IC_FLOAT) {
                        Type* type = expr->type;
                        void* data;
                        u32 relative_ptr;
                        
                        if (type == t_f64) {
                            data = arena_push_size(&cu->data_packer->rdata_arena, type->size*2, type->align*2);
                            relative_ptr = (u32) arena_relative_pointer(&cu->data_packer->rdata_arena, data);
                            u64* values = (u64*) data;
                            for (int i = 0; i < 2; i++) *values++ = 0x8000000000000000ull;
                        } else {
                            data = arena_push_size(&cu->data_packer->rdata_arena, type->size*4, type->align*4);
                            relative_ptr = (u32) arena_relative_pointer(&cu->data_packer->rdata_arena, data);
                            u32* values = (u32*) data;
                            for (int i = 0; i < 4; i++) *values++ = 0x80000000;
                        }
                        
                        Intermediate_Code* ic = ic_add(cu, IC_FXOR);
                        ic->dest = ic_reg(rt);
                        ic->src0 = src;
                        ic->src1 = ic_rip_disp32(cu, rt, IcDataArea_Read_Only, data, relative_ptr);
                        result = ic->dest;
                        
                    } else {
                        Intermediate_Code* ic = ic_add(cu, IC_NEG);
                        ic->dest = ic_reg(rt);
                        ic->src0 = src;
                        result = ic->dest;
                    }
                } break;
                
                case Op_Logical_Not: {
                    assert(src.type & IC_STK | IC_REG);
                    
                    result = ic_reg(src.raw_type);
                    src = ic_reg_mov(cu, X64_RAX, src);
                    
                    Ic_Basic_Block* bb_unset = ic_basic_block();
                    Ic_Basic_Block* bb_exit = ic_basic_block();
                    
                    Intermediate_Code* ic = ic_add(cu, IC_CMP);
                    ic->src0 = src;
                    ic->src1 = ic_imm(src.raw_type, 0);
                    
                    ic_jump(cu, IC_JNE, bb_unset);
                    ic_mov(cu, result, ic_imm(rt, 1));
                    ic_jump(cu, IC_JMP, bb_exit);
                    
                    ic_label(cu, bb_unset);
                    ic_mov(cu, result, ic_imm(rt, 0));
                    
                    ic_label(cu, bb_exit);
                } break;
                
                case Op_Bitwise_Not: {
                    assert(src.type & IC_STK | IC_REG);
                    
                    Intermediate_Code* ic = ic_add(cu, IC_NOT);
                    ic->dest = ic_reg(rt);
                    ic->src0 = src;
                    result = ic->dest;
                } break;
                
                case Op_Address_Of: {
                    Type* first_type = expr->Unary_Expr.first->type;
                    if (first_type && first_type->kind == TypeKind_Function) {
                        result = src;
                        
                    } else {
                        assert(src.type & IC_DISP_STK_RIP);
                        
                        Intermediate_Code* ic = ic_add(cu, IC_LEA);
                        ic->src0 = ic_reg(IC_T64);
                        ic->src1 = src;
                        result = ic->src0;
                    }
                } break;
                
                case Op_Dereference: {
                    if (src.type & IC_REG) {
                        result = ic_stk(rt, 0, IcStkArea_None, src.reg);
                        //Intermediate_Code* ic = ic_add(cu, IC_MOV);
                        //ic->src0 = ic_reg(rt, cu->data_reg);
                        //ic->src1 = src;
                        //result = ic->src0;
                    } else {
                        Intermediate_Code* ic = ic_add(cu, IC_MOV);
                        ic->src0 = ic_reg(IC_T64, cu->data_reg);
                        ic->src1 = src;
                        result= ic_stk(rt, 0, IcStkArea_None, ic->src0.reg);
                        
                        //ic = ic_add(cu, IC_MOV);
                        //ic->src0 = ic_reg(rt, cu->data_reg);
                        //ic->src1 = src;
                        //result = ic->src0;
                    }
                } break;
                
                case Op_Pre_Increment: {
                    result = ic_inc(cu, expr->type, src, IC_INC, IC_ADD, false);
                } break;
                
                case Op_Pre_Decrement: {
                    result = ic_inc(cu, expr->type, src, IC_DEC, IC_SUB, false);
                } break;
                
                case Op_Post_Increment: {
                    result = ic_inc(cu, expr->type, src, IC_INC, IC_ADD, true);
                } break;
                
                case Op_Post_Decrement: {
                    result = ic_inc(cu, expr->type, src, IC_DEC, IC_SUB, true);
                } break;
                
                default: assert(0 && "invalid unary operator");
            }
            
        } break;
        
        case Ast_Binary_Expr: {
            result = convert_binary_expr_to_intermediate_code(cu, expr);
        } break;
        
        case Ast_Ternary_Expr: {
            Ic_Basic_Block* bb_else = ic_basic_block();
            Ic_Basic_Block* bb_exit = ic_basic_block();
            
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, expr->Ternary_Expr.first);
            convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_else);
            
            Ic_Arg value = convert_expr_to_intermediate_code(cu, expr->Ternary_Expr.second);
            result = ic_reg(value.raw_type);
            convert_assign_to_intermediate_code(cu, expr->type, result, value, false);
            ic_jump(cu, IC_JMP, bb_exit);
            
            ic_label(cu, bb_else);
            value = convert_expr_to_intermediate_code(cu, expr->Ternary_Expr.third);
            convert_assign_to_intermediate_code(cu, expr->type, result, value, false);
            
            ic_label(cu, bb_exit);
        } break;
        
        case Ast_Call_Expr: {
            array(Ast*)* args = 0;
            
            // Setup arguments
            int arg_count = 0;
            for_compound(expr->Call_Expr.args, arg) {
                assert(arg->kind == Ast_Argument);
                array_push(args, arg->Argument.assign);
                arg_count++;
            }
            
            Type* function_type = expr->Call_Expr.function_type;
            result = convert_function_call_to_intermediate_code(cu, function_type, args, 
                                                                expr->Call_Expr.ident,
                                                                expr->Call_Expr.var_args);
        } break;
        
        case Ast_Cast_Expr: {
            assert(expr->Cast_Expr.expr);
            Type* t_dest = expr->type;
            Type* t_src = expr->Cast_Expr.expr->type;
            
            Ic_Arg src = convert_expr_to_intermediate_code(cu, expr->Cast_Expr.expr);
            result = src;
            result.raw_type = convert_type_to_raw_type(expr->type);
            
            if (result.type & IC_DISP) {
                return result;
            }
            
            if (t_src->kind == TypeKind_Pointer) {
                t_src = t_s64;
            }
            
            if (t_dest->kind == TypeKind_Pointer) {
                t_dest = t_s64;
            }
            
            if (t_src->kind == TypeKind_Enum) { 
                t_src = t_src->Enum.type;
            }
            
            if (t_dest->kind == TypeKind_Enum) { 
                t_dest = t_dest->Enum.type;
            }
            
            if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
                if (t_src->Basic.flags & BasicFlag_Integer) {
                    if (t_dest->Basic.flags & BasicFlag_Boolean) {
                        result = ic_reg(IC_U8);
                        
                        Ic_Basic_Block* bb_else = ic_basic_block();
                        Ic_Basic_Block* bb_exit = ic_basic_block();
                        
                        convert_ic_to_conditional_jump(cu, cu->ic_last, src, bb_else);
                        ic_mov(cu, result, ic_imm(IC_U8, 1));
                        ic_jump(cu, IC_JMP, bb_exit);
                        ic_label(cu, bb_else);
                        ic_mov(cu, result, ic_imm(IC_U8, 0));
                        ic_label(cu, bb_exit);
                        
                    } else if (t_dest->Basic.flags & BasicFlag_Integer) {
                        if (t_dest->size > t_src->size) {
                            Intermediate_Code* ic = ic_add(cu, 
                                                           t_src->Basic.flags & BasicFlag_Unsigned ? 
                                                           IC_MOVZX : IC_MOVSX);
                            ic->src0 = ic_reg(result.raw_type);
                            ic->src1 = src;
                            result = ic->src0;
                        }
                        
                    } else if (t_dest->Basic.flags & BasicFlag_Floating) {
                        if (t_src->size < 4) {
                            Intermediate_Code* ic_ext = ic_add(cu, 
                                                               t_src->Basic.flags & BasicFlag_Unsigned ? 
                                                               IC_MOVZX : IC_MOVSX);
                            ic_ext->src0 = ic_reg(t_src->Basic.flags & BasicFlag_Unsigned ? 
                                                  IC_U32 : IC_S32);
                            ic_ext->src1 = src;
                            src = ic_ext->src0;
                        }
                        
                        Intermediate_Code* ic = ic_add(cu, IC_CAST_S2F);
                        ic->src0 = ic_reg(result.raw_type);
                        ic->src1 = src;
                        result = ic->src0;
                    } else {
                        assert((t_dest->Basic.flags & BasicFlag_String) > 0 && "invalid type cast");
                    }
                    
                } else if (t_src->Basic.flags & BasicFlag_Floating) {
                    if (t_dest->Basic.flags & BasicFlag_Integer) {
                        Intermediate_Code* ic = ic_add(cu, IC_CAST_F2S);
                        ic->src0 = ic_reg(result.raw_type);
                        ic->src1 = src;
                        result = ic->src0;
                    } else if (t_dest->Basic.flags & BasicFlag_Floating) {
                        if (t_dest->size != t_src->size) {
                            Intermediate_Code* ic = ic_add(cu, IC_CAST_F2F);
                            ic->src0 = ic_reg(result.raw_type);
                            ic->src1 = src;
                            result = ic->src0;
                        }
                    } else {
                        assert(0 && "invalid type cast");
                    }
                } else if (t_src->Basic.kind == Basic_cstring) {
                } else if (t_src->Basic.kind == Basic_string) {
                    Ic_Arg tmp = ic_reg(IC_U64);
                    ic_mov(cu, tmp, src);
                    Intermediate_Code* ic_test = ic_add(cu, IC_TEST);
                    ic_test->src0 = tmp;
                    ic_test->src1 = tmp;
                    ic_add(cu, IC_SETNE);
                    result = ic_reg(result.raw_type);
                    
                } else {
                    unimplemented;
                }
            } else if (t_dest->kind == TypeKind_Array && t_src->kind == TypeKind_Array) {
                if (t_src->Array.capacity > 0) {
                    if (t_dest->Array.capacity == 0) {
                        // convert inplace array to "wide"-pointer array
                        // TODO(Alexander): we can improve this by e.g. clearing it and 
                        // also passing multiple values as arguments
                        result = ic_push_local(cu, t_dest);
                        Intermediate_Code* copy_ptr = ic_add(cu, IC_LEA);
                        copy_ptr->src0 = ic_reg(result.raw_type);
                        copy_ptr->src1 = src;
                        ic_mov(cu, result, copy_ptr->src0);
                        ic_mov(cu, ic_stk_offset(IC_S64, result, 8),
                               ic_imm(IC_S64, t_src->Array.capacity));
                        // TODO(Alexander): set capacity if dynamic array
                    }
                } else {
                    unimplemented;
                }
            }
            
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

void
convert_stmt_to_intermediate_code(Compilation_Unit* cu, Ast* stmt, Ic_Basic_Block* bb_break, Ic_Basic_Block* bb_continue) {
    switch (stmt->kind) {
        
        case Ast_Assign_Stmt: {
            
            
            // NOTE(Alexander): push local first then it can be found elsewhere
            if (stmt->Assign_Stmt.ident->kind == Ast_Ident) {
                
            } else if (stmt->Assign_Stmt.ident->kind == Ast_Compound) {
#if 0
                // TODO: add this to bytecode too
                for_compound(stmt->Assign_Stmt.ident, it) {
                    string_id ident = ast_unwrap_ident(it);
                    ic_push_local(cu, type, ident);
                    
                    it->type = type; // TODO(Alexander): isn't type checker doing this?
                    Intermediate_Code* ic_curr = cu->ic_last;
                    Ic_Arg dest = convert_expr_to_intermediate_code(cu, it);
                    src = ic_clobber_register(cu, ic_curr, type, src, dest);
                    convert_assign_to_intermediate_code(cu, stmt->type, dest, src, true);
                }
#endif
            } else {
                assert(0 && "invalid assign statment identifier");
            }
            
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                convert_stmt_to_intermediate_code(cu, it, bb_break, bb_continue);
            }
        } break;
        
        case Ast_Break_Stmt: {
            assert(bb_break);
            ic_jump(cu, IC_JMP, bb_break);
        } break;
        
        case Ast_Continue_Stmt: {
            assert(bb_continue);
            ic_jump(cu, IC_JMP, bb_continue);
        } break;
        
        case Ast_If_Stmt: {
            Ic_Basic_Block* bb_else = ic_basic_block();
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->If_Stmt.cond);
            convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_else);
            
            convert_stmt_to_intermediate_code(cu, stmt->If_Stmt.then_block, bb_break, bb_continue);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                Ic_Basic_Block* bb_exit = ic_basic_block();
                ic_jump(cu, IC_JMP, bb_exit);
                ic_label(cu, bb_else);
                convert_stmt_to_intermediate_code(cu, stmt->If_Stmt.else_block, bb_break, bb_continue);
                ic_label(cu, bb_exit);
            } else {
                ic_label(cu, bb_else);
            }
        } break;
        
        case Ast_For_Stmt: {
            Ic_Basic_Block* bb_enter = ic_basic_block();
            Ic_Basic_Block* bb_exit = ic_basic_block();
            Ic_Basic_Block* bb_update = ic_basic_block();
            
            convert_stmt_to_intermediate_code(cu, stmt->For_Stmt.init, bb_break, bb_continue);
            
            ic_label(cu, bb_enter);
            if (is_valid_ast(stmt->For_Stmt.cond)) {
                Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->For_Stmt.cond);
                convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_exit);
            }
            
            convert_stmt_to_intermediate_code(cu, stmt->For_Stmt.block, bb_exit, bb_update);
            
            ic_label(cu, bb_update);
            Ic_Arg src = convert_expr_to_intermediate_code(cu, stmt->For_Stmt.update);
            
            ic_jump(cu, IC_JMP, bb_enter);
            ic_label(cu, bb_exit);
        } break;
        
        case Ast_While_Stmt: {
            Ic_Basic_Block* bb_enter = ic_basic_block();
            Ic_Basic_Block* bb_exit = ic_basic_block();
            
            ic_label(cu, bb_enter);
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->While_Stmt.cond);
            convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_exit);
            
            convert_stmt_to_intermediate_code(cu, stmt->While_Stmt.block, bb_exit, bb_enter);
            ic_jump(cu, IC_JMP, bb_enter);
            ic_label(cu, bb_exit);
        } break;
        
        case Ast_Switch_Stmt: {
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->Switch_Stmt.cond);
            assert(cond.type & IC_STK_RIP);
            
            Ic_Basic_Block* bb_exit = ic_basic_block();
            Ic_Basic_Block* bb_curr_stmt = ic_basic_block();
            
            bool has_default_case = false;
            Ast* default_case_stmt = 0;
            
            for_compound(stmt->Switch_Stmt.cases, it) {
                assert(it->kind == Ast_Switch_Case);
                Ast* case_cond = it->Switch_Case.cond;
                if (case_cond && case_cond->kind == Ast_Value) {
                    // TODO(Alexander): we can optimize this by merging cases with same stmt
                    Intermediate_Code* ic_case = ic_add(cu, IC_SUB);
                    ic_case->dest = ic_reg(cond.raw_type);
                    ic_case->src0 = cond;
                    ic_case->src1 = ic_imm(cond.raw_type, case_cond->Value.data.signed_int);
                } else {
                    has_default_case = true;
                }
                
                Ic_Basic_Block* bb_next_case = ic_basic_block();
                if (it->Switch_Case.stmt) {
                    ic_jump(cu, IC_JNE, bb_next_case);
                    ic_label(cu, bb_curr_stmt);
                    if (has_default_case && !default_case_stmt) {
                        default_case_stmt = it->Switch_Case.stmt;
                    }
                    convert_stmt_to_intermediate_code(cu, it->Switch_Case.stmt, 
                                                      bb_exit, bb_continue);
                    ic_jump(cu, IC_JMP, bb_exit);
                    
                    bb_curr_stmt = ic_basic_block();
                } else {
                    ic_jump(cu, IC_JE, bb_curr_stmt);
                }
                
                ic_label(cu, bb_next_case);
            }
            
            if (default_case_stmt) {
                ic_label(cu, bb_curr_stmt);
                convert_stmt_to_intermediate_code(cu, default_case_stmt, 
                                                  bb_exit, bb_continue);
            }
            
            ic_label(cu, bb_exit);
            
        } break;
        
        case Ast_Return_Stmt: {
            // TODO(Alexander): this is platform/architecture specific
            Ic_Arg result = convert_expr_to_intermediate_code(cu, stmt->Return_Stmt.expr);
            if (cu->ic_return.type) {
                if (!((result.type & IC_REG) && (result.reg == 0) &&
                      (result.type & (IC_FLOAT | IC_SINT | IC_UINT)))) {
                    convert_assign_to_intermediate_code(cu, stmt->type, cu->ic_return, result, true);
                }
            }
            ic_jump(cu, IC_JMP, cu->bb_return);
        } break;
        
        default: unimplemented;
    }
}

void
convert_procedure_to_intermediate_code(Compilation_Unit* cu, bool insert_debug_break) {
    assert(cu->ast->type->kind == TypeKind_Function);
    assert(cu->ast->kind == Ast_Decl_Stmt);
    
    if (!cu->ast->Decl_Stmt.stmt) {
        return;
    }
    
    cu->data_reg = X64_RDX;
    
    Type_Function* proc = &cu->ast->type->Function;
    
    Ic_Basic_Block* bb_begin = ic_basic_block();
    Ic_Basic_Block* bb_end = ic_basic_block();
    cu->bb_return = bb_end;
    ic_label(cu, bb_begin);
    
    // Save home registers
    array(Ic_Arg)* home_register = 0;
    array(X64_Arg_Copy)* copy_args = 0;
    int arg_index = 0;
    
    if (proc->return_type) {
        Ic_Raw_Type rt = convert_type_to_raw_type(proc->return_type);
        
        if (proc->return_type->size > 8) {
            rt |= IC_SINT; // TODO(Alexander): strange way of disable ptr dereference
            array_push(home_register, ic_stk(rt, 8));
            cu->ic_return = ic_stk(rt, 0, IcStkArea_Caller_Args);
            arg_index++;
        } else {
            cu->ic_return = ic_reg(rt, X64_RAX); // NOTE: RAX == XMM0
        }
    }
    
    // Save HOME registers
    for_array(proc->arg_types, type, type_index) {
        Ic_Raw_Type rt = convert_type_to_raw_type(*type);
        s64 disp = arg_index*8;
        string_id ident = proc->arg_idents[type_index];
        
        if (arg_index < 4) {
            // None is used because this code is outside the stack frame
            Ic_Arg dest = ic_stk(rt, disp + 8, IcStkArea_None);
            array_push(home_register, dest);
        }
        Ic_Arg dest = ic_stk(rt, disp, IcStkArea_Caller_Args);
        
        int size = (*type)->size;
        if (size != 1 && size != 2 && size != 4 && size != 8) {
            Ic_Arg src = dest;
            src.type |= IC_SINT; // TODO(Alexander): strange way of disable ptr dereference
            
            X64_Arg_Copy copy = { *type, 0, src };
            copy.ident = ident;
            array_push(copy_args, copy);
        } else {
            map_put(cu->locals, ident, dest); 
        }
        
        arg_index++;
    }
    
    
    for_array_reverse(home_register, arg, reg_index) {
        if (reg_index < 4) {
            Ic_Arg src;
            if (arg->raw_type & IC_FLOAT) {
                src = ic_reg(arg->raw_type, float_arg_registers_ccall_windows[reg_index]);
            } else {
                src = ic_reg(arg->raw_type, int_arg_registers_ccall_windows[reg_index]);
            }
            ic_mov(cu, *arg, src);
        }
    }
    
    if (insert_debug_break) {
        ic_add(cu, IC_DEBUG_BREAK);
    }
    
    ic_add(cu, IC_PRLG);
    for_array(copy_args, c, _) {
        Ic_Arg dest = ic_push_local(cu, c->type, c->ident);
        convert_assign_to_intermediate_code(cu, c->type, dest, c->dest, true);
    }
    
    convert_stmt_to_intermediate_code(cu, cu->ast, 0, 0);
    ic_label(cu, bb_end);
    ic_add(cu, IC_EPLG);
    
    // Align stack by 16-bytes (excluding 8 bytes for return address)
    cu->stk_usage = cu->stk_locals + cu->stk_args + 8;
    cu->stk_usage = align_forward(cu->stk_usage, 16);
    cu->stk_locals = cu->stk_usage - (cu->stk_args);
    cu->stk_usage -= 8;
}
#endif

Ic_Raw_Type
convert_bytecode_type_to_x64(Bytecode_Type type) {
    switch (type) {
        case BytecodeType_i32: return IC_S32;
        case BytecodeType_i64: return IC_S64;
        case BytecodeType_f32: return IC_F32;
        case BytecodeType_f64: return IC_F64;
    }
    return 0;
}

Ic_Raw_Type
convert_bytecode_type_to_x64(Bytecode_Type type, int size) {
    switch (type) {
        case BytecodeType_i32: 
        case BytecodeType_i64: {
            switch (size) {
                case 1: return IC_S8;
                case 2: return IC_S16;
                case 4: return IC_S32;
                case 8: return IC_S64;
                default: unimplemented;
            }
        } break;
        
        case BytecodeType_f32: 
        case BytecodeType_f64: {
            switch (size) {
                case 4: return IC_F32;
                case 8: return IC_F64;
                default: unimplemented;
            }
        } break;
    }
    return 0;
}

Ic_Arg
convert_bytecode_operand_to_x64(X64_Assembler* x64, Buffer* buf, Bytecode_Operand op, Bytecode_Type type) {
    Ic_Raw_Type rt = convert_bytecode_type_to_x64(type);
    Ic_Arg result = {};
    switch (op.kind) {
        case BytecodeOperand_const_i32: {
            result.type = IC_DISP | rt;
            result.disp = op.const_i32; 
        } break;
        
        case BytecodeOperand_const_i64: {
            result.type = IC_DISP | rt;
            result.disp = op.const_i64; 
        } break;
        
        case BytecodeOperand_const_f32: {
            void* data = arena_push_struct(&x64->data_packer->rdata_arena, f32);
            u32 relative_ptr = (u32) arena_relative_pointer(&x64->data_packer->rdata_arena, data);
            *((f32*) data) = op.const_f32;
            result.type = IC_RIP_DISP32 | rt;
            if (x64->use_absolute_ptrs) {
                result.disp = (s64) data;
            } else {
                result.data.disp = relative_ptr;
                result.data.area = IcDataArea_Read_Only;
            }
        } break;
        
        case BytecodeOperand_const_f64: {
            void* data = arena_push_struct(&x64->data_packer->rdata_arena, f64);
            u32 relative_ptr = (u32) arena_relative_pointer(&x64->data_packer->rdata_arena, data);
            *((f64*) data) = op.const_f64;
            result.type = IC_RIP_DISP32 | rt;
            if (x64->use_absolute_ptrs) {
                result.disp = (s64) data;
            } else {
                result.data.disp = relative_ptr;
                result.data.area = IcDataArea_Read_Only;
            }
        } break;
        
        case BytecodeOperand_register: {
            result = x64->virtual_registers[op.register_index];
            if (!result.type) {
                // TODO(Alexander): only allocaing RAX for now!
                X64_Reg reg = (rt & IC_FLOAT) ? X64_XMM0 : X64_RAX;
                result = x64_alloc_register(x64, buf, op.register_index, reg, rt);
            }
            assert(result.type && "register not allocated");
        } break;
        
        case BytecodeOperand_stack: {
            result.type = IC_STK | rt;
            result.reg = X64_RSP; 
            
            result.disp = op.stack_offset + x64->stack_offsets[op.stack_index];
        } break;
        
        case BytecodeOperand_memory: {
            result.type = IC_RIP_DISP32 | rt;
            result.reg = X64_RIP;
            result.disp = op.memory_offset;
        } break;
    }
    
    return result;
}


void
convert_bytecode_function_to_x64_machine_code(X64_Assembler* x64, Bytecode_Function* func, Buffer* buf) {
    
    assert(array_count(x64->functions) > func->type_index && "allocate functions to x64");
    
    array_set_count(x64->labels, func->block_count + 1);
    x64->label_index = 1; // NOTE(Alexander): treat label 0 as end of function
    x64->functions[func->type_index] = buf->data + buf->curr_used;
    
    int register_count = (int) array_count(func->register_lifetimes);
    if (register_count > 0) {
        x64->virtual_registers = (Ic_Arg*) calloc(register_count, sizeof(Ic_Arg));
    }
    
    memset(&x64->registers, 0, sizeof(x64->registers));
    
    // Prologue
    x64_rex(buf, REX_FLAG_64_BIT); // sub rsp, stack_usage
    push_u8(buf, 0x81);
    x64_modrm(buf, IC_REG | IC_S64, 0, 5, X64_RSP, 0);
    u32* stack_usage = (u32*) (buf->data + buf->curr_used);
    push_u32(buf, 0);
    
    // Local stack
    x64->stack_offsets = (s32*) calloc(array_count(func->stack), sizeof(s32));
    for (int i = 0; i < array_count(func->stack); i++) {
        Stack_Entry stk = func->stack[i];
        x64->stack_usage = (s32) align_forward(x64->stack_usage, stk.align);
        x64->stack_offsets[i] = x64->stack_usage;
        x64->stack_usage += stk.size;
    }
    
    // Save HOME registers (TODO: move this it's windows calling only)
    Bytecode_Type* arg_types = (Bytecode_Type*) (func + 1);
    for (int  arg_index = (int) func->arg_count - 1; arg_index >= 0; arg_index--) {
        if (arg_index < 4) {
            Bytecode_Type arg_type = arg_types[arg_index];
            Ic_Raw_Type rt = convert_bytecode_type_to_x64(arg_type, func->stack[arg_index].size);
            if (rt & IC_FLOAT) {
                X64_Reg reg = float_arg_registers_ccall_windows[arg_index];
                x64_fmov(buf,
                         IC_STK | rt, X64_RSP, x64->stack_offsets[arg_index],
                         IC_REG | rt, reg, 0, 0);
            } else {
                X64_Reg reg = int_arg_registers_ccall_windows[arg_index];
                x64_mov(buf,
                        IC_STK | rt, X64_RSP, x64->stack_offsets[arg_index],
                        IC_REG | rt, reg, 0, 0);
            }
        }
    }
    
    // TODO(Alexander): handle multiple returns
    
    
    Bytecode_Instruction* curr = iter_bytecode_instructions(func, 0);
    x64->curr_bytecode_insn_index = 0;
    while (curr->kind) {
        convert_bytecode_insn_to_x64_machine_code(x64, buf, func, curr);
        curr = iter_bytecode_instructions(func, curr);
        x64->curr_bytecode_insn_index++;
    }
    
    // Align stack by 16-bytes (excluding 8 bytes for return address)
    x64->stack_usage += 8;
    x64->stack_usage = (u32) align_forward(x64->stack_usage, 16);
    //x64->stk_locals = cu->stk_usage - (cu->stk_args);
    x64->stack_usage -= 8;
    
    // Epilogue
    x64->labels[0] = buf->data + buf->curr_used;
    x64_rex(buf, REX_FLAG_64_BIT); // add rsp, stack_usage
    push_u8(buf, 0x81);
    x64_modrm(buf, IC_REG | IC_S64, 0, 0, X64_RSP, 0);
    push_u32(buf, x64->stack_usage);
    *stack_usage = x64->stack_usage;
    push_u8(buf, 0xC3); // RET near
    
    // Cleanup
    if (x64->virtual_registers) {
        free(x64->virtual_registers);
        x64->virtual_registers = 0;
    }
}

void 
convert_bytecode_insn_to_x64_machine_code(X64_Assembler* x64, Buffer* buf, 
                                          Bytecode_Function* func, 
                                          Bytecode_Instruction* insn) {
    s64 rip = 0;
    
    const int flag_floating = bit(31);
    int opcode = insn->opcode;
    if (insn->type == BytecodeType_f32 ||
        insn->type == BytecodeType_f64) {
        opcode |= flag_floating;
    }
    
    switch (opcode) {
        case BC_DEBUG_BREAK: {
            push_u8(buf, 0xCC);
        } break;
        
        case BC_CALL | flag_floating:
        case BC_CALL: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            Bytecode_Operand* args = (Bytecode_Operand*) (call + 1);
            Bytecode_Function* target = x64->bytecode->functions[call->func_index];
            Bytecode_Type* arg_types = (Bytecode_Type*) (target + 1);
            
            
            for (int i = (int) target->arg_count - 1; i >= 0; i--) {
                Ic_Arg arg = convert_bytecode_operand_to_x64(x64, buf, args[i], arg_types[i]);
                if (i < 4) {
                    // TODO(Alexander): spill float regs too
                    if (arg.type & IC_FLOAT) {
                        X64_Reg reg = float_arg_registers_ccall_windows[i];
                        x64_spill_register(x64, buf, reg);
                        x64_fmov(buf, 
                                 IC_REG | arg.raw_type, reg, 0,
                                 arg.type, arg.reg, arg.disp, rip);
                    } else {
                        X64_Reg reg = int_arg_registers_ccall_windows[i];
                        x64_spill_register(x64, buf, reg);
                        x64_mov(buf, 
                                IC_REG | arg.raw_type, reg, 0,
                                arg.type, arg.reg, arg.disp, rip);
                    }
                    
                } else {
                    x64_mov(buf, 
                            IC_STK | arg.raw_type, X64_RSP, x64->stack_usage + i*8,
                            arg.type, arg.reg, arg.disp, rip);
                }
            }
            x64->arg_stack_usage = max(x64->arg_stack_usage, ((s32) target->arg_count-4)*8);
            
            if (target->ret_count > 0) {
                Ic_Raw_Type rt = convert_bytecode_type_to_x64(insn->type);
                x64_alloc_register(x64, buf, args[target->arg_count].register_index, X64_RAX, rt);
            }
            
            // Direct function call
            
            // E8 cd 	CALL rel32 	D
            push_u8(buf, 0xE8);
            X64_Relocation reloc = {};
            reloc.from_ptr = (s32*) (buf->data + buf->curr_used);
            assert(array_count(x64->functions) > target->type_index);
            reloc.target = &x64->functions[target->type_index];
            array_push(x64->relocations, reloc);
            push_u32(buf, 0);
            
            //} else {
            // Indirect function call from pointer
            
            //if (call->ptr.type & IC_DISP) {
            //x64_mov_rax_u64(ic, (u64) call->ptr.disp);
            //}
            
            // FF /2 	CALL r/m64 	M 	
            //push_u8(buf, 0xFF);
            //x64_modrm(ic, IC_REG + IC_S64, 0, 2, X64_RAX, rip);
            //}
        } break;
        
        case BC_RETURN: {
            Ic_Arg val = convert_bytecode_operand_to_x64(x64, buf, bc_unary_first(insn), insn->type);
            assert(val.type);
            
            x64_mov(buf, 
                    IC_REG | val.raw_type, X64_RAX, 0,
                    val.type, val.reg, val.disp, rip);
            
            Bytecode_Block* next_insn = (Bytecode_Block*) iter_bytecode_instructions(func, insn);
            
            if (!(next_insn->opcode == BC_BLOCK && next_insn->label_index == 0)) {
                // jump to end of function
                push_u8(buf, 0xE9);
                x64_push_rel32(x64, buf, func, 0);
            }
        } break;
        
        case BC_MOV: {
            Bytecode_Operand first = bc_binary_first(insn);
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            
            x64_mov(buf, 
                    dest.type, dest.reg, dest.disp,
                    src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_MOV | flag_floating: {
            Bytecode_Operand first = bc_binary_first(insn);
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            
            x64_fmov(buf, 
                     dest.type, dest.reg, dest.disp,
                     src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_MOV_8: {
            Bytecode_Operand first = bc_binary_first(insn);
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            dest.type = (IC_TF_MASK & dest.type) | IC_S8;
            src.type = (IC_TF_MASK & src.type) | IC_S8;
            
            x64_mov(buf, 
                    dest.type, dest.reg, dest.disp,
                    src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_INC: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_unary_first(insn), insn->type);
            x64_unary(buf, first.type, first.reg, first.disp, 0xFF, 0, rip);
        } break;
        
        case BC_ADD: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       0, 0, rip);
        } break;
        
        case BC_ADD | flag_floating: {
            x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x58, rip);
        } break;
        
        case BC_SUB: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       5, 0x28, rip);
        } break;
        
        case BC_SUB | flag_floating: {
            x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x5C, rip);
        } break;
        
        case BC_MUL: {
            x64_mul(x64, buf, (Bytecode_Binary*) insn, rip);
        } break;
        
        case BC_MUL | flag_floating: {
            x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x59, rip);
        } break;
        
        case BC_DIV_S:
        case BC_DIV_U: {
            x64_div(x64, buf, (Bytecode_Binary*) insn, false, rip);
        } break;
        
        case BC_DIV_S | flag_floating:
        case BC_DIV_U | flag_floating: {
            x64_float_binary(x64, buf, (Bytecode_Binary*) insn, 0x5E, rip);
        } break;
        
        case BC_MOD_S:
        case BC_MOD_U: {
            x64_div(x64, buf, (Bytecode_Binary*) insn, true, rip);
        } break;
        
        case BC_AND: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       4, 0x20, rip);
        } break;
        
        case BC_OR: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       1, 0x8, rip);
        } break;
        
        case BC_XOR: {
            Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            x64_binary(buf,
                       first.type, first.reg, first.disp,
                       second.type, second.reg, second.disp,
                       6, 0x30, rip);
        } break;
        
        case BC_SHL: {
            x64_shr(x64, buf, (Bytecode_Binary*) insn, 4, rip);
        } break;
        
        case BC_SHR: {
            x64_shr(x64, buf, (Bytecode_Binary*) insn, 5, rip);
        } break;
        
        case BC_SAR: {
            x64_shr(x64, buf, (Bytecode_Binary*) insn, 7, rip);
        } break;
        
        case BC_WRAP_I64: {
            // noop
        } break;
        
        
        case BC_EXTEND_U8: {
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            src.type = (IC_TF_MASK & src.type) | IC_U8;
            
            x64_movzx(buf, 
                      dest.type, dest.reg, dest.disp,
                      src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_EXTEND_S8: {
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            src.type = (IC_TF_MASK & src.type) | IC_S8;
            
            x64_movsx(buf, 
                      dest.type, dest.reg, dest.disp,
                      src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_CONVERT_F32_S:
        case BC_CONVERT_F64_S: {
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            src.raw_type = (opcode == BC_CONVERT_F32_S) ? IC_F32 : IC_F64;
            
            x64_convert_float_to_int_type(buf, 
                                          dest.type, dest.reg, dest.disp,
                                          src.type, src.reg, src.disp, rip);
        } break;
        
        case BC_EQ:
        case BC_NEQ:
        case BC_GT_S:
        case BC_GT_U:
        case BC_GE_S:
        case BC_GE_U:
        case BC_LT_S:
        case BC_LT_U:
        case BC_LE_S:
        case BC_LE_U: {
            Ic_Arg dest = convert_bytecode_operand_to_x64(x64, buf, bc_binary_first(insn), insn->type);
            Ic_Arg src = convert_bytecode_operand_to_x64(x64, buf, bc_binary_second(insn), insn->type);
            
            // cmp
            x64_binary(buf,
                       dest.type, dest.reg, dest.disp,
                       src.type, src.reg, src.disp, 7, 0x38, rip);
            
            Bytecode_Branch* branch = (Bytecode_Branch*) ((u8*) insn + insn->next_insn);
            if (branch->opcode == BC_BRANCH) {
                // jcc
                push_u16(buf, x64_jcc_opcodes[BC_NEQ - insn->opcode]);
                x64_push_rel32(x64, buf, func, branch->label_index);
            } else {
                // setcc
                push_u24(buf, x64_setcc_opcodes[insn->opcode - BC_EQ]); 
            }
        } break;
        
        case BC_BRANCH: {
            // NOTE(Alexander): conditional branch is handled by operations above
            Bytecode_Branch* branch = (Bytecode_Branch*) insn;
            if (!branch->cond.kind) {
                push_u8(buf, 0xE9);
                x64_push_rel32(x64, buf, func, branch->label_index);
            }
        } break;
        
        case BC_LOOP:
        case BC_BLOCK: {
            x64->labels[x64->label_index] = buf->data + buf->curr_used;
            X64_Block block = {};
            block.label_index = x64->label_index++;
            block.is_loop = opcode == BC_LOOP;
            array_push(x64->block_stack, block);
        } break;
        
        case BC_END: {
            X64_Block block = array_pop(x64->block_stack);
            if (!block.is_loop) {
                x64->labels[block.label_index] = buf->data + buf->curr_used;
            }
        } break;
        
        default: {
            assert(0 && "invalid bytecode instruction");
        } break;
    }
}



inline void
x64_sib(Buffer* buf, u8 scale, u8 index, u8 base) {
    push_u8(buf, scale << 6 | index << 3 | base);
}

void
x64_rip_relative(Buffer* buf, s64 r, s64 data, s64 rip) {
    push_u8(buf, ((u8) (r&7)<<3) | (u8) X64_RBP);
    
    //X64_Relocation reloc = {};
    //reloc.from_ptr = (u32*) (buf->data + buf->curr_used);
    //reloc.target = 
    
    u8* x64_machine_code_ptr = buf->data + buf->curr_used;
    s64 disp = data - (s64) x64_machine_code_ptr - 4;
    
    push_u32(buf, (u32) disp);
}

inline void
x64_modrm(Buffer* buf, Ic_Type t, s64 d, s64 r, s64 rm, s64 rip) {
    r = r % 16; rm = rm % 16;
    
    if (t & IC_STK) {
        if (d < S8_MIN || d > S8_MAX) {
            push_u8(buf, MODRM_INDIRECT_DISP32 | (((u8) r&7)<<3) | (u8) rm&7);
            if (rm == X64_RSP) {
                push_u8(buf, (u8) (rm << 3) | (u8) rm);
            }
            push_u32(buf, (u32) d);
        } else {
            push_u8(buf, MODRM_INDIRECT_DISP8 | (((u8) r&7)<<3) | (u8) rm&7);
            if (rm == X64_RSP) {
                push_u8(buf, (u8) (rm << 3) | (u8) rm);
            }
            push_u8(buf, (u8) d);
        }
    } else if (t & IC_RIP_DISP32) {
        x64_rip_relative(buf, r, d, rip);
    } else {
        push_u8(buf, MODRM_DIRECT | (((u8) r&7)<<3) | (u8) rm&7);
    }
}

inline void
x64_unary(Buffer* buf, Ic_Type t, s64 r, s64 d, u8 opcode, u8 reg_field, s64 rip) {
    if (t & IC_T64) {
        x64_rex(buf, REX_FLAG_64_BIT);
    }
    // F7 /3 	NEG r/m32 	M
    push_u8(buf, opcode);
    x64_modrm(buf, t, d, reg_field, r, rip);
}

inline void
x64_binary(Buffer* buf,
           Ic_Type t1, s64 r1, s64 d1, 
           Ic_Type t2, s64 r2, s64 d2,
           u8 reg_field, u8 opcode, s64 rip) {
    
    if (t1 & IC_DISP) {
        Ic_Type tmpt = IC_REG + (t1 & IC_RT_MASK);
        s64 tmpr = X64_RAX;
        x64_mov(buf, tmpt, tmpr, 0, t1, r1, d1, rip);
        t1 = tmpt;
        r1 = tmpr;
        d1 = 0;
    }
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_DISP) {
                if (d2 > U32_MAX) {
                    unimplemented;
                }
                
                if (t1 & IC_T64) {
                    x64_rex(buf, REX_FLAG_64_BIT);
                }
                
                // 81 /0 id 	ADD r/m32, imm32 	MI
                //push_u8(buf, 0x81);
                push_u8(buf, (t1 & IC_T8) ? 0x80 : 0x81);
                push_u8(buf, 0xC0 | (reg_field << 3) | (u8) r1);
                if (t1 & IC_T8) {
                    push_u8(buf, (u8) d2);
                } else {
                    push_u32(buf, (u32) d2);
                }
                
            } else if (t2 & IC_STK_RIP_REG) {
                if (t1 & IC_T64) {
                    x64_rex(buf, REX_FLAG_64_BIT);
                }
                
                // 03 /r 	ADD r32, r/m32 	RM
                push_u8(buf, (t1 & IC_T8) ? opcode + 2 : opcode + 3);
                x64_modrm(buf, t2, d2, r1, r2, rip);
            } else {
                unimplemented;
            }
        } break;
        
        case IC_STK:
        case IC_RIP_DISP32: {
            if (t2 & IC_DISP) {
                
                if ((u64) d2 > U32_MAX && (d2 != -1 || t2 & IC_UINT)) {
                    unimplemented;
                } else if (t2 & IC_UINT && (s32) d2 < 0) {
                    Ic_Type tmpt = IC_T32 + IC_REG;
                    s64 tmpr = X64_RAX;
                    x64_mov(buf, tmpt, tmpr, 0, t2, r2, d2, rip);
                    x64_binary(buf, t1, r1, d1, tmpt, tmpr, 0, reg_field, opcode, rip);
                } else {
                    if (t1 & IC_T64) {
                        x64_rex(buf, REX_FLAG_64_BIT);
                    }
                    
                    // 81 /0 id 	ADD r/m32, imm32 	MI
                    push_u8(buf, (t1 & IC_T8) ? 0x80 : 0x81);
                    //push_u8(buf, 0xC7);
                    s64 disp_size = t1 & IC_T8 ? 1 : 4;
                    x64_modrm(buf, t1, d1, reg_field, r1, rip + disp_size);
                    if (t1 & IC_T8) {
                        push_u8(buf, (u8) d2);
                    } else {
                        push_u32(buf, (u32) d2);
                    }
                }
            } else if (t2 & IC_REG) {
                if (t1 & IC_T16) {
                    push_u8(buf, X64_OP_SIZE_PREFIX);
                }
                
                if (t1 & IC_T64) {
                    x64_rex(buf, REX_FLAG_64_BIT);
                }
                
                // 01 /r 	ADD r/m32, r32 	MR
                push_u8(buf, (t1 & IC_T8) ? opcode : opcode + 1);
                x64_modrm(buf, t1, d1, r2, r1, rip);
                
            } else if (t2 & IC_STK_RIP) {
                s64 tmpr = X64_RAX;
                if (r2 == tmpr || r1 == tmpr) {
                    tmpr = X64_RCX;
                }
                x64_mov(buf, IC_REG + (t2 & IC_RT_MASK), tmpr, 0, t2, r2, d2, rip);
                x64_binary(buf, t1, r1, d1, IC_REG + (t1 & IC_RT_MASK), tmpr, 0, reg_field, opcode, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

void
x64_zero(Buffer* buf, s64 r) {
    // REX.W + 33 /r 	XOR r64, r/m64 	RM
    u8 rflag = (u8) (r&8) >> 1;
    x64_rex(buf, REX_FLAG_W | rflag | rflag >> 2);
    push_u8(buf, 0x33);
    x64_modrm(buf, IC_S64 + IC_REG, 0, r, r, 0);
}

void
x64_mov_rax_u64(Buffer* buf, u64 disp) {
    // REX.W + B8+ rd io 	MOV r64, imm64 	OI
    x64_rex(buf, REX_FLAG_W);
    push_u8(buf, 0xB8);
    push_u64(buf, disp);
}

void
x64_mov(Buffer* buf,
        Ic_Type t1, s64 r1, s64 d1,
        Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_DISP) {
                if (d2 == 0) {
                    x64_zero(buf, r1);
                } else if (t1 & IC_T64 || d2 < S32_MIN || d2 > S32_MAX) {
                    // REX.W + B8+ rd io 	MOV r64, imm64 	OI
                    x64_rex(buf, REX_FLAG_W | (r1&8)>>3);
                    push_u8(buf, 0xB8+((u8)r1&7));
                    push_u64(buf, (u64) d2);
                } else {
                    // B8+ rd id 	MOV r32, imm32 	OI
                    if (r1&8) {
                        x64_rex(buf, REX_FLAG_B);
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
                x64_modrm(buf, t2, d2, r1, r2, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        case IC_STK:
        case IC_RIP_DISP32: {
            if (t2 & IC_DISP) {
                if (d2 < S32_MIN || d2 > S32_MAX) {
                    x64_mov_rax_u64(buf, d2);
                    x64_mov(buf, t1, r1, d1, IC_T64 + IC_REG, X64_RAX, 0, rip);
                } else {
                    // C7 /0 id 	MOV r/m32, imm32 	MI
                    if (t1 & IC_T16) {
                        push_u8(buf, X64_OP_SIZE_PREFIX);
                    }
                    
                    if (t1 & IC_T64) {
                        x64_rex(buf, REX_FLAG_64_BIT);
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
            x64_rex(buf, REX_FLAG_R);
        }
        push_u16(buf, 0xB60F);
        x64_modrm(buf, t2, d2, r1, r2, rip);
        
    } else if (t2 & IC_T16) {
        if (r1 & 8) {
            x64_rex(buf, REX_FLAG_R);
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
        x64_rex(buf, REX_FLAG_W);
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

inline void
x64_mul(X64_Assembler* x64, Buffer* buf, Bytecode_Binary* binary, s64 rip) {
    Ic_Arg first = convert_bytecode_operand_to_x64(x64, buf, binary->first, binary->type);
    Ic_Arg second = convert_bytecode_operand_to_x64(x64, buf, binary->second, binary->type);
    
    assert(first.type & IC_REG); // TODO(Alexander): we can allow stack/ memory later
    
    if (second.type & IC_DISP) {
        // 69 /r id 	IMUL r32, r/m32, imm32 	RMI
        if (first.type & IC_T64) {
            x64_rex(buf, REX_FLAG_64_BIT);
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
        x64_rex(buf, REX_FLAG_64_BIT);
    }
    if (first.type & IC_SINT) {
        push_u8(buf, 0x99);
    } else {
        x64_zero(buf, X64_RDX);
    }
    
    // F7 /6  	DIV r/m32 	M
    // F7 /7 	IDIV r/m32 	M
    if (first.type & IC_T64) {
        x64_rex(buf, REX_FLAG_64_BIT);
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
        x64_rex(buf, REX_FLAG_64_BIT);
    }
    push_u16(buf, 0x2A0F);
    x64_modrm(buf, t2, d2, r1, r2, rip);
}

inline void
x64_convert_float_to_int_type(Buffer* buf, 
                              Ic_Type t1, s64 r1, s64 d1, 
                              Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    assert(t1 & IC_REG);
    assert(t2 & (IC_REG | IC_STK | IC_RIP_DISP32));
    
    // F3 0F 2C /r CVTTSS2SI r32, xmm1/m32
    // F2 0F 2C /r CVTTSD2SI r32, xmm1/m64
    push_u8(buf, t2 & IC_T64 ? 0xF2 : 0xF3);
    if (t1 & IC_T64) {
        x64_rex(buf, REX_FLAG_64_BIT);
    }
    push_u16(buf, 0x2C0F);
    x64_modrm(buf, t2, d2, r1, r2, rip);
}


void
x64_lea(Buffer* buf, s64 r1, s64 r2, s64 d2, s64 rip) {
    // REX.W + 8D /r 	LEA r64,m 	RM
    x64_rex(buf, REX_FLAG_W|((u8)r1&8)>>1);
    push_u8(buf, 0x8D);
    
    if (r2 == X64_RIP) {
        push_u8(buf, (u8) (r1&7)<<3 | (u8) X64_RBP);
        push_u32(buf, (u32) (d2 - (rip + 4)));
    } else {
        x64_modrm(buf, IC_STK, d2, r1&7, r2, rip);
    }
}


void
x64_push_rel32(X64_Assembler* x64, Buffer* buf, Bytecode_Function* func, u32 label_index) {
    if (label_index > 0) {
        label_index = x64->block_stack[label_index - 1].label_index;
    }
    
    X64_Relocation reloc = {};
    reloc.from_ptr = (s32*) (buf->data + buf->curr_used);
    reloc.target = &x64->labels[label_index];
    array_push(x64->relocations, reloc);
    push_u32(buf, 0);
}


#if 0
#endif

void
x64_string_op(Buffer* buf, 
              Ic_Type destt, s64 destr, s64 destd, 
              Ic_Type srct, s64 srcr, s64 srcd, 
              s64 count, u16 opcode, s64 rip, s64 src_int_reg=X64_RAX) {
    // Move RCX bytes from [RSI] to [RDI].
    x64_mov(buf, IC_S64 + IC_REG, X64_RCX, 0, IC_S64 + IC_DISP, 0, count, rip);
    
    if (destt & (IC_UINT | IC_SINT)) {
        x64_mov(buf, IC_S64 + IC_REG, X64_RDI, 0, IC_S64 + IC_STK, X64_RSP, destd, rip);
    } else {
        x64_lea(buf, X64_RDI, destr, destd, rip);
    }
    
    if (srct & (IC_UINT | IC_SINT)) {
        x64_mov(buf, IC_S64 + IC_REG, src_int_reg, 0, srct, srcr, srcd, rip);
    } else if (srct & IC_T64) {
        //if (srct & IC_DISP) {
        //x64_lea(buf, X64_RSI, X64_RIP, disp, rip);
        //} else {
        x64_lea(buf, X64_RSI, srcr, srcd, rip);
        //}
    } else {
        unimplemented;
    }
    
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

s64
convert_to_x64_machine_code(Buffer* buf, s64 stk_usage, u8* buffer, s64 buffer_offset, s64 buf_size, s64 rip) {
    
    // We arrange the stack so arguments have positive displacement
    // and local variables have negative displacement.
    // But we are only allowed to write to RSP with positive displacement.
    // The rsp_adjust subtracts the arguments stack usage so they will
    // come first then followed by the local variables.
    
#if 0
    s64 rsp_adjust = 0;
    
    while (ic) {
        ic->code_count = 0;
        
        switch (ic->opcode) {
            case IC_NOOP: break;
            
            case IC_PRLG: {
                x64_binary(ic, IC_REG + IC_S64, X64_RSP, 0, IC_DISP + IC_S32, 0, stk_usage, 5, 28, (s64) buffer_offset + rip);
            } break;
            
            case IC_EPLG: {
                x64_binary(ic, IC_REG + IC_S64, X64_RSP, 0, IC_DISP + IC_S32, 0, stk_usage, 0, 0, (s64) buffer_offset + rip);
                push_u8(buf, 0xC3);
            } break;
            
            case IC_NEG: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xF7, 3, (s64) buffer_offset + rip);
            } break;
            
            case IC_NOT: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xF7, 2, (s64) buffer_offset + rip);
            } break;
            
            case IC_INC: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xFF, 0, (s64) buffer_offset + rip);
            } break;
            
            case IC_DEC: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xFF, 1, (s64) buffer_offset + rip);
            } break;
            
            case IC_ADD: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 0, 0, (s64) buffer_offset + rip);
            } break;
            
            case IC_SUB: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 5, 0x28, (s64) buffer_offset + rip);
            } break;
            
            case IC_AND: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 4, 0x20, (s64) buffer_offset + rip);
            } break;
            
            case IC_OR: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 1, 0x8, (s64) buffer_offset + rip);
            } break;
            
            case IC_XOR: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 6, 0x30, (s64) buffer_offset + rip);
            } break;
            
            case IC_SHL: {
                x64_shr(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 4, 4, (s64) buffer_offset + rip);
                
            } break;
            
            case IC_SHR: {
                x64_shr(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 7, 5, (s64) buffer_offset + rip);
            } break;
            
            case IC_MUL: {
                x64_mul(ic, (s64) buffer_offset + rip);
            } break;
            
            case IC_DIV: {
                x64_div(ic, false, (s64) buffer_offset + rip);
            } break;
            
            case IC_MOD: {
                x64_div(ic, true, (s64) buffer_offset + rip);
            } break;
            
            case IC_MOV: {
                x64_mov(ic,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, (s64) buffer_offset + rip);
            } break;
            
            case IC_MOVZX: {
                assert(ic->src1.type & IC_STK_RIP_REG);
                
                if (ic->src1.type & IC_T8) {
                    if (ic->src0.reg & 8) {
                        x64_rex(ic, REX_FLAG_R);
                    }
                    push_u16(buf, 0xB60F);
                    x64_modrm(ic, ic->src1.type, ic->src1.disp, ic->src0.reg, ic->src1.reg, (s64) buffer_offset + rip);
                    
                } else if (ic->src1.type & IC_T16) {
                    if (ic->src0.reg & 8) {
                        x64_rex(ic, REX_FLAG_R);
                    }
                    push_u16(buf, 0xB70F);
                    x64_modrm(ic, ic->src1.type, ic->src1.disp, ic->src0.reg, ic->src1.reg, (s64) buffer_offset + rip);
                    
                } else {
                    assert(ic->src0.type & IC_T64 && ic->src1.type & IC_T32);
                    Ic_Type t1 = (ic->src0.type & IC_TF_MASK) | ic->src1.raw_type;
                    //Ic_Type t1 = ic->src0.type;
                    //
                    //ic->code[ic->code_count++] = 0xCC;
                    
                    x64_mov(ic,
                            t1, ic->src0.reg, ic->src0.disp,
                            ic->src1.type, ic->src1.reg, ic->src1.disp, (s64) buffer_offset + rip);
                }
            } break;
            
            case IC_MOVSX: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, d1 = ic->src0.disp, r2 = ic->src1.reg, d2 = ic->src1.disp;
                
                if (t1 & IC_T16) {
                    push_u8(buf, X64_OP_SIZE_PREFIX);
                }
                
                if (t1 & IC_T64) {
                    x64_rex(ic, REX_FLAG_W);
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
                x64_modrm(ic, t2, d2, r1, r2, rip);
                
            } break;
            
            case IC_FMOV: {
                x64_fmov(ic,
                         ic->src0.type, ic->src0.reg, ic->src0.disp,
                         ic->src1.type, ic->src1.reg, ic->src1.disp,
                         (s64) buffer_offset + rip);
                
            } break;
            
            case IC_REINTERP_F2S: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, d1 = ic->src0.disp, r2 = ic->src1.reg, d2 = ic->src1.disp;
                
                if (t1 & (IC_SINT | IC_UINT) && t2 & IC_FLOAT) {
                    if (t1 & IC_REG) {
                        // 66 0F 7E /r MOVD r/m32, xmm
                        push_u8(buf, 0x66);
                        if (t1 & IC_T64 || r1&8) {
                            x64_rex(ic, REX_FLAG_64_BIT | ((u8)r1&8)>>3);
                        }
                        push_u16(buf, 0x7E0F);
                        x64_modrm(ic, t2, d2, r2&7, r1&7, rip);
                    } else {
                        unimplemented;
                    }
                } else {
                    unimplemented;
                }
            } break;
            
            case IC_CAST_F2S: {
                x64_convert_float_to_int_type(ic, (s64) buffer_offset + rip);
            } break;
            
            case IC_CAST_S2F: {
                x64_convert_int_to_float_type(ic, (s64) buffer_offset + rip);
            } break;
            
            case IC_CAST_F2F: {
                x64_convert_float_type(ic, (s64) buffer_offset + rip);
            } break;
            
            case IC_MEMCPY: {
                x64_string_op(ic, 
                              ic->dest.type, ic->dest.reg, ic->dest.disp, 
                              ic->src0.type, ic->src0.reg, ic->src0.disp,
                              ic->src1.disp, 0xA4F3, (s64) buffer_offset + rip, X64_RSI);
            } break;
            
            case IC_MEMSET: {
                x64_string_op(ic, 
                              ic->dest.type, ic->dest.reg, ic->dest.disp,
                              ic->src0.type, ic->src0.reg, ic->src0.disp,
                              ic->src1.disp, 0xAAF3, (s64) buffer_offset + rip);
            } break;
            
            case IC_LEA: {
                assert(ic->src0.type & IC_REG);
                if (ic->src1.type & IC_RIP_DISP32) {
                    x64_lea(ic, ic->src0.reg, X64_RIP, ic->src1.disp, (s64) buffer_offset + rip);
                } else {
                    x64_lea(ic, ic->src0.reg, ic->src1.reg, ic->src1.disp, (s64) buffer_offset + rip);
                }
            } break;
            
            case IC_LPROC: {
                Compilation_Unit* target = (Compilation_Unit*) ic->data;
                if (target && target->bb_first) {
                    // REX.W + 8D /r 	LEA r64,m 	RM
                    x64_rex(ic, REX_FLAG_W);
                    push_u8(buf, 0x8D);
                    //push_u8(buf, 0x8B); //mov
                    x64_rip_relative(ic, X64_RAX, (s64) buffer_offset + target->bb_first->addr, (s64) buffer_offset + rip);
                } else {
                    x64_mov_rax_u64(ic, (u64) target->bb_first);
                }
            } break;
            
            case IC_CMP: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, d1 = ic->src0.disp;
                if (t1 & IC_DISP) {
                    Ic_Type tmpt = IC_REG + (ic->src0.type & IC_RT_MASK);
                    s64 tmpr = (t2 & IC_REG) ? X64_RCX : X64_RAX, tmpd = 0;
                    x64_mov(ic, tmpt, tmpr, tmpd, ic->src0.type, ic->src0.reg, ic->src0.disp, (s64) buffer_offset + rip);
                    t1 = tmpt;
                    r1 = tmpr;
                    d1 = tmpd;
                }
                x64_binary(ic, t1, r1, d1,
                           ic->src1.type, ic->src1.reg, ic->src1.disp, 7, 0x38, (s64) buffer_offset + rip);
            } break;
            
            case IC_TEST: {
                assert(ic->src0.type & (IC_REG | IC_STK));
                assert(ic->src1.type & IC_REG);
                
                if (ic->src0.type & IC_T8) {
                    // 84 /r 	TEST r/m8, r8 	MR
                    push_u8(buf, 0x84);
                    x64_modrm(ic, ic->src0.type, ic->src0.disp, ic->src1.reg, ic->src0.reg, (s64) buffer_offset + rip);
                } else {
                    if (ic->src0.type & IC_T64) {
                        x64_rex(ic, REX_FLAG_64_BIT);
                    }
                    // 85 /r 	TEST r/m32, r32 	MR
                    push_u8(buf, 0x85);
                    x64_modrm(ic, ic->src0.type, ic->src0.disp, ic->src1.reg, ic->src0.reg, (s64) buffer_offset + rip);
                }
            } break;
            
            case IC_FADD: {
                x64_float_binary(ic, 0x58, (s64) buffer_offset + rip);
            } break;
            
            case IC_FSUB: {
                x64_float_binary(ic, 0x5C, (s64) buffer_offset + rip);
            } break;
            
            case IC_FMUL: {
                x64_float_binary(ic, 0x59, (s64) buffer_offset + rip);
            } break;
            
            case IC_FDIV: {
                x64_float_binary(ic, 0x5E, (s64) buffer_offset + rip);
            } break;
            
            case IC_FXOR: {
                x64_float_binary(ic, 0x57, (s64) buffer_offset + rip, ic->dest.type & IC_T64 ? 0x66 : -2);
            } break;
            
            case IC_FCMP: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, r2 = ic->src1.reg;
                s64 d2 = ic->src1.disp;
                
                if (t1 & IC_STK_RIP) {
                    Ic_Type tmpt = IC_REG + (ic->src0.type & IC_RT_MASK);
                    x64_fmov(ic, tmpt, X64_XMM5, 0,
                             ic->src0.type, ic->src0.reg, ic->src0.disp, 
                             (s64) buffer_offset + rip);
                    t1 = tmpt;
                    r1 = X64_XMM5;
                }
                
                if (t2 & (IC_STK | IC_DISP)) {
                    Ic_Type tmpt = IC_REG + (ic->src1.type & IC_RT_MASK);
                    x64_fmov(ic, tmpt, X64_XMM2, 0,
                             ic->src1.type, ic->src1.reg, ic->src1.disp, 
                             (s64) buffer_offset + rip);
                    t2 = tmpt;
                    r2 = X64_XMM2;
                }
                
                
                // NP 0F 2E /r UCOMISS xmm1, xmm2/m32
                if (ic->src0.type & IC_T64) {
                    push_u8(buf, 0x66);
                }
                push_u16(buf, 0x2F0F);
                x64_modrm(ic, t2, d2, r1, r2, (s64) buffer_offset + rip);
            } break;
            
            case IC_SETA: 
            case IC_SETAE:
            case IC_SETB:
            case IC_SETBE:
            case IC_SETE:
            case IC_SETG:
            case IC_SETGE:
            case IC_SETL:
            case IC_SETLE:
            case IC_SETNE: {
                // setcc al
                ic_u24(ic, x64_setcc_opcodes[ic->opcode - IC_SETA]); 
            } break;
            
            case IC_JA: 
            case IC_JAE:
            case IC_JB:
            case IC_JBE:
            case IC_JE:
            case IC_JG:
            case IC_JGE:
            case IC_JL:
            case IC_JLE:
            case IC_JNE: {
                push_u16(buf, x64_jcc_opcodes[ic->opcode - IC_JA]);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip)
            } break;
            
            case IC_JMP_INDIRECT: {
                // TODO(Alexander): short jumps
                push_u8(buf, 0xFF);
                x64_modrm(ic, ic->src0.type, ic->src0.disp, 4, ic->src0.reg, (s64) buffer_offset + rip);
            } break;
            
            case IC_JMP: {
                // TODO(Alexander): short jumps
                push_u8(buf, 0xE9);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            case IC_CALL: {
                Ic_Call* call = (Ic_Call*) ic;
                Compilation_Unit* func = call->func;
                //assert(func);
                
                if (func && func->bb_first) {
                    // Direct function call
                    
                    // E8 cd 	CALL rel32 	D
                    push_u8(buf, 0xE8);
                    x64_jump(ic, func->bb_first, rip);
                    
#if 0
                } else if (func && func->ast->type->Function.intrinsic) {
                    assert(func->ast && func->ast->type->kind == TypeKind_Function &&
                           func->ast->type->Function.intrinsic);
                    
                    // Indirect function call to intrinsic function
                    
                    x64_mov_rax_u64(ic, (u64) func->ast->type->Function.intrinsic);
                    
                    // FF /2 	CALL r/m64 	M 	
                    push_u8(buf, 0xFF);
                    x64_modrm(ic, IC_REG + IC_S64, 0, 2, X64_RAX, rip);
                    
#endif
                    
                } else {
                    // Indirect function call from pointer
                    
                    if (call->ptr.type & IC_DISP) {
                        x64_mov_rax_u64(ic, (u64) call->ptr.disp);
                    }
                    
                    // FF /2 	CALL r/m64 	M 	
                    push_u8(buf, 0xFF);
                    x64_modrm(ic, IC_REG + IC_S64, 0, 2, X64_RAX, rip);
                }
            } break;
            
            case IC_DEBUG_BREAK: {
                push_u8(buf, 0xCC);
            } break;
            
            case IC_RDTSC: {
                push_u16(buf, 0x310F);
                
                // REX.W + C1 /4 ib  -> shl rdx, 32
                x64_rex(ic, REX_FLAG_W);
                push_u8(buf, 0xC1);
                x64_modrm(ic, IC_REG, 0, 4, X64_RDX, rip);
                push_u8(buf, 32);
                
                // REX.W + 09 /r  -> or rax, rdx
                x64_rex(ic, REX_FLAG_W);
                push_u8(buf, 0x09);
                x64_modrm(ic, IC_REG, 0, X64_RDX, X64_RAX, rip);
            } break;
            
            case IC_LABEL: {
                ((Ic_Basic_Block*) ic->data)->addr = rip;
            } break;
            
            default: unimplemented;
        }
        
        if (buffer) {
            assert(rip < buf_size);
            memcpy(buffer + rip, ic->code, ic->code_count);
        }
        
        rip += ic->code_count;
        ic = ic->next;
    }
#endif
    
    return rip;
}

#if 0
void
string_builder_push(String_Builder* sb, Ic_Arg arg) {
    // TODO(Alexander): make use of the type
    switch (arg.type & IC_TF_MASK) {
        case IC_DISP: {
            string_builder_push_format(sb, "% (0x%)", f_s64(arg.disp), f_u64_HEX(arg.disp));
        } break;
        
        case IC_REG: {
            if (arg.raw_type & IC_FLOAT) {
                if (arg.reg < fixed_array_count(float_register_names)) {
                    string_builder_push(sb, float_register_names[arg.reg]);
                }
            } else {
                if (arg.reg < fixed_array_count(int_register_names)) {
                    string_builder_push(sb, int_register_names[arg.reg]);
                }
            }
        } break;
        
        case IC_STK: {
            if (arg.stk.disp != 0) {
                string_builder_push_format(sb, "[% % %]",
                                           f_cstring(int_register_names[arg.reg]),
                                           f_char(arg.stk.disp > 0 ? '+' : '-'), 
                                           f_s64(arg.stk.disp > 0 ? arg.stk.disp : -arg.stk.disp));
            } else {
                string_builder_push_format(sb, "[%]", f_cstring(int_register_names[arg.reg]));
            }
        } break;
        
        case IC_RIP_DISP32: {
            string_builder_push_format(sb, "[0x%]", f_u64_HEX(arg.disp));
        } break;
    }
}

void
string_builder_push(String_Builder* sb, Buffer* buf, int bb_index) {
    smm start_used = sb->curr_used;
    
    string_builder_push(sb, "    ");
    
    switch (ic->opcode) {
        case IC_LABEL: {
            if (bb_index > 0) {
                string_builder_push_format(sb, "  bb%:", f_int(bb_index));
            }
            
        } break;
        
        case IC_CALL: {
            Ic_Call* call = (Ic_Call*) ic;
            
            string_builder_push(sb, "CALL ");
            Compilation_Unit* func = (Compilation_Unit*) call->func;
            if (func && func->ident) {
                string_builder_push(sb, func->ident);
            } else {
                string_builder_push(sb, call->ptr);
            }
        } break;
        
        
        default: {
            if (ic->dest.type) {
                string_builder_push(sb, ic->dest);
                string_builder_push(sb, " = ");
            }
            
            string_builder_push(sb, ic_opcode_names[ic->opcode]);
            
            if (ic->opcode >= IC_JMP && ic->opcode <= IC_JNE) {
                Ic_Basic_Block* bb = (Ic_Basic_Block*) ic->data;
                if (bb) {
                    string_builder_push_format(sb, " bb%", f_int(bb->index));
                }
                break;
            }
            
            if (ic->src0.type) {
                string_builder_push(sb, " ");
                string_builder_push(sb, ic->src0);
                
                if (ic->src1.type) {
                    string_builder_push(sb, ", ");
                    string_builder_push(sb, ic->src1);
                    
                }
            }
        } break;
    }
    
    if (ic->comment) {
        smm used = sb->curr_used - start_used;
        for (smm i = used; i < 35; i++) {
            string_builder_push(sb, " ");
        }
        string_builder_push_format(sb, " // %", f_cstring(ic->comment));
    }
}

void 
print_intermediate_code(Intermediate_Code* value) {
    String_Builder sb = {};
    string_builder_alloc(&sb, 60);
    string_builder_push(&sb, value);
    string result = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(result));
    string_builder_free(&sb);
}
#endif