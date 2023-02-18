
#define convert_assign_to_intermediate_code(cu, type, dest, src) _convert_assign_to_intermediate_code(cu, type, dest, src, __FILE__ ":" S2(__LINE__))

void
_convert_assign_to_intermediate_code(Compilation_Unit* cu, Type* type, Ic_Arg dest, Ic_Arg src, cstring comment=0) {
    if (type->kind == TypeKind_Array || 
        type->kind == TypeKind_Struct || 
        type->kind == TypeKind_Union || 
        (type->kind == TypeKind_Basic && type->Basic.kind == Basic_string)) {
        
        if (type->size > 8) {
            assert(dest.type & IC_STK_RIP);
            
            if (src.type) {
                Intermediate_Code* ic = _ic_add(cu, IC_MEMCPY, comment);
                ic->dest = dest;
                ic->src0 = src;
                ic->src1 = ic_imm(0, type->size);
                
            } else {
                Intermediate_Code* ic = _ic_add(cu, IC_MEMSET, comment);
                ic->dest = dest;
                ic->src0 = ic_imm(IC_U8, 0);
                ic->src1 = ic_imm(0, type->size);
                
                if (src.type & IC_DISP_STK_RIP) {
                    ic->opcode = IC_MEMCPY;
                    ic->src0 = src;
                }
            }
        } else {
            if (src.type) {
                _ic_mov(cu, dest, src, comment);
            } else {
                if (dest.type & IC_FLOAT) {
                    Intermediate_Code* ic = _ic_add(cu, IC_FXOR, comment);
                    ic->dest = dest;
                    ic->src0 = ic_reg(dest.raw_type, X64_XMM0);
                    ic->src1 = ic_reg(dest.raw_type, X64_XMM0);
                } else {
                    _ic_mov(cu, dest, ic_imm(dest.raw_type, 0), comment);
                }
            }
        }
    } else {
        if (type->kind == TypeKind_Basic && type->Basic.kind == Basic_cstring) {
            ic_lea(cu, dest, src);
            
        } else if (src.type) {
            if ((dest.type & IC_DISP) && (dest.type & IC_FLOAT) == 0) {
                Ic_Arg tmp = ic_reg(IC_T64, X64_RDX);
                ic_mov(cu, tmp, dest);
                dest = ic_stk(IC_T64, 0, IcStkArea_None, tmp.reg);
            }
            _ic_mov(cu, dest, src, comment);
        }
    }
}

Ic_Arg
x64_clobber_register(Compilation_Unit* cu, Intermediate_Code* curr, Type* type, Ic_Arg prev_result) {
    Ic_Arg clobber = ic_push_local(cu, type); // TODO(Alexander): temp allocation
    
    Intermediate_Code* mov_clobber = ic_add_orphan(cu, (prev_result.type & IC_FLOAT) ? IC_FMOV : IC_MOV);
    mov_clobber->src0 = clobber;
    mov_clobber->src1 = prev_result;
    
    Intermediate_Code* tmp = curr->next;
    curr->next = mov_clobber;
    mov_clobber->next = tmp;
    
    return clobber;
}

Ic_Arg
convert_function_call_to_intermediate_code(Compilation_Unit* cu, 
                                           Type* function_type,
                                           array(Ast*)* args) {
    
    
    Ic_Arg result = {};
    
    assert(function_type && function_type->kind == TypeKind_Function);
    
    Type_Function* proc = &function_type->Function;
    
    if (proc->intrinsic) {
        if (proc->intrinsic == &interp_intrinsic_debug_break) {
            ic_add(cu, IC_DEBUG_BREAK);
            return result;
        }
    }
    
    s64 stk_args = 0;
    array(X64_Arg_Copy)* copy_args = 0;
    
    // Setup return type
    if (proc->return_type) {
        Type* type = proc->return_type;
        Ic_Raw_Type rt = convert_type_to_raw_type(type);
        
        if (type->size > 8) {
            // NOTE(Alexander): if return argument cannot fit in RAX/ XMM0 then
            //                  pass a pointer to it as the first argument
            s64 disp = stk_args;
            stk_args += 8;
            // TODO(Alexander): we should use argument stack but this has to be allocated last
            //Ic_Arg src = ic_stk(rt, disp, IcStkArea_Args);
            
            X64_Arg_Copy copy = {};
            copy.type = proc->return_type;
            array_push(copy_args, copy);
        } else {
            result = ic_reg(rt, X64_RAX); // NOTE: RAX == 0 && XMM0 == 0
        }
    }
    
    {
        // Setup arguments
        for_array_v(args, arg, arg_index) {
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
    
    // Store arguments according to the windows calling convention
    for_array_reverse(copy_args, arg, arg_index) {
        Ic_Arg src;
        if (arg->expr) {
            src = convert_expr_to_intermediate_code(cu, arg->expr);
        } else {
            src = ic_push_local(cu, arg->type);
            arg->dest = src;
            result = src;
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
        
        if (src.type & IC_FLOAT) {
            Intermediate_Code* ic = ic_add(cu, IC_FMOV);
            ic->src0 = dest;
            ic->src1 = src;
        } else if ((src.type & (IC_SINT | IC_UINT | IC_DISP)) ||
                   arg->type->kind == TypeKind_Pointer ||
                   arg->type->kind == TypeKind_Function) {
            ic_mov(cu, dest, src);
        } else {
            ic_lea(cu, dest, src);
        }
        
        if (proc->is_variadic && arg_index < 4 && src.type & IC_FLOAT) {
            Intermediate_Code* ic = ic_add(cu, IC_REINTERP_F2S);
            ic->src0 = ic_reg(IC_S64, int_arg_registers_ccall_windows[arg_index]);
            ic->src1 = dest;
        }
    }
    
    cu->stk_args = max(cu->stk_args, stk_args);
    if (proc->is_variadic) {
        // NOTE(Alexander): make sure to allocate space for HOME registers
        // even if they aren't used!
        cu->stk_args = max(cu->stk_args, 4*8);
    }
    
    Intermediate_Code* ic = ic_add(cu, IC_CALL, proc->unit);
    if (proc->intrinsic) {
        ic->dest = ic_imm(IC_S64, (s64) proc->intrinsic);
    }
    
    return result;
}

Ic_Arg
convert_binary_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr, Ic_Arg prev_result={}) {
    assert(expr && expr->kind == Ast_Binary_Expr && "not a binary expression");
    
    if (expr->Binary_Expr.overload) {
        
        array(Ast*)* args = 0;
        array_push(args, expr->Binary_Expr.first);
        array_push(args, expr->Binary_Expr.second);
        
        return convert_function_call_to_intermediate_code(cu, expr->Binary_Expr.overload, args);
    }
    
    Ic_Arg result;
    
    Operator op = expr->Binary_Expr.op;
    
    if (op == Op_Logical_Or) {
        Intermediate_Code* test_lhs, *test_rhs;
        Ic_Basic_Block* bb_true = ic_basic_block();
        Ic_Basic_Block* bb_exit = ic_basic_block();
        
        Ic_Arg src0 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.first);
        result = ic_reg_mov(cu, X64_RAX, src0);
        test_lhs = ic_add(cu, IC_TEST);
        test_lhs->src0 = result;
        test_lhs->src1 = result;
        ic_add(cu, IC_JNE, bb_true);
        
        Ic_Arg src1 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.second);
        ic_reg_mov(cu, X64_RAX, src1);
        test_rhs = ic_add(cu, IC_TEST);
        test_rhs->src0 = result;
        test_rhs->src1 = result;
        ic_add(cu, IC_JNE, bb_true);
        
        // false
        ic_mov(cu, result, ic_imm(result.raw_type, 0));
        ic_add(cu, IC_JMP, bb_exit);
        
        // true
        ic_add(cu, IC_LABEL, bb_true);
        ic_mov(cu, result, ic_imm(result.raw_type, 1));
        ic_add(cu, IC_JMP, bb_exit);
        ic_add(cu, IC_LABEL, bb_exit);
        
        assert(src0.raw_type == src1.raw_type);
        
    } else if (op == Op_Logical_And) {
        Intermediate_Code* test_lhs, *test_rhs;
        Ic_Basic_Block* bb_false = ic_basic_block();
        Ic_Basic_Block* bb_exit = ic_basic_block();
        
        Ic_Arg src0 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.first);
        result = ic_reg_mov(cu, X64_RAX, src0);
        test_lhs = ic_add(cu, IC_TEST);
        test_lhs->src0 = result;
        test_lhs->src1 = result;
        ic_add(cu, IC_JE, bb_false);
        
        Ic_Arg src1 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.second);
        ic_reg_mov(cu, X64_RAX, src1);
        test_rhs = ic_add(cu, IC_TEST);
        test_rhs->src0 = result;
        test_rhs->src1 = result;
        ic_add(cu, IC_JE, bb_false);
        
        // true
        ic_mov(cu, result, ic_imm(result.raw_type, 1));
        ic_add(cu, IC_JMP, bb_exit);
        
        // false
        ic_add(cu, IC_LABEL, bb_false);
        ic_mov(cu, result, ic_imm(result.raw_type, 0));
        ic_add(cu, IC_JMP, bb_exit);
        ic_add(cu, IC_LABEL, bb_exit);
        
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
        
        {
            Intermediate_Code* curr = cu->ic_last;
            Ast* second = expr->Binary_Expr.second;
            if (second->kind == Ast_Binary_Expr) { 
                src1 = convert_binary_expr_to_intermediate_code(cu, second, prev_result);
            } else {
                src1 = convert_expr_to_intermediate_code(cu, second);
            }
            
            if (src0.type & IC_REG && src0.type == src1.type && src0.reg == src1.reg) {
                src0 = x64_clobber_register(cu, curr, second->type, src0);
            }
        }
        
        bool is_pointer_arithmetic = src0.type > 0 && (src0.type & (IC_SINT | IC_UINT | IC_FLOAT)) == 0; 
        if (is_pointer_arithmetic) {
            if (src1.type & (IC_SINT | IC_UINT)) {
                // pointer +/- integral
                //pln("AST:\n%", f_ast(expr));
                assert(op == Op_Add || op == Op_Subtract ||
                       op == Op_Add_Assign || op == Op_Subtract_Assign);
                
                Type* ptr_type = expr->Binary_Expr.first->type;
                int sizeof_elem = ptr_type->Pointer->size;
                
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
            Intermediate_Code* ic = ic_add(cu, IC_NOOP);
            ic->dest = ic_reg(src0.raw_type);
            ic->src0 = src0;
            ic->src1 = src1;
            result = ic->dest;
            
            if (operator_is_comparator(op)) {
                result.raw_type = IC_U8;
                ic->opcode = isFloat ? IC_FCMP : IC_CMP;
                ic->dest = result;
                
                ic = ic_add(cu, IC_NOOP);
                ic->dest = result;
            }
            
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
                case Op_Greater_Than: ic->opcode = isFloat ? IC_SETA : IC_SETG; break;
                case Op_Greater_Equals: ic->opcode = isFloat ? IC_SETAE : IC_SETGE; break;
                case Op_Less_Than: ic->opcode = isFloat ? IC_SETB : IC_SETL; break;
                case Op_Less_Equals: ic->opcode = isFloat ? IC_SETBE : IC_SETLE; break;
                case Op_None: break;
                
                default: unimplemented;
            }
        }
        
        if (is_assign) {
            Intermediate_Code* curr = cu->ic_last;
            src0 = convert_expr_to_intermediate_code(cu, expr->Binary_Expr.first);
            
#if BUILD_DEBUG
            if (src0.raw_type != result.raw_type) {
                pln("AST:\n%", f_ast(expr));
                assert(0);
            }
#endif
            
            //if (src0.type & IC_DISP) {
            //pln("%", f_ast(expr));
            //}
            
            convert_assign_to_intermediate_code(cu, expr->type, src0, result);
        }
    }
    
    return result;
}

inline void
convert_ic_to_conditional_jump(Compilation_Unit* cu, Intermediate_Code* code, Ic_Arg cond, Ic_Basic_Block* false_target) {
    if (code && ic_is_setcc(code->opcode)) {
        switch (code->opcode) {
            case IC_SETE: code->opcode = IC_JNE; break;
            case IC_SETNE: code->opcode = IC_JE; break;
            case IC_SETG: code->opcode = IC_JNG; break;
            case IC_SETGE: code->opcode = IC_JNGE; break;
            case IC_SETL: code->opcode = IC_JNL; break;
            case IC_SETLE: code->opcode = IC_JNLE; break;
            case IC_SETA: code->opcode = IC_JNA; break;
            case IC_SETAE: code->opcode = IC_JNAE; break;
            case IC_SETB: code->opcode = IC_JNB; break;
            case IC_SETBE: code->opcode = IC_JNBE; break;
            
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
        code = ic_add(cu, IC_JE, false_target);
    }
}

Ic_Arg
convert_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr) {
    Ic_Arg result = {};
    
    switch (expr->kind) {
        case Ast_None: {
        } break;
        
        case Ast_Value: {
            if (is_integer(expr->Value)) {
                Ic_Raw_Type raw_type = convert_type_to_raw_type(expr->type);
                result.type = raw_type + IC_DISP;
                result.disp = value_to_s64(expr->Value);
                
            } else if (is_floating(expr->Value)) {
                Type* type = expr->type;
                Ic_Raw_Type raw_type = convert_type_to_raw_type(expr->type);
                void* data = malloc(type->size);
                value_store_in_memory(type, data, expr->Value.data);
                result = ic_rip_disp32(raw_type, data);
                
            } else if (is_string(expr->Value)) {
                Ic_Raw_Type raw_type = IC_T64;
                string* str = (string*) malloc(sizeof(string));
                *str = expr->Value.data.str;
                result = ic_rip_disp32(raw_type, str);
                
            } else if (is_cstring(expr->Value)) {
                cstring cstr = expr->Value.data.cstr;
                if (cstr == 0) {
                    result = ic_imm(IC_S64, 0);
                } else {
                    result = ic_rip_disp32(IC_T64, (void*) cstr);
                }
                
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Ident: {
            assert(expr->type);
            Type* type = expr->type;
            
            string_id ident = ast_unwrap_ident(expr);
            if (map_key_exists(cu->locals, ident)) {
                Ic_Raw_Type raw_type = convert_type_to_raw_type(type);
                result = map_get(cu->locals, ident);
            } else {
                if (type->kind == TypeKind_Function && type->Function.unit) {
                    
                    Intermediate_Code* ic = ic_add(cu, IC_LPROC, type->Function.unit);
                    ic->dest = ic_reg(IC_T64);
                    result = ic->dest;
                    
                } else {
                    Ic_Raw_Type raw_type = convert_type_to_raw_type(type);
                    void* data = get_interp_value_pointer(cu->interp, ident);
                    if (!data) {
                        Type_Context tcx = {};
                        type_error(&tcx, string_format("compiler bug: value of `%` is void", f_var(ident)), expr->span);
                        //pln();
                        assert(0);
                    }
                    result = ic_rip_disp32(raw_type, data);
                }
            }
        } break;
        
        case Ast_Paren_Expr: {
            result = convert_expr_to_intermediate_code(cu, expr->Paren_Expr.expr);
        } break;
        
        case Ast_Aggregate_Expr: {
            Type* type = expr->type;
            void* data = convert_aggregate_literal_to_memory(expr);
            
            if (data) {
                result = ic_rip_disp32(IC_T64, data);
                
                // Write to non-constant fields
                Ic_Arg result_dest = {};
                int field_index = 0;
                for_compound(expr->Aggregate_Expr.elements, field) {
                    assert(field->kind == Ast_Argument);
                    
                    string_id ident = try_unwrap_ident(field->Argument.ident);
                    Ast* assign = field->Argument.assign;
                    // TODO(Alexander): make it possible to store dynamic things
                    if (assign->kind != Ast_Value && 
                        assign->kind != Ast_Aggregate_Expr) {
                        
                        Struct_Field_Info info = {};
                        if (ident) {
                            info = get_field_info(&type->Struct_Like, ident);
                        } else {
                            info = get_field_info_by_index(&type->Struct_Like, field_index);
                        }
                        //pln("Adding non-constant field: %", f_ast(assign));
                        
                        Ic_Arg src = convert_expr_to_intermediate_code(cu, assign);
                        if (!result_dest.type) {
                            Ic_Arg dest = ic_reg(IC_T64, X64_RDX);
                            ic_mov(cu, dest, result);
                            result_dest = ic_stk(IC_T64, 0, IcStkArea_None, dest.reg);
                        }
                        
                        Ic_Arg dest = result_dest;
                        dest.raw_type = src.raw_type;
                        dest.disp += info.offset;
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
                assert(result.type & (IC_STK | IC_DISP | IC_RIP_DISP32));
                
                if (type->kind == TypeKind_Pointer) {
                    Ic_Arg tmp = ic_reg(IC_T64, X64_RDX);
                    ic_mov(cu, tmp, result);
                    result = ic_stk(tmp.raw_type, 0, IcStkArea_None, tmp.reg);
                    
                    type = type->Pointer;
                }
                
                if (type->kind == TypeKind_Struct || type->kind == TypeKind_Union) {
                    Struct_Field_Info info = get_field_info(&type->Struct_Like, ident);
                    result.disp += info.offset;
                    result.raw_type = convert_type_to_raw_type(expr->type);
                    
                } else if (type->kind == TypeKind_Array || type == t_string) {
                    // TODO(Alexander): this has hardcoded sizes and types for now
                    switch (ident) {
                        case Sym_data: {
                            result.raw_type = IC_T64;
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
            Type* array_type = expr->Index_Expr.array->type;
            
            
            Ic_Raw_Type rt = convert_type_to_raw_type(type);
            Ic_Arg arr = convert_expr_to_intermediate_code(cu, expr->Index_Expr.array);
            Ic_Arg index = convert_expr_to_intermediate_code(cu, expr->Index_Expr.index);
            
            if (array_type->kind == TypeKind_Pointer ||
                (array_type->kind == TypeKind_Array &&
                 (array_type->Array.capacity == 0 || array_type->Array.is_dynamic))) {
                
                // Array is stored as pointer or "wide" pointer
                Ic_Arg tmp = ic_reg(arr.raw_type, X64_RDX);
                ic_mov(cu, tmp, arr);
                arr = ic_stk(tmp.raw_type, 0, IcStkArea_None, tmp.reg);
            }
            // else if (array_type->kind == TypeKind_Pointer) {
            // Ic_Arg tmp = ic_reg(arr.raw_type, X64_RDX);
            // ic_mov(cu, tmp, arr);
            // arr = ic_stk(tmp.raw_type, 0, IcStkArea_None, tmp.reg);
            // }
            
            // Array is stored in place
            result = arr;
            result.raw_type = rt;
            if (result.type & IC_STK) {
                if (index.type & IC_DISP) {
                    result.disp += type->size * index.disp;
                } else {
                    if (index.type & IC_STK) {
                        Ic_Arg tmp = ic_reg(index.raw_type);
                        ic_mov(cu, tmp, index);
                        index = tmp;
                    }
                    
                    // TODO(Alexander): add support for SIB
                    //if (x64_is_scalar_type(type->size)) {
                    //SIB: RSP + RBP * scale
                    //result.scale = (u8) intrin_index_of_first_set_bit(type->size);
                    //result.index = index.reg;
                    //}
                    
                    Intermediate_Code* mul_ic = ic_add(cu, IC_MUL);
                    mul_ic->dest = ic_reg(IC_S64);
                    mul_ic->src0 = index;
                    mul_ic->src1 = ic_imm(IC_S64, type->size);
                    
                    Intermediate_Code* add_ic = ic_add(cu, IC_ADD);
                    add_ic->dest = mul_ic->dest;
                    add_ic->src0 = mul_ic->dest;
                    add_ic->src1 = ic_reg(IC_S64, arr.reg);
                    
                    result.reg = add_ic->dest.reg;
                }
            } else {
                if (index.type & IC_DISP) {
                    result.disp += type->size * index.disp;
                } else {
                    unimplemented;
                }
            }
        } break;
        
        case Ast_Unary_Expr: {
            Ic_Arg src = convert_expr_to_intermediate_code(cu, expr->Unary_Expr.first);
            Ic_Raw_Type rt = convert_type_to_raw_type(expr->type);
            
            switch (expr->Unary_Expr.op) {
                case Op_Negate: {
                    assert(src.type & IC_STK | IC_REG);
                    
                    if (src.type & IC_FLOAT) {
                        Type* type = expr->type;
                        void* data = malloc(type->size);
                        if (type == t_f64) {
                            u64* values = (u64*) data;
                            for (int i = 0; i < 2; i++) *values++ = 0x8000000000000000ull;
                        } else {
                            u32* values = (u32*) data;
                            for (int i = 0; i < 4; i++) *values++ = 0x80000000;
                        }
                        
                        Intermediate_Code* ic = ic_add(cu, IC_FXOR);
                        ic->dest = ic_reg(rt);
                        ic->src0 = src;
                        ic->src1 = ic_rip_disp32(rt, data);
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
                    
                    Intermediate_Code* ic = ic_add(cu, IC_TEST);
                    ic->src0 = src;
                    ic->src1 = src;
                    
                    ic_add(cu, IC_JNE, bb_unset);
                    ic_mov(cu, result, ic_imm(rt, 1));
                    ic_add(cu, IC_JMP, bb_exit);
                    
                    ic_add(cu, IC_LABEL, bb_unset);
                    ic_mov(cu, result, ic_imm(rt, 0));
                    
                    ic_add(cu, IC_LABEL, bb_exit);
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
                    } else {
                        assert(src.type & IC_STK);
                        
                        Intermediate_Code* ic = ic_add(cu, IC_MOV);
                        ic->src0 = ic_reg(IC_T64, X64_RDX);
                        ic->src1 = src;
                        result = ic_stk(rt, 0, IcStkArea_None, ic->src0.reg);
                    }
                } break;
                
                case Op_Post_Decrement:
                case Op_Post_Increment: {
                    assert(src.type & IC_DISP_STK_RIP);
                    
                    result = ic_reg(convert_type_to_raw_type(expr->type), X64_RDX);
                    ic_mov(cu, result, src);
                    
                    Ic_Opcode opcode = expr->Unary_Expr.op == Op_Post_Decrement ? IC_DEC : IC_INC;
                    Intermediate_Code* ic = ic_add(cu, opcode);
                    ic->src0 = src;
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
            ic_mov(cu, result, value);
            ic_add(cu, IC_JMP, bb_exit);
            
            ic_add(cu, IC_LABEL, bb_else);
            value = convert_expr_to_intermediate_code(cu, expr->Ternary_Expr.third);
            ic_mov(cu, result, value);
            
            ic_add(cu, IC_LABEL, bb_exit);
        } break;
        
        case Ast_Call_Expr: {
            array(Ast*)* args = 0;
            
            // Setup arguments
            for_compound(expr->Call_Expr.args, arg) {
                assert(arg->kind == Ast_Argument);
                array_push(args, arg->Argument.assign);
            }
            
            Type* function_type = expr->Call_Expr.function_type;
            result = convert_function_call_to_intermediate_code(cu, function_type, args);
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
            
            
            if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
                Ic_Raw_Type dest_raw_type = basic_type_to_raw_type(t_dest->Basic.kind);
                
                if (t_src->Basic.flags & BasicFlag_Integer) {
                    if (t_dest->Basic.flags & BasicFlag_Integer) {
                        if (t_dest->size > t_src->size) {
                            Intermediate_Code* ic = ic_add(cu, 
                                                           t_src->Basic.flags & BasicFlag_Unsigned ? 
                                                           IC_MOVZX : IC_MOVSX);
                            ic->src0 = ic_reg(dest_raw_type);
                            ic->src1 = src;
                            result = ic->src0;
                            
                        } else {
                            // TODO(Alexander): do we need truncation instruction?
                            result.raw_type = dest_raw_type;
                        }
                        
                    } else if (t_dest->Basic.flags & BasicFlag_Floating) {
                        Intermediate_Code* ic = ic_add(cu, IC_CAST_S2F);
                        ic->src0 = ic_reg(dest_raw_type);
                        ic->src1 = src;
                        result = ic->src0;
                    } else {
                        assert(0 && "invalid type cast");
                    }
                } else if (t_src->Basic.flags & BasicFlag_Floating) {
                    if (t_dest->Basic.flags & BasicFlag_Integer) {
                        Intermediate_Code* ic = ic_add(cu, IC_CAST_F2S);
                        ic->src0 = ic_reg(dest_raw_type);
                        ic->src1 = src;
                        result = ic->src0;
                    } else if (t_dest->Basic.flags & BasicFlag_Floating) {
                        if (t_dest->size != t_src->size) {
                            Intermediate_Code* ic = ic_add(cu, IC_CAST_F2F);
                            ic->src0 = ic_reg(dest_raw_type);
                            ic->src1 = src;
                            result = ic->src0;
                        }
                    } else {
                        assert(0 && "invalid type cast");
                    }
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
        case Ast_Decl_Stmt: {
            Ast* decl = stmt->Decl_Stmt.stmt;
            if (is_ast_stmt(decl)) {
                convert_stmt_to_intermediate_code(cu, decl, bb_break, bb_continue);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            convert_expr_to_intermediate_code(cu, stmt->Expr_Stmt);
        } break;
        
        case Ast_Assign_Stmt: {
            Type* type = stmt->type;
            
            Ic_Arg src = {};
            if (is_valid_ast(stmt->Assign_Stmt.expr)) {
                src = convert_expr_to_intermediate_code(cu, stmt->Assign_Stmt.expr);
            }
            
            
            // NOTE(Alexander): push local first then it can be found elsewhere
            if (stmt->Assign_Stmt.ident->kind == Ast_Ident) {
                string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
                ic_push_local(cu, type, ident);
                
                Ic_Arg dest = convert_expr_to_intermediate_code(cu, stmt->Assign_Stmt.ident);
                convert_assign_to_intermediate_code(cu, stmt->type, dest, src);
                
            } else if (stmt->Assign_Stmt.ident->kind == Ast_Compound) {
                for_compound(stmt->Assign_Stmt.ident, it) {
                    string_id ident = ast_unwrap_ident(it);
                    ic_push_local(cu, type, ident);
                    
                    it->type = type; // TODO(Alexander): isn't type checker doing this?
                    Ic_Arg dest = convert_expr_to_intermediate_code(cu, it);
                    convert_assign_to_intermediate_code(cu, stmt->type, dest, src);
                }
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
            ic_add(cu, IC_JMP, bb_break);
        } break;
        
        case Ast_Continue_Stmt: {
            assert(bb_continue);
            ic_add(cu, IC_JMP, bb_continue);
        } break;
        
        case Ast_If_Stmt: {
            Ic_Basic_Block* bb_else = ic_basic_block();
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->If_Stmt.cond);
            convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_else);
            
            convert_stmt_to_intermediate_code(cu, stmt->If_Stmt.then_block, bb_break, bb_continue);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                Ic_Basic_Block* bb_exit = ic_basic_block();
                ic_add(cu, IC_JMP, bb_exit);
                ic_add(cu, IC_LABEL, bb_else);
                convert_stmt_to_intermediate_code(cu, stmt->If_Stmt.else_block, bb_break, bb_continue);
                ic_add(cu, IC_LABEL, bb_exit);
            } else {
                ic_add(cu, IC_LABEL, bb_else);
            }
        } break;
        
        case Ast_For_Stmt: {
            Ic_Basic_Block* bb_enter = ic_basic_block();
            Ic_Basic_Block* bb_exit = ic_basic_block();
            
            convert_stmt_to_intermediate_code(cu, stmt->For_Stmt.init, bb_break, bb_continue);
            
            ic_add(cu, IC_LABEL, bb_enter);
            if (is_valid_ast(stmt->For_Stmt.cond)) {
                Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->For_Stmt.cond);
                convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_exit);
            }
            
            convert_stmt_to_intermediate_code(cu, stmt->For_Stmt.block, bb_exit, bb_enter);
            convert_expr_to_intermediate_code(cu, stmt->For_Stmt.update);
            ic_add(cu, IC_JMP, bb_enter);
            ic_add(cu, IC_LABEL, bb_exit);
        } break;
        
        case Ast_While_Stmt: {
            Ic_Basic_Block* bb_enter = ic_basic_block();
            Ic_Basic_Block* bb_exit = ic_basic_block();
            
            ic_add(cu, IC_LABEL, bb_enter);
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->While_Stmt.cond);
            convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_exit);
            
            convert_stmt_to_intermediate_code(cu, stmt->While_Stmt.block, bb_exit, bb_enter);
            ic_add(cu, IC_JMP, bb_enter);
            ic_add(cu, IC_LABEL, bb_exit);
        } break;
        
        case Ast_Switch_Stmt: {
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->Switch_Stmt.cond);
            assert(cond.type & IC_STK);
            
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
                    ic_add(cu, IC_JNE, bb_next_case);
                    ic_add(cu, IC_LABEL, bb_curr_stmt);
                    if (has_default_case && !default_case_stmt) {
                        default_case_stmt = it->Switch_Case.stmt;
                    }
                    convert_stmt_to_intermediate_code(cu, it->Switch_Case.stmt, 
                                                      bb_exit, bb_continue);
                    ic_add(cu, IC_JMP, bb_exit);
                    
                    bb_curr_stmt = ic_basic_block();
                } else {
                    ic_add(cu, IC_JE, bb_curr_stmt);
                }
                
                ic_add(cu, IC_LABEL, bb_next_case);
            }
            
            if (default_case_stmt) {
                ic_add(cu, IC_LABEL, bb_curr_stmt);
                convert_stmt_to_intermediate_code(cu, default_case_stmt, 
                                                  bb_exit, bb_continue);
            }
            
            ic_add(cu, IC_LABEL, bb_exit);
            
        } break;
        
        case Ast_Return_Stmt: {
            // TODO(Alexander): this is platform/architecture specific
            Ic_Arg result = convert_expr_to_intermediate_code(cu, stmt->Return_Stmt.expr);
            if (cu->ic_return.type) {
                if (!((result.type & IC_REG) && (result.reg == 0) &&
                      (result.type & (IC_FLOAT | IC_SINT | IC_UINT)))) {
                    convert_assign_to_intermediate_code(cu, stmt->type, cu->ic_return, result);
                }
            }
            ic_add(cu, IC_JMP, cu->bb_return);
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
    
    Type_Function* proc = &cu->ast->type->Function;
    
    Ic_Basic_Block* bb_begin = ic_basic_block();
    Ic_Basic_Block* bb_end = ic_basic_block();
    cu->bb_return = bb_end;
    ic_add(cu, IC_LABEL, bb_begin);
    
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
        convert_assign_to_intermediate_code(cu, c->type, dest, c->dest);
    }
    
    convert_stmt_to_intermediate_code(cu, cu->ast, 0, 0);
    ic_add(cu, IC_LABEL, bb_end);
    ic_add(cu, IC_EPLG);
    
    // Align stack by 16-bytes (excluding 8 bytes for return address)
    cu->stk_usage = cu->stk_locals + cu->stk_args + 8;
    cu->stk_usage = align_forward(cu->stk_usage, 16);
    cu->stk_locals = cu->stk_usage - (cu->stk_args);
    cu->stk_usage -= 8;
}

void
x64_sib(Intermediate_Code* ic, u8 scale, u8 index, u8 base) {
    ic_u8(ic, scale << 6 | index << 3 | base);
}

void
x64_rip_relative(Intermediate_Code* ic, s64 r, s64 data, s64 rip) {
    ic_u8(ic, ((u8) (r&7)<<3) | (u8) X64_RBP);
    
    s64 disp = (s64) data - (rip + ic->count + 4);
    ic_u32(ic, (u32) disp);
}

void
x64_modrm(Intermediate_Code* ic, Ic_Type t, s64 d, s64 r, s64 rm, s64 rip=0) {
    if (t & IC_STK) {
        if (d < S8_MIN || d > S8_MAX) {
            ic_u8(ic, MODRM_INDIRECT_DISP32 | (((u8) r&7)<<3) | (u8) rm&7);
            if (rm == X64_RSP) {
                ic_u8(ic, (u8) (rm << 3) | (u8) rm);
            }
            ic_u32(ic, (u32) d);
        } else {
            ic_u8(ic, MODRM_INDIRECT_DISP8 | (((u8) r&7)<<3) | (u8) rm&7);
            if (rm == X64_RSP) {
                ic_u8(ic, (u8) (rm << 3) | (u8) rm);
            }
            ic_u8(ic, (u8) d);
        }
    } else if (t & IC_RIP_DISP32) {
        x64_rip_relative(ic, r, d, rip);
    } else {
        ic_u8(ic, MODRM_DIRECT | (((u8) r&7)<<3) | (u8) rm&7);
    }
}

inline void
x64_unary(Intermediate_Code* ic, Ic_Type t, s64 r, s64 d, u8 opcode, u8 reg_field, s64 rip) {
    if (ic->dest.type & IC_REG) {
        x64_mov(ic, ic->dest.type, ic->dest.reg, ic->dest.disp, t, r, d, rip);
        t = ic->dest.type;
        r = ic->dest.reg;
        d = ic->dest.disp;
    }
    
    // F7 /3 	NEG r/m32 	M
    ic_u8(ic, opcode);
    x64_modrm(ic, t, d, reg_field, r, rip);
}

inline void
x64_binary(Intermediate_Code* ic,
           Ic_Type t1, s64 r1, s64 d1, 
           Ic_Type t2, s64 r2, s64 d2,
           u8 reg_field, u8 opcode, s64 rip) {
    
    if (t1 & IC_DISP) {
        Ic_Type tmpt = IC_REG + (t1 & IC_RT_MASK);
        s64 tmpr = X64_RAX;
        x64_mov(ic, tmpt, tmpr, 0, t1, r1, d1, rip);
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
                    x64_rex(ic, REX_FLAG_64_BIT);
                }
                
                // 81 /0 id 	ADD r/m32, imm32 	MI
                //ic_u8(ic, 0x81);
                ic_u8(ic, (t1 & IC_T8) ? 0x80 : 0x81);
                ic_u8(ic, 0xC0 | (reg_field << 3) | (u8) r1);
                if (t1 & IC_T8) {
                    ic_u8(ic, (u8) d2);
                } else {
                    ic_u32(ic, (u32) d2);
                }
                
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
        
        case IC_STK:
        case IC_RIP_DISP32: {
            if (t2 & IC_DISP) {
                
                if (d2 > U32_MAX) {
                    unimplemented;
                }
                
                if (t1 & IC_T64) {
                    x64_rex(ic, REX_FLAG_64_BIT);
                }
                
                // 81 /0 id 	ADD r/m32, imm32 	MI
                ic_u8(ic, (t1 & IC_T8) ? 0x80 : 0x81);
                //ic_u8(ic, 0xC7);
                x64_modrm(ic, t1, d1, reg_field, r1, rip);
                if (t1 & IC_T8) {
                    ic_u8(ic, (u8) d2);
                } else {
                    ic_u32(ic, (u32) d2);
                }
                
            } else if (t2 & IC_REG) {
                // 01 /r 	ADD r/m32, r32 	MR
                ic_u8(ic, opcode + 1);
                x64_modrm(ic, t1, d1, r2, r1, rip);
                
            } else if (t2 & IC_STK_RIP) {
                x64_mov(ic, IC_REG + (t2 & IC_RT_MASK), X64_RAX, 0, t2, r2, d2, rip);
                x64_binary(ic, t1, r1, d1, IC_REG + (t1 & IC_RT_MASK), X64_RAX, 0, reg_field, opcode, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}


inline void
x64_add(Intermediate_Code* ic, 
        Ic_Type t1, s64 r1, s64 d1, 
        Ic_Type t2, s64 r2, s64 d2, 
        Ic_Type t3, s64 r3, s64 d3, 
        u8 reg_field, u8 opcode, s64 rip) {
    
    s64 tmpr = -1;
    if (t1 & IC_REG) {
        if (!(t2 & IC_REG) && t1 != t3 && r1 != r3) {
            x64_mov(ic, t1, r1, d1, t2, r2, d2, rip);
        } else  {
            x64_mov(ic, t1, X64_RCX, 0, t2, r2, d2, rip);
            tmpr = r1;
            r1 = X64_RCX;
        }
    } else {
        unimplemented;
    }
    
    x64_binary(ic, t1, r1, d1, t3, r3, d3, reg_field, opcode, rip);
    
    if (tmpr != -1) {
        x64_mov(ic, t1, tmpr, 0, t1, X64_RCX, 0, rip);
        tmpr = -1;
    }
}

void
x64_zero(Intermediate_Code* ic, s64 r) {
    // REX.W + 33 /r 	XOR r64, r/m64 	RM
    u8 rflag = (u8) (r&8) >> 1;
    x64_rex(ic, REX_FLAG_W | rflag | rflag >> 2);
    ic_u8(ic, 0x33);
    x64_modrm(ic, IC_S64 + IC_REG, 0, r, r);
}

void
x64_mov_rax_u64(Intermediate_Code* ic, u64 disp) {
    // REX.W + B8+ rd io 	MOV r64, imm64 	OI
    x64_rex(ic, REX_FLAG_W);
    ic_u8(ic, 0xB8);
    ic_u64(ic, disp);
}

void
x64_mov(Intermediate_Code* ic,
        Ic_Type t1, s64 r1, s64 d1,
        Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_DISP) {
                if (d2 == 0) {
                    x64_zero(ic, r1);
                } else if ((u64) d2 > U32_MAX) {
                    // REX.W + B8+ rd io 	MOV r64, imm64 	OI
                    x64_rex(ic, REX_FLAG_W | (r1&8)>>3);
                    ic_u8(ic, 0xB8+((u8)r1&7));
                    ic_u64(ic, (u64) d2);
                } else {
                    // B8+ rd id 	MOV r32, imm32 	OI
                    if (r1&8) {
                        x64_rex(ic, REX_FLAG_B);
                    }
                    ic_u8(ic, 0xB8+((u8)r1&7));
                    ic_u32(ic, (u32) d2);
                }
                
            } else if (t2 & (IC_REG | IC_STK | IC_RIP_DISP32)) {
                if (t1 & IC_T64 || r1&8 || r2&8) {
                    x64_rex(ic, (t1&IC_T64) | ((u8) r1&8)>>1 | ((u8)r2&8)>>3);
                }
                // 8B /r 	MOV r32,r/m32 	RM
                ic_u8(ic, 0x8B);
                x64_modrm(ic, t2, d2, r1, r2, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        case IC_STK:
        case IC_RIP_DISP32: {
            if (t2 & IC_DISP) {
                if ((u64) d2 > U32_MAX) {
                    x64_mov_rax_u64(ic, d2);
                    x64_mov(ic, t1, r1, d1, IC_T64 + IC_REG, X64_RAX, 0, rip);
                } else {
                    // C7 /0 id 	MOV r/m32, imm32 	MI
                    if (t1 & IC_T64) {
                        x64_rex(ic, REX_FLAG_64_BIT);
                    }
                    
                    ic_u8(ic, t1 & IC_T8 ? 0xC6 : 0xC7);
                    if (t1 & IC_RIP_DISP32)  {
                        x64_modrm(ic, t1, d1, 0, r1, rip + 4);
                    } else {
                        x64_modrm(ic, t1, d1, 0, r1, rip);
                    }
                    if (t1 & IC_T8) {
                        ic_u8(ic, (u8) d2);
                    } else {
                        ic_u32(ic, (u32) d2);
                    }
                }
            } else if (t2 & IC_REG) {
                // 89 /r 	MOV r/m32,r32 	MR
                if ((t2 & IC_T64) || (r2 & 8)) {
                    x64_rex(ic, t2&IC_T64|((u8) r2&8)>>1);
                }
                ic_u8(ic, t1 & IC_T8 ? 0x88 : 0x89);
                x64_modrm(ic, t1, d1, r2&7, r1, rip);
            } else if (t2 & (IC_STK_RIP)) {
                // NOTE(Alexander): x64 doesn't allow MOV STK, STK, move to tmp reg
                // TODO(Alexander): reg hardcoded RAX
                s64 tmpr = X64_RAX;
                if (r2 == tmpr || r1 == tmpr) {
                    tmpr = X64_RCX;
                }
                x64_mov(ic, IC_REG + (t2 & IC_RT_MASK), tmpr, 0, t2, r2, d2, rip);
                x64_mov(ic, t1, r1, d1, IC_REG + (t1 & IC_RT_MASK), tmpr, 0, rip);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        default: assert(0 && "invalid instruction");
    }
}

void
x64_fmov(Intermediate_Code* ic,
         Ic_Type t1, s64 r1, s64 d1,
         Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    assert(t1 & IC_FLOAT);
    
    if ((t1 & IC_STK_RIP) && (t2 & IC_STK_RIP)) {
        Ic_Type tmpt = IC_REG + (t2 & IC_RT_MASK);
        s64 tmpr = X64_XMM0;
        x64_fmov(ic, tmpt, tmpr, 0, t2, r2, d2, rip);
        t2 = tmpt;
        r2 = tmpr;
        d2 = 0;
    }
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            // F3 0F 10 /r MOVSS xmm1, m32 or
            // F2 0F 10 /r MOVSD xmm1, m64
            ic_u8(ic, (t1 & IC_T32) ? 0xF3 : 0xF2);
            ic_u8(ic, 0x0F);
            ic_u8(ic, 0x10);
            x64_modrm(ic, t2, d2, r1, r2, rip);
        } break;
        
        case IC_STK:
        case IC_RIP_DISP32: {
            assert(t2 & IC_REG);
            
            // F3 0F 11 /r MOVSS xmm2/m32, xmm1 or
            // F2 0F 11 /r MOVSD xmm1/m64, xmm2
            ic_u8(ic, (t1 & IC_T32) ? 0xF3 : 0xF2);
            ic_u8(ic, 0x0F);
            ic_u8(ic, 0x11);
            x64_modrm(ic, t1, d1, r2, r1, rip);
        } break;
        
        default: unimplemented;
    }
    
}

inline void
x64_mul(Intermediate_Code* ic, s64 rip) {
    assert(ic->dest.type & IC_REG);
    
    if (ic->src0.type & IC_DISP) {
        Ic_Arg tmp = ic->src0;
        ic->src0 = ic->src1;
        ic->src1 = tmp;
    }
    
    if (ic->src0.type & IC_DISP) {
        ic->src0.disp = ic->src0.disp * ic->src1.disp;
        x64_mov(ic, 
                ic->dest.type, ic->dest.reg, ic->dest.disp, 
                ic->src0.type, ic->src0.reg, ic->src0.disp, rip);
        
        return;
    }
    
    if (ic->src1.type & IC_DISP) {
        // 69 /r id 	IMUL r32, r/m32, imm32 	RMI
        if (ic->dest.type & IC_T64) {
            x64_rex(ic, REX_FLAG_64_BIT);
        }
        
        ic_u8(ic, 0x69);
        x64_modrm(ic, ic->src0.type, ic->src0.disp, ic->dest.reg, ic->src0.reg);
        assert(ic->src1.disp >= S32_MIN && ic->src1.disp <= S32_MAX && "cannot fit in imm32");
        ic_u32(ic, (u32) ic->src1.disp);
    } else {
        Ic_Type t1 = ic->src0.type;
        s64 r1 = ic->src0.reg;
        s64 d1 = ic->src0.disp;
        
        Ic_Type t2 = ic->src1.type;
        s64 r2 = ic->src1.reg;
        s64 d2 = ic->src1.disp;
        
        if (t1 & IC_STK) {
            // NOTE(Alexander): swap the order of mul
            if (t2 & IC_STK || r2 != ic->dest.reg) {
                x64_mov(ic, ic->dest.type, ic->dest.reg, ic->dest.disp, t2, r2, d2, rip);
            }
            t2 = t1;
            r2 = r1;
            d2 = d1;
            
            t1 = ic->dest.type;
            r1 = ic->dest.reg;
            d1 = ic->dest.disp;
        }
        
        // 0F AF /r 	IMUL r32, r/m32
        ic_u16(ic, 0xAF0F);
        x64_modrm(ic, t2, d2, r1, r2);
    }
}

inline void
x64_div(Intermediate_Code* ic, bool remainder, s64 rip) {
    
    x64_mov(ic, ic->src0.raw_type + IC_REG, X64_RAX, 0, ic->src0.type, ic->src0.reg, ic->src0.disp, rip);
    x64_mov(ic, ic->src1.raw_type + IC_REG, X64_RCX, 0, ic->src1.type, ic->src1.reg, ic->src1.disp, rip);
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
    x64_mov(ic, ic->dest.type, ic->dest.reg, ic->dest.disp, IC_REG, dest_reg, 0, rip);
}

inline void
x64_float_binary(Intermediate_Code* ic, u8 opcode, s64 rip, s64 prefix_opcode=-1) {
    
    Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
    s64 r1 = ic->src0.reg, r2 = ic->src1.reg;
    s64 d1 = ic->src0.disp, d2 = ic->src1.disp;
    assert(t1 & IC_FLOAT);
    // NOTE(Alexander): assumes destination to be a register
    assert(ic->dest.type & IC_REG);
    
    // Make sure first argument is a register
    if (t1 & IC_DISP_STK_RIP) {
        if (t2 & IC_REG && ic->dest.reg == r2) {
            x64_fmov(ic, ic->dest.type, X64_XMM1, 0, t2, r2, d2, rip);
            t2 = ic->dest.type;
            r2 = X64_XMM1;
        }
        x64_fmov(ic, ic->dest.type, ic->dest.reg, ic->dest.disp, 
                 t1, r1, d1, rip);
        
        t1 = ic->dest.type;
        r1 = ic->dest.reg;
        d1 = ic->dest.disp;
    }
    
    //t1 = ic->dest.type;
    //r1 = ic->dest.reg;
    //d1 = ic->dest.disp;
    
    
    // F3 0F 5E /r DIVSS xmm1, xmm2/m32
    // F2 0F 5E /r DIVSD xmm1, xmm2/m64
    if (prefix_opcode == -1) {
        prefix_opcode = (t1 & IC_T32) ? 0xF3 : 0xF2;
    }
    if (prefix_opcode >= 0) {
        ic_u8(ic, (u8) prefix_opcode);
    }
    ic_u8(ic, 0x0F);
    ic_u8(ic, opcode);
    x64_modrm(ic, t2, d2, r1, r2, rip);
    
    if (!(ic->dest.type == t1 && ic->dest.reg == r1)) {
        x64_fmov(ic, ic->dest.type, ic->dest.reg, 0, t1, r1, d1, rip);
    }
    
}

inline void
x64_convert_type(Intermediate_Code* ic, u8 opcode, s64 rip) {
    Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
    s64 r1 = ic->src0.reg, d1 = ic->src0.disp, r2 = ic->src1.reg, d2 = ic->src1.disp;
    assert(t1 & IC_REG);
    assert(t2 & (IC_REG | IC_STK | IC_RIP_DISP32));
    
    // F3 0F 2D /r CVTSS2SI r32, xmm1/m32
    ic_u8(ic, t2 & IC_T64 ? 0xF2 : 0xF3);
    if (t1 & IC_T64) {
        x64_rex(ic, REX_FLAG_64_BIT);
    }
    ic_u16(ic, 0x0F|(opcode<<8));
    x64_modrm(ic, t2, d2, r1, r2, rip);
}


void
x64_lea(Intermediate_Code* ic, s64 r1, s64 r2, s64 d2, s64 rip) {
    // REX.W + 8D /r 	LEA r64,m 	RM
    x64_rex(ic, REX_FLAG_W|((u8)r1&8)>>1);
    ic_u8(ic, 0x8D);
    
    if (r2 == X64_RIP) {
        ic_u8(ic, (u8) (r1&7)<<3 | (u8) X64_RBP);
        ic_u32(ic, (u32) (d2 - rip - ic->count - 4));
    } else {
        x64_modrm(ic, IC_STK, d2, r1&7, r2);
    }
}

void
x64_jump(Intermediate_Code* ic, Ic_Basic_Block* bb, s64 rip) {
    if (bb && bb->addr != IC_INVALID_ADDR) {
        // TODO(Alexander): add support for short jumps
        assert(bb->addr >= S32_MIN && bb->addr <= S32_MAX && "cannot fit addr in rel32");
        ic_u32(ic, (s32) (bb->addr - rip - ic->count - 4));
    } else {
        ic_u32(ic, (s32) 0);
    }
}

void
x64_string_op(Intermediate_Code* ic, 
              Ic_Type destt, s64 destr, s64 destd, 
              Ic_Type srct, s64 srcr, s64 srcd, 
              s64 count, u16 opcode, s64 rip, s64 src_int_reg=X64_RAX) {
    // Move RCX bytes from [RSI] to [RDI].
    x64_mov(ic, IC_S64 + IC_REG, X64_RCX, 0, IC_S64 + IC_DISP, 0, count, rip);
    
    if (destt & (IC_UINT | IC_SINT)) {
        x64_mov(ic, IC_S64 + IC_REG, X64_RDI, 0, IC_S64 + IC_STK, X64_RSP, destd, rip);
    } else {
        x64_lea(ic, X64_RDI, destr, destd, rip);
    }
    
    if (srct & (IC_UINT | IC_SINT)) {
        x64_mov(ic, IC_S64 + IC_REG, src_int_reg, 0, srct, srcr, srcd, rip);
    } else if (srct & IC_T64) {
        //if (srct & IC_DISP) {
        //x64_lea(ic, X64_RSI, X64_RIP, disp, rip);
        //} else {
        x64_lea(ic, X64_RSI, srcr, srcd, rip);
        //}
    } else {
        unimplemented;
    }
    
    // e.g. F3 A4 	REP MOVS m8, m8 	ZO
    ic_u16(ic, opcode);
}

inline void
x64_shr(Intermediate_Code* ic, 
        Ic_Type t1, s64 r1, s64 d1, 
        Ic_Type t2, s64 r2, s64 d2, 
        Ic_Type t3, s64 r3, s64 d3, 
        u8 sar_reg_field, u8 shr_reg_field, s64 rip) {
    
    u8 reg_field = (t2 & IC_SINT) ? sar_reg_field : shr_reg_field;
    
    // TODO(Alexander): non immediate right-hand side
    x64_mov(ic, t1, r1, d1, t2, r2, d2, rip);
    
    if (t3 & IC_DISP) {
        // C1 /7 ib 	SAR r/m32, imm8 	MI (signed)
        // C1 /5 ib 	SHR r/m32, imm8 	MI (unsigned)
        ic_u8(ic, 0xC1);
        x64_modrm(ic, t1, d1, reg_field, r1);
        ic_u8(ic, (u8) d3);
    } else { 
        
        // D3 /7 	SAR r/m32, CL 	MC (signed)
        // D3 /5 	SHR r/m16, CL     MC (unsigned)
        ic_u8(ic, 0xC1);
        x64_modrm(ic, t1, d1, reg_field, r1);
    }
}

s64
convert_to_x64_machine_code(Intermediate_Code* ic, s64 stk_usage, u8* buf, s64 buf_size, s64 rip) {
    
    // We arrange the stack so arguments have positive displacement
    // and local variables have negative displacement.
    // But we are only allowed to write to RSP with positive displacement.
    // The rsp_adjust subtracts the arguments stack usage so they will
    // come first then followed by the local variables.
    s64 rsp_adjust = 0;
    
    while (ic) {
        ic->count = 0;
        
        switch (ic->opcode) {
            case IC_NOOP: break;
            
            case IC_PRLG: {
                x64_binary(ic, IC_REG + IC_S64, X64_RSP, 0, IC_DISP + IC_S32, 0, stk_usage, 5, 28, (s64) buf + rip);
            } break;
            
            case IC_EPLG: {
                x64_binary(ic, IC_REG + IC_S64, X64_RSP, 0, IC_DISP + IC_S32, 0, stk_usage, 0, 0, (s64) buf + rip);
                ic_u8(ic, 0xC3);
            } break;
            
            case IC_NEG: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xF7, 3, (s64) buf + rip);
            } break;
            
            case IC_NOT: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xF7, 2, (s64) buf + rip);
            } break;
            
            case IC_INC: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xFF, 0, (s64) buf + rip);
            } break;
            
            case IC_DEC: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 0xFF, 1, (s64) buf + rip);
            } break;
            
            case IC_ADD: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 0, 0, (s64) buf + rip);
            } break;
            
            case IC_SUB: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 5, 0x28, (s64) buf + rip);
            } break;
            
            case IC_AND: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 4, 0x20, (s64) buf + rip);
            } break;
            
            case IC_OR: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 1, 0x8, (s64) buf + rip);
            } break;
            
            case IC_XOR: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 6, 0x30, (s64) buf + rip);
            } break;
            
            case IC_SHL: {
                x64_shr(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 4, 4, (s64) buf + rip);
                
            } break;
            
            case IC_SHR: {
                x64_shr(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 7, 5, (s64) buf + rip);
            } break;
            
            case IC_MUL: {
                x64_mul(ic, (s64) buf + rip);
            } break;
            
            case IC_DIV: {
                x64_div(ic, false, (s64) buf + rip);
            } break;
            
            case IC_MOD: {
                x64_div(ic, true, (s64) buf + rip);
            } break;
            
            case IC_MOV: {
                x64_mov(ic,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, (s64) buf + rip);
            } break;
            
            case IC_MOVZX: {
                if (ic->src1.type & IC_T8) {
                    ic_u16(ic, 0xB60F);
                    x64_modrm(ic, ic->src1.type, ic->src1.disp, ic->src0.reg, ic->src1.reg, (s64) buf + rip);
                    
                } else if (ic->src1.type & IC_T16) {
                    ic_u16(ic, 0xB70F);
                    x64_modrm(ic, ic->src1.type, ic->src1.disp, ic->src0.reg, ic->src1.reg, (s64) buf + rip);
                    
                } else {
                    x64_mov(ic,
                            ic->src0.type, ic->src0.reg, ic->src0.disp,
                            ic->src1.type, ic->src1.reg, ic->src1.disp, (s64) buf + rip);
                }
                
            } break;
            
            case IC_MOVSX: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, d1 = ic->src0.disp, r2 = ic->src1.reg, d2 = ic->src1.disp;
                
                // REX.W + 63 /r 	MOVSXD r64, r/m32 	RM
                x64_rex(ic, REX_FLAG_W);
                ic_u8(ic, 0x63);
                x64_modrm(ic, t2, d2, r1, r2);
            } break;
            
            case IC_FMOV: {
                x64_fmov(ic,
                         ic->src0.type, ic->src0.reg, ic->src0.disp,
                         ic->src1.type, ic->src1.reg, ic->src1.disp,
                         (s64) buf + rip);
                
            } break;
            
            case IC_REINTERP_F2S: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, d1 = ic->src0.disp, r2 = ic->src1.reg, d2 = ic->src1.disp;
                
                if (t1 & (IC_SINT | IC_UINT) && t2 & IC_FLOAT) {
                    if (t1 & IC_REG) {
                        // 66 0F 7E /r MOVD r/m32, xmm
                        ic_u8(ic, 0x66);
                        if (t1 & IC_T64 || r1&8) {
                            x64_rex(ic, REX_FLAG_64_BIT | ((u8)r1&8)>>3);
                        }
                        ic_u16(ic, 0x7E0F);
                        x64_modrm(ic, t2, d2, r2&7, r1&7);
                    } else {
                        unimplemented;
                    }
                } else {
                    unimplemented;
                }
            } break;
            
            case IC_CAST_F2S: {
                x64_convert_type(ic, 0x2C, (s64) buf + rip);
            } break;
            
            case IC_CAST_S2F: {
                x64_convert_type(ic, 0x2A, (s64) buf + rip);
            } break;
            
            case IC_CAST_F2F: {
                x64_convert_type(ic, 0x5A, (s64) buf + rip);
            } break;
            
            case IC_MEMCPY: {
                x64_string_op(ic, 
                              ic->dest.type, ic->dest.reg, ic->dest.disp, 
                              ic->src0.type, ic->src0.reg, ic->src0.disp,
                              ic->src1.disp, 0xA4F3, (s64) buf + rip, X64_RSI);
            } break;
            
            case IC_MEMSET: {
                x64_string_op(ic, 
                              ic->dest.type, ic->dest.reg, ic->dest.disp,
                              ic->src0.type, ic->src0.reg, ic->src0.disp,
                              ic->src1.disp, 0xAAF3, (s64) buf + rip);
            } break;
            
            case IC_LEA: {
                assert(ic->src0.type & IC_REG);
                if (ic->src1.type & IC_RIP_DISP32) {
                    x64_lea(ic, ic->src0.reg, X64_RIP, ic->src1.disp, (s64) buf + rip);
                } else {
                    x64_lea(ic, ic->src0.reg, ic->src1.reg, ic->src1.disp, (s64) buf + rip);
                }
            } break;
            
            case IC_LPROC: {
                Compilation_Unit* target = (Compilation_Unit*) ic->data;
                if (target && target->bb_first) {
                    // REX.W + 8D /r 	LEA r64,m 	RM
                    x64_rex(ic, REX_FLAG_W);
                    ic_u8(ic, 0x8D);
                    //ic_u8(ic, 0x8B); //mov
                    x64_rip_relative(ic, X64_RAX, (s64) buf + target->bb_first->addr, (s64) buf + rip);
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
                    x64_mov(ic, tmpt, tmpr, tmpd, ic->src0.type, ic->src0.reg, ic->src0.disp, (s64) buf + rip);
                    t1 = tmpt;
                    r1 = tmpr;
                    d1 = tmpd;
                }
                x64_binary(ic, t1, r1, d1,
                           ic->src1.type, ic->src1.reg, ic->src1.disp, 7, 0x38, (s64) buf + rip);
            } break;
            
            case IC_TEST: {
                assert(ic->src0.type & (IC_REG | IC_STK));
                assert(ic->src1.type & IC_REG);
                
                if (ic->src0.type & IC_T8) {
                    // 84 /r 	TEST r/m8, r8 	MR
                    ic_u8(ic, 0x84);
                    x64_modrm(ic, ic->src0.type, ic->src0.disp, ic->src1.reg, ic->src0.reg, (s64) buf + rip);
                } else {
                    // 85 /r 	TEST r/m32, r32 	MR
                    ic_u8(ic, 0x85);
                    x64_modrm(ic, ic->src0.type, ic->src0.disp, ic->src1.reg, ic->src0.reg, (s64) buf + rip);
                }
            } break;
            
            case IC_FADD: {
                x64_float_binary(ic, 0x58, (s64) buf + rip);
            } break;
            
            case IC_FSUB: {
                x64_float_binary(ic, 0x5C, (s64) buf + rip);
            } break;
            
            case IC_FMUL: {
                x64_float_binary(ic, 0x59, (s64) buf + rip);
            } break;
            
            case IC_FDIV: {
                x64_float_binary(ic, 0x5E, (s64) buf + rip);
            } break;
            
            case IC_FXOR: {
                x64_float_binary(ic, 0x57, (s64) buf + rip, ic->dest.type & IC_T64 ? 0x66 : -2);
            } break;
            
            case IC_FCMP: {
                Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
                s64 r1 = ic->src0.reg, r2 = ic->src1.reg;
                s64 d2 = ic->src1.disp;
                
                if (t1 & IC_STK) {
                    Ic_Type tmpt = IC_REG + (ic->src0.type & IC_RT_MASK);
                    x64_fmov(ic, tmpt, X64_XMM1, 0,
                             ic->src0.type, ic->src0.reg, ic->src0.disp, 
                             (s64) buf + rip);
                    t1 = tmpt;
                    r1 = X64_XMM1;
                }
                
                if (t2 & (IC_STK | IC_DISP)) {
                    Ic_Type tmpt = IC_REG + (ic->src1.type & IC_RT_MASK);
                    x64_fmov(ic, tmpt, X64_XMM2, 0,
                             ic->src1.type, ic->src1.reg, ic->src1.disp, 
                             (s64) buf + rip);
                    t2 = tmpt;
                    r2 = X64_XMM2;
                }
                
                
                // NP 0F 2E /r UCOMISS xmm1, xmm2/m32
                if (ic->src0.type & IC_T64) {
                    ic_u8(ic, 0x66);
                }
                ic_u16(ic, 0x2F0F);
                x64_modrm(ic, t2, d2, r1, r2);
            } break;
            
            case IC_SETA: {
                ic_u16(ic, 0x970F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETAE: {
                ic_u16(ic, 0x930F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETB: {
                ic_u16(ic, 0x920F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETBE: {
                ic_u16(ic, 0x960F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETG: {
                ic_u16(ic, 0x9F0F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETGE: {
                ic_u16(ic, 0x9D0F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETL: {
                ic_u16(ic, 0x9C0F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETLE: {
                ic_u16(ic, 0x9E0F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETE: {
                ic_u16(ic, 0x940F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_SETNE: {
                ic_u16(ic, 0x950F);
                ic_u8(ic, 0xC0); // RAX
            } break;
            
            case IC_JNA: {
                ic_u16(ic, 0x860F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            case IC_JNAE: {
                ic_u16(ic, 0x820F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            case IC_JNB: {
                ic_u16(ic, 0x830F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            case IC_JNBE: {
                ic_u16(ic, 0x870F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            //case IC_JZ:
            case IC_JE: {
                ic_u16(ic, 0x840F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            //case IC_JNZ:
            case IC_JNE: {
                ic_u16(ic, 0x850F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            //case IC_JLE:
            case IC_JNG: {
                ic_u16(ic, 0x8E0F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            //case IC_JL:
            case IC_JNGE: {
                ic_u16(ic, 0x8C0F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            //case IC_JGE:
            case IC_JNL: {
                // TODO(Alexander): short jumps
                ic_u16(ic, 0x8D0F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            case IC_JNLE: {
                // TODO(Alexander): short jumps
                ic_u16(ic, 0x8F0F);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            case IC_JMP: {
                // TODO(Alexander): short jumps (and indirect jump?)
                ic_u8(ic, 0xE9);
                x64_jump(ic, (Ic_Basic_Block*) ic->data, rip);
            } break;
            
            case IC_CALL: {
                Compilation_Unit* target = (Compilation_Unit*) ic->data;
                if (target && target->bb_first) {
                    // E8 cd 	CALL rel32 	D
                    ic_u8(ic, 0xE8);
                    x64_jump(ic, target->bb_first, rip);
                } else {
                    x64_mov_rax_u64(ic, (u64) ic->dest.disp);
                    
                    // FF /2 	CALL r/m64 	M 	
                    ic_u8(ic, 0xFF);
                    x64_modrm(ic, IC_REG + IC_S64, 0, 2, X64_RAX);
                }
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
            if (arg.disp != 0) {
                string_builder_push_format(sb, "[% % %]",
                                           f_cstring(int_register_names[arg.reg]),
                                           f_char(arg.disp > 0 ? '+' : '-'), 
                                           f_s64(arg.disp > 0 ? arg.disp : -arg.disp));
            } else {
                string_builder_push(sb, "[RBP]");
            }
        } break;
        
        case IC_RIP_DISP32: {
            string_builder_push_format(sb, "[0x%]", f_u64_HEX(arg.disp));
        } break;
    }
}
