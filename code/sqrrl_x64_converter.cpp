
void
convert_assign_to_intermediate_code(Compilation_Unit* cu, Type* type, Ic_Arg dest, Ic_Arg src) {
    if (type->kind == TypeKind_Array || type->kind == TypeKind_Struct || (type->kind == TypeKind_Basic && type->Basic.kind == Basic_string)) {
        Intermediate_Code* ic = ic_add(cu, IC_MEMSET);
        ic->dest = dest;
        ic->src0 = ic_imm(IC_U8, 0);
        ic->src1 = ic_imm(0, type->size);
        
        if (src.type & (IC_IMM | IC_STK)) {
            ic->opcode = IC_MEMCPY;
            ic->src0 = src;
        }
    } else {
        if (src.type) {
            ic_mov(cu, dest, src);
        }
    }
}

Ic_Arg
convert_binary_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr, Ic_Arg prev_result={}) {
    assert(expr && expr->kind == Ast_Binary_Expr && "not a binary expression");
    
    Ic_Arg result;
    
    Binary_Op op = expr->Binary_Expr.op;
    
    if (op == BinaryOp_Logical_Or) {
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
        
    } else {
        Ic_Arg src0, src1;
        
        {
            Ic_Arg clobber = {};
            Ast* first = expr->Binary_Expr.first;
            if (first->kind == Ast_Binary_Expr) { 
                src0 = convert_binary_expr_to_intermediate_code(cu, first, prev_result);
                if (src0.type & IC_REG && src0.reg == 0) {
                    prev_result = src0;
                }
            } else {
                
                // NOTE(Alexander): assumes RAX or XMM0 are ONLY used as tmp registers and that RBX && XMM6 are nonvolatile registers
                if (prev_result.type && (first->kind == Ast_Call_Expr || first->kind == Ast_Cast_Expr)) {
                    clobber = ic_push_local(cu, first->type); // TODO(Alexander): temp allocation
                    ic_mov(cu, clobber, prev_result);
                }
                
                src0 = convert_expr_to_intermediate_code(cu, first);
                
                if (clobber.type) {
                    src0 = ic_reg(clobber.raw_type);
                    ic_mov(cu, src0, clobber);
                }
                
                if (src0.type & IC_REG && src0.reg == 0) {
                    prev_result = src0;
                }
            }
        }
        
        {
            Ic_Arg clobber = {};
            Ast* second = expr->Binary_Expr.second;
            if (second->kind == Ast_Binary_Expr) { 
                src1 = convert_binary_expr_to_intermediate_code(cu, second, prev_result);
                if (src0.type & IC_REG && src1.type & IC_REG && src0.reg == src1.reg) {
                    src1 = ic_reg(src1.raw_type, (u8) (src1.type & IC_FLOAT ? X64_XMM1 : X64_RCX));
                    cu->ic_last->dest = src1;
                }
            } else {
                
                // NOTE(Alexander): assumes RAX or XMM0 are ONLY used as tmp registers and that RBX && XMM6 are nonvolatile registers
                if (prev_result.type && (second->kind == Ast_Call_Expr || second->kind == Ast_Cast_Expr)) {
                    clobber = ic_push_local(cu, second->type); // TODO(Alexander): temp allocation
                    ic_mov(cu, clobber, prev_result);
                }
                
                src1 = convert_expr_to_intermediate_code(cu, second);
                
                if (clobber.type) {
                    src0 = ic_reg(clobber.raw_type, (u8) (clobber.type & IC_FLOAT ? X64_XMM1 : X64_RCX));
                    ic_mov(cu, src0,  clobber);
                } 
            }
        }
        
        
        assert(src0.raw_type == src1.raw_type);
        bool isFloat = src0.type & IC_FLOAT;
        
        bool is_assign = is_binary_assign(op);
        if (is_assign) {
            switch (op) {
                case BinaryOp_Assign: op = BinaryOp_None; break;
                case BinaryOp_Add_Assign: op = BinaryOp_Add; break;
                case BinaryOp_Subtract_Assign: op = BinaryOp_Subtract; break;
                case BinaryOp_Multiply_Assign: op = BinaryOp_Multiply; break;
                case BinaryOp_Divide_Assign: op = BinaryOp_Divide; break;
                case BinaryOp_Modulo_Assign: op = BinaryOp_Modulo; break;
                case BinaryOp_Bitwise_And_Assign: op = BinaryOp_Bitwise_And; break;
                case BinaryOp_Bitwise_Or_Assign: op = BinaryOp_Bitwise_Or; break;
                case BinaryOp_Bitwise_Xor_Assign: op = BinaryOp_Bitwise_Xor; break;
                case BinaryOp_Shift_Left_Assign: op = BinaryOp_Shift_Left; break;
                case BinaryOp_Shift_Right_Assign: op = BinaryOp_Shift_Right; break;
                
                default: assert(0 && "invalid assign op");
            }
        }
        
        
        if (op == BinaryOp_None) {
            result = src1;
        } else {
            Intermediate_Code* ic = ic_add(cu);
            ic->dest = ic_reg(src0.raw_type);
            ic->src0 = src0;
            ic->src1 = src1;
            result = ic->dest;
            
            if (binary_is_comparator_table[op]) {
                result.raw_type = IC_S8;
                ic->opcode = isFloat ? IC_FCMP : IC_CMP;
                ic->dest = result;
                
                ic = ic_add(cu);
                ic->dest = result;
            }
            
            switch (op) {
                case BinaryOp_Add: ic->opcode = isFloat ? IC_FADD : IC_ADD; break;
                case BinaryOp_Subtract: ic->opcode = isFloat ? IC_FSUB : IC_SUB; break;
                case BinaryOp_Multiply: ic->opcode = isFloat ? IC_FMUL : IC_MUL; break;
                case BinaryOp_Divide: ic->opcode = isFloat ? IC_FDIV : IC_DIV; break;
                case BinaryOp_Modulo: ic->opcode = isFloat ? IC_FMOD : IC_MOD; break;
                case BinaryOp_Equals: ic->opcode = IC_SETE; break;
                case BinaryOp_Bitwise_And: ic->opcode = IC_AND; break;
                case BinaryOp_Bitwise_Or: ic->opcode = IC_OR; break;
                case BinaryOp_Bitwise_Xor: ic->opcode = IC_XOR; break;
                case BinaryOp_Not_Equals: ic->opcode = IC_SETNE; break;
                case BinaryOp_Greater_Than: ic->opcode = isFloat ? IC_SETA : IC_SETG; break;
                case BinaryOp_Greater_Equals: ic->opcode = isFloat ? IC_SETAE : IC_SETGE; break;
                case BinaryOp_Less_Than: ic->opcode = isFloat ? IC_SETB : IC_SETL; break;
                case BinaryOp_Less_Equals: ic->opcode = isFloat ? IC_SETBE : IC_SETLE; break;
                case BinaryOp_None: break;
                
                default: unimplemented;
            }
        }
        
        if (is_assign) {
            convert_assign_to_intermediate_code(cu, expr->type, src0, result);
        }
    }
    
    return result;
}


Ic_Arg
convert_expr_to_intermediate_code(Compilation_Unit* cu, Ast* expr) {
    Ic_Arg result = {};
    
    switch (expr->kind) {
        case Ast_Value: {
            if (is_integer(expr->Value.value)) {
                Ic_Raw_Type raw_type = convert_type_to_raw_type(expr->type);
                result.type = raw_type + IC_IMM;
                result.disp = value_to_s64(expr->Value.value);
                
            } else if (is_floating(expr->Value.value)) {
                Type* type = expr->type;
                Ic_Raw_Type raw_type = convert_type_to_raw_type(expr->type);
                void* data = malloc(type->size);
                value_store_in_memory(type, data, expr->Value.value.data);
                result = ic_data(raw_type, (s64) data);
                
            } else if (is_string(expr->Value.value)) {
                Ic_Raw_Type raw_type = IC_T64;
                string* str = (string*) malloc(sizeof(string));
                *str = expr->Value.value.data.str;
                result = ic_data(raw_type, (s64) str);
                
            } else if (is_cstring(expr->Value.value)) {
                cstring cstr = expr->Value.value.data.cstr;
                if (cstr == 0) {
                    result = ic_imm(IC_S64, 0);
                } else {
                    result = ic_data(IC_T64, (s64) cstr);
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
                if (type->kind == TypeKind_Function) {
                    Intermediate_Code* ic = ic_add(cu, IC_LPROC, type->Function.unit);
                    ic->dest = ic_reg(IC_T64);
                    result = ic->dest;
                } else {
                    unimplemented;
                }
            }
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
            void* data = calloc(1, size);
            
            u8* curr = (u8*) data;
            for_compound(expr->Array_Expr.elements, e) {
                assert(e->kind == Ast_Value);
                value_store_in_memory(e->type, curr, e->Value.value.data);
                curr += type->size;
            }
            
            result = ic_data(IC_T64, (s64) data);
        } break;
        
        case Ast_Struct_Expr: {
            Type* type = expr->type;
            assert(type->kind == TypeKind_Struct);
            
            u8* data = 0;
            // TODO(Alexander): temporary use Memory_Arena
            for_compound(expr->Struct_Expr.fields, field) {
                assert(field->kind == Ast_Argument);
                
                if (!data) {
                    data = (u8*) calloc(1, type->size);
                }
                
                string_id ident = ast_unwrap_ident(field->Argument.ident);
                Ast* assign = field->Argument.assign;
                // TODO(Alexander): make it possible to store dynamic things
                assert(assign->kind == Ast_Value);
                
                Struct_Field_Info info = get_field_info(&type->Struct, ident);
                value_store_in_memory(info.type, data + info.offset, assign->Value.value.data);
            }
            
            if (data) {
                result = ic_data(IC_T64, (s64) data);
            }
        } break;
        
        case Ast_Field_Expr: {
            Type* type = expr->Field_Expr.var->type;
            string_id ident = ast_unwrap_ident(expr->Field_Expr.field);
            
            result = convert_expr_to_intermediate_code(cu, expr->Field_Expr.var);
            assert(result.type & IC_STK);
            
            if (type->kind == TypeKind_Struct) {
                Struct_Field_Info info = get_field_info(&type->Struct, ident);
                result.disp += info.offset;
                result.raw_type = convert_type_to_raw_type(expr->type);
            } else if (type->kind == TypeKind_Array || type == t_string) {
                // TODO(Alexander): this has hardcoded sizes and types for now
                switch (ident) {
                    case Sym_data: {
                        result.raw_type = IC_T64;
                    } break;
                    
                    case Sym_count: {
                        result.raw_type = IC_S64;
                        result.disp += 8;
                    } break;
                    
                    case Sym_capacity: {
                        result.raw_type = IC_S64;
                        result.disp += 16;
                    } break;
                }
            } else {
                assert(0 && "invalid type for field expr");
            }
        } break;
        
        case Ast_Index_Expr: {
            Type* type = expr->type;
            assert(expr->Index_Expr.array->type->kind == TypeKind_Array);
            Type_Array* array_type = &expr->Index_Expr.array->type->Array;
            
            Ic_Raw_Type rt = convert_type_to_raw_type(type);
            Ic_Arg arr = convert_expr_to_intermediate_code(cu, expr->Index_Expr.array);
            Ic_Arg index = convert_expr_to_intermediate_code(cu, expr->Index_Expr.index);
            
            if (array_type->capacity == 0 || array_type->is_dynamic) {
                // Array is stored as "wide" pointer
                ic_mov(cu, ic_reg(arr.raw_type, X64_RDX), arr);
                if (index.type & IC_IMM) {
                    result = ic_stk(rt, type->size*index.disp, IcStkArea_None, X64_RDX);
                } else {
                    unimplemented;
                }
                
            } else {
                // Array is stored in place
                result = arr;
                result.raw_type = rt;
                if (result.type & IC_STK) {
                    if (index.type & IC_IMM) {
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
                        add_ic->src1 = ic_reg(IC_S64, X64_RSP);
                        
                        result.reg = add_ic->dest.reg;
                    }
                } else {
                    unimplemented;
                }
            }
        } break;
        
        case Ast_Unary_Expr: {
            Ic_Arg src = convert_expr_to_intermediate_code(cu, expr->Unary_Expr.first);
            Ic_Raw_Type rt = convert_type_to_raw_type(expr->type);
            
            switch (expr->Unary_Expr.op) {
                case UnaryOp_Negate: {
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
                        ic->src1 = ic_data(rt, (s64) data);
                        result = ic->dest;
                        
                    } else {
                        Intermediate_Code* ic = ic_add(cu, IC_NEG);
                        ic->dest = ic_reg(rt);
                        ic->src0 = src;
                        result = ic->dest;
                    }
                } break;
                
                case UnaryOp_Logical_Not: {
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
                
                case UnaryOp_Bitwise_Not: {
                    assert(src.type & IC_STK | IC_REG);
                    
                    Intermediate_Code* ic = ic_add(cu, IC_NOT);
                    ic->dest = ic_reg(rt);
                    ic->src0 = src;
                    result = ic->dest;
                } break;
                
                case UnaryOp_Address_Of: {
                    assert(src.type & IC_STK);
                    
                    Intermediate_Code* ic = ic_add(cu, IC_LEA);
                    ic->src0 = ic_reg(IC_T64);
                    ic->src1 = src;
                    result = ic->src0;
                } break;
                
                case UnaryOp_Dereference: {
                    if (src.type & IC_REG) {
                        result = ic_stk(rt, 0, IcStkArea_None, src.reg);
                    } else {
                        assert(src.type & IC_STK);
                        
                        Intermediate_Code* ic = ic_add(cu, IC_MOV);
                        ic->src0 = ic_reg(IC_T64);
                        ic->src1 = src;
                        result = ic_stk(rt, 0, IcStkArea_None, ic->src0.reg);
                    }
                } break;
                
                default: assert(0 && "invalid unary operator");
            }
            
        } break;
        
        case Ast_Binary_Expr: {
            result = convert_binary_expr_to_intermediate_code(cu, expr);
        } break;
        
        case Ast_Call_Expr: {
            assert(expr->Call_Expr.function_type && 
                   expr->Call_Expr.function_type->kind == TypeKind_Function);
            Type_Function* proc = &expr->Call_Expr.function_type->Function;
            
            if (proc->intrinsic) {
                if (proc->intrinsic == &interp_intrinsic_debug_break) {
                    ic_add(cu, IC_DEBUG_BREAK);
                    break;
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
                    Ic_Arg src = ic_push_local(cu, type);
                    
                    X64_Arg_Copy copy = {};
                    copy.type = proc->return_type;
                    copy.src = src;
                    copy.dest = src;
                    array_push(copy_args, copy);
                    
                    result = src;
                } else {
                    result = ic_reg(rt, X64_RAX); // NOTE: RAX == 0 && XMM0 == 0
                }
            }
            
            
            // Setup arguments
            for_compound(expr->Call_Expr.args, arg) {
                Ic_Raw_Type rt = convert_type_to_raw_type(arg->type);
                Ic_Arg src = convert_expr_to_intermediate_code(cu, arg->Argument.assign);
                s64 disp = stk_args;
                stk_args += 8;
                Ic_Arg dest = ic_stk(rt, disp, IcStkArea_Args);
                
                X64_Arg_Copy copy = {};
                copy.type = arg->type;
                copy.src = src;
                copy.dest = dest;
                array_push(copy_args, copy);
            }
            
            // Store arguments according to the windows calling convention
            for_array_reverse(copy_args, arg, arg_index) {
                Ic_Arg dest;
                if (arg_index < 4) {
                    if (arg->src.type & IC_FLOAT) {
                        dest = ic_reg(arg->src.raw_type, float_arg_registers_ccall_windows[arg_index]);
                    } else {
                        dest = ic_reg(arg->src.raw_type, int_arg_registers_ccall_windows[arg_index]);
                    }
                } else {
                    dest = arg->dest;
                }
                
                if (arg->src.type & IC_FLOAT) {
                    Intermediate_Code* ic = ic_add(cu, IC_FMOV);
                    ic->src0 = dest;
                    ic->src1 = arg->src;
                } else if (arg->src.type & (IC_SINT | IC_UINT | IC_IMM) ||
                           arg->type->kind == TypeKind_Pointer) {
                    ic_mov(cu, dest, arg->src);
                } else {
                    Ic_Arg tmp = {};
                    if (dest.type & IC_STK) {
                        tmp = dest;
                        dest = ic_reg(dest.type & IC_RT_MASK, X64_RAX);
                    }
                    Intermediate_Code* ic = ic_add(cu, IC_LEA);
                    ic->src0 = dest;
                    ic->src1 = arg->src;
                    
                    if (tmp.type & IC_STK) {
                        ic_mov(cu, tmp, dest);
                        dest = tmp;
                    }
                }
                
                if (proc->is_variadic && arg_index < 4 && arg->src.type & IC_FLOAT) {
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
        } break;
        
        case Ast_Cast_Expr: {
            assert(expr->Cast_Expr.expr);
            Type* t_dest = expr->type;
            Type* t_src = expr->Cast_Expr.expr->type;
            
            Ic_Arg src = convert_expr_to_intermediate_code(cu, expr->Cast_Expr.expr);
            result = src;
            
            if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
                Ic_Raw_Type dest_raw_type = basic_type_to_raw_type(t_dest->Basic.kind);
                
                if (t_src->Basic.flags & BasicFlag_Integer) {
                    if (t_dest->Basic.flags & BasicFlag_Integer) {
                        if (t_dest->size > t_src->size) {
                            Intermediate_Code* ic = ic_add(cu, 
                                                           t_dest->Basic.flags & BasicFlag_Unsigned ? 
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
                        unimplemented;
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
        Intermediate_Code* cmp = ic_add(cu, IC_CMP);
        cmp->src0 = cond;
        cmp->src1 = ic_imm(IC_S8, 0);
        code = ic_add(cu, IC_JE, false_target);
    }
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
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            
            Ic_Arg src = {};
            if (is_valid_ast(stmt->Assign_Stmt.expr)) {
                src = convert_expr_to_intermediate_code(cu, stmt->Assign_Stmt.expr);
            }
            
            // NOTE(Alexander): push local first then it can be found elsewhere
            ic_push_local(cu, type, ident);
            
            Ic_Arg dest = convert_expr_to_intermediate_code(cu, stmt->Assign_Stmt.ident);
            convert_assign_to_intermediate_code(cu, stmt->type, dest, src);
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
            Intermediate_Code* ic_jump = cu->ic_last;
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
            Ic_Arg cond = convert_expr_to_intermediate_code(cu, stmt->For_Stmt.cond);
            convert_ic_to_conditional_jump(cu, cu->ic_last, cond, bb_exit);
            
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
        
        if (arg_index < 4) {
            // None is used because this code is outside the stack frame
            Ic_Arg dest = ic_stk(rt, disp + 8, IcStkArea_None);
            array_push(home_register, dest);
        }
        Ic_Arg dest = ic_stk(rt, disp, IcStkArea_Caller_Args);
        
        int size = (*type)->size;
        if (size != 1 && size != 2 && size != 4 && size != 8) {
            Ic_Arg src = dest;
            dest = ic_push_local(cu, *type);
            src.type |= IC_SINT; // TODO(Alexander): strange way of disable ptr dereference
            
            X64_Arg_Copy copy = { *type, dest, src };
            array_push(copy_args, copy);
        }
        
        map_put(cu->locals, proc->arg_idents[type_index], dest);
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
        convert_assign_to_intermediate_code(cu, c->type, c->dest, c->src);
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
x64_modrm(Intermediate_Code* ic, Ic_Type t, s64 d, s64 r, s64 rm) {
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
    } else {
        ic_u8(ic, MODRM_DIRECT | (((u8) r&7)<<3) | (u8) rm&7);
    }
}

inline void
x64_unary(Intermediate_Code* ic, Ic_Type t, s64 r, s64 d, u8 reg_field) {
    if (ic->dest.type & IC_REG) {
        x64_mov(ic, ic->dest.type, ic->dest.reg, ic->dest.disp, t, r, d);
        t = ic->dest.type;
        r = ic->dest.reg;
        d = ic->dest.disp;
    }
    
    // F7 /3 	NEG r/m32 	M
    ic_u8(ic, 0xF7);
    x64_modrm(ic, t, d, reg_field, r);
}

inline void
x64_binary(Intermediate_Code* ic,
           Ic_Type t1, s64 r1, s64 d1, 
           Ic_Type t2, s64 r2, s64 d2,
           u8 reg_field, u8 opcode) {
    
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
                x64_modrm(ic, t1, d1, reg_field, r1);
                ic_u32(ic, (u32) d2);
            } else if (t2 & IC_REG) {
                // 01 /r 	ADD r/m32, r32 	MR
                ic_u8(ic, opcode + 1);
                x64_modrm(ic, t1, d1, r2, r1);
            } else if (t2 & IC_STK) {
                x64_mov(ic, IC_REG + (t2 & IC_RT_MASK), X64_RAX, 0, t2, r2, d2);
                x64_binary(ic, t1, r1, d1, IC_REG + (t1 & IC_RT_MASK), X64_RAX, 0, reg_field, opcode);
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
        u8 reg_field, u8 opcode) {
    
    if (t1 & IC_REG) {
        if (!(t2 & IC_REG)) {
            x64_mov(ic, t1, r1, d1, t2, r2, d2);
        }
    } else {
        unimplemented;
    }
    
    x64_binary(ic, t1, r1, d1, t3, r3, d3, reg_field, opcode);
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
        Ic_Type t2, s64 r2, s64 d2) {
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            if (t2 & IC_IMM) {
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
                
            } else if (t2 & (IC_REG | IC_STK)) {
                if (t1 & IC_T64 || r1&8 || r2&8) {
                    x64_rex(ic, (t1&IC_T64) | ((u8) r1&8)>>1 | ((u8)r2&8)>>3);
                }
                // 8B /r 	MOV r32,r/m32 	RM
                ic_u8(ic, 0x8B);
                x64_modrm(ic, t2, d2, r1, r2);
            } else {
                assert(0 && "invalid instruction");
            }
        } break;
        
        case IC_STK: {
            if (t2 & IC_IMM) {
                if ((u64) d2 > U32_MAX) {
                    x64_mov_rax_u64(ic, d2);
                    x64_mov(ic, t1, r1, d1, IC_T64 + IC_REG, X64_RAX, 0);
                } else {
                    // C7 /0 id 	MOV r/m32, imm32 	MI
                    if (t1 & IC_T64) {
                        x64_rex(ic, REX_FLAG_64_BIT);
                    }
                    
                    ic_u8(ic, 0xC7);
                    x64_modrm(ic, t1, d1, 0, r1);
                    ic_u32(ic, (u32) d2);
                }
            } else if (t2 & IC_REG) {
                // 89 /r 	MOV r/m32,r32 	MR
                if ((t2 & IC_T64) || (r2 & 8)) {
                    x64_rex(ic, t2&IC_T64|((u8) r2&8)>>1);
                }
                ic_u8(ic, 0x89);
                x64_modrm(ic, t1, d1, r2&7, r1);
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

void
x64_rip_relative(Intermediate_Code* ic, s64 r, s64 data, s64 rip) {
    ic_u8(ic, (u8) r<<3 | (u8) X64_RBP);
    
    s64 disp = (s64) data - (rip + ic->count + 4);
    ic_u32(ic, (u32) disp);
}

void
x64_fmov(Intermediate_Code* ic,
         Ic_Type t1, s64 r1, s64 d1,
         Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    assert(t1 & IC_FLOAT);
    
    
    if (t1 & IC_STK && (t2 & (IC_IMM | IC_STK))) {
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
            if (t2 & IC_IMM) {
                x64_rip_relative(ic, r1, d2, rip);
            } else {
                x64_modrm(ic, t2, d2, r1, r2);
            }
        } break;
        
        case IC_STK: {
            assert(t2 & IC_REG);
            
            // F3 0F 11 /r MOVSS xmm2/m32, xmm1 or
            // F2 0F 11 /r MOVSD xmm1/m64, xmm2
            ic_u8(ic, (t1 & IC_T32) ? 0xF3 : 0xF2);
            ic_u8(ic, 0x0F);
            ic_u8(ic, 0x11);
            x64_modrm(ic, t1, d1, r2, r1);
        } break;
        
        default: unimplemented;
    }
    
}

inline void
x64_mul(Intermediate_Code* ic) {
    assert(ic->dest.type & IC_REG);
    assert(ic->src0.type & (IC_REG | IC_STK));
    
    if (ic->src1.type & IC_IMM) {
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
        
        if (ic->src0.type & IC_STK || ic->src0.reg == ic->dest.reg) {
            x64_mov(ic, ic->dest.type, ic->dest.reg, ic->dest.disp, t1, r1, d1);
            t1 = ic->dest.type;
            r1 = ic->dest.reg;
            d1 = ic->dest.disp;
        }
        
        // 0F AF /r 	IMUL r32, r/m32
        ic_u16(ic, 0xAF0F);
        x64_modrm(ic, ic->src1.type, ic->src1.disp, r1, ic->src1.reg);
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

inline void
x64_float_binary(Intermediate_Code* ic, u8 opcode, s64 rip, s64 prefix_opcode=-1) {
    
    Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
    s64 r1 = ic->src0.reg, r2 = ic->src1.reg;
    s64 d1 = ic->src0.disp, d2 = ic->src1.disp;
    assert(t1 & IC_FLOAT);
    // NOTE(Alexander): assumes destination to be a register
    assert(ic->dest.type & IC_REG);
    
    // Make sure first argument is a register
    switch (t1 & IC_TF_MASK) {
        case IC_STK: {
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
        } break;
        
        case IC_IMM: {
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
        } break;
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
    
    if (t2 & IC_IMM) {
        x64_rip_relative(ic, r1, d2, rip);
    } else {
        x64_modrm(ic, t2, d2, r1, r2);
    }
    
    if (!(ic->dest.type == t1 && ic->dest.reg == r1)) {
        x64_fmov(ic, ic->dest.type, ic->dest.reg, 0, t1, r1, d1, rip);
    }
    
}

inline void
x64_convert_type(Intermediate_Code* ic, u8 opcode) {
    Ic_Type t1 = ic->src0.type, t2 = ic->src1.type;
    s64 r1 = ic->src0.reg, d1 = ic->src0.disp, r2 = ic->src1.reg, d2 = ic->src1.disp;
    assert(t1 & IC_REG);
    assert(t2 & (IC_REG | IC_STK));
    
    // F3 0F 2D /r CVTSS2SI r32, xmm1/m32
    ic_u8(ic, t2 & IC_T64 ? 0xF2 : 0xF3);
    if (t1 & IC_T64) {
        x64_rex(ic, REX_FLAG_64_BIT);
    }
    ic_u16(ic, 0x0F|(opcode<<8));
    x64_modrm(ic, t2, d2, r1, r2);
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
    x64_mov(ic, IC_S64 + IC_REG, X64_RCX, 0, IC_S64 + IC_IMM, 0, count);
    
    if (destt & (IC_UINT | IC_SINT)) {
        x64_mov(ic, IC_S64 + IC_REG, X64_RDI, 0, IC_S64 + IC_STK, X64_RSP, destd);
    } else {
        x64_lea(ic, X64_RDI, destr, destd, rip);
    }
    
    if (srct & (IC_UINT | IC_SINT)) {
        x64_mov(ic, IC_S64 + IC_REG, src_int_reg, 0, srct, srcr, srcd);
    } else if (srct & IC_T64) {
        //if (srct & IC_IMM) {
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
                x64_binary(ic, IC_REG + IC_S64, X64_RSP, 0, IC_IMM + IC_S32, 0, stk_usage, 5, 28);
            } break;
            
            case IC_EPLG: {
                x64_binary(ic, IC_REG + IC_S64, X64_RSP, 0, IC_IMM + IC_S32, 0, stk_usage, 0, 0);
                ic_u8(ic, 0xC3);
            } break;
            
            case IC_NEG: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 3);
            } break;
            
            case IC_NOT: {
                x64_unary(ic, ic->src0.type, ic->src0.reg, ic->src0.disp, 2);
            } break;
            
            case IC_ADD: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 0, 0);
            } break;
            
            case IC_SUB: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 5, 28);
            } break;
            
            case IC_AND: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 4, 20);
            } break;
            
            case IC_OR: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 1, 8);
            } break;
            
            case IC_XOR: {
                x64_add(ic, 
                        ic->dest.type, ic->dest.reg, ic->dest.disp,
                        ic->src0.type, ic->src0.reg, ic->src0.disp,
                        ic->src1.type, ic->src1.reg, ic->src1.disp, 6, 30);
            } break;
            
            case IC_MUL: {
                x64_mul(ic);
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
                x64_convert_type(ic, 0x2C);
            } break;
            
            case IC_CAST_S2F: {
                x64_convert_type(ic, 0x2A);
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
                x64_lea(ic, ic->src0.reg, ic->src1.reg, ic->src1.disp, (s64) buf + rip);
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
                if (t1 & IC_IMM) {
                    Ic_Type tmpt = IC_REG + (ic->src0.type & IC_RT_MASK);
                    s64 tmpr = (t2 & IC_REG) ? X64_RCX : X64_RAX, tmpd = 0;
                    x64_mov(ic, tmpt, tmpr, tmpd, ic->src0.type, ic->src0.reg, ic->src0.disp);
                    t1 = tmpt;
                    r1 = tmpr;
                    d1 = tmpd;
                }
                x64_binary(ic, t1, r1, d1,
                           ic->src1.type, ic->src1.reg, ic->src1.disp, 7, 0x38);
            } break;
            
            case IC_TEST: {
                assert(ic->src0.type & (IC_REG | IC_STK));
                assert(ic->src1.type & IC_REG);
                
                if (ic->src0.type & IC_T8) {
                    // 85 /r 	TEST r/m32, r32 	MR
                    ic_u8(ic, 0x84);
                    x64_modrm(ic, ic->src0.type, ic->src0.disp, ic->src1.reg, ic->src0.reg);
                } else {
                    // 85 /r 	TEST r/m32, r32 	MR
                    ic_u8(ic, 0x85);
                    x64_modrm(ic, ic->src0.type, ic->src0.disp, ic->src1.reg, ic->src0.reg);
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
                
                if (t2 & (IC_STK | IC_IMM)) {
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
        case IC_IMM: {
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
    }
}
