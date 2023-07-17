
Bytecode_Operand
convert_expression_to_bytecode(Bytecode_Builder* bc, Ast* expr) {
    Bytecode_Operand result = {};
    
    switch (expr->kind) {
        case Ast_None: {
        } break;
        
        case Ast_Ident: {
            assert(expr->type);
            Type* type = expr->type;
            
            string_id ident = ast_unwrap_ident(expr);
            if (map_key_exists(bc->locals, ident)) {
                result = map_get(bc->locals, ident);
                
            } else {
                if (type->kind == TypeKind_Function && type->Function.unit) {
                    Bytecode_Binary* insn = add_insn_t(bc, BC_LOAD_FUNCTION_PTR, Binary);
                    // TODO(Alexander): need to give each compilation unit a unique ID at start!!!
                    // TODO(Alexander): function pointer type?
                    //insn->first = push_bytecode_register(bc);
                    //insn->second = {}; // type->Function.unit;
                    unimplemented;
                    
                } else {
                    // Global variable
                    if (map_key_exists(bc->globals, ident)) {
                        result = map_get(bc->globals, ident);
                        
                    } else {
                        Ic_Raw_Type raw_type = convert_type_to_raw_type(type);
                        void* data = get_interp_value_pointer(bc->interp, ident);
                        if (!data) {
                            Type_Context tcx = {};
                            type_error(&tcx, string_print("compiler bug: value of `%` is void", f_var(ident)), expr->span);
                            assert(0);
                        }
                        
                        result = push_bytecode_memory(bc, BytecodeMemory_read_write, 
                                                      type->size, type->align, data);
                        map_put(bc->globals, ident, result);
                    }
                }
            }
        } break;
        
        case Ast_Exported_Data: {
            result.kind = BytecodeOperand_memory;
            result.memory_offset = expr->Exported_Data.relative_ptr; 
            result.memory_kind = BytecodeMemory_read_only;
        } break;
        
        case Ast_Value: {
            Bytecode_Type result_type = to_bytecode_type(expr->type);
            
            if (is_integer(expr->Value) || is_floating(expr->Value)) {
                result.kind = get_const_operand_from_type(result_type);
                value_store_in_memory(expr->type, &result.const_i64, expr->Value.data);
                
            } else if (is_string(expr->Value)) {
                smm string_count = expr->Value.data.str.count;
                Bytecode_Operand str_data = push_bytecode_memory(bc, BytecodeMemory_read_only, 
                                                                 string_count, 1, 
                                                                 expr->Value.data.str.data);
                result = push_bytecode_stack_t(bc, string);
                
                // Assign the 
                add_mov_insn(bc, BytecodeType_i64, result, str_data);
                
                Bytecode_Binary* store_count = add_insn_t(bc, BC_MOV, Binary);
                store_count->first = result;
                store_count->first.stack_offset += 8;
                store_count->second = result;
                store_count->second.kind = BytecodeOperand_const_i64;
                store_count->second.const_i64 = (s64) string_count;
                
            } else if (is_cstring(expr->Value)) {
                if (expr->Value.data.cstr) {
                    smm string_count = cstring_count(expr->Value.data.cstr);
                    result = push_bytecode_memory(bc, BytecodeMemory_read_only, 
                                                  string_count, 1, 
                                                  (void*) expr->Value.data.cstr);
                    
                } else {
                    result.kind = BytecodeOperand_const_i64;
                    result.const_i64 = 0;
                }
                
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Binary_Expr: {
            // TODO: add correct opcode
            Type* type = expr->type;
            Bytecode_Type result_type = to_bytecode_type(type);
            Bytecode_Operand first = convert_expression_to_bytecode(bc, expr->Binary_Expr.first);
            
            Bytecode_Operand tmp_register = {};
            if (first.kind != BytecodeOperand_register) {
                Bytecode_Operand tmp = add_bytecode_register(bc);
                add_mov_insn(bc, result_type, tmp, first);
                first = tmp;
                tmp_register = tmp;
            }
            
            Operator op = expr->Binary_Expr.op;
            bool is_assign = operator_is_assign(op);
            if (is_assign) {
                switch (op) {
                    case Op_Assign:             op = Op_None; break;
                    case Op_Add_Assign:         op = Op_Add; break;
                    case Op_Subtract_Assign:    op = Op_Subtract; break;
                    case Op_Multiply_Assign:    op = Op_Multiply; break;
                    case Op_Divide_Assign:      op = Op_Divide; break;
                    case Op_Modulo_Assign:      op = Op_Modulo; break;
                    case Op_Bitwise_And_Assign: op = Op_Bitwise_And; break;
                    case Op_Bitwise_Or_Assign:  op = Op_Bitwise_Or; break;
                    case Op_Bitwise_Xor_Assign: op = Op_Bitwise_Xor; break;
                    case Op_Shift_Left_Assign:  op = Op_Shift_Left; break;
                    case Op_Shift_Right_Assign: op = Op_Shift_Right; break;
                    
                    default: assert(0 && "invalid assign op");
                }
            }
            
            bool is_signed = !(type->kind == TypeKind_Basic && 
                               type->Basic.flags & BasicFlag_Unsigned);
            Bytecode_Operator opcode = BC_NOOP;
            switch (op) {
                case Op_Add:            opcode = BC_ADD; break;
                case Op_Subtract:       opcode = BC_SUB; break;
                case Op_Multiply:       opcode = BC_MUL; break;
                case Op_Divide:         opcode = is_signed ? BC_IDIV : BC_DIV; break;
                case Op_Modulo:         opcode = is_signed ? BC_IMOD : BC_MOD; break;
                case Op_Bitwise_And:    opcode = BC_AND; break;
                case Op_Bitwise_Or:     opcode = BC_OR; break;
                case Op_Bitwise_Xor:    opcode = BC_XOR; break;
                case Op_Shift_Left:     opcode = BC_SHL; break;
                case Op_Shift_Right:    opcode = BC_SHR; break;
                case Op_Equals:         opcode = BC_SET_EQ; break;
                case Op_Not_Equals:     opcode = BC_SET_NEQ; break;
                case Op_Greater_Than:   opcode = BC_SET_GT; break;
                case Op_Greater_Equals: opcode = BC_SET_GE; break;
                case Op_Less_Than:      opcode = BC_SET_LT; break;
                case Op_Less_Equals:    opcode = BC_SET_LE; break;
                case Op_None: break;
                
                default: unimplemented;
            }
            
            Bytecode_Operand second = convert_expression_to_bytecode(bc, expr->Binary_Expr.second);
            
            Bytecode_Binary* insn = add_insn_t(bc, opcode, Binary);
            insn->type = result_type;
            insn->first = first;
            insn->second = second;
            result = insn->first;
            
            if (tmp_register.kind) {
                drop_bytecode_register(bc, tmp_register.register_index);
            }
        } break;
        
        case Ast_Call_Expr: {
            Type* type = expr->Call_Expr.function_type;
            assert(type && type->kind == TypeKind_Function);
            
            Compilation_Unit* target_cu = type->Function.unit;
            assert(target_cu && target_cu->bc_func);
            
            Bytecode_Function* func = target_cu->bc_func;
            
            umm insn_size = sizeof(Bytecode_Call) + sizeof(Bytecode_Operand)*(func->ret_count +
                                                                              func->arg_count);
            umm insn_align = max(alignof(Bytecode_Call), alignof(Bytecode_Operand));
            Bytecode_Call* insn = (Bytecode_Call*) add_bytecode_insn(bc, BC_CALL, 
                                                                     BytecodeInstructionKind_Call, 
                                                                     insn_size, insn_align,
                                                                     __FILE__ ":" S2(__LINE__));
            insn->func_index = func->type_index;
            
            Bytecode_Operand* curr_operand = (Bytecode_Operand*) (insn + 1);
            for_compound(expr->Call_Expr.args, arg) {
                *curr_operand++ = convert_expression_to_bytecode(bc, arg->Argument.assign);
            }
            // TODO(Alexander): add default args
            
            // TODO(Alexander): multiple args
            if (is_valid_type(type->Function.return_type)) {
                insn->type = to_bytecode_type(type->Function.return_type);
                result = add_bytecode_register(bc);
                *curr_operand++ = result;
            }
            
        } break;
        
        case Ast_Cast_Expr: {
            Type* t_dest = expr->type;
            Type* t_src = expr->Cast_Expr.expr->type;
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
            
            Bytecode_Type dest_type = to_bytecode_type(expr->type);
            Bytecode_Operand src = convert_expression_to_bytecode(bc, expr->Cast_Expr.expr);
            result = src;
            
            if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
                if (t_src->Basic.flags & BasicFlag_Integer) {
                    if (t_dest->Basic.flags & BasicFlag_Boolean) {
#if 0 
                        result = ic_reg(IC_U8);
                        
                        Ic_Basic_Block* bb_else = ic_basic_block();
                        Ic_Basic_Block* bb_exit = ic_basic_block();
                        
                        convert_ic_to_conditional_jump(cu, cu->ic_last, src, bb_else);
                        ic_mov(cu, result, ic_imm(IC_U8, 1));
                        ic_jump(cu, IC_JMP, bb_exit);
                        ic_label(cu, bb_else);
                        ic_mov(cu, result, ic_imm(IC_U8, 0));
                        ic_label(cu, bb_exit);
#endif
                        unimplemented;
                        
                    } else if (t_dest->Basic.flags & BasicFlag_Integer) {
                        if (t_dest->size < t_src->size) {
                            Bytecode_Binary* insn = add_insn_t(bc, BC_NOOP, Binary);
                            insn->type = dest_type;
                            //insn->first = push_bytecode_register(bc);
                            insn->second = src;
                            result = insn->first;
                            
                            switch (t_src->Basic.kind) {
                                case Basic_s64: 
                                case Basic_u64: insn->opcode = BC_WRAP_I64; break;
                                default: unimplemented;
                            }
                            
                        } else if (t_dest->size > t_src->size) {
                            Bytecode_Binary* insn = add_insn_t(bc, BC_NOOP, Binary);
                            insn->type = dest_type;
                            //insn->first = push_bytecode_register(bc);
                            insn->second = src;
                            result = insn->first;
                            
                            switch (t_src->Basic.kind) {
                                case Basic_s8: insn->opcode = BC_EXTEND_S8; break;
                                case Basic_s16: insn->opcode = BC_EXTEND_S16; break;
                                case Basic_s32: insn->opcode = BC_EXTEND_S32; break;
                                case Basic_u8: insn->opcode = BC_EXTEND_U8; break;
                                case Basic_u16: insn->opcode = BC_EXTEND_U16; break;
                                case Basic_u32: insn->opcode = BC_EXTEND_U32; break;
                                default: unimplemented;
                            }
                        }
                    } break;
                    
                } else {
                    unimplemented;
                }
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Paren_Expr: {
            return convert_expression_to_bytecode(bc, expr->Paren_Expr.expr);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return result;
}


void
convert_statement_to_bytecode(Bytecode_Builder* bc, Ast* stmt, s32 break_label, s32 continue_label) {
    switch (stmt->kind) {
        case Ast_Decl_Stmt: {
            Ast* decl = stmt->Decl_Stmt.stmt;
            if (is_ast_stmt(decl)) {
                convert_statement_to_bytecode(bc, decl, break_label, continue_label);
            }
        } break;
        
        case Ast_Expr_Stmt: {
            convert_expression_to_bytecode(bc, stmt->Expr_Stmt);
        } break;
        
        case Ast_Assign_Stmt: {
            Type* type = stmt->type;
            
            Bytecode_Operand src = {};
            if (is_valid_ast(stmt->Assign_Stmt.expr)) {
                src = convert_expression_to_bytecode(bc, stmt->Assign_Stmt.expr);
            }
            
            if (stmt->Assign_Stmt.ident->kind == Ast_Ident) {
                string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
                
                Bytecode_Operand dest;
                if (stmt->Assign_Stmt.mods & AstDeclModifier_Local_Persist) {
                    dest = push_bytecode_memory(bc, BytecodeMemory_read_write, 
                                                type->size, type->align);
                } else {
                    dest = push_bytecode_stack(bc, type->size, type->align);
                }
                map_put(bc->locals, ident, dest);
                
                if (src.kind) {
                    add_mov_insn(bc, to_bytecode_type(type), dest, src);
                }
            }
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                convert_statement_to_bytecode(bc, it, break_label, continue_label);
            }
        } break;
        
        case Ast_Return_Stmt: {
            if (is_valid_ast(stmt->Return_Stmt.expr)) {
                Bytecode_Operand result = convert_expression_to_bytecode(bc, stmt->Return_Stmt.expr);
                Bytecode_Unary* insn = add_insn_t(bc, BC_RETURN, Unary);
                insn->first = result;
                if (result.kind == BytecodeOperand_register) {
                    drop_bytecode_register(bc, result.register_index);
                }
            }
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
}


Bytecode_Function*
convert_function_to_bytecode(Bytecode_Builder* bc, Compilation_Unit* cu,
                             bool is_main, bool insert_debug_break) {
    assert(cu->ast->type->kind == TypeKind_Function);
    assert(cu->ast->kind == Ast_Decl_Stmt);
    
    // TODO: handle extern functions here
    if (!cu->ast->Decl_Stmt.stmt) {
        return 0;
    }
    
    Type* type = cu->ast->type;
    
    
    Bytecode_Function* func = begin_bytecode_function(bc, type);
    if (is_main) {
        bc->bytecode.entry_func_index = func->type_index;
    }
    
    if (insert_debug_break) {
        add_insn(bc, BC_DEBUG_BREAK);
    }
    
    convert_statement_to_bytecode(bc, cu->ast->Decl_Stmt.stmt, 0, 0);
    
    end_bytecode_function(bc);
    
    return func;
}


Bytecode_Function*
begin_bytecode_function(Bytecode_Builder* bc, Type* type) {
    assert(type && type->kind == TypeKind_Function && "not a function type");
    
    int ret_count = is_valid_type(type->Function.return_type) ? 1 : 0;
    int arg_count = (int) array_count(type->Function.arg_types);
    int size = sizeof(Bytecode_Function) + ret_count + arg_count;
    
    Bytecode_Function* func = (Bytecode_Function*) arena_push_size(&bc->arena, size, 
                                                                   alignof(Bytecode_Function));
    func->type_index = bc->next_type_index++;
    func->ret_count = ret_count;
    func->arg_count = arg_count;
    bc->curr_function = func;
    bc->curr_insn = 0;
    
    Bytecode_Type* curr_type = (Bytecode_Type*) (func + 1);
    for (int i = 0; i < arg_count; i++) {
        string_id arg_ident = type->Function.arg_idents[i];
        Type* arg_type = type->Function.arg_types[i];
        *curr_type++ = to_bytecode_type(arg_type);
        
        Bytecode_Operand arg = push_bytecode_stack(bc, arg_type->size, arg_type->align);
        map_put(bc->locals, arg_ident, arg);
    }
    
    if (ret_count > 0) {
        assert(ret_count == 1 && "TODO: multiple arguments");
        *curr_type++ = to_bytecode_type(type->Function.return_type);
    }
    
    array_push(bc->bytecode.functions, func);
    array_push(bc->bytecode.function_names, type->Function.ident);
    
    return func;
}

void
end_bytecode_function(Bytecode_Builder* bc) {
    bc->curr_function = 0;
}

Bytecode_Instruction*
add_bytecode_insn(Bytecode_Builder* bc, 
                  Bytecode_Operator opcode, 
                  Bytecode_Instruction_Kind kind, 
                  umm size, umm align, cstring loc) {
    
    assert(bc->curr_function && "cannot add instruction outside function scope");
    
    // TODO(Alexander): when we run out of memory we need to make sure we have pointer to next instruction
    Bytecode_Instruction* insn = (Bytecode_Instruction*) arena_push_size(&bc->arena, size, align);
    insn->opcode = opcode;
    insn->kind = kind;
    insn->comment = loc;
    
    if (!bc->curr_function->first_insn) {
        bc->curr_function->first_insn = (u32) ((u8*) insn - (u8*) bc->curr_function);
    }
    
    if (bc->curr_insn) {
        bc->curr_insn->next_insn = (u32) ((u8*) insn - (u8*) bc->curr_insn);
    }
    bc->curr_insn = insn;
    bc->curr_function->insn_count++;
    
    return insn;
}

void
string_builder_dump_bytecode_operand(String_Builder* sb, Bytecode_Operand op) {
    switch (op.kind) {
        case BytecodeOperand_const_i32: {
            string_builder_push_format(sb, "%", f_int(op.const_i32));
        } break;
        
        case BytecodeOperand_const_i64: {
            string_builder_push_format(sb, "%", f_s64(op.const_i64));
        } break;
        
        case BytecodeOperand_const_f32: {
            string_builder_push_format(sb, "%", f_float(op.const_f32));
        } break;
        
        case BytecodeOperand_const_f64: {
            string_builder_push_format(sb, "%", f_float(op.const_f64));
        } break;
        
        case BytecodeOperand_register: {
            string_builder_push_format(sb, "r%", f_u32(op.register_index));
        } break;
        
        case BytecodeOperand_stack: {
            if (op.stack_offset == 0) {
                string_builder_push_format(sb, "stack[s%]", f_u32(op.stack_index));
            } else {
                string_builder_push_format(sb, "stack[s% + %]", f_u32(op.stack_index), f_int(op.stack_offset));
            }
        } break;
    }
    
}

inline void
string_builder_dump_bytecode_function_name(String_Builder* sb, Bytecode* bc, Bytecode_Function* func) {
    string_id ident = 0;
    if (bc->function_names) {
        ident = bc->function_names[func->type_index];
    }
    if (ident) {
        string_builder_push_format(sb, "%", f_var(ident));
    } else {
        string_builder_push_format(sb, "func%", f_int(func->type_index));
    }
}

inline void
string_builder_dump_bytecode_opcode(String_Builder* sb, Bytecode_Instruction* insn) {
    if (insn->type) {
        string_builder_push_format(sb, "%.", f_cstring(bc_type_names[insn->type]));
    }
    string_builder_push(sb, bc_opcode_names[insn->opcode]);
    string_builder_push_format(sb, " ");
}

void
string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn) {
    switch (insn->kind) {
        case BytecodeInstructionKind_None: break;
        
        case BytecodeInstructionKind_Base: {
            string_builder_dump_bytecode_opcode(sb, insn);
        } break;
        
        case BytecodeInstructionKind_Unary: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Unary*) insn)->first);
        } break;
        
        case BytecodeInstructionKind_Binary: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Binary*) insn)->first);
            string_builder_push(sb, ",");
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Binary*) insn)->second);
        } break;
        
        case BytecodeInstructionKind_Call: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            
            Bytecode_Function* func = bc->functions[call->func_index];
            Bytecode_Operand* args = (Bytecode_Operand*) (call + 1);
            
            
            if (func->ret_count == 1) {
                // TODO(Alexander): multiple returns
                string_builder_dump_bytecode_operand(sb, args[func->arg_count]);
                string_builder_push(sb, " = ");
            }
            
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_dump_bytecode_function_name(sb, bc, func);
            string_builder_push(sb, "(");
            
            for (u32 i = 0; i < call->func_index; i++) {
                string_builder_dump_bytecode_operand(sb, args[i]);
                if (i + 1 < call->func_index) {
                    string_builder_push(sb, " ");
                }
            }
            string_builder_push(sb, ")");
        } break;
    }
}

void
string_builder_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Function* func) {
    if (!func) return;
    
    Bytecode_Type* types = (Bytecode_Type*) (func + 1);
    if (func->ret_count == 1) {
        // TODO(Alexander): multiple returns
        string_builder_push(sb, bc_type_names[types[func->arg_count]]);
        string_builder_push(sb, " ");
    }
    string_builder_dump_bytecode_function_name(sb, bc, func);
    string_builder_push(sb, "(");
    for (u32 i = 0; i < func->arg_count; i++) {
        string_builder_push(sb, bc_type_names[types[i]]);
    }
    string_builder_push(sb, ") {");
    
    int bb_index = 0;
    
    Bytecode_Instruction* curr = iter_bytecode_instructions(func, 0);
    while (curr->kind) {
        string_builder_push(sb, "\n    ");
        string_builder_dump_bytecode_insn(sb, bc, curr);
        curr = iter_bytecode_instructions(func, curr);
    }
    
    string_builder_push(sb, "\n}");
}

void
dump_bytecode(Bytecode* bc) {
    String_Builder sb = {};
    for_array_v(bc->functions, func, _) {
        string_builder_dump_bytecode(&sb, bc, func);
    }
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}