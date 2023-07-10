
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
                    insn->first = push_bytecode_register(bc, BytecodeType_i64);
                    insn->second = {}; // type->Function.unit;
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
            result.type = BytecodeType_i64;
            result.memory_offset = expr->Exported_Data.relative_ptr; 
            result.memory_kind = BytecodeMemory_read_only;
        } break;
        
        case Ast_Value: {
            result.type = to_bytecode_type(expr->type);
            
            if (is_integer(expr->Value) || is_floating(expr->Value)) {
                result.kind = get_const_operand_from_type(result.type);
                value_store_in_memory(expr->type, &result.const_i64, expr->Value.data);
                
            } else if (is_string(expr->Value)) {
                Ic_Raw_Type raw_type = IC_T64;
                
                smm string_count = expr->Value.data.str.count;
                Bytecode_Operand str_data = push_bytecode_memory(bc, BytecodeMemory_read_only, 
                                                                 string_count, 1, 
                                                                 expr->Value.data.str.data);
                result = push_bytecode_stack_t(bc, string);
                
                // Assign the 
                add_store_insn(bc, result, str_data);
                
                Bytecode_Binary* store_count = add_insn_t(bc, BC_STORE, Binary);
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
            Bytecode_Operator first = convert_expression_to_bytecode(bc, expr->Binary_Expr.first);
            
            
            Bytecode_Binary* insn = add_insn_t(bc, BC_ADD, Binary);
            insn->first = ;
            insn->second = convert_expression_to_bytecode(bc, expr->Binary_Expr.second);
            
            
            
            result = insn->first;
            
        } break;
        
        case Ast_Call_Expr: {
            
            Type* type = expr->Call_Expr.function_type;
            assert(type && type->kind == TypeKind_Function);
            
            Compilation_Unit* target_cu = type->Function.unit;
            assert(target_cu && target_cu->bc_func);
            
            Bytecode_Function* func = target_cu->bc_func;
            
            Bytecode_Call* insn = add_insn_t(bc, BC_CALL, Call);
            func.type_index
                
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
                    add_store_insn(bc, dest, src);
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
            }
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
}


Bytecode_Function*
convert_function_to_bytecode(Bytecode_Builder* bc, Compilation_Unit* cu, bool insert_debug_break) {
    assert(cu->ast->type->kind == TypeKind_Function);
    assert(cu->ast->kind == Ast_Decl_Stmt);
    
    // TODO: handle extern functions here
    if (!cu->ast->Decl_Stmt.stmt) {
        return 0;
    }
    
    Type* type = cu->ast->type;
    
    
    Bytecode_Function* func = begin_bytecode_function(bc, type);
    
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
    
    Bytecode_Type* curr_type = (Bytecode_Type*) (func + 1);
    if (ret_count > 0) {
        assert(ret_count == 1 && "TODO: multiple arguments");
        *curr_type++ = to_bytecode_type(type->Function.return_type);
    }
    
    for (int i = 0; i < arg_count; i++) {
        string_id arg_ident = type->Function.arg_idents[i];
        Type* arg_type = type->Function.arg_types[i];
        *curr_type++ = to_bytecode_type(arg_type);
        
        Bytecode_Operand arg = push_bytecode_stack(bc, arg_type->size, arg_type->align);
        map_put(bc->locals, arg_ident, arg);
    }
    
    array_push(bc->bytecode.functions, func);
    array_push(bc->function_names, type->Function.ident);
    
    bc->curr_function = func;
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
    
    return insn;
}

void
string_builder_dump_bytecode_operand(String_Builder* sb, Bytecode_Operand op) {
    switch (op.kind) {
        case BytecodeOperand_const_i32: {
            string_builder_push_format(sb, " %", f_int(op.const_i32));
        } break;
        
        case BytecodeOperand_const_i64: {
            string_builder_push_format(sb, " %", f_s64(op.const_i64));
        } break;
        
        case BytecodeOperand_const_f32: {
            string_builder_push_format(sb, " %", f_float(op.const_f32));
        } break;
        
        case BytecodeOperand_const_f64: {
            string_builder_push_format(sb, " %", f_float(op.const_f64));
        } break;
    }
    
}

void
string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode_Instruction* insn) {
    switch (insn->kind) {
        case BytecodeInstructionKind_None: break;
        
        case BytecodeInstructionKind_Base: {
            string_builder_push(sb, bc_opcode_names[insn->opcode]);
        } break;
        
        case BytecodeInstructionKind_Unary: {
            string_builder_push(sb, bc_opcode_names[insn->opcode]);
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Unary*) insn)->first);
        } break;
        
        case BytecodeInstructionKind_Binary: {
            string_builder_push(sb, bc_opcode_names[insn->opcode]);
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Binary*) insn)->first);
            string_builder_push(sb, ',');
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Binary*) insn)->second);
        } break;
    }
}

void
string_builder_dump_bytecode(String_Builder* sb, Bytecode_Function* func, Type* type) {
    if (!func) return;
    
    if (type) {
        string_builder_push_format(sb, "\n% {", f_type(type));
    } else {
        string_builder_push_format(sb, "func (t%) {", f_u32(func->type_index));
    }
    // TODO: add function params!
    
    int bb_index = 0;
    
    Bytecode_Instruction* curr = iter_bytecode_instructions(func, 0);
    while (curr->kind) {
        string_builder_push(sb, "\n    ");
        string_builder_dump_bytecode_insn(sb, curr);
        curr = iter_bytecode_instructions(func, curr);
    }
    
    string_builder_push(sb, "\n}");
}

void
dump_bytecode(Compilation_Unit* cu) {
    if (!cu->bc_func) {
        return;
    }
    
    String_Builder sb = {};
    string_builder_dump_bytecode(&sb, cu->bc_func, cu->ast->type);
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}