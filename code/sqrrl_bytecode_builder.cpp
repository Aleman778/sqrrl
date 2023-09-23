
int
convert_lvalue_expression_to_bytecode(Bytecode_Builder* bc, Ast* expr) {
    int result = -1;
    switch (expr->kind) {
        case Ast_Ident: {
            Type* type = expr->type;
            string_id ident = ast_unwrap_ident(expr);
            if (map_key_exists(bc->locals, ident)) {
                result = map_get(bc->locals, ident);
                
            } else {
                if (type->kind == TypeKind_Function && type->Function.unit) {
                    
                    //bc_instruction
                    unimplemented;
                    
                } else if (map_key_exists(bc->globals, ident)) { 
                    int global_index = map_get(bc->globals, ident);
                    result = bc_instruction_global(bc, global_index);
                } else {
                    unimplemented;
                }
            }
        } break;
        
        case Ast_Unary_Expr: {
            if (expr->Unary_Expr.op == Op_Dereference) {
                result = convert_lvalue_expression_to_bytecode(bc, expr->Unary_Expr.first);
                result = bc_instruction_load(bc, expr->Unary_Expr.first->type, result);
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Field_Expr: {
            Type* type = expr->Field_Expr.var->type;
            result = convert_lvalue_expression_to_bytecode(bc, expr->Field_Expr.var);
            string_id ident = ast_unwrap_ident(expr->Field_Expr.field);
            
            switch (type->kind) {
                case TypeKind_Struct:
                case TypeKind_Union: {
                    Struct_Field_Info field = get_field_info(&type->Struct_Like, ident);
                    
                    int tmp = add_bytecode_register(bc, t_void_ptr);
                    bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, (int) field.offset);
                    result = tmp;
                } break;
                
                case TypeKind_Array:
                case TypeKind_Basic: {
                    assert(type->kind == TypeKind_Array ||
                           type->Basic.kind == Basic_string && "unsupported type");
                    
                    if (ident == Sym_data) {
                        if (!(type->kind == TypeKind_Array && type->Array.kind == ArrayKind_Fixed_Inplace)) {
                            int tmp = add_bytecode_register(bc, type);
                            bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, 0);
                            result = tmp;
                        } else {
                            bc->curr_function->register_types[result].flags |= BC_FLAG_RVALUE;
                        }
                        
                    } else if (ident == Sym_count) {
                        int tmp = add_bytecode_register(bc, type);
                        bc_instruction(bc, BC_FIELD_ACCESS, tmp, result, 8);
                        result = tmp;
                    } else {
                        unimplemented;
                    }
                } break;
            }
        } break;
        
        case Ast_Index_Expr: {
            Type* array_type = expr->Index_Expr.array->type;
            
            int array_ptr = convert_lvalue_expression_to_bytecode(bc, expr->Index_Expr.array);
            if ((array_type->kind == TypeKind_Array &&
                 array_type->Array.kind != ArrayKind_Fixed_Inplace) ||
                array_type == t_cstring) {
                array_ptr = bc_instruction_load(bc, array_type, array_ptr);
            }
            
            int array_index = convert_expression_to_bytecode(bc, expr->Index_Expr.index);
            result = add_bytecode_register(bc, expr->Index_Expr.array->type);
            result = bc_instruction_array_accesss(bc, array_type->Array.type, array_ptr, array_index);
        } break;
        
        case Ast_Cast_Expr: {
            result = convert_type_cast_to_bytecode(bc, expr);
        } break;
        
        case Ast_Paren_Expr: {
            result = convert_lvalue_expression_to_bytecode(bc, expr->Paren_Expr.expr);
        } break;
        
        default: unimplemented;
    }
    
    return result;
}

int
convert_function_call_to_bytecode(Bytecode_Builder* bc, Type* type, array(Ast*)* args) {
    assert(type && type->kind == TypeKind_Function);
    
    int result = -1;
    
    if (type->Function.is_intrinsic) {
        bool handled = false;
        
        switch (type->Function.ident) {
            case Sym_rdtsc: {
                result = add_bytecode_register(bc, t_s64);
                bc_instruction(bc, BC_X64_RDTSC, result, -1, -1);
                handled = true;
            } break;
            
            case Sym_debug_break: {
                bc_instruction(bc, BC_DEBUG_BREAK, -1, -1, -1);
                handled = true;
            } break;
        }
        
        if (handled) {
            return result;
        }
    }
    
    Compilation_Unit* target_cu = type->Function.unit;
    assert(target_cu && target_cu->bytecode_function);
    Bytecode_Function* func = target_cu->bytecode_function;
    
    int arg_index = 0;
    array(int)* arg_operands = 0;
    if (func->return_as_first_arg) {
        Type* ret_type = type->Function.return_type;
        result = bc_instruction_local(bc, ret_type);
        array_push(arg_operands, result);
        arg_index++;
    }
    
    Bytecode_Function_Arg* formal_args = function_arg_types(func);
    for_array_v(args, arg, _) {
        Type* arg_type = arg->type;
        Bytecode_Function_Arg formal_arg = formal_args[arg_index];
        //pln("arg %, size %", f_int(arg_index), f_int(formal_arg.size));
        int reg;
        if (is_aggregate_type(arg_type)) {
            // Create copy (except for ARRAY types) and pass it via ptr
            reg = convert_lvalue_expression_to_bytecode(bc, arg);
            if (arg_type->kind != TypeKind_Array) {
                int copy = bc_instruction_local(bc, arg_type);
                bc_instruction(bc, BC_MEMCPY, copy, reg, arg_type->size);
                reg = copy;
            }
            
        } else {
            reg = convert_expression_to_bytecode(bc, arg);
        }
        array_push(arg_operands, reg);
        arg_index++;
    }
    
    umm arg_size = sizeof(int)*(func->ret_count + func->arg_count);
    umm insn_size = sizeof(Bytecode_Call) + arg_size;
    umm insn_align = max(alignof(Bytecode_Call), alignof(int));
    Bytecode_Call* insn = (Bytecode_Call*) add_bytecode_insn(bc, BC_CALL, 
                                                             BytecodeInstructionKind_Call, 
                                                             insn_size, insn_align,
                                                             __FILE__ ":" S2(__LINE__));
    insn->func_index = func->type_index;
    
    u32 arg_count = (u32) array_count(arg_operands);
    bc->curr_function->max_caller_arg_count = max(bc->curr_function->max_caller_arg_count,
                                                  arg_count);
    
    // TODO(Alexander): multiple args
    if (!func->return_as_first_arg && is_valid_type(type->Function.return_type)) {
        result = add_bytecode_register(bc, type->Function.return_type);
        array_push(arg_operands, result);
    }
    
    if (arg_operands) {
        memcpy((int*) (insn + 1), arg_operands, arg_size);
        array_free(arg_operands);
    }
    
    return result;
}

int
convert_expression_to_bytecode(Bytecode_Builder* bc, Ast* expr) {
    int result = -1;
    
    switch (expr->kind) {
        case Ast_None: {
        } break;
        
        case Ast_Ident: {
            Type* type = expr->type;
            
            string_id ident = ast_unwrap_ident(expr);
            if (map_key_exists(bc->locals, ident)) {
                result = map_get(bc->locals, ident);
                int flags = register_type(bc->curr_function, result).flags;
                if (!is_bitflag_set(flags, BC_FLAG_RVALUE)) {
                    result = bc_instruction_load(bc, type, result);
                }
                
            } else {
                if (type->kind == TypeKind_Function && type->Function.unit) {
                    //Bytecode_Binary* insn = add_insn_t(bc, BC_LOAD_FUNCTION_PTR, Binary);
                    // TODO(Alexander): need to give each compilation unit a unique ID at start!!!
                    // TODO(Alexander): function pointer type?
                    //insn->first = push_bytecode_register(bc);
                    //insn->second = {}; // type->Function.unit;
                    unimplemented;
                    
                } else {
                    // Global variable
                    if (map_key_exists(bc->globals, ident)) {
                        int global_index = map_get(bc->globals, ident);
                        result = bc_instruction_global(bc, global_index);
                        result = bc_instruction_load(bc, type, result);
                        
                    } else {
                        unimplemented;
                    }
                }
            }
        } break;
        
        case Ast_Exported_Data: {
            //result.kind = BytecodeOperand_memory;
            //result.memory_offset = expr->Exported_Data.relative_ptr; 
            //result.memory_kind = BytecodeMemory_read_only;
            unimplemented;
        } break;
        
        case Ast_Value: {
            Type* type = expr->type;
            if (is_integer(expr->Value)) {
                result = bc_instruction_const_int(bc, type, (s64) expr->Value.data.signed_int);
                
            } else if (is_floating(expr->Value)) {
                if (type->size == 8) {
                    result = bc_instruction_const_f64(bc, (f64) expr->Value.data.floating);
                } else {
                    result = bc_instruction_const_f32(bc, (f32) expr->Value.data.floating);
                }
            } else if (is_string(expr->Value)) {
                //smm string_count = expr->Value.data.str.count;
                //int str_data = bc_instruction_global(bc, BytecodeMemory_read_only, 
                //string_count, 1, 
                //expr->Value.data.str.data);
                //result = push_bytecode_stack_t(bc, string);
                
                //Bytecode_Binary* store_data = add_insn_t(bc, BC_ADDR_OF, Binary);
                //store_data->type = bc_instruction_pointer_type(bc);
                //store_data->first = add_bytecode_register(bc);
                //store_data->second = str_data;
                //store_instruction(bc, t_void_ptr, result, store_data->first);
                
                
                unimplemented;
                //Bytecode_Instruction insn = bc_instruction_insn_int(func, (s64) string_count);
                
                //Bytecode_Binary* store_count = add_insn_t(bc, BC_MOV, Binary);
                //store_count->type = BytecodeType_i64;
                //store_count->first = result;
                //store_count->first.memory_offset += 8;
                //store_count->second = result;
                //store_count->second.kind = BytecodeOperand_const;
                //store_count->second.const_i64 = (s64) string_count;
                
            } else if (is_cstring(expr->Value)) {
                result = add_bytecode_register(bc, t_cstring);
                if (expr->Value.data.cstr) {
                    smm string_count = cstring_count(expr->Value.data.cstr);
                    
                    int global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY,
                                                           string_count + 1, 1,
                                                           (void*) expr->Value.data.cstr);
                    result = bc_instruction_global(bc, global_index);
                } else {
                    result = bc_instruction_const_int(bc, t_void_ptr, 0);
                }
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Unary_Expr: {
            Type* type = expr->type;
            Operator op = expr->Unary_Expr.op;
            
            switch (op) {
                case Op_Address_Of: {
                    result = convert_lvalue_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    //result = bc_instruction_local(bc, t_void_ptr);
                    //bc_instruction_store(bc, result, address);
                } break;
                
                case Op_Dereference: {
                    result = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    result = bc_instruction_load(bc, type, result);
                } break;
                
                case Op_Post_Increment:
                case Op_Post_Decrement:
                case Op_Pre_Increment:
                case Op_Pre_Decrement: {
                    int first_ptr = convert_lvalue_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    int first = bc_instruction_load(bc, type, first_ptr);
                    
                    int first_modified;
                    if (type->kind == TypeKind_Pointer) {
                        int index = bc_instruction_const_int(bc, t_s64, 
                                                             (op == Op_Post_Increment ||
                                                              op == Op_Pre_Increment) ? 1 : -1);
                        first_modified = bc_instruction_array_accesss(bc, type, first, index);
                    } else {
                        first_modified = add_bytecode_register(bc, type);
                        bc_instruction(bc, (op == Op_Post_Increment ||
                                            op == Op_Pre_Increment) ? BC_INC : BC_DEC,
                                       first_modified, first, -1);
                    }
                    bc_instruction_store(bc, first_ptr, first_modified);
                    
                    result = (op == Op_Post_Increment || 
                              op == Op_Post_Decrement) ? first : first_modified;
                } break;
                
                case Op_Bitwise_Not: {
                    int first = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    result = add_bytecode_register(bc, type);
                    bc_instruction(bc, BC_NOT, result, first, -1);
                } break;
                
                case Op_Logical_Not: {
                    int first = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    int second = bc_instruction_const_int(bc, type, 0);
                    result = add_bytecode_register(bc, t_bool);
                    bc_instruction(bc, BC_EQ, result, first, second);
                } break;
                
                case Op_Negate: {
                    int first = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
                    result = add_bytecode_register(bc, type);
                    bc_instruction(bc, BC_NEG, result, first, -1);
                } break;
                
                default: unimplemented;
            }
        } break;
        
        case Ast_Binary_Expr: {
            // TODO(Alexander): operator overloading
            // TODO(Alexander): add proper AND/ OR logical operator
            Operator op = expr->Binary_Expr.op;
            bool is_assign = operator_is_assign(op);
            
            if (expr->Binary_Expr.overload) {
                array(Ast*)* args = 0;
                array_push(args, expr->Binary_Expr.first);
                array_push(args, expr->Binary_Expr.second);
                result = convert_function_call_to_bytecode(bc, expr->Binary_Expr.overload, args);
                array_free(args);
                return result;
            }
            
            Type* result_type = expr->type;
            Type* type = expr->Binary_Expr.first->type;
            
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
                               type->Basic.flags & (BasicFlag_Unsigned | BasicFlag_Floating));
            Bytecode_Operator opcode = BC_NOOP;
            switch (op) {
                case Op_None: break;
                case Op_Add:            opcode = BC_ADD; break;
                case Op_Subtract:       opcode = BC_SUB; break;
                case Op_Multiply:       opcode = BC_MUL; break;
                case Op_Divide:         opcode = is_signed ? BC_DIV_S : BC_DIV_U; break;
                case Op_Modulo:         opcode = is_signed ? BC_MOD_S : BC_MOD_U; break;
                case Op_Bitwise_And:    opcode = BC_AND; break;
                case Op_Bitwise_Or:     opcode = BC_OR; break;
                case Op_Bitwise_Xor:    opcode = BC_XOR; break;
                case Op_Shift_Left:     opcode = BC_SHL; break;
                case Op_Shift_Right:    opcode = BC_SHR; break;
                case Op_Equals:         opcode = BC_EQ; break;
                case Op_Not_Equals:     opcode = BC_NEQ; break;
                case Op_Greater_Than:   opcode = is_signed ? BC_GT_S : BC_GT_U; break;
                case Op_Greater_Equals: opcode = is_signed ? BC_GE_S : BC_GE_U; break;
                case Op_Less_Than:      opcode = is_signed ? BC_LT_S : BC_LT_U; break;
                case Op_Less_Equals:    opcode = is_signed ? BC_LE_S : BC_LE_U; break;
                default: unimplemented;
            }
            
            int first_ptr = -1;
            int first = -1;
            if (is_assign) {
                first_ptr = convert_lvalue_expression_to_bytecode(bc, expr->Binary_Expr.first);
                if (opcode != BC_NOOP) {
                    first = bc_instruction_load(bc, result_type, first_ptr);
                }
            } else {
                first = convert_expression_to_bytecode(bc, expr->Binary_Expr.first);
            }
            int second = convert_expression_to_bytecode(bc, expr->Binary_Expr.second);
            result = second;
            
            if (result_type->kind == TypeKind_Pointer) {
                // Pointer arithmetic
                assert(opcode == BC_ADD || opcode == BC_SUB);
                if (opcode == BC_SUB) {
                    // Negate `second` 
                    int tmp = add_bytecode_register(bc, expr->Binary_Expr.second->type);
                    bc_instruction(bc, BC_NEG, tmp, second, -1);
                    second = tmp;
                }
                result = bc_instruction_array_accesss(bc, result_type->Pointer, first, second);
                
            } else if (opcode != BC_NOOP) {
                result = add_bytecode_register(bc, result_type);
                bc_instruction(bc, opcode, result, first, second);
            }
            
            if (first_ptr != -1) {
                if (result_type->size > 8) {
                    bc_instruction(bc, BC_MEMCPY, first_ptr, result, result_type->size);
                } else {
                    bc_instruction_store(bc, first_ptr, result);
                }
            }
        } break;
        
        case Ast_Aggregate_Expr: {
            Type* type = expr->type;
            
            int global_index = -1;
            if (is_valid_ast(expr->Aggregate_Expr.elements->Compound.node)) {
                global_index = add_bytecode_global(bc, BC_MEM_READ_ONLY, type->size, type->align);
                Bytecode_Global* g = &bc->bytecode.globals[global_index];
                convert_aggregate_literal_to_memory(expr, g->address);
            }
            
            if (global_index >= 0) {
                result = bc_instruction_global(bc, global_index);
                
                // Write non-constant fields to stack
                int field_index = (int) expr->Aggregate_Expr.first_index;
                for_compound(expr->Aggregate_Expr.elements, field) {
                    assert(field->kind == Ast_Argument);
                    
                    string_id ident = try_unwrap_ident(field->Argument.ident);
                    Ast* assign = field->Argument.assign;
                    
                    Struct_Field_Info field_info = {};
                    if (type->kind == TypeKind_Struct || type->kind == TypeKind_Union)  {
                        if (ident) {
                            field_info = get_field_info(&type->Struct_Like, ident);
                        } else {
                            field_info = get_field_info_by_index(&type->Struct_Like, field_index);
                        }
                    } else if (type->kind == TypeKind_Array) {
                        field_info.type = type->Array.type;
                        field_info.offset = field_index * field_info.type->size;
                    } else {
                        compiler_bug("unexpected aggregate type");
                    }
                    
                    if (assign->kind != Ast_Value && 
                        assign->kind != Ast_Aggregate_Expr) {
                        unimplemented;
                        //pln("Adding non-constant field: %", f_ast(assign));
                        
                        //if (result.kind == BytecodeOperand_memory) { 
                        // Allocate stack space for storing the aggregate type
                        //int tmp = push_bytecode_stack(bc, type->size, type->align);
                        //convert_assign_to_bytecode(bc, type, tmp, result, true);
                        //result = tmp;
                        //}
                        //
                        //int src = convert_expression_to_bytecode(bc, assign);
                        //int dest = result;
                        //dest.memory_offset += (u32) field_info.offset;
                        //convert_assign_to_bytecode(bc, field_info.type, dest, src, true);
                    }
                    
                    field_index++;
                }
            }
            
        } break;
        
        case Ast_Call_Expr: {
            array(Ast*)* args = 0;
            for_compound(expr->Call_Expr.args, arg) {
                array_push(args, arg->Argument.assign);
            }
            result = convert_function_call_to_bytecode(bc, expr->Call_Expr.function_type, args);
            array_free(args);
        } break;
        
        case Ast_Cast_Expr: {
            result = convert_type_cast_to_bytecode(bc, expr);
        } break;
        
        case Ast_Index_Expr:
        case Ast_Field_Expr: {
            result = convert_lvalue_expression_to_bytecode(bc, expr);
            int flags = register_type(bc->curr_function, result).flags;
            if (!is_bitflag_set(flags, BC_FLAG_RVALUE)) {
                result = bc_instruction_load(bc, expr->type, result);
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

int
convert_type_cast_to_bytecode(Bytecode_Builder* bc, Ast* expr) {
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
    
    int result = -1;
    
    if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
        int src = convert_expression_to_bytecode(bc, expr->Cast_Expr.expr);
        result = src;
        
        if (t_dest->Basic.flags & BasicFlag_Boolean &&
            t_src->Basic.flags & BasicFlag_Integer) {
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
            
        } else if (t_dest->Basic.flags & BasicFlag_Integer &&
                   t_src->Basic.flags & BasicFlag_Integer) {
            if (t_dest->size < t_src->size) {
                result = add_bytecode_register(bc, t_dest);
                bc_instruction(bc, BC_TRUNCATE, result, src, -1);
                
            } else if (t_dest->size > t_src->size) {
                result = add_bytecode_register(bc, t_dest);
                bc_instruction(bc, BC_EXTEND, result, src, -1);
            }
            
        } else if (t_dest->Basic.flags & BasicFlag_Integer &&
                   t_src->Basic.flags & BasicFlag_Floating) {
            result = add_bytecode_register(bc, t_dest);
            bc_instruction(bc, BC_FLOAT_TO_INT, result, src, -1);
            
        } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                   t_src->Basic.flags & BasicFlag_Integer) {
            result = add_bytecode_register(bc, t_dest);
            bc_instruction(bc, BC_INT_TO_FLOAT, result, src, -1);
            
        } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                   t_src->Basic.flags & BasicFlag_Floating) {
            result = add_bytecode_register(bc, t_dest);
            bc_instruction(bc, BC_FLOAT_TO_FLOAT, result, src, -1);
            
        } else if (t_src->Basic.kind == Basic_cstring) {
            // noop
            
        } else {
            unimplemented;
        }
        
        
    } else if (t_dest->kind == TypeKind_Array && 
               t_src->kind == TypeKind_Array) {
        int src_ptr = convert_lvalue_expression_to_bytecode(bc, expr->Cast_Expr.expr);
        
        if (t_dest->Array.kind == ArrayKind_Fixed &&
            t_src->Array.kind == ArrayKind_Fixed_Inplace) {
            
            // Converts inplace fixed array to "wide"-pointer array
            result = bc_instruction_local(bc, t_dest);
            int array_ptr = add_bytecode_register(bc, t_src);
            bc_instruction(bc, BC_FIELD_ACCESS, array_ptr, result, 0);
            bc_instruction_store(bc, array_ptr, src_ptr);
            
            int count = bc_instruction_const_int(bc, t_s64, t_src->Array.capacity);
            int count_ptr = add_bytecode_register(bc, t_s64);
            bc_instruction(bc, BC_FIELD_ACCESS, count_ptr, result, 8);
            bc_instruction_store(bc, count_ptr, count);
        } else {
            unimplemented;
        }
        
    } else if (t_dest->kind == TypeKind_Struct && 
               t_src->kind == TypeKind_Struct) {
        // NOTE(Alexander): this is a NOOP, not possible to cast to different struct
        result = convert_expression_to_bytecode(bc, expr->Cast_Expr.expr);
        
    } else {
        unimplemented;
    }
    
    return result;
}

int
convert_condition_to_bytecode(Bytecode_Builder* bc, Ast* cond) {
    Type* type = cond->type;
    int result = convert_expression_to_bytecode(bc, cond);
    
    if (!bc_is_comparator(bc->curr_insn->opcode)) {
        
        int zero;
        if (type->kind == TypeKind_Basic) {
            if (type->Basic.kind == Basic_f32) {
                zero = bc_instruction_const_f32(bc, 0);
            } else if (type->Basic.kind == Basic_f64) {
                zero = bc_instruction_const_f64(bc, 0);
            } else {
                zero = bc_instruction_const_int(bc, type, 0);
            } 
        } else {
            zero = bc_instruction_const_int(bc, type, 0);
        }
        int tmp = add_bytecode_register(bc, t_bool);
        bc_instruction(bc, BC_NEQ, tmp, result, zero);
        result = tmp;
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
            
            
            int local;
            if (stmt->Assign_Stmt.mods & AstDeclModifier_Local_Persist) {
                local = -1;
                unimplemented;
            } else {
                local = bc_instruction_local(bc, type);
            }
            
            if (is_valid_ast(stmt->Assign_Stmt.expr)) {
                int src = convert_expression_to_bytecode(bc, stmt->Assign_Stmt.expr);
                assert(src != -1);
                
                if (is_aggregate_type(type)) {
                    bc_instruction(bc, BC_MEMCPY, local, src, type->size);
                } else {
                    bc_instruction_store(bc, local, src);
                }
            }
            
            assert(stmt->Assign_Stmt.ident->kind == Ast_Ident);
            string_id ident = ast_unwrap_ident(stmt->Assign_Stmt.ident);
            map_put(bc->locals, ident, local);
            
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(stmt->Block_Stmt.stmts, it) {
                convert_statement_to_bytecode(bc, it, break_label, continue_label);
            }
        } break;
        
        case Ast_If_Stmt: {
            begin_block(bc);
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                begin_block(bc);
            }
            
            int cond = convert_condition_to_bytecode(bc, stmt->If_Stmt.cond);
            bc_instruction_branch(bc, bc->block_depth, cond);
            
            // Then case
            convert_statement_to_bytecode(bc, stmt->If_Stmt.then_block, break_label, continue_label);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                // Else case
                bc_instruction_branch(bc, bc->block_depth - 1, -1);
                
                end_block(bc);
                convert_statement_to_bytecode(bc, stmt->If_Stmt.else_block, break_label, continue_label);
            }
            end_block(bc);
            
        } break;
        
        case Ast_For_Stmt: {
            // init
            convert_statement_to_bytecode(bc, stmt->For_Stmt.init, 0, 0);
            
            begin_block(bc);
            begin_block(bc, BC_LOOP);
            
            // Condition
            if (is_valid_ast(stmt->For_Stmt.cond)) {
                int cond = convert_condition_to_bytecode(bc, stmt->For_Stmt.cond);
                bc_instruction_branch(bc, bc->block_depth - 1, cond);
            }
            
            // Block
            begin_block(bc);
            convert_statement_to_bytecode(bc, stmt->For_Stmt.block, bc->block_depth - 2, bc->block_depth);
            end_block(bc);
            
            // Update
            convert_expression_to_bytecode(bc, stmt->For_Stmt.update);
            bc_instruction_branch(bc, bc->block_depth, -1);
            
            // Exit
            end_block(bc);
            end_block(bc);
        } break;
        
        case Ast_While_Stmt: {
            begin_block(bc);
            begin_block(bc, BC_LOOP);
            
            // Condition
            if (is_valid_ast(stmt->While_Stmt.cond)) {
                int cond = convert_condition_to_bytecode(bc, stmt->While_Stmt.cond);
                bc_instruction_branch(bc, bc->block_depth - 1, cond);
            }
            
            convert_statement_to_bytecode(bc, stmt->While_Stmt.block, bc->block_depth - 1, bc->block_depth);
            bc_instruction_branch(bc, bc->block_depth, -1);
            
            // Exit
            end_block(bc);
            end_block(bc);
        } break;
        
        case Ast_Return_Stmt: {
            int result = -1;
            if (is_valid_ast(stmt->Return_Stmt.expr)) {
                if (bc->curr_function->return_as_first_arg) {
                    int src_ptr = convert_lvalue_expression_to_bytecode(bc, stmt->Return_Stmt.expr);
                    Bytecode_Function_Arg src_type = function_ret_types(bc->curr_function)[0];
                    bc_instruction(bc, BC_MEMCPY, 0, src_ptr, src_type.size);
                } else {
                    result = convert_expression_to_bytecode(bc, stmt->Return_Stmt.expr);
                }
            }
            bc_instruction(bc, BC_RETURN, -1, result, -1);
        } break;
        
        case Ast_Break_Stmt: {
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
            branch->cond = -1;
            branch->label_index = break_label;
        } break;
        
        case Ast_Continue_Stmt: {
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
            branch->cond = -1;
            branch->label_index = continue_label;
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
}


Bytecode_Function*
convert_function_to_bytecode(Bytecode_Builder* bc, Bytecode_Function* func, Ast* ast,
                             bool is_main, bool insert_debug_break) {
    assert(ast->type->kind == TypeKind_Function);
    assert(ast->kind == Ast_Decl_Stmt);
    
    if (!is_valid_ast(ast->Decl_Stmt.stmt)) {
        return 0;
    }
    
    Type* type = ast->type;
    
    // Build the function
    bc->curr_function = func;
    bc->curr_insn = 0;
    bc->block_depth = 0;
    if (is_main) {
        bc->bytecode.entry_func_index = func->type_index;
    }
    
    for_array_v(type->Function.arg_idents, arg_ident, i) {
        int arg_index = func->return_as_first_arg ? (i + 1) : i;
        map_put(bc->locals, arg_ident, arg_index);
    }
    
    if (insert_debug_break) {
        add_insn(bc, BC_DEBUG_BREAK);
    }
    
    convert_statement_to_bytecode(bc, ast->Decl_Stmt.stmt, 0, 0);
    
    return func;
}


Bytecode_Function*
add_bytecode_function(Bytecode_Builder* bc, Type* type) {
    assert(type && type->kind == TypeKind_Function && "not a function type");
    
    int ret_count = is_valid_type(type->Function.return_type) ? 1 : 0;
    int arg_count = (int) array_count(type->Function.arg_types);
    
    int size = (sizeof(Bytecode_Function) + 
                (ret_count + arg_count)*sizeof(Bytecode_Function_Arg));
    
    Bytecode_Function* func = (Bytecode_Function*) arena_push_size(&bc->arena, size, 
                                                                   alignof(Bytecode_Function));
    func->relative_ptr = (u32) arena_relative_pointer(&bc->arena, func);
    func->type_index = bc->next_type_index++;
    func->ret_count = ret_count;
    func->arg_count = arg_count;
    
    bc->curr_function = func;
    bc->curr_insn = 0;
    
    if (type->Function.unit) {
        type->Function.unit->bytecode_function = func;
    }
    
    Bytecode_Function_Arg* curr_arg = function_ret_types(func);
    if (ret_count == 1) {
        Type* ret_type = type->Function.return_type;
        if (is_aggregate_type(ret_type)) {
            add_bytecode_register(bc, type->Function.return_type);
            func->arg_count++;
            func->ret_count--;
            func->return_as_first_arg = true;
        }
        
        curr_arg->type = to_bytecode_type(bc, ret_type);
        curr_arg->size = ret_type->size;
        curr_arg->align = ret_type->align;
        curr_arg++;
    }
    
    for (int i = 0; i < arg_count; i++) {
        string_id arg_ident = type->Function.arg_idents[i];
        Type* arg_type = type->Function.arg_types[i];
        curr_arg->type = to_bytecode_type(bc, arg_type);
        curr_arg->size = arg_type->size;
        curr_arg->align = arg_type->align;
        
        int arg = add_bytecode_register(bc, arg_type);
        func->register_types[arg].flags |= BC_FLAG_RVALUE;
        
        curr_arg++;
    }
    
    array_push(bc->bytecode.functions, func);
    array_push(bc->bytecode.function_names, type->Function.ident);
    
    return func;
}

int
add_bytecode_global(Bytecode_Builder* bc, 
                    Bytecode_Memory_Kind kind, 
                    smm size, smm align, void* init) {
    
    Memory_Arena* arena = (kind == BC_MEM_READ_ONLY ? 
                           &bc->data_packer->rdata_arena : 
                           &bc->data_packer->data_arena);
    void* data = arena_push_size(arena, size, align);
    int offset = (int) arena_relative_pointer(arena, data);
    if (init) {
        memcpy(data, init, size);
    }
    
    Bytecode_Global global_var = {};
    global_var.address = data;
    global_var.offset = offset;
    global_var.size = (u32) size;
    global_var.align = (u32) align;
    global_var.kind = kind;
    
    int global_index = (int) array_count(bc->bytecode.globals);
    array_push(bc->bytecode.globals, global_var);
    return global_index;
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
    //if (insn->type) {
    //string_builder_push_format(sb, "%.", f_cstring(bc_instruction_type_names[insn->type]));
    //}
    string_builder_push(sb, bc_operator_names[insn->opcode]);
    string_builder_push_format(sb, " ");
}

void
string_builder_dump_bytecode_insn(String_Builder* sb, Bytecode* bc, Bytecode_Instruction* insn, int block_depth) {
    
    smm from_byte = sb->curr_used;
    string_builder_pad(sb, sb->curr_used, block_depth*2);
    
    switch (insn->kind) {
        case BytecodeInstructionKind_None: break;
        
        case BytecodeInstructionKind_Base: {
            string_builder_dump_bytecode_opcode(sb, insn);
            
            switch (insn->opcode) {
                case BC_BLOCK:
                case BC_LOOP:{
                    string_builder_push_format(sb, "(label = %)", f_int(block_depth));
                } break;
            }
        } break;
        
        case BytecodeInstructionKind_Binary: {
            Bytecode_Binary* bc_instruction_insn = (Bytecode_Binary*) insn;
            if (bc_instruction_insn->res_index >= 0 && insn->opcode != BC_MEMCPY) {
                string_builder_push_format(sb, "r% = ", f_int(bc_instruction_insn->res_index));
            }
            
            string_builder_dump_bytecode_opcode(sb, insn);
            
            switch (insn->opcode) {
                case BC_INT_CONST: {
                    string_builder_push_format(sb, "%", f_int(bc_instruction_insn->const_i64));
                } break;
                
                case BC_F32_CONST: {
                    string_builder_push_format(sb, "%", f_float(bc_instruction_insn->const_f32));
                } break;
                
                case BC_F64_CONST: {
                    string_builder_push_format(sb, "%", f_float(bc_instruction_insn->const_f64));
                } break;
                
                case BC_LOCAL: {
                    string_builder_push_format(sb, "size %, align %", f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index));
                } break;
                
                case BC_GLOBAL: {
                    string_builder_push_format(sb, "%", f_int(bc_instruction_insn->arg0_index));
                } break;
                
                case BC_MEMCPY: {
                    string_builder_push_format(sb, "r%, r%, size %",
                                               f_int(bc_instruction_insn->res_index),
                                               f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index));
                } break;
                
                case BC_ARRAY_ACCESS: {
                    string_builder_push_format(sb, "r%, r%, stride %",
                                               f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index),
                                               f_int(bc_instruction_insn->stride));
                } break;
                
                case BC_FIELD_ACCESS: {
                    string_builder_push_format(sb, "r%, %", f_int(bc_instruction_insn->arg0_index),
                                               f_int(bc_instruction_insn->arg1_index));
                } break;
                
                case BC_FLOAT_TO_INT: {
                    string_builder_push_format(sb, "r%", f_int(bc_instruction_insn->arg0_index));
                } break;
                
                default: {
                    if (bc_instruction_insn->arg1_index >= 0) {
                        string_builder_push_format(sb, "r%, ", f_int(bc_instruction_insn->arg0_index));
                        string_builder_push_format(sb, "r%", f_int(bc_instruction_insn->arg1_index));
                    } else if (bc_instruction_insn->arg0_index >= 0) {
                        string_builder_push_format(sb, "r%", f_int(bc_instruction_insn->arg0_index));
                    }
                } break;
            }
        } break;
        
        case BytecodeInstructionKind_Call: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            
            Bytecode_Function* func = bc->functions[call->func_index];
            int* args = (int*) (call + 1);
            
            if (func->ret_count == 1) {
                // TODO(Alexander): multiple returns
                string_builder_push_format(sb, "r%", f_int(args[func->arg_count]));
                //string_builder_dump_bytecode_operand(sb, args[func->arg_count], insn->type);
                string_builder_push(sb, " = ");
            }
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_dump_bytecode_function_name(sb, bc, func);
            string_builder_push(sb, "(");
            
            Bytecode_Function_Arg* formal_args = function_arg_types(func);
            for (int i = 0; i < func->arg_count; i++) {
                string_builder_push_format(sb, "r%", f_int(args[i]));
                if (i + 1 < func->arg_count) {
                    string_builder_push(sb, ", ");
                }
            }
            string_builder_push(sb, ")");
        } break;
        
        case BytecodeInstructionKind_Block: {
            u32 label_index = ((Bytecode_Block*) insn)->label_index;
            if (label_index > 0) {
                from_byte = sb->curr_used + 5;
                string_builder_push_format(sb, "\nBasic Block %:", f_u32(label_index));
            }
        } break;
        
        case BytecodeInstructionKind_Branch: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_push_format(sb, "%", f_u32(((Bytecode_Branch*) insn)->label_index));
            
            if (((Bytecode_Branch*) insn)->cond != -1) {
                string_builder_push(sb, ", ");
                string_builder_push_format(sb, "r%", f_int(((Bytecode_Branch*) insn)->cond));
            }
        } break;
        
        case BytecodeInstructionKind_Memory: {
            //string_builder_dump_bytecode_opcode(sb, insn);
            //string_builder_dump_bytecode_operand(sb, ((Bytecode_Memory*) insn)->dest, insn->type);
            //string_builder_push(sb, ", ");
            //string_builder_dump_bytecode_operand(sb, ((Bytecode_Memory*) insn)->src, insn->type);
            //string_builder_push_format(sb, ", %", f_int(((Bytecode_Memory*) insn)->size));
            unimplemented;
        } break;
    }
    
    if (insn->comment) {
        string_builder_pad(sb, from_byte, 30);
        string_builder_push_format(sb, " // %", f_cstring(insn->comment));
    }
}

void
string_builder_dump_bytecode_type(String_Builder* sb, Bytecode_Type type) {
    
    switch (type.kind) {
        case BC_TYPE_INT: {
            if (type.flags & BC_FLAG_SIGNED) {
                string_builder_push_format(sb, "s%", f_int((int) type.size*8));
            } else {
                string_builder_push_format(sb, "u%", f_int((int) type.size*8));
            }
        } break;
        
        case BC_TYPE_FLOAT: {
            string_builder_push_format(sb, "f%", f_int((int) type.size*8));
        } break;
        
        case BC_TYPE_PTR: {
            string_builder_push(sb, "ptr");
        } break;
    }
    
}

void
string_builder_dump_bytecode_function(String_Builder* sb, Bytecode* bc, Bytecode_Function* func) {
    if (!func) return;
    
    if (func->is_intrinsic || func->is_imported) {
        return;
    }
    
    if (func->is_imported) {
        string_builder_push(sb, "import ");
    }
    
    Bytecode_Function_Arg* formal_args = function_arg_types(func);
    if (func->ret_count == 1) {
        Bytecode_Function_Arg* formal_ret = function_ret_types(func);
        // TODO(Alexander): multiple returns
        string_builder_dump_bytecode_type(sb, formal_ret[0].type);
        string_builder_push(sb, " ");
    } else {
        string_builder_push(sb, "void ");
    }
    string_builder_dump_bytecode_function_name(sb, bc, func);
    string_builder_push(sb, "(");
    for (int i = 0; i < func->arg_count; i++) {
        string_builder_dump_bytecode_type(sb, formal_args[i].type);
        string_builder_push_format(sb, " r%", f_int(i));
        if (i == 0 && func->return_as_first_arg) {
            string_builder_push_format(sb, " (ret)", f_int(i));
        }
        if (i + 1 < func->arg_count) {
            string_builder_push(sb, ", ");
        }
    }
    
    int bb_index = 0;
    
    if (func->is_imported) {
        if (func->code_ptr) {
            string_builder_push_format(sb, ") = %;", f_u64_HEX(func->code_ptr));
        } else {
            string_builder_push(sb, ");");
        }
    } else {
        string_builder_push(sb, ") {");
    }
    
    Bytecode_Instruction* curr = iter_bytecode_instructions(func, 0);
    int block_depth = 1;
    while (curr->kind) {
        if (curr->opcode == BC_END) {
            block_depth--;
        }
        
        string_builder_push(sb, "\n");
        string_builder_dump_bytecode_insn(sb, bc, curr, block_depth);
        
        if (curr->opcode == BC_BLOCK || curr->opcode == BC_LOOP) {
            block_depth++;
        }
        curr = iter_bytecode_instructions(func, curr);
    }
    
    if (!func->is_imported) {
        string_builder_push(sb, "\n}");
    }
    
    string_builder_push(sb, "\n");
}

void
string_builder_dump_bytecode_globals(String_Builder* sb, Bytecode* bc) {
    if (array_count(bc->globals) > 0) {
        string_builder_push(sb, "Globals:");
        for_array(bc->globals, g, global_index) {
            const cstring bc_instruction_memory_names[] = { "read only", "read write" };
            string_builder_push_format(sb, "\n  %: size %, align %, kind \"%\" - ",
                                       f_int(global_index),
                                       f_int(g->size),
                                       f_int(g->align),
                                       f_cstring(bc_instruction_memory_names[g->kind]));
            
            if (g->address) {
                string_builder_push(sb, "\"");
                u8* curr = (u8*) g->address;
                for (u32 i = 0; i < g->size; i++) {
                    string_builder_push_char_literal(sb, *curr++);
                    //if (i < g->size - 1) {
                    //string_builder_push(sb, " ");
                    //}
                }
                string_builder_push(sb, "\"");
            }
        }
    }
    
}

void
dump_bytecode(Bytecode* bc) {
    String_Builder sb = {};
    for_array_v(bc->functions, func, _) {
        string_builder_dump_bytecode_function(&sb, bc, func);
    }
    string_builder_dump_bytecode_globals(&sb, bc);
    
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}