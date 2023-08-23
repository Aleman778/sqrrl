
#define convert_assign_to_bytecode(bc, type, dest, src, store_inplace) \
_convert_assign_to_bytecode(bc, type, dest, src, store_inplace, __FILE__ ":" S2(__LINE__))

void
_convert_assign_to_bytecode(Bytecode_Builder* bc, Type* type, 
                            Bytecode_Operand dest, Bytecode_Operand src,
                            bool store_inplace, cstring comment=0) {
    
    
    if (type->size > 8) {
        if (store_inplace) {
            if (src.kind) {
                Bytecode_Memory* insn = add_insn_t(bc, BC_MEMORY_COPY, Memory);
                insn->comment = comment;
                insn->dest = dest;
                insn->src = src;
                insn->size = type->size;
                
            } else {
                Bytecode_Memory* insn = add_insn_t(bc, BC_MEMORY_SET, Memory);
                insn->comment = comment;
                insn->dest = dest;
                insn->value  = 0;
                insn->size = type->size;
            }
        } else {
            assert(dest.kind);
            unimplemented;
            //_ic_lea(bc, dest, src, comment);
        }
        
    } else {
        if (type->kind == TypeKind_Basic && type->Basic.kind == Basic_cstring && 
            src.kind == BytecodeOperand_memory) {
            Bytecode_Binary* load_ptr = add_insn_t(bc, BC_ADDR_OF, Binary);
            load_ptr->type = bc_pointer_type(bc);
            load_ptr->first = add_bytecode_register(bc);
            load_ptr->second = src;
            src = load_ptr->first;
        }
        
        if (!src.kind) {
            src.kind = BytecodeOperand_const;
            src.const_i64 = 0;
        }
        _add_mov_insn(bc, type, dest, src, comment);
    }
}

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
            Bytecode_Type result_type = to_bytecode_type(bc, expr->type);
            
            if (is_integer(expr->Value) || is_floating(expr->Value)) {
                s64 constant = 0;
                result = add_bytecode_register(bc);
                value_store_in_memory(expr->type, &constant, expr->Value.data);
                const_instruction(bc, BC_CONST, result, constant);
                
            } else if (is_string(expr->Value)) {
                smm string_count = expr->Value.data.str.count;
                Bytecode_Operand str_data = push_bytecode_memory(bc, BytecodeMemory_read_only, 
                                                                 string_count, 1, 
                                                                 expr->Value.data.str.data);
                result = push_bytecode_stack_t(bc, string);
                
                Bytecode_Binary* store_data = add_insn_t(bc, BC_ADDR_OF, Binary);
                store_data->type = bc_pointer_type(bc);
                store_data->first = add_bytecode_register(bc);
                store_data->second = str_data;
                add_mov_insn(bc, t_void_ptr, result, store_data->first);
                drop_bytecode_register(bc, store_data->first.register_index);
                
                Bytecode_Binary* store_count = add_insn_t(bc, BC_MOV, Binary);
                store_count->type = BytecodeType_i64;
                store_count->first = result;
                store_count->first.memory_offset += 8;
                store_count->second = result;
                store_count->second.kind = BytecodeOperand_const;
                store_count->second.const_i64 = (s64) string_count;
                
            } else if (is_cstring(expr->Value)) {
                if (expr->Value.data.cstr) {
                    smm string_count = cstring_count(expr->Value.data.cstr);
                    result = push_bytecode_memory(bc, BytecodeMemory_read_only, 
                                                  string_count, 1, 
                                                  (void*) expr->Value.data.cstr);
                    
                } else {
                    result.kind = BytecodeOperand_const;
                    result.const_i64 = 0;
                }
                
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Unary_Expr: {
            Type* type = expr->type;
            Operator op = expr->Unary_Expr.op;
            Bytecode_Operand first = convert_expression_to_bytecode(bc, expr->Unary_Expr.first);
            
            
            if (op == Op_Address_Of) {
                Bytecode_Binary* insn = add_insn_t(bc, BC_ADDR_OF, Binary);
                insn->type  = to_bytecode_type(bc, type);
                insn->first = add_bytecode_register(bc);
                insn->second = first;
                result = insn->first;
                
            } else {
                Bytecode_Operator opcode = BC_NOOP;
                switch (op) {
                    case Op_Negate:  opcode = BC_NEG; break;
                    //case Op_Logical_Not: opcode = BC_NOT; break;
                    case Op_Bitwise_Not: opcode = BC_NOT; break;
                    case Op_Dereference: opcode = BC_DEREF; break;
                    case Op_Pre_Increment: opcode = BC_INC; break;
                    case Op_Pre_Decrement: opcode = BC_DEC; break;
                    
                    case Op_Post_Increment: {
                        result = add_bytecode_register(bc);
                        add_mov_insn(bc, type, result, first);
                        opcode = BC_INC;
                    } break;
                    
                    case Op_Post_Decrement: {
                        result = add_bytecode_register(bc);
                        add_mov_insn(bc, type, result, first);
                        opcode = BC_DEC;
                    } break;
                }
                
                Bytecode_Unary* insn = add_insn_t(bc, opcode, Unary);
                insn->type  = to_bytecode_type(bc, type);
                insn->first = first;
                
                if (!result.kind) {
                    result = insn->first;
                }
            }
        } break;
        
        case Ast_Binary_Expr: {
            // TODO(Alexander): operator overloading
            // TODO(Alexander): add proper AND/ OR logical operator
            Operator op = expr->Binary_Expr.op;
            bool is_assign = operator_is_assign(op);
            
            Type* result_type = expr->type;
            Type* type = expr->Binary_Expr.first->type;
            Bytecode_Operand first = convert_expression_to_bytecode(bc, expr->Binary_Expr.first);
            
            //bool allocate_tmp_register = (first.kind != BytecodeOperand_register && !is_assign);
            //if (allocate_tmp_register) {
            //Bytecode_Operand tmp_register = add_bytecode_register(bc);
            //add_mov_insn(bc, type, tmp_register, first);
            //first = tmp_register;
            //}
            
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
            }
            
            Bytecode_Operand second = convert_expression_to_bytecode(bc, expr->Binary_Expr.second);
            
            if (opcode != BC_NOOP) {
                result = add_bytecode_register(bc);
                instruction(bc, opcode, result, first, second);
            }
            
            if (is_assign) {
                convert_assign_to_bytecode(bc, result_type, first, result, true);
            }
        } break;
        
        case Ast_Aggregate_Expr: {
            Type* type = expr->type;
            
            Exported_Data base = {};
            if (is_valid_ast(expr->Aggregate_Expr.elements->Compound.node)) {
                base = export_size(bc->data_packer, Read_Data_Section, type->size, type->align);
                convert_aggregate_literal_to_memory(expr, base.data);
            }
            
            if (base.data) {
                result.kind = BytecodeOperand_memory;
                result.memory_kind = BytecodeMemory_read_only;
                result.memory_offset = base.relative_ptr;
                
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
                        //pln("Adding non-constant field: %", f_ast(assign));
                        
                        if (result.kind == BytecodeOperand_memory) { 
                            // Allocate stack space for storing the aggregate type
                            Bytecode_Operand tmp = push_bytecode_stack(bc, type->size, type->align);
                            convert_assign_to_bytecode(bc, type, tmp, result, true);
                            result = tmp;
                        }
                        
                        Bytecode_Operand src = convert_expression_to_bytecode(bc, assign);
                        Bytecode_Operand dest = result;
                        dest.memory_offset += (u32) field_info.offset;
                        convert_assign_to_bytecode(bc, field_info.type, dest, src, true);
                    }
                    
                    field_index++;
                }
            }
            
        } break;
        
        case Ast_Call_Expr: {
            Type* type = expr->Call_Expr.function_type;
            assert(type && type->kind == TypeKind_Function);
            
            Compilation_Unit* target_cu = type->Function.unit;
            assert(target_cu && target_cu->bytecode_function);
            Bytecode_Function* func = target_cu->bytecode_function;
            
            if (func->is_intrinsic) {
                switch (func->intrinsic_id) {
                    case Sym_rdtsc: {
                        Bytecode_Unary* intrin = add_insn_t(bc, BC_RDTSC, Unary);
                        intrin->type = BytecodeType_i64;
                        intrin->first = add_bytecode_register(bc);
                        result = intrin->first;
                    } break;
                }
                
                return result;
            }
            
            
            array(Bytecode_Operand)* arg_operands = 0;
            for_compound(expr->Call_Expr.args, arg) {
                Bytecode_Operand op = convert_expression_to_bytecode(bc, arg->Argument.assign);
                Type* arg_type = arg->Argument.assign->type;
                if (arg_type->size > 8) {
                    Bytecode_Binary* arg_insn = add_insn_t(bc, BC_ADDR_OF, Binary);
                    arg_insn->type = to_bytecode_type(bc, arg_type);
                    arg_insn->first = add_bytecode_register(bc);
                    arg_insn->second = op;
                    op = arg_insn->first;
                }
                // TODO(Alexander): add default args
                
                array_push(arg_operands, op);
            }
            
            umm arg_size = sizeof(Bytecode_Operand)*(func->ret_count + func->arg_count);
            umm insn_size = sizeof(Bytecode_Call) + arg_size;
            umm insn_align = max(alignof(Bytecode_Call), alignof(Bytecode_Operand));
            Bytecode_Call* insn = (Bytecode_Call*) add_bytecode_insn(bc, BC_CALL, 
                                                                     BytecodeInstructionKind_Call, 
                                                                     insn_size, insn_align,
                                                                     __FILE__ ":" S2(__LINE__));
            insn->func_index = func->type_index;
            
            // TODO(Alexander): multiple args
            if (is_valid_type(type->Function.return_type)) {
                insn->type = to_bytecode_type(bc, type->Function.return_type);
                result = add_bytecode_register(bc);
                array_push(arg_operands, result);
            }
            
            if (arg_operands) {
                memcpy((Bytecode_Operand*) (insn + 1), arg_operands, arg_size);
                array_free(arg_operands);
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
            
            Bytecode_Type dest_type = to_bytecode_type(bc, expr->type);
            Bytecode_Operand src = convert_expression_to_bytecode(bc, expr->Cast_Expr.expr);
            result = src;
            
            if (t_dest->kind == TypeKind_Basic && t_src->kind == TypeKind_Basic) {
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
                        Bytecode_Binary* insn = add_insn_t(bc, BC_NOOP, Binary);
                        insn->type = dest_type;
                        insn->first = add_bytecode_register(bc);
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
                        insn->first = add_bytecode_register(bc);
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
                    
                } else if (t_dest->Basic.flags & BasicFlag_Integer &&
                           t_src->Basic.flags & BasicFlag_Floating) {
                    Bytecode_Binary* insn;
                    bool is_unsigned = t_dest->Basic.flags & BasicFlag_Unsigned;
                    if (t_src->Basic.kind == Basic_f32) {
                        insn = add_insn_t(bc, is_unsigned ? BC_CONVERT_F32_U : BC_CONVERT_F32_S, Binary);
                    } else {
                        insn = add_insn_t(bc, is_unsigned ? BC_CONVERT_F64_U : BC_CONVERT_F64_S, Binary);
                    }
                    
                    insn->type = dest_type;
                    insn->first = add_bytecode_register(bc);
                    insn->second = src;
                    result = insn->first;
                    
                } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                           t_src->Basic.flags & BasicFlag_Integer) {
                    Bytecode_Binary* insn = add_insn_t(bc, BC_NOOP, Binary);
                    switch (t_src->Basic.kind) {
                        case Basic_s8:
                        case Basic_s16:
                        case Basic_s32: insn->opcode = BC_CONVERT_S32; break;
                        case Basic_s64: insn->opcode = BC_CONVERT_S64; break;
                        case Basic_u8: 
                        case Basic_u16: 
                        case Basic_u32: insn->opcode = BC_CONVERT_U32; break;
                        case Basic_u64: insn->opcode = BC_CONVERT_U64; break;
                        default: unimplemented;
                    }
                    
                    insn->type = dest_type;
                    insn->first = add_bytecode_register(bc);
                    insn->second = src;
                    result = insn->first;
                    
                } else if (t_dest->Basic.flags & BasicFlag_Floating &&
                           t_src->Basic.flags & BasicFlag_Floating) {
                    Bytecode_Binary* insn = add_insn_t(bc, BC_CONVERT_F2F, Binary);
                    insn->type = dest_type;
                    insn->first = add_bytecode_register(bc);
                    insn->second = src;
                    result = insn->first;
                    
                } else if (t_src->Basic.kind == Basic_cstring) {
                    // noop
                    
                } else {
                    unimplemented;
                }
                
            } else if (t_dest->kind == TypeKind_Array && 
                       t_src->kind == TypeKind_Array) {
                
                if (t_src->Array.capacity > 0) {
                    if (t_dest->Array.capacity == 0) {
                        // convert inplace array to "wide"-pointer array
                        // TODO(Alexander): we can improve this by e.g. clearing it and 
                        // also passing multiple values as arguments
                        result = push_bytecode_stack(bc, 16, 8);
                        
                        Bytecode_Binary* copy_ptr = add_insn_t(bc, BC_ADDR_OF, Binary);
                        copy_ptr->type = bc_pointer_type(bc);
                        copy_ptr->first = add_bytecode_register(bc);
                        copy_ptr->second = src;
                        add_mov_insn(bc, t_void_ptr, result, copy_ptr->first);
                        
                        Bytecode_Binary* store_count = add_insn_t(bc, BC_MOV, Binary);
                        store_count->type = BytecodeType_i64;
                        store_count->first = result;
                        store_count->first.memory_offset += 8;
                        store_count->second.kind = BytecodeOperand_const;
                        store_count->second.const_i64 = t_src->Array.capacity;
                        // TODO(Alexander): set capacity if dynamic array
                    }
                } else {
                    unimplemented;
                }
            } else {
                unimplemented;
            }
        } break;
        
        case Ast_Field_Expr: {
            Type* type = expr->Field_Expr.var->type;
            result = convert_expression_to_bytecode(bc, expr->Field_Expr.var);
            string_id ident = ast_unwrap_ident(expr->Field_Expr.field);
            
            switch (type->kind) {
                case TypeKind_Struct:
                case TypeKind_Union: {
                    Struct_Field_Info info = get_field_info(&type->Struct_Like, ident);
                    result.memory_offset += (s32) info.offset;
                } break;
                
                case TypeKind_Array:
                case TypeKind_Basic: {
                    assert(type->kind == TypeKind_Array ||
                           type->Basic.kind == Basic_string && "unsupported type");
                    if (ident == Sym_count) {
                        result.memory_offset += 8;
                    }
                } break;
            }
        } break;
        
        case Ast_Index_Expr: {
#if 0
            Type* type = expr->type;
            Type* index_type = expr->Index_Expr.index->type;
            Type* array_type = expr->Index_Expr.array->type;
            
            Bytecode_Operand index = convert_expression_to_bytecode(bc, expr->Index_Expr.index);
            
            result = convert_expression_to_bytecode(bc, expr->Index_Expr.array);
            if (array_type->kind == TypeKind_Array && array_type->Array.is_inplace &&
                index.kind == BytecodeOperand_const) {
                assert(result.kind == BytecodeOperand_stack ||
                       result.kind == BytecodeOperand_memory);
                result.memory_offset += index.const_i32*type->size;
                return result;
            }
            
            // TODO(Alexander): this is a bit hacky, later this should be a single instruction
            if (index.kind != BytecodeOperand_const && type->size > 1) {
                Bytecode_Operand tmp = add_bytecode_register(bc);
                add_mov_insn(bc, index_type, tmp, index);
                index = tmp;
                
                Bytecode_Binary* mul_index = add_insn_t(bc, BC_MUL, Binary);
                mul_index->type = to_bytecode_type(bc, index_type);
                mul_index->first = index;
                mul_index->second.kind = BytecodeOperand_const;
                mul_index->second.const_i64 = type->size;
            }
            
            Bytecode_Operator opcode = (array_type->kind == TypeKind_Array && 
                                        array_type->Array.is_inplace) ? BC_ADDR_OF : BC_MOV;
            Bytecode_Binary* array_ptr = add_insn_t(bc, opcode, Binary);
            array_ptr->type = bc_pointer_type(bc);
            array_ptr->first = add_bytecode_register(bc);
            array_ptr->second = result;
            result = array_ptr->first;
            
            array_ptr = add_insn_t(bc, BC_ADD, Binary);
            array_ptr->type = bc_pointer_type(bc);
            array_ptr->first = result;
            array_ptr->second = index;
            result = array_ptr->first;
            
            array_ptr = add_insn_t(bc, BC_DEREF, Binary);
            array_ptr->type = to_bytecode_type(bc, type);
            array_ptr->first = add_bytecode_register(bc);
            array_ptr->second = result;
            result = array_ptr->first;
#endif
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
                
                if (!src.kind) {
                    if (stmt->Assign_Stmt.mods & AstDeclModifier_Local_Persist) {
                        src = push_bytecode_memory(bc, BytecodeMemory_read_write, 
                                                   type->size, type->align);
                        unimplemented;
                    } else {
                        src = add_bytecode_register(bc);
                    }
                }
                map_put(bc->locals, ident, src);
                
                //if (src.kind) {
                //convert_assign_to_bytecode(bc, type, dest, src, true);
                //}
            }
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
            
            Bytecode_Operand cond = convert_expression_to_bytecode(bc, stmt->If_Stmt.cond);
            Bytecode_Branch* if_branch = add_insn_t(bc, BC_BRANCH, Branch);
            if_branch->cond = cond;
            if_branch->label_index = bc->block_depth;
            
            
            // Then case
            convert_statement_to_bytecode(bc, stmt->If_Stmt.then_block, break_label, continue_label);
            
            if (is_valid_ast(stmt->If_Stmt.else_block)) {
                // Else case
                Bytecode_Branch* else_branch = add_insn_t(bc, BC_BRANCH, Branch);
                else_branch->label_index = bc->block_depth - 1;
                
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
                Bytecode_Operand cond = convert_expression_to_bytecode(bc, stmt->For_Stmt.cond);
                Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
                branch->label_index = bc->block_depth - 1;
                branch->cond = cond;
            }
            
            // Block
            begin_block(bc);
            convert_statement_to_bytecode(bc, stmt->For_Stmt.block, bc->block_depth - 2, bc->block_depth);
            end_block(bc);
            
            // Update
            convert_expression_to_bytecode(bc, stmt->For_Stmt.update);
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
            branch->label_index = bc->block_depth;
            
            // Exit
            end_block(bc);
            end_block(bc);
        } break;
        
        case Ast_While_Stmt: {
            begin_block(bc);
            begin_block(bc, BC_LOOP);
            
            // Condition
            if (is_valid_ast(stmt->While_Stmt.cond)) {
                Bytecode_Operand cond = convert_expression_to_bytecode(bc, stmt->While_Stmt.cond);
                Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
                branch->label_index = bc->block_depth - 1;
                branch->cond = cond;
            }
            
            convert_statement_to_bytecode(bc, stmt->While_Stmt.block, bc->block_depth - 1, bc->block_depth);
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
            branch->label_index = bc->block_depth;
            
            // Exit
            end_block(bc);
            end_block(bc);
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
        
        case Ast_Break_Stmt: {
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
            branch->label_index = break_label;
        } break;
        
        case Ast_Continue_Stmt: {
            Bytecode_Branch* branch = add_insn_t(bc, BC_BRANCH, Branch);
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
    if (is_main) {
        bc->bytecode.entry_func_index = func->type_index;
    }
    
    if (insert_debug_break) {
        add_insn(bc, BC_DEBUG_BREAK);
    }
    
    convert_statement_to_bytecode(bc, ast->Decl_Stmt.stmt, 0, 0);
    
    // Update function lifetimes
    // TODO(Alexander): maybe this doesn't need to be done, if we make sure to drop registers manually
    int insn_index = 0;
    Bytecode_Instruction* it = iter_bytecode_instructions(func, 0);
    while (it->kind) {
        switch (it->kind) {
            
            case BytecodeInstructionKind_Unary: {
                Bytecode_Operand op = bc_unary_first(it);
                if (op.kind == BytecodeOperand_register) {
                    func->register_lifetimes[op.register_index] = insn_index;
                }
            } break;
            
            case BytecodeInstructionKind_Binary: {
                Bytecode_Operand op = bc_binary_first(it);
                if (op.kind == BytecodeOperand_register) {
                    func->register_lifetimes[op.register_index] = insn_index;
                }
                op = bc_binary_second(it);
                if (op.kind == BytecodeOperand_register) {
                    func->register_lifetimes[op.register_index] = insn_index;
                }
            } break;
        }
        
        insn_index++;
        it = iter_bytecode_instructions(func, it);
    }
    
    return func;
}


Bytecode_Function*
add_bytecode_function(Bytecode_Builder* bc, Type* type) {
    assert(type && type->kind == TypeKind_Function && "not a function type");
    
    int ret_count = is_valid_type(type->Function.return_type) ? 1 : 0;
    int arg_count = (int) array_count(type->Function.arg_types);
    int size = sizeof(Bytecode_Function) + ret_count + arg_count;
    
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
    
    Bytecode_Type* curr_type = (Bytecode_Type*) (func + 1);
    for (int i = 0; i < arg_count; i++) {
        string_id arg_ident = type->Function.arg_idents[i];
        Type* arg_type = type->Function.arg_types[i];
        *curr_type++ = to_bytecode_type(bc, arg_type);
        
        //Bytecode_Operand arg = push_bytecode_stack(bc, arg_type->size, arg_type->align);
        //map_put(bc->locals, arg_ident, arg);
    }
    
    if (ret_count > 0) {
        assert(ret_count == 1 && "TODO: multiple returns");
        *curr_type++ = to_bytecode_type(bc, type->Function.return_type);
        //push_bytecode_stack(bc, 
        //type->Function.return_type->size,
        //type->Function.return_type->align);
    }
    
    array_push(bc->bytecode.functions, func);
    array_push(bc->bytecode.function_names, type->Function.ident);
    
    return func;
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
string_builder_dump_bytecode_operand(String_Builder* sb, Bytecode_Operand op, Bytecode_Type type) {
    switch (op.kind) {
        case BytecodeOperand_const: {
            
            switch (type) {
                case BytecodeType_i32: {
                    string_builder_push_format(sb, "%", f_int(op.const_i32));
                } break;
                
                case BytecodeType_i64: {
                    string_builder_push_format(sb, "%", f_s64(op.const_i64));
                } break;
                
                case BytecodeType_f32: {
                    string_builder_push_format(sb, "%", f_float(op.const_f32));
                } break;
                
                case BytecodeType_f64: {
                    string_builder_push_format(sb, "%", f_float(op.const_f64));
                } break;
            }
        } break;
        
        case BytecodeOperand_register: {
            string_builder_push_format(sb, "r%", f_u32(op.register_index));
        } break;
        
        case BytecodeOperand_memory: {
            if (op.memory_kind == BytecodeMemory_absolute) {
                string_builder_push_format(sb, "memory[%]", f_u64_HEX(op.memory_absolute));
                
            } else {
                string_builder_push_format(sb, "memory[% + %]", 
                                           f_cstring(bc_memory_kind_names[op.memory_kind]), 
                                           f_u64_HEX(op.memory_offset));
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
    smm from_byte = sb->curr_used;
    
    switch (insn->kind) {
        case BytecodeInstructionKind_None: break;
        
        case BytecodeInstructionKind_Base: {
            string_builder_dump_bytecode_opcode(sb, insn);
        } break;
        
        case BytecodeInstructionKind_Alloca: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Alloca*) insn)->dest, insn->type);
        } break;
        
        case BytecodeInstructionKind_Unary: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Unary*) insn)->first, insn->type);
        } break;
        
        case BytecodeInstructionKind_Binary: {
            string_builder_push_format(sb, "r% = ", f_int(((Bytecode_Binary*) insn)->res_index));
            string_builder_dump_bytecode_opcode(sb, insn);
            if (insn->opcode == BC_CONST) {
                string_builder_push_format(sb, "%", f_int(((Bytecode_Binary*) insn)->const_i64));
            } else {
                string_builder_push_format(sb, "r%, ", f_int(((Bytecode_Binary*) insn)->arg0_index));
                string_builder_push_format(sb, "r%", f_int(((Bytecode_Binary*) insn)->arg1_index));
            }
        } break;
        
        case BytecodeInstructionKind_Call: {
            Bytecode_Call* call = (Bytecode_Call*) insn;
            
            Bytecode_Function* func = bc->functions[call->func_index];
            Bytecode_Operand* args = (Bytecode_Operand*) (call + 1);
            
            string_builder_dump_bytecode_opcode(sb, insn);
            if (func->ret_count == 1) {
                // TODO(Alexander): multiple returns
                string_builder_dump_bytecode_operand(sb, args[func->arg_count], insn->type);
                string_builder_push(sb, ", ");
            }
            
            string_builder_dump_bytecode_function_name(sb, bc, func);
            string_builder_push(sb, "(");
            
            Bytecode_Function_Arg* formal_args = (Bytecode_Function_Arg*) (func + 1);
            for (u32 i = 0; i < func->arg_count; i++) {
                string_builder_dump_bytecode_operand(sb, args[i], formal_args[i].type);
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
            
            if (((Bytecode_Branch*) insn)->cond.kind) {
                string_builder_push(sb, ", ");
                string_builder_dump_bytecode_operand(sb, ((Bytecode_Branch*) insn)->cond, insn->type);
            }
        } break;
        
        case BytecodeInstructionKind_Memory: {
            string_builder_dump_bytecode_opcode(sb, insn);
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Memory*) insn)->dest, insn->type);
            string_builder_push(sb, ", ");
            string_builder_dump_bytecode_operand(sb, ((Bytecode_Memory*) insn)->src, insn->type);
            string_builder_push_format(sb, ", %", f_int(((Bytecode_Memory*) insn)->size));
        } break;
    }
    
    if (insn->comment) {
        string_builder_pad(sb, from_byte, 25);
        string_builder_push_format(sb, " // %", f_cstring(insn->comment));
    }
}

void
string_builder_dump_bytecode(String_Builder* sb, Bytecode* bc, Bytecode_Function* func) {
    if (!func) return;
    
    if (func->is_intrinsic || func->is_imported) {
        return;
    }
    
    if (func->is_imported) {
        string_builder_push(sb, "import ");
    }
    
    Bytecode_Type* types = (Bytecode_Type*) (func + 1);
    if (func->ret_count == 1) {
        // TODO(Alexander): multiple returns
        string_builder_push(sb, bc_type_names[types[func->arg_count]]);
        string_builder_push(sb, " ");
    } else {
        string_builder_push(sb, "void ");
    }
    string_builder_dump_bytecode_function_name(sb, bc, func);
    string_builder_push(sb, "(");
    for (u32 i = 0; i < func->arg_count; i++) {
        string_builder_push(sb, bc_type_names[types[i]]);
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
    while (curr->kind) {
        string_builder_push(sb, "\n    ");
        string_builder_dump_bytecode_insn(sb, bc, curr);
        curr = iter_bytecode_instructions(func, curr);
    }
    
    if (!func->is_imported) {
        string_builder_push(sb, "\n}");
    }
    
    string_builder_push(sb, "\n");
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