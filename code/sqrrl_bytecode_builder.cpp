
Bc_Instruction*
bc_push_instruction(Bc_Builder* bc, Bc_Opcode opcode) {
    Bc_Basic_Block* basic_block = bc->curr_basic_block;
    assert(basic_block);
    
    Bc_Instruction* prev_insn = bc->curr_instruction;
    if (prev_insn) {
        // Record live lengths of temporary registers
        if (prev_insn->src0.kind == BcOperand_Register) {
            map_put(bc->live_lengths, prev_insn->src0.Register, bc->instruction_count - 1);
        }
        if (prev_insn->src1.kind == BcOperand_Register) {
            map_put(bc->live_lengths, prev_insn->src1.Register, bc->instruction_count - 1);
        }
    }
    
    if (!arena_can_fit(&bc->code_arena, Bc_Instruction)) {
        arena_grow(&bc->code_arena);
        array_push(bc->code.blocks, bc->code_arena.base);
        bc->code.block_size = bc->code_arena.size;
        basic_block = bc_push_basic_block(bc);
    }
    
    Bc_Instruction* insn = arena_push_struct(&bc->code_arena, Bc_Instruction);
    insn->opcode = opcode;
    
    bc->curr_instruction = insn;
    bc->instruction_count++;
    
    basic_block->instruction_count++;
    
    return insn;
}

Bc_Basic_Block*
bc_push_basic_block(Bc_Builder* bc, Bc_Label label) {
    Bc_Decl decl = {};
    decl.kind = BcDecl_Basic_Block;
    
    if (!arena_can_fit_size(&bc->code_arena, 
                            sizeof (Bc_Basic_Block) + sizeof(Bc_Instruction),
                            max(alignof (Bc_Basic_Block), alignof(Bc_Instruction)))) {
        arena_grow(&bc->code_arena);
        array_push(bc->code.blocks, bc->code_arena.base);
        bc->code.block_size = bc->code_arena.size;
    }
    
    // NOTE(Alexander): accumulated offset for preceding bytecode blocks
    assert(array_count(bc->code.blocks) > 0);
    Bc_Basic_Block* block = arena_push_struct(&bc->code_arena, Bc_Basic_Block);
    block->next_byte_offset = -1;
    decl.first_byte_offset = (bc->code.block_size*(array_count(bc->code.blocks) - 1) + 
                              bc->code_arena.curr_used - sizeof(Bc_Basic_Block));
    
    if (bc->curr_basic_block) {
        bc->curr_basic_block->next_byte_offset = decl.first_byte_offset;
    }
    bc->curr_basic_block = block;
    
    if (label.ident != 0) {
        block->label = label;
    } else {
        block->label = create_unique_bc_label(bc);
    }
    
    bc_label(bc, block->label);
    
    map_put(bc->declarations, label, decl);
    return block;
}

Bc_Type
bc_build_type(Bc_Builder* bc, Type* type) {
    Bc_Type result = {};
    
    assert(type->kind != Type_None);
    
    switch (type->kind) {
        case Type_Void: {
            result.kind = BcType_void;
        } break;
        
        case Type_Primitive: {
            Primitive_Type_Kind primitive_kind = type->Primitive.kind;
            
            switch (primitive_kind) {
                case PrimitiveType_int:  result.kind = BcType_s32; break; // Arch dep?
                case PrimitiveType_s8:   result.kind = BcType_s8; break;
                case PrimitiveType_s16:  result.kind = BcType_s16; break;
                case PrimitiveType_s32:  result.kind = BcType_s32; break;
                case PrimitiveType_s64:  result.kind = BcType_s64; break;
                case PrimitiveType_smm:  result.kind = BcType_s64; break; // Arch dep.
                case PrimitiveType_uint: result.kind = BcType_u32; break; // Arch dep?
                case PrimitiveType_u8:   result.kind = BcType_u8; break;
                case PrimitiveType_u16:  result.kind = BcType_u16; break;
                case PrimitiveType_u32:  result.kind = BcType_u32; break;
                case PrimitiveType_u64:  result.kind = BcType_u64; break;
                case PrimitiveType_umm:  result.kind = BcType_u64; break; // Arch dep.
                case PrimitiveType_f32:  result.kind = BcType_f32; break;
                case PrimitiveType_f64:  result.kind = BcType_f64; break;
                case PrimitiveType_char: result.kind = BcType_u8; break;
                case PrimitiveType_bool: result.kind = BcType_s8; break;
                case PrimitiveType_b32:  result.kind = BcType_s32; break;
                default: assert(0 && "invalid primitive type");
            }
        } break;
        
        case Type_Pointer: {
            result = bc_build_type(bc, type->Pointer);
            result.ptr_depth++;
        } break;
        
        default: {
            result.kind = BcType_Aggregate;
            result.aggregate = type;
        } break;
    }
    
    return result;
}

Bc_Type
bc_conform_to_larger_type(Bc_Builder* bc, 
                          Bc_Operand* first, Bc_Type first_type,
                          Bc_Operand* second, Bc_Type second_type) {
    
    Bc_Type smaller_type = first_type;
    Bc_Type larger_type = second_type;
    
    if (first_type.kind == second_type.kind &&
        first_type.ptr_depth == second_type.ptr_depth) {
        return larger_type;
    }
    
    s32 first_bitsize = bc_type_to_bitsize(first_type);
    s32 second_bitsize = bc_type_to_bitsize(second_type);
    
    // If they are the same size then we are done
    if (first_bitsize == second_bitsize) {
        return larger_type;
    }
    
    Bc_Operand* smaller = first;
    Bc_Operand* larger = second;
    if (first_bitsize > second_bitsize) {
        smaller_type = second_type;
        larger_type = first_type;
        larger = first;
        smaller = second;
    }
    
    // NOTE(Alexander): we don't need to cast constant values
    if (is_bc_operand_value(smaller->kind)) {
        return larger_type;
    }
    
    Bc_Opcode opcode = is_bc_type_uint(larger_type.kind) ?
        Bytecode_zero_extend : Bytecode_sign_extend;
    Bc_Instruction* insn = bc_push_instruction(bc, opcode);
    insn->dest = create_unique_bc_register(bc);
    insn->dest_type = larger_type;
    insn->src0 = *smaller;
    insn->src1 = bc_type_op(smaller_type);
    *smaller = insn->dest;
    
    return larger_type;
}

Bc_Operand
bc_build_type_cast(Bc_Builder* bc, 
                   Bc_Operand* src, Bc_Type src_type,
                   Bc_Type dest_type) {
    assert(src_type.kind != BcType_Aggregate);
    assert(dest_type.kind != BcType_Aggregate);
    if (src_type.kind == dest_type.kind) {
        return *src;
    }
    
    Bc_Opcode opcode = Bytecode_noop;
    bool is_src_float = is_bc_type_floating(src_type.kind);
    bool is_dest_float = is_bc_type_floating(dest_type.kind);
    
    if (is_src_float && is_dest_float) {
        if (dest_type.kind == BcType_f64)  {
            opcode = Bytecode_float_extend;
        } else {
            opcode = Bytecode_float_truncate;
        }
    } else if (is_src_float) {
        if (is_bc_type_sint(dest_type.kind)) {
            opcode = Bytecode_float_to_sint;
        } else {
            opcode = Bytecode_float_to_uint;
        }
    } else if (is_dest_float) {
        if (is_bc_type_sint(src_type.kind)) {
            opcode = Bytecode_sint_to_float;
        } else {
            opcode = Bytecode_uint_to_float;
        }
    } else {
        int src_size = bc_type_to_bitsize(src_type);
        int dest_size = bc_type_to_bitsize(dest_type);
        
        if (src_size > dest_size) {
            opcode = Bytecode_truncate;
        } else {
            if (is_bc_type_sint(src_type.kind)) {
                opcode = Bytecode_sign_extend;
            } else {
                opcode = Bytecode_zero_extend;
            }
        }
    }
    
    return bc_binary(bc, opcode, *src, bc_type_op(src_type), dest_type);
}

Bc_Operand
bc_build_expression(Bc_Builder* bc, Ast* node) {
    Bc_Operand result = {};
    
    switch (node->kind) {
        case Ast_Ident: {
            result = map_get(bc->local_variable_mapper, node->Ident);
            if (!result.kind) {
                // TODO(Alexander): handle global variables in a better way
                // using real declarations with real type info!!!
#if 0
                Bc_Register reg = {};
                reg.ident = ast_unwrap_ident(node);
                Value value = map_get(bc->declarations, reg);
                if (value.type != Value_void) {
                    
                    Type* value_type = bc_build_type(bc, node->type);
                    result = bc_load(bc, );
                    
                    Bc_Type type = bc_build_type(bc, node->type);
                    
                    Bc_Operand variable = {};
                    variable.kind = BcOperand_Register;
                    variable.Register = reg;
                    variable.type = type_operand.type;
                    variable.type.ptr_depth++;
                    
                    result = bc_get_unique_register_operand(bc, type_operand.type);
                    
                    bc_load(result, type_
                            bc_push_instruction(bc, Bytecode_load);
                            bc_push_operand(bc, result);
                            bc_push_operand(bc, type_operand);
                            bc_push_operand(bc, variable);
                }
#endif
            }
            
            
            assert(result.kind && "bug: failed to load operand from identifier");
        } break;
        
        case Ast_Value: {
            switch (node->Value.value.type) {
                case Value_boolean:
                case Value_signed_int: {
                    s64 v = node->Value.value.data.signed_int;
                    result = bc_signed_int_op(v);
                } break;
                
                case Value_unsigned_int: {
                    u64 v = node->Value.value.data.unsigned_int;
                    result = bc_unsigned_int_op(v);
                } break;
                
                case Value_floating: {
                    f64 v = node->Value.value.data.floating;
                    result = bc_float_op(v);
                } break;
                
                case Value_string: {
                    Memory_String v = bc_save_string(bc, node->Value.value.data.str);
                    result = bc_string_op(v);
                } break;
                
                default: {
                    assert(0 && "unexpected value type");
                } break;
            }
        } break;
        
        case Ast_Unary_Expr: {
            switch (node->Unary_Expr.op) {
                case UnaryOp_Negate: {
                    Bc_Type first_type = bc_build_type(bc, node->Unary_Expr.first->type);
                    Bc_Operand first = bc_build_expression(bc, node->Unary_Expr.first);
                    result = bc_unary(bc, Bytecode_neg, first, first_type);
                } break;
                
                case UnaryOp_Logical_Not: {
                    Bc_Type first_type = bc_build_type(bc, node->Unary_Expr.first->type);
                    Bc_Operand first = bc_build_expression(bc, node->Unary_Expr.first);
                    result = bc_unary(bc, Bytecode_not, first, first_type);
                } break;
                
                case UnaryOp_Bitwise_Not: {
                    Bc_Type first_type = bc_build_type(bc, node->Unary_Expr.first->type);
                    Bc_Operand first = bc_build_expression(bc, node->Unary_Expr.first);
                    result = bc_binary(bc, Bytecode_xor, first, bc_signed_int_op(-1), first_type);
                } break;
                
                case UnaryOp_Address_Of: {
                    string_id ident = ast_unwrap_ident(node->Unary_Expr.first);
                    result = map_get(bc->local_variable_mapper, ident);
                } break;
                
                case UnaryOp_Dereference: {
                    string_id ident = ast_unwrap_ident(node->Unary_Expr.first);
                    Bc_Type first_type = bc_build_type(bc, node->Unary_Expr.first->type);
                    Bc_Operand first = map_get(bc->local_variable_mapper, ident);
                    
                    result.kind = BcOperand_Memory;
                    //value.kind = BcOperand_Stack;
                    //value_type.ptr_depth--;
                    //result = value;
                    //result = bc_load(bc, value, value_type);
                } break;
                
                default: {
                    assert(0 && "bug: not a valid unary op");
                } break;
            }
        } break;
        
        case Ast_Binary_Expr: {
            Binary_Op binary_op = node->Binary_Expr.op;
            Bc_Type first_var_type = bc_build_type(bc, node->Binary_Expr.first->type);
            Bc_Operand first_var = bc_build_expression(bc, node->Binary_Expr.first);
            
            Bc_Operand first = first_var;
            Bc_Type first_type = first_var_type;
            
            Bc_Operand second;
            Bc_Type second_type;
            
            if (binary_op == BinaryOp_Logical_And) {
                // Store the result in new local variable
                Bc_Operand dest = bc_stack_alloc(bc, first_var_type);
                bc_copy(bc, dest, first, bc_type_s1);
                
                Bc_Operand cond = bc_build_type_cast(bc, &first, first_var_type, bc_type_s1);
                Bc_Label cont_label = create_unique_bc_label(bc);
                Bc_Label exit_label = create_unique_bc_label(bc);
                bc_branch(bc, cond, cont_label, exit_label);
                
                // Continue to evaluate second expression
                bc_push_basic_block(bc, cont_label);
                second = bc_build_expression(bc, node->Binary_Expr.second);
                bc_copy(bc, dest, second, bc_type_s1);
                
                // Exit or skipping second expression
                bc_push_basic_block(bc, exit_label);
                second = dest;
                second_type = bc_type_s1;
                
            } else if (binary_op == BinaryOp_Logical_Or) {
                
                // Store the result in new local variable
                Bc_Operand dest = bc_stack_alloc(bc, first_var_type);
                bc_copy(bc, dest, first, bc_type_s1);
                
                Bc_Operand cond = bc_build_type_cast(bc, &first, first_var_type, bc_type_s1);
                Bc_Label cont_label = create_unique_bc_label(bc);
                Bc_Label exit_label = create_unique_bc_label(bc);
                bc_branch(bc, cond, exit_label, cont_label);
                
                bc_push_basic_block(bc, cont_label);
                second = bc_build_expression(bc, node->Binary_Expr.second);
                bc_copy(bc, dest, second, bc_type_s1);
                
                bc_push_basic_block(bc, exit_label);
                first = dest;
                second = bc_signed_int_op(-1);
                second_type = bc_type_s1;
                binary_op = BinaryOp_Logical_And;
                
            } else {
                second_type = bc_build_type(bc, node->Binary_Expr.second->type);
                second = bc_build_expression(bc, node->Binary_Expr.second);
            }
            
            if (node->Binary_Expr.op == BinaryOp_Assign) {
                result = second;
            } else {
                // TODO(Alexander): conform to largest, maybe this will be done in type stage instead
                Bc_Type type = bc_conform_to_larger_type(bc, 
                                                         &first, first_type, 
                                                         &second, second_type);
                
                Bc_Opcode binary_opcode = Bytecode_noop;
                switch (binary_op) {
#define BINOP(name, op, prec, assoc, is_comparator, bc_mnemonic) \
case BinaryOp_##name: binary_opcode = Bytecode_##bc_mnemonic; break;
                    DEF_BINARY_OPS
#undef BINOP
                }
                
                result = bc_binary(bc, binary_opcode, first, second, type);
            }
            
            if (is_binary_assign(node->Binary_Expr.op)) {
                Ast* assign = node->Binary_Expr.first;
                bc_copy(bc, first_var, result, bc_build_type(bc, assign->type));
            }
        } break;
        
        case Ast_Ternary_Expr: {
            unimplemented;
        } break;
        
        case Ast_Call_Expr: {
            string_id ident = ast_unwrap_ident(node->Call_Expr.ident); 
            //Bc_Operand function = map_get(bc->local_variable_mapper, ident);
            
            Type* function_type = node->Call_Expr.function_type;
            assert(function_type->kind == Type_Function);
            
            Bc_Type return_type = bc_build_type(bc, function_type->Function.return_type);
            Bc_Type proc_type = bc_build_type(bc, function_type);
            
            int arg_count = 0;
            array(Bc_Argument)* arguments = 0;
            for_compound(node->Call_Expr.args, arg) {
                Ast* arg_expr = arg->Argument.assign;
                
                Bc_Argument bc_arg;
                bc_arg.type = bc_build_type(bc, arg_expr->type);
                bc_arg.src = bc_build_expression(bc, arg_expr);
                array_push(arguments, bc_arg);
                
                // HACK(Alexander): for now print_format pushes the format type first then the value 
                if (arg_count > 0 && function_type->Function.intrinsic == &print_format) {
                    Format_Type fmt_type = convert_type_to_format_type(arg_expr->type);
                    
                    if (fmt_type == FormatType_string) {
                        // NOTE(Alexander): memory strings are easier to work with
                        fmt_type = FormatType_memory_string;
                    }
                    
                    Bc_Argument fmt_arg;
                    fmt_arg.type = create_bc_type(BcType_s32);
                    fmt_arg.src = bc_signed_int_op(fmt_type);
                    array_push(arguments, fmt_arg);
                }
                
                arg_count++;
            }
            
            result = bc_call(bc, proc_type, arguments, return_type);
        } break;
        
        case Ast_Field_Expr: {
            unimplemented;
        } break;
        
        case Ast_Cast_Expr: {
            Bc_Type src_type = bc_build_type(bc, node->Cast_Expr.expr->type);
            Bc_Operand src = bc_build_expression(bc, node->Cast_Expr.expr);
            Bc_Type dest_type = bc_build_type(bc, node->type);
            result = bc_build_type_cast(bc, &src, src_type, dest_type);
        } break;
        
        case Ast_Paren_Expr: {
            result = bc_build_expression(bc, node->Paren_Expr.expr);
        } break;
        
        case Ast_Index_Expr: {
            unimplemented;
        } break;
        
        case Ast_Array_Expr: {
            unimplemented;
        } break;
        
        case Ast_Struct_Expr: {
            unimplemented;
        } break;
        
        case Ast_Tuple_Expr: {
            unimplemented;
        } break;
        
        default: assert(0 && "bug: not an expression");
    }
    
    return result;
}

Bc_Operand
bc_build_compare_expression(Bc_Builder* bc, Ast* node) {
    
    Bc_Opcode cmp_code = Bytecode_noop;
    Bc_Operand first = {};
    Bc_Operand second = {};
    
    // NOTE(Alexander): unwrap expressions inside statement
    if (node->kind == Ast_Expr_Stmt) {
        node = node->Expr_Stmt;
    }
    
    if (node->kind == Ast_Binary_Expr) {
        switch (node->Binary_Expr.op) {
            case BinaryOp_Equals: cmp_code = Bytecode_cmpeq; break;
            case BinaryOp_Not_Equals: cmp_code = Bytecode_cmpneq; break;
            case BinaryOp_Less_Equals: cmp_code = Bytecode_cmple; break;
            case BinaryOp_Less_Than: cmp_code = Bytecode_cmplt; break;
            case BinaryOp_Greater_Equals: cmp_code = Bytecode_cmpge; break;
            case BinaryOp_Greater_Than: cmp_code = Bytecode_cmpgt; break;
        }
    }
    
    Bc_Type cmp_type = {};
    if (cmp_code == Bytecode_noop) {
        cmp_code = Bytecode_cmpneq;
        Bc_Type first_type = bc_build_type(bc, node->type);
        first = bc_build_expression(bc, node);
        second = bc_signed_int_op(0);
        cmp_type = first_type;
    } else {
        Bc_Type first_type = bc_build_type(bc, node->Binary_Expr.first->type);
        first = bc_build_expression(bc, node->Binary_Expr.first);
        cmp_type = first_type;
        
        Bc_Type second_type = bc_build_type(bc, node->Binary_Expr.second->type);
        second = bc_build_expression(bc, node->Binary_Expr.second);
    }
    
    return bc_binary(bc, cmp_code, first, second, cmp_type);
}

void
bc_build_statement(Bc_Builder* bc, Ast* node, bool last_statement=false) {
    
    switch (node->kind) {
        case Ast_Assign_Stmt: {
            
            Bc_Type type = bc_build_type(bc, node->type);
            Bc_Operand dest = bc_stack_alloc(bc, type);
            string_id ident = ast_unwrap_ident(node->Assign_Stmt.ident);
            map_put(bc->local_variable_mapper, ident, dest);
            
            Bc_Type source_type = bc_build_type(bc, node->Assign_Stmt.expr->type);
            Bc_Operand source = bc_build_expression(bc, node->Assign_Stmt.expr);
            source = bc_build_type_cast(bc, &source, source_type, type);
            bc_copy(bc, dest, source, type);
        } break;
        
        case Ast_Expr_Stmt: {
            bc_build_expression(bc, node->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            Ast* stmts = node->Block_Stmt.stmts;
            for_compound(stmts, stmt) {
                bc_build_statement(bc, stmt);
            }
        } break;
        
        case Ast_Break_Stmt: {
            unimplemented;
        } break;
        
        case Ast_Continue_Stmt: {
            unimplemented;
        } break;
        
        case Ast_Decl_Stmt: {
            unimplemented;
        } break;
        
        case Ast_If_Stmt: {
            Bc_Label then_label = create_unique_bc_label(bc);
            Bc_Label else_label = create_unique_bc_label(bc);
            
            Bc_Operand cond = bc_build_compare_expression(bc, node->If_Stmt.cond);
            bc_branch(bc, cond, then_label, else_label);
            
            {
                bc_push_basic_block(bc, then_label);
                bc_build_statement(bc, node->If_Stmt.then_block);
            }
            
            {
                bc_push_basic_block(bc, else_label);
                if (is_ast_none(node->If_Stmt.else_block)) {
                    bc_build_statement(bc, node->If_Stmt.then_block);
                    
                    Bc_Label exit_label = create_unique_bc_label(bc);
                    bc_goto(bc, exit_label);
                    bc_push_basic_block(bc, exit_label);
                }
            }
            
        } break;
        
        case Ast_For_Stmt: {
            Bc_Label cont_label = create_unique_bc_label(bc);
            Bc_Label update_label = create_unique_bc_label(bc);
            Bc_Label exit_label = create_unique_bc_label(bc);
            
            // TODO(Alexander): we need to use the For_Stmt.label if specified
            bc_build_statement(bc, node->For_Stmt.init);
            
            {
                bc_push_basic_block(bc, cont_label);
                Bc_Operand cond = bc_build_compare_expression(bc, node->For_Stmt.cond);
                bc_branch(bc, cond, update_label, exit_label);
            }
            
            {
                bc_push_basic_block(bc, update_label);
                bc_build_statement(bc, node->For_Stmt.block);
                bc_build_expression(bc, node->For_Stmt.update);
                bc_goto(bc, cont_label);
            }
            
            bc_push_basic_block(bc, exit_label);
        } break;
        
        case Ast_While_Stmt: {
            // TODO(Alexander): we need to use the While_Stmt.label if specified
            Bc_Label cont_label = create_unique_bc_label(bc);
            Bc_Label update_label = create_unique_bc_label(bc);
            Bc_Label exit_label = create_unique_bc_label(bc);
            
            {
                bc_push_basic_block(bc, cont_label);
                Bc_Operand cond = bc_build_compare_expression(bc, node->While_Stmt.cond);
                bc_branch(bc, cond, update_label, exit_label);
                
                {
                    bc_push_basic_block(bc, update_label);
                    bc_build_statement(bc, node->While_Stmt.block);
                    bc_goto(bc, cont_label);
                }
            }
            
            bc_push_basic_block(bc, exit_label);
        } break;
        
        case Ast_Return_Stmt: {
            Bc_Type source_type = bc_build_type(bc, node->Return_Stmt.expr->type);
            Bc_Operand source = bc_build_expression(bc, node->Return_Stmt.expr);
            if (bc->curr_return_dest.kind != BcOperand_None) {
                bc_copy(bc, bc->curr_return_dest, source, source_type);
            }
            bc_goto(bc, bc->curr_epilogue);
        } break;
        
        default: assert(0 && "bug: not an expression");
    }
}

void
bc_register_declaration(Bc_Builder* bc, string_id ident, Ast* decl, Type* type) {
    switch (type->kind) {
        case Type_Function: {
            assert(decl->kind == Ast_Block_Stmt);
            
            bc->curr_decl = ident;
            bc->curr_prologue = create_unique_bc_label(bc);
            bc->curr_epilogue = create_unique_bc_label(bc);
            bc->curr_return_dest = {};
            bc->curr_basic_block = bc_push_basic_block(bc, bc->curr_prologue);
            
            Bc_Decl result = map_get(bc->declarations, bc->curr_prologue);
            result.kind = BcDecl_Procedure;
            result.Procedure.first_register = bc->next_register;
            
            // TODO(Alexander): we should use type checker function Type* instead
            Bc_Type return_type = bc_build_type(bc, type->Function.return_type);
            if (return_type.kind != BcType_void) {
                bc->curr_return_dest = bc_stack_alloc(bc, return_type);
                result.Procedure.first_return_reg = bc->curr_return_dest.Register;
            }
            result.Procedure.first_arg_reg = bc->next_register;
            
            // Allocate arguments
            array(Bc_Argument)* actual_args = 0;
            Type_Table* formal_args = &type->Function.arguments;
            for_array_v(formal_args->idents, arg_ident, arg_index) {
                
                Type* arg_type = map_get(formal_args->ident_to_type, arg_ident);
                Bc_Argument arg = {};
                arg.type = bc_build_type(bc, arg_type);
                arg.src = create_unique_bc_register(bc);
                array_push(actual_args, arg);
                
                Bc_Operand arg_dest = bc_stack_alloc(bc, arg.type);
                bc_copy(bc, arg_dest, arg.src, arg.type);
                map_put(bc->local_variable_mapper, arg_ident, arg_dest);
                
            }
            Bc_Instruction* label_insn = get_first_bc_instruction(bc->curr_basic_block);
            label_insn->src0.kind = BcOperand_Type;
            label_insn->src0.Type = return_type;
            label_insn->src1.kind = BcOperand_Argument_List;
            label_insn->src1.Argument_List = actual_args;
            
            // Build the actual declaration
            bc_build_statement(bc, decl);
            
            {
                // Epilogue
                bc_push_basic_block(bc, bc->curr_epilogue);
                
                if (bc->curr_return_dest.kind != BcOperand_None) {
                    Bc_Operand return_op = bc->curr_return_dest;
                    bc_ret(bc, return_op, return_type);
                } else {
                    bc_push_instruction(bc, Bytecode_ret);
                }
            }
            
            // Save declaration
            map_put(bc->declarations, bc->curr_prologue, result);
        } break;
    }
}

void
bc_analyze_top_level_declaration(Bc_Builder* bc, Ast* decl, string_id ident) {
    Bc_Instruction result = {};
    
    switch (decl->kind) {
        case Ast_Assign_Stmt: {
            
            Type* type = decl->type;
            Ast* expr = decl->Assign_Stmt.expr;
            // TODO(Alexander): we also want to be able to create values from struct literals etc.
            assert(expr && expr->kind == Ast_Value && "unimplemnted: only constant values supported in global scope");
            Value value = expr->Value.value;
            
            Bc_Decl bc_decl = {};
            bc_decl.kind = BcDecl_Data;
            bc_decl.first_byte_offset = bc->data_arena.curr_used;
            bc_decl.Data.type = bc_build_type(bc, type);
            
            // TODO(Alexander): for data types with arbitrary size we need to push it on the data arena
            //void* data = arena_push_size(&bc->code_arena, type->cached_size, type->cached_align);
            //interp_save_value(type, data, value.data);
            
            Bc_Label label = create_unique_bc_label(bc, ident);
            map_put(bc->declarations, label, bc_decl);
        } break;
    }
}

void
bc_build_from_top_level_declaration(Bc_Builder* bc, Ast* decl) {
    Bc_Instruction result = {};
    
    switch (decl->kind) {
        case Ast_Decl_Stmt: {
            string_id ident = ast_unwrap_ident(decl->Decl_Stmt.ident);
            
            Type* type = decl->Decl_Stmt.type->type;
            Ast* stmt = decl->Decl_Stmt.stmt;
            bc_register_declaration(bc, ident, stmt, type);
        } break;
    }
}

void
bc_build_from_ast(Bc_Builder* bc, Ast_File* ast_file) {
    // NOTE(Alexander): we want to minimize this as far as possible
    // to be able to compile large programs.
    //pln("sizeof(Bc_Instruction) = %", f_umm(sizeof(Bc_Instruction)));
    //pln("sizeof(Bc_Operand) = %\n", f_umm(sizeof(Bc_Operand)));
    
    // Define global declarations
    for_map(ast_file->decls, it) {
        bc_analyze_top_level_declaration(bc, it->value, it->key);
    }
    
    // Build bytecode for each declaration
    for_map(ast_file->decls, decl) {
        bc->curr_basic_block = 0;
        bc->curr_instruction = 0;
        bc_build_from_top_level_declaration(bc, decl->value);
    }
}
