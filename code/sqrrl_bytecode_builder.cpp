
// TODO(Alexander): we might want to use this approach, but keep it simple for now
// This approach essentially only allocates the number of operands that are actually needed
// given the opcode.
#if 0
Bc_Instruction
push_instruction(Bc_Builder* bc, Bc_Opcode opcode) {
    Bc_Instruction result = {};
    
    Bc_Basic_Block* bb = bc->curr_basic_block;
    
    Bc_Opcode* opcode = push_size(&bc->arena, sizeof(Bc_Opcode), alignof(Bc_Opcode));
    
    u32 num_operands = bytecode_num_operands[opcode];
    if (num_operands > 0) {
        u32 size =  + num_operands * sizeof(Bc_Operand);
        Bc_Operands* operands = push_size(&bc->arena, sizeof(Bc_Opcode), alignof(Bc_Opcode));
    }
    
    array_push(bb->instructions, result);
    return result;
}
#endif



void
bc_push_instruction(Bc_Builder* bc, Bc_Opcode opcode) {
    Bc_Basic_Block* bb = bc->curr_basic_block;
    assert(bb);
    
    if (!arena_can_fit_size(&bc->arena, sizeof(Bc_Instruction), alignof(Bc_Instruction))) {
        arena_reallocate(&bc->arena);
        bb = bc_push_basic_block(bc);
    }
    
    Bc_Instruction* insn = arena_push_struct(&bc->arena, Bc_Instruction);
    insn->opcode = opcode;
    
    bc->curr_instruction = insn;
    bc->instruction_count++;
    
    bb->count++;
    if (!bb->first) {
        bb->first = insn;
    }
    
#if 0
    else {
        if (bb->first + (bb->count - 1) != insn) {
            // NOTE(Alexander): special case, instruction is not stored contigously
            
            
            
            pln("\ncurr_block: %\n", f_string(vars_load_string(bb->label.ident)));
            assert(0);
        }
    }
#endif
}

void
bc_push_operand(Bc_Builder* bc, Bc_Operand operand) {
    Bc_Instruction* insn = bc->curr_instruction;
    if (!insn) {
        assert(0 && "no current instruction, forgot to bc_push_instruction() first?");
        return;
    }
    
    if (operand.kind == BcOperand_Register) {
        map_put(bc->live_lengths, operand.Register, bc->instruction_count - 1);
    }
    
    // HACK(Alexander): this is not really pushing at the moment
    // we are just inserting the next slot available
    if (!insn->dest.kind) {
        insn->dest = operand;
    } else if (!insn->src0.kind) {
        insn->src0 = operand;
    } else if (!insn->src1.kind) {
        insn->src1 = operand;
    } else {
        assert(0 && "reached maximum allowed operands per instruction");
    }
}

Bc_Basic_Block*
bc_push_basic_block(Bc_Builder* bc, Bc_Register label) {
    
    Bc_Basic_Block* block = arena_push_struct(&bc->arena, Bc_Basic_Block);
    if (bc->curr_basic_block) {
        bc->curr_basic_block->next = block;
    }
    bc->curr_basic_block = block;
    
    if (label.ident != 0) {
        block->label = label;
    } else {
        block->label = bc_get_unique_register(bc);
    }
    
    Bc_Operand block_operand = {};
    block_operand.kind = BcOperand_Basic_Block;
    block_operand.Basic_Block = block;
    bc_push_instruction(bc, Bytecode_label);
    bc_push_operand(bc, block_operand);
    
    block->first = bc->curr_instruction;
    block->count = 1;
    
    // TODO(Alexander): maybe we need this later, but right now this makes x64 backend compile the same function multiple times.
    //Value decl = {};
    //decl.type = Value_basic_block;
    //decl.data.basic_block = block;
    //map_put(bc->declarations, block->label, decl);
    
    return block;
}

// TODO(Alexander): this is a temporary solution to get the types from the frontend
// later on the type checker will do this for us
Bc_Type
bc_build_type(Bc_Builder* bc, Ast* type) {
    Bc_Type result = {};
    
    switch (type->kind) {
        case Ast_Named_Type: {
            string_id ident = ast_unwrap_ident(type->Named_Type);
            
            if (ident == Kw_string) {
                // TODO(Alexander): string type
            } else if (is_builtin_type_keyword(ident)) {
                // HACK(Alexander): this is a little bit hack of trying to convert between enums
                
                switch (ident) {
                    case Kw_int:  result.kind = BcType_s32; break; // Arch dep?
                    case Kw_s8:   result.kind = BcType_s8; break;
                    case Kw_s16:  result.kind = BcType_s16; break;
                    case Kw_s32:  result.kind = BcType_s32; break;
                    case Kw_s64:  result.kind = BcType_s64; break;
                    case Kw_smm:  result.kind = BcType_s64; break; // Arch dep.
                    case Kw_uint: result.kind = BcType_u32; break; // Arch dep?
                    case Kw_u8:   result.kind = BcType_u8; break;
                    case Kw_u16:  result.kind = BcType_u16; break;
                    case Kw_u32:  result.kind = BcType_u32; break;
                    case Kw_u64:  result.kind = BcType_u64; break;
                    case Kw_umm:  result.kind = BcType_u64; break; // Arch dep.
                    case Kw_f32:  result.kind = BcType_f32; break;
                    case Kw_f64:  result.kind = BcType_f64; break;
                    case Kw_char: result.kind = BcType_u8; break;
                    case Kw_bool: result.kind = BcType_s8; break;
                    case Kw_b32:  result.kind = BcType_s32; break;
                    case Kw_void: result.kind = BcType_void; break;
                    default: assert(0 && "invalid primitive type");
                }
            }
        } break;
        
        case Ast_Pointer_Type: {
            result = bc_build_type(bc, type->Pointer_Type);
            result.ptr_depth++;
        } break;
    }
    
    return result;
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
                case PrimitiveType_void: result.kind = BcType_void; break;
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

void
bc_conform_highest_type(Bc_Builder* bc, Bc_Operand* first, Bc_Operand* second) {
    if (first->type.kind != second->type.kind) {
        s32 first_bitsize = bc_type_to_bitsize(first->type.kind);
        s32 second_bitsize = bc_type_to_bitsize(second->type.kind);
        
        // If they are the same size then we are done
        if (first_bitsize == second_bitsize) {
            return;
        }
        
        Bc_Operand* smaller = first;
        Bc_Operand* larger = second;
        if (first_bitsize > second_bitsize) {
            larger = first;
            smaller = second;
        }
        
        // Values don't need to be casted
        if (smaller->kind == BcOperand_Value) {
            smaller->type = larger->type;
            return;
        }
        
        bc_push_instruction(bc, is_bc_type_uint(larger->type.kind) ?
                            Bytecode_zero_extend : Bytecode_sign_extend);
        
        Bc_Operand new_operand = bc_get_unique_register_operand(bc, larger->type);
        bc_push_operand(bc, new_operand);
        bc_push_operand(bc, *smaller);
        
        Bc_Operand type_operand = {};
        type_operand.kind = BcOperand_Type;
        type_operand.type = larger->type;
        bc_push_operand(bc, type_operand);
        
        *smaller = new_operand;
    }
}

Bc_Operand
bc_build_type_cast(Bc_Builder* bc, Bc_Operand* expr, Bc_Type dest_type) {
    Bc_Operand result = {};
    
    Bc_Type src_type = expr->type;
    if (expr->kind == BcOperand_Value) {
        Value value;
        value.data = expr->Value;
        value.type = bc_type_to_value_type(src_type.kind);
        
        Primitive_Type_Kind dest_primitive_type = bc_type_to_primitive_type_kind(dest_type.kind);
        result.Value = value_cast(value, dest_primitive_type).data;
        result.kind = BcOperand_Value;
        result.type = dest_type;
        
    } else {
        if (src_type.kind == dest_type.kind) {
            result = *expr;
        } else {
            Bc_Opcode opcode = Bytecode_noop;
            bool is_src_fp = is_bc_type_floating(src_type.kind);
            bool is_dest_fp = is_bc_type_floating(dest_type.kind);
            
            if (is_src_fp && is_dest_fp) {
                if (dest_type.kind == BcType_f64)  {
                    opcode = Bytecode_fp_extend;
                } else {
                    opcode = Bytecode_fp_truncate;
                }
            } else if (is_src_fp) {
                if (is_bc_type_sint(dest_type.kind)) {
                    opcode = Bytecode_cast_fp_to_sint;
                } else {
                    opcode = Bytecode_cast_fp_to_uint;
                }
            } else if (is_dest_fp) {
                if (is_bc_type_sint(src_type.kind)) {
                    opcode = Bytecode_cast_sint_to_fp;
                } else {
                    opcode = Bytecode_cast_uint_to_fp;
                }
            } else {
                int src_size = bc_type_to_bitsize(src_type.kind);
                int dest_size = bc_type_to_bitsize(dest_type.kind);
                
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
            
            Bc_Operand dest_type_operand = {};
            dest_type_operand.kind = BcOperand_Type;
            dest_type_operand.type = dest_type;
            
            Bc_Operand dest = bc_get_unique_register_operand(bc, dest_type);
            bc_push_instruction(bc, opcode);
            bc_push_operand(bc, dest);
            bc_push_operand(bc, *expr);
            bc_push_operand(bc, dest_type_operand);
            
            result = dest;
        }
    }
    
    
    return result;
}

inline Bc_Operand
bc_build_stack_alloc(Bc_Builder* bc, Bc_Type value_type) {
    Bc_Operand dest = bc_get_unique_register_operand(bc, value_type);
    
    Bc_Operand alloc_type_op = dest;
    alloc_type_op.kind = BcOperand_Type;
    alloc_type_op.type = value_type;
    dest.type.ptr_depth++;
    
    bc_push_instruction(bc, Bytecode_stack_alloc);
    bc_push_operand(bc, dest);
    bc_push_operand(bc, alloc_type_op);
    // TODO(Alexander): we should maybe also push the allignment 
    
    return dest;
}

inline Bc_Operand
bc_build_load_value(Bc_Builder* bc, Bc_Operand variable) {
    Bc_Operand result = variable;
    
    if (variable.type.ptr_depth > 0) {
        Bc_Type value_type = variable.type;
        value_type.ptr_depth--;
        result = bc_get_unique_register_operand(bc, value_type);
        Bc_Operand type_operand = {};
        type_operand.kind = BcOperand_Type;
        type_operand.type = value_type;
        
        bc_push_instruction(bc, Bytecode_load);
        bc_push_operand(bc, result);
        bc_push_operand(bc, type_operand);
        bc_push_operand(bc, variable);
        
    }
    
    return result;
}

Bc_Operand
bc_build_expression(Bc_Builder* bc, Ast* node) {
    Bc_Operand result = {};
    
    switch (node->kind) {
        case Ast_Ident: {
            result = map_get(bc->ident_to_operand, node->Ident);
            if (!result.kind) {
                Bc_Register reg = {};
                reg.ident = ast_unwrap_ident(node);
                Value value = map_get(bc->declarations, reg);
                if (value.type != Value_void) {
                    Bc_Operand type_operand = {};
                    type_operand.kind = BcOperand_Type;
                    type_operand.type = bc_build_type(bc, node->type);
                    
                    Bc_Operand variable = {};
                    variable.kind = BcOperand_Register;
                    variable.Register = reg;
                    variable.type = type_operand.type;
                    variable.type.ptr_depth++;
                    
                    result = bc_get_unique_register_operand(bc, type_operand.type);
                    
                    bc_push_instruction(bc, Bytecode_load);
                    bc_push_operand(bc, result);
                    bc_push_operand(bc, type_operand);
                    bc_push_operand(bc, variable);
                }
            }
            
            
            assert(result.kind && "bug: failed to load operand from identifier");
        } break;
        
        case Ast_Value: {
            result.kind = BcOperand_Value;
            result.type = bc_build_type(bc, node->type);
            result.Value = node->Value.value.data;
        } break;
        
        case Ast_Unary_Expr: {
            switch (node->Unary_Expr.op) {
                
                case UnaryOp_Negate: {
                    Bc_Operand first = bc_build_expression(bc, node->Unary_Expr.first);
                    if (first.kind == BcOperand_Value) {
                        result = first;
                        if (is_bc_type_sint(first.type.kind) || is_bc_type_uint(first.type.kind)) {
                            result.Value.signed_int = -first.Value.signed_int;
                        } else if (is_bc_type_floating(first.type.kind)) {
                            result.Value.floating = -first.Value.floating;
                        } else {
                            assert(0 && "invalid literal type for neg (-) unary");
                        }
                    } else {
                        first = bc_build_load_value(bc, first);
                        bc_push_instruction(bc, Bytecode_neg);
                        result = bc_get_unique_register_operand(bc, first.type);
                        bc_push_operand(bc, result);
                        bc_push_operand(bc, first);
                    }
                } break;
                
                case UnaryOp_Logical_Not: {
                    Bc_Operand first = bc_build_expression(bc, node->Unary_Expr.first);
                    if (first.kind == BcOperand_Value) {
                        result = first;
                        if (is_bc_type_sint(first.type.kind) || is_bc_type_uint(first.type.kind)) {
                            result.Value.signed_int = !first.Value.signed_int;
                        } else {
                            assert(0 && "invalid literal type for neg (!) unary");
                        }
                    } else {
                        first = bc_build_load_value(bc, first);
                        bc_push_instruction(bc, Bytecode_not);
                        result = bc_get_unique_register_operand(bc, first.type);
                        bc_push_operand(bc, result);
                        bc_push_operand(bc, first);
                    }
                } break;
                
                case UnaryOp_Bitwise_Not: {
                    Bc_Operand first = bc_build_expression(bc, node->Unary_Expr.first);
                    if (first.kind == BcOperand_Value) {
                        result = first;
                        if (is_bc_type_sint(first.type.kind) || is_bc_type_uint(first.type.kind)) {
                            result.Value.signed_int = ~first.Value.signed_int;
                        } else {
                            assert(0 && "invalid literal type for bitwise not (~) unary");
                        }
                    } else {
                        Bc_Operand second = first;
                        second.kind = BcOperand_Value;
                        second.Value.signed_int = -1;
                        
                        first = bc_build_load_value(bc, first);
                        bc_push_instruction(bc, Bytecode_xor);
                        result = bc_get_unique_register_operand(bc, first.type);
                        bc_push_operand(bc, result);
                        bc_push_operand(bc, first);
                        bc_push_operand(bc, second);
                    } 
                } break;
                
                case UnaryOp_Address_Of: {
                    string_id ident = ast_unwrap_ident(node->Unary_Expr.first);
                    result = map_get(bc->ident_to_operand, ident);
                } break;
                
                case UnaryOp_Dereference: {
                    string_id ident = ast_unwrap_ident(node->Unary_Expr.first);
                    Bc_Operand first = map_get(bc->ident_to_operand, ident);
                    Bc_Type deref_type = first.type;
                    assert(deref_type.ptr_depth > 0);
                    deref_type.ptr_depth--;
                    
                    Bc_Operand deref_type_op = {};
                    deref_type_op.kind = BcOperand_Type;
                    deref_type_op.type = deref_type;
                    
                    bc_push_instruction(bc, Bytecode_load);
                    result = bc_get_unique_register_operand(bc, deref_type);
                    bc_push_operand(bc, result);
                    bc_push_operand(bc, deref_type_op);
                    bc_push_operand(bc, first);
                    //
                    //deref_type.ptr_depth--;
                    //deref_type_op.type = deref_type;
                    //
                    //bc_push_instruction(bc, Bytecode_load);
                    //first = result;
                    //result = bc_get_unique_register_operand(bc, deref_type);
                    //bc_push_operand(bc, result);
                    //bc_push_operand(bc, deref_type_op);
                    //bc_push_operand(bc, first);
                } break;
                
                default: {
                    assert(0 && "bug: not a valid unary op");
                } break;
            }
        } break;
        
        case Ast_Binary_Expr: {
            
            Binary_Op binary_op = node->Binary_Expr.op;
            Bc_Operand first_var = bc_build_expression(bc, node->Binary_Expr.first);
            Bc_Operand first = first_var;
            Bc_Operand second;
            
            if (node->Binary_Expr.op != BinaryOp_Assign) {
                // NOTE(Alexander): we don't need to load value if it's an assignment
                first = bc_build_load_value(bc, first_var);
            }
            
            if (node->Binary_Expr.op == BinaryOp_Logical_And && first.kind != BcOperand_Value) {
                // Store the result in new local variable
                Bc_Operand dest = bc_build_stack_alloc(bc, first.type);
                bc_push_instruction(bc, Bytecode_store);
                bc_push_operand(bc, dest);
                bc_push_operand(bc, first);
                
                Bc_Type type = {};
                type.kind = BcType_s1;
                Bc_Operand cond = bc_build_type_cast(bc, &first, bc_type_s1);
                Bc_Instruction* branch = bc_push_branch(bc, &cond);
                
                branch->true_block.Basic_Block = bc_push_basic_block(bc);
                
                second = bc_build_expression(bc, node->Binary_Expr.second);
                bc_push_instruction(bc, Bytecode_store);
                bc_push_operand(bc, dest);
                bc_push_operand(bc, second);
                
                branch->false_block.Basic_Block = bc_push_basic_block(bc);
                
                
                // Load the result
                Bc_Operand temp_register = bc_get_unique_register_operand(bc, bc_type_s1);
                Bc_Operand type_op = {};
                type_op.kind = BcOperand_Type;
                type_op.type = bc_type_s1;
                bc_push_instruction(bc, Bytecode_load);
                bc_push_operand(bc, temp_register);
                bc_push_operand(bc, type_op);
                bc_push_operand(bc, dest);
                second = temp_register;
                
            } else if (node->Binary_Expr.op == BinaryOp_Logical_Or && first.kind != BcOperand_Value) {
                
                // Store the result in new local variable
                Bc_Operand dest = bc_build_stack_alloc(bc, first.type);
                bc_push_instruction(bc, Bytecode_store);
                bc_push_operand(bc, dest);
                bc_push_operand(bc, first);
                
                // Branch
                Bc_Operand cond = bc_build_type_cast(bc, &first, bc_type_s1);
                Bc_Instruction* branch = bc_push_branch(bc, &cond);
                
                branch->false_block.Basic_Block = bc_push_basic_block(bc);
                
                second = bc_build_expression(bc, node->Binary_Expr.second);
                bc_push_instruction(bc, Bytecode_store);
                bc_push_operand(bc, dest);
                bc_push_operand(bc, second);
                
                branch->true_block.Basic_Block = bc_push_basic_block(bc);
                
                // Load the result
                Bc_Operand temp_register = bc_get_unique_register_operand(bc, bc_type_s1);
                Bc_Operand type_op = {};
                type_op.kind = BcOperand_Type;
                type_op.type = bc_type_s1;
                bc_push_instruction(bc, Bytecode_load);
                bc_push_operand(bc, temp_register);
                bc_push_operand(bc, type_op);
                bc_push_operand(bc, dest);
                
                // Perform and on the result
                // TODO(Alexander): is this necessary?
                first = temp_register;
                second.kind = BcOperand_Value;
                second.Value.signed_int = 1;
                second.type = bc_type_s1;
                result = temp_register;
                binary_op = BinaryOp_Logical_And;
                
            } else {
                second = bc_build_expression(bc, node->Binary_Expr.second);
                second = bc_build_load_value(bc, second);
            }
            
            if (node->Binary_Expr.op == BinaryOp_Assign) {
                result = second;
            } else {
                // TODO(Alexander): conform to largest, maybe this will be done in type stage instead
                bc_conform_highest_type(bc, &first, &second);
                
                Bc_Opcode binary_opcode = Bytecode_noop;
                switch (binary_op) {
#define BINOP(name, op, prec, assoc, is_comparator, bc_mnemonic) \
case BinaryOp_##name: binary_opcode = Bytecode_##bc_mnemonic; break;
                    DEF_BINARY_OPS
#undef BINOP
                }
                
                // Figure out the type of the result
                Bc_Type result_type;
                if (binary_is_comparator_table[node->Binary_Expr.op]) {
                    result_type = { BcType_s1, 0 };
                } else {
                    result_type = first.type;
                }
                
                result = bc_get_unique_register_operand(bc, result_type);
                bc_push_instruction(bc, binary_opcode);
                bc_push_operand(bc, result);
                bc_push_operand(bc, first);
                bc_push_operand(bc, second);
            }
            
            if (is_binary_assign(node->Binary_Expr.op)) {
                Ast* assign = node->Binary_Expr.first;
                bc_push_instruction(bc, Bytecode_store);
                bc_push_operand(bc, first_var);
                bc_push_operand(bc, result);
            }
        } break;
        
        case Ast_Ternary_Expr: {
            unimplemented;
        } break;
        
        case Ast_Call_Expr: {
            string_id ident = ast_unwrap_ident(node->Call_Expr.ident); 
            //Bc_Operand function = map_get(bc->ident_to_operand, ident);
            
            Type* function_type = node->Call_Expr.function_type;
            assert(function_type->kind == Type_Function);
            
            Bc_Type return_type = bc_build_type(bc, function_type->Function.return_type);
            Bc_Operand return_operand = {};
            if (return_type.kind) {
                return_operand = bc_get_unique_register_operand(bc, return_type);
            } else {
                return_operand.kind = BcOperand_Void;
            }
            
            Bc_Operand target_label = {};
            target_label.kind = BcOperand_Register;
            target_label.Register = { ident, 0 };
            target_label.type = bc_build_type(bc, function_type);
            
            Bc_Operand function_args = {};
            
            function_args.kind = BcOperand_Argument_List;
            int arg_count = 0;
            for_compound(node->Call_Expr.args, arg) {
                Ast* arg_expr = arg->Argument.assign;
                Bc_Operand bc_arg = bc_build_expression(bc, arg_expr);
                bc_arg = bc_build_load_value(bc, bc_arg);
                
                // HACK(Alexander): for now print_format pushes the format type first then the value 
                if (arg_count > 0 && function_type->Function.intrinsic == &print_format) {
                    Format_Type fmt_type = convert_type_to_format_type(arg_expr->type);
                    
                    if (fmt_type == FormatType_string) {
                        // NOTE(Alexander): memory strings are easier to work with
                        fmt_type = FormatType_memory_string;
                    }
                    
                    Bc_Operand bc_arg_type = {};
                    bc_arg_type.kind = BcOperand_Value;
                    bc_arg_type.type.kind = BcType_s32;
                    bc_arg_type.Value.signed_int = fmt_type;
                    array_push(function_args.Argument_List, bc_arg_type);
                }
                
                array_push(function_args.Argument_List, bc_arg);
                arg_count++;
            }
            
            //result = bc_push_instruction(bc, Bytecode_call, target_label, function_args);
            
            bc_push_instruction(bc, Bytecode_call);
            bc_push_operand(bc, return_operand);
            bc_push_operand(bc, target_label);
            bc_push_operand(bc, function_args);
            result = return_operand;
        } break;
        
        case Ast_Field_Expr: {
            unimplemented;
        } break;
        
        case Ast_Cast_Expr: {
            Bc_Operand expr = bc_build_expression(bc, node->Cast_Expr.expr);
            expr = bc_build_load_value(bc, expr);
            Bc_Type dest_type = bc_build_type(bc, node->Cast_Expr.type);
            result = bc_build_type_cast(bc, &expr, dest_type);
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
    
    Bc_Operand dest;
    Bc_Type dest_type = {};
    dest_type.kind = BcType_s1;
    
    if (cmp_code == Bytecode_noop) {
        cmp_code = Bytecode_cmpneq;
        first = bc_build_expression(bc, node);
        first = bc_build_load_value(bc, first);
        second.kind = BcOperand_Value;
        second.Value.signed_int = 0;
        second.type = first.type;
    } else {
        first = bc_build_expression(bc, node->Binary_Expr.first);
        first = bc_build_load_value(bc, first);
        second = bc_build_expression(bc, node->Binary_Expr.second);
        second = bc_build_load_value(bc, second);
    }
    
    dest = bc_get_unique_register_operand(bc, dest_type);
    bc_push_instruction(bc, cmp_code);
    bc_push_operand(bc, dest);
    bc_push_operand(bc, first);
    bc_push_operand(bc, second);
    
    return dest;
}

void
bc_build_statement(Bc_Builder* bc, Ast* node, bool last_statement=false) {
    
    switch (node->kind) {
        case Ast_Assign_Stmt: {
            Bc_Type type = bc_build_type(bc, node->type);
            Bc_Operand dest = bc_build_stack_alloc(bc, type);
            string_id ident = ast_unwrap_ident(node->Assign_Stmt.ident);
            map_put(bc->ident_to_operand, ident, dest);
            
            Bc_Operand source = bc_build_expression(bc, node->Assign_Stmt.expr);
            source = bc_build_type_cast(bc, &source, type);
            
            bc_push_instruction(bc, Bytecode_store);
            bc_push_operand(bc, dest);
            bc_push_operand(bc, source);
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
            Bc_Operand cond = bc_build_compare_expression(bc, node->If_Stmt.cond);
            Bc_Instruction* branch = bc_push_branch(bc, &cond);
            
            branch->src0.Basic_Block = bc_push_basic_block(bc);
            
            bc_build_statement(bc, node->If_Stmt.then_block);
            
            Bc_Instruction* exit = bc_push_branch(bc, 0);
            
            if (is_ast_none(node->If_Stmt.else_block)) {
                Bc_Basic_Block* exit_block = bc_push_basic_block(bc);
                branch->src1.Basic_Block = exit_block;
                exit->dest.Basic_Block = exit_block;
                
            } else {
                
                branch->src1.Basic_Block = bc_push_basic_block(bc);
                bc_build_statement(bc, node->If_Stmt.else_block);
                
                Bc_Instruction* exit2 = bc_push_branch(bc, 0);
                Bc_Basic_Block* exit_block = bc_push_basic_block(bc);
                exit->dest.Basic_Block = exit_block;
                exit2->dest.Basic_Block = exit_block;
            }
            
        } break;
        
        case Ast_For_Stmt: {
            // TODO(Alexander): we need to use the For_Stmt.label if specified
            bc_build_statement(bc, node->For_Stmt.init);
            
            // Condition
            Bc_Basic_Block* entry_block = bc_push_basic_block(bc);
            Bc_Operand cond = bc_build_compare_expression(bc, node->For_Stmt.cond);
            Bc_Instruction* branch = bc_push_branch(bc, &cond);
            branch->true_block.Basic_Block = bc_push_basic_block(bc);
            
            // Update and block
            bc_build_statement(bc, node->For_Stmt.block);
            bc_build_expression(bc, node->For_Stmt.update);
            
            // Next iteration
            Bc_Instruction* next_it = bc_push_branch(bc, 0);
            next_it->dest.Basic_Block = entry_block;
            
            branch->false_block.Basic_Block = bc_push_basic_block(bc);
        } break;
        
        case Ast_While_Stmt: {
            // TODO(Alexander): we need to use the While_Stmt.label if specified
            // Condition
            Bc_Basic_Block* cond_block = bc_push_basic_block(bc);
            Bc_Operand cond = bc_build_compare_expression(bc, node->While_Stmt.cond);
            Bc_Instruction* branch = bc_push_branch(bc, &cond);
            branch->src0.Basic_Block = bc_push_basic_block(bc);
            
            // Update and block
            bc_build_statement(bc, node->While_Stmt.block);
            
            // Next iteration
            Bc_Instruction* next_it = bc_push_branch(bc, 0);
            next_it->op0.Basic_Block = cond_block;
            
            branch->src1.Basic_Block = bc_push_basic_block(bc);
        } break;
        
        case Ast_Return_Stmt: {
            Bc_Operand source = bc_build_expression(bc, node->Return_Stmt.expr);
            source = bc_build_load_value(bc, source);
            
            bc_push_instruction(bc, Bytecode_store);
            bc_push_operand(bc, bc->curr_return_dest);
            bc_push_operand(bc, source);
            
            //bc_push_instruction(bc, Bytecode_ret);
            // HACK(Alexander): we can't set the operands directly
            //bc->curr_instruction->src0 = source;
            
            // Need to branch to the epilogue of the current function
            Bc_Instruction* branch = bc_push_branch(bc, 0);
            branch->dest.Basic_Block = bc->curr_declaration_epilogue;
            
        } break;
        
        default: assert(0 && "bug: not an expression");
    }
}

void
bc_register_declaration(Bc_Builder* bc, string_id ident, Ast* type, Ast* decl) {
    switch (type->kind) {
        case Ast_Function_Type: {
            assert(decl->kind == Ast_Block_Stmt);
            
            Bc_Register label = { ident, 0 };
            Bc_Basic_Block* block_epilogue = bc_push_basic_block(bc);
            block_epilogue->label = label;
            
            Bc_Basic_Block* block = bc_push_basic_block(bc);
            block->label = label;
            block_epilogue->next = 0; // NOTE(Alexander): don't not create a cycle to the start
            
            bc->curr_declaration = block;
            bc->curr_declaration_epilogue = block_epilogue;
            bc->curr_local_count = 1;
            
            // TODO(Alexander): we should use type checker function Type* instead
            Bc_Type return_type = bc_build_type(bc, type->Function_Type.return_type);
            if (return_type.kind  != BcType_void) {
                bc->curr_return_dest = bc_build_stack_alloc(bc, return_type);
            }
            
            // Allocate arguments
            Bc_Instruction* label_insn = block->first;
            label_insn->op1.kind = BcOperand_Argument_List;
            for_compound(type->Function_Type.arguments, it) {
                string_id arg_ident = ast_unwrap_ident(it->Argument.ident);
                Bc_Type arg_type = bc_build_type(bc, it->Argument.type);
                
                Bc_Operand arg = {};//bc_get_unique_register_operand(bc, arg_type);
                arg.kind = BcOperand_Register;
                arg.type = arg_type;
                arg.Register.ident = arg_ident;
                array_push(label_insn->op1.Argument_List, arg);
            }
            
            // Allocate and store arguments
            array(Bc_Operand*) args = label_insn->op1.Argument_List;
            for_array(args, arg, _) {
                Bc_Operand arg_dest = bc_build_stack_alloc(bc, arg->type);
                
                bc_push_instruction(bc, Bytecode_store);
                bc_push_operand(bc, arg_dest);
                bc_push_operand(bc, *arg);
                
                map_put(bc->ident_to_operand, arg->Register.ident, arg_dest);
                //array_push(block->args, arg_dest.Register.index);
            }
            
            // Build the actual declaration
            bc_build_statement(bc, decl);
            
            {
                // Epilogue
                block_epilogue->label.index = bc->curr_local_count++;
                block_epilogue->count = 0;
                if (bc->curr_basic_block) {
                    bc->curr_basic_block->next = block_epilogue;
                } else {
                    block->next = block_epilogue;
                }
                bc->curr_basic_block = block_epilogue;
                
                // We have to repush the block because it is currently at the at top of memory block
                Bc_Operand block_operand = {};
                block_operand.kind = BcOperand_Basic_Block;
                block_operand.Basic_Block = block;
                block_epilogue->first = 0;
                bc_push_instruction(bc, Bytecode_label);
                bc_push_operand(bc, block_operand);
                
                if (return_type.kind != BcType_void) {
                    Bc_Operand return_value_op = bc_get_unique_register_operand(bc, return_type);
                    Bc_Operand return_type_op = {};
                    return_type_op.kind = BcOperand_Type;
                    return_type_op.type = return_type;
                    bc_push_instruction(bc, Bytecode_load);
                    bc_push_operand(bc, return_value_op);
                    bc_push_operand(bc, return_type_op);
                    bc_push_operand(bc, bc->curr_return_dest);
                    
                    bc_push_instruction(bc, Bytecode_ret);
                    bc_push_operand(bc, return_value_op);
                } else {
                    bc_push_instruction(bc, Bytecode_ret);
                }
            }
            
            // Save declaration
            Value value;
            value.type = Value_basic_block;
            value.data.basic_block = block;
            map_put(bc->declarations, label, value);
        } break;
    }
}

void
bc_analyze_top_level_declaration(Bc_Builder* bc, Ast* decl, string_id ident) {
    Bc_Instruction result = {};
    
    switch (decl->kind) {
        case Ast_Decl_Stmt: {
            Ast* type = decl->Decl_Stmt.type;
            
            switch (type->kind) {
                case Ast_Function_Type: {
                    // TODO(Alexander): for now we store only the return type here first
                    // this should be done at type checking/ inference stage
                    Ast* return_type = type->Function_Type.return_type;
                    Bc_Operand return_type_op = {};
                    return_type_op.kind = BcOperand_Type;
                    return_type_op.type = bc_build_type(bc, return_type);
                    map_put(bc->ident_to_operand, ident, return_type_op);
                } break;
            }
        } break;
        
        case Ast_Assign_Stmt: {
            Bc_Register reg = {};
            reg.ident = ident;
            
            bc_push_instruction(bc, Bytecode_memory_alloc);
            
            Bc_Operand dest_op = {};
            dest_op.kind = BcOperand_Register;
            dest_op.Register = reg;
            bc_push_operand(bc, dest_op);
            
            Ast* expr = decl->Assign_Stmt.expr;
            Value value = {};
            if (expr && expr->kind == Ast_Value) {
                value = expr->Value.value;
            } else {
                value.type = Value_signed_int;
                unimplemented;
                // TODO(Alexander): we need to implement initialization code for this
                // EDIT: although we should probably just pre compute at compile time in prev. stage
            }
            
            Bc_Operand type_op = {};
            type_op.kind = BcOperand_Type;
            type_op.type = bc_build_type(bc, decl->type);
            bc_push_operand(bc, type_op);
            
            Bc_Operand value_op = {};
            value_op.kind = BcOperand_Value;
            value_op.Value = value.data;
            bc_push_operand(bc, value_op);
            
            // TODO(Alexander): we should do this if we have const assignment
            // e.g. const int x = 10;
            // we currently don't store const info in the types yet.
            //map_put(bc->ident_to_operand, ident, value_op);
            
            map_put(bc->declarations, reg, value);
        } break;
    }
}

void
bc_build_from_top_level_declaration(Bc_Builder* bc, Ast* decl) {
    Bc_Instruction result = {};
    
    switch (decl->kind) {
        case Ast_Decl_Stmt: {
            string_id ident = ast_unwrap_ident(decl->Decl_Stmt.ident);
            
            Ast* type = decl->Decl_Stmt.type;
            Ast* stmt = decl->Decl_Stmt.stmt;
            bc_register_declaration(bc, ident, type, stmt);
        } break;
    }
}

void
bc_build_from_ast(Bc_Builder* bc, Ast_File* ast_file) {
    // NOTE(Alexander): we want to minimize this as far as possible
    // to be able to compile large programs.
    //pln("sizeof(Bc_Instruction) = %", f_umm(sizeof(Bc_Instruction)));
    //pln("sizeof(Bc_Operand) = %\n", f_umm(sizeof(Bc_Operand)));
    
    
    {
        // Define global declarations
        Bc_Register reg = {};
        reg.ident = Kw_global;
        Bc_Basic_Block* global_block = bc_push_basic_block(bc, reg);
        
        Value decl = {};
        decl.type = Value_basic_block;
        decl.data.basic_block = global_block;
        map_put(bc->declarations, global_block->label, decl);
    }
    
    
    // TODO(Alexander): this will likely get replacecd with the type checker later on
    // Analyse the ast first
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
