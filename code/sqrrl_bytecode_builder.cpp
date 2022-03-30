

struct Bc_Builder {
    Memory_Arena arena;
    
    Bc_Basic_Block entry_basic_block;
    
    // Current building block
    Bc_Basic_Block* curr_declaration;
    Bc_Instruction* curr_instruction;
    Bc_Basic_Block* curr_basic_block;
    u32 curr_local_count;
    map(string_id, Bc_Operand)* ident_to_operand;
    
    Bc_Label_To_Value_Table* declarations;
};

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
    if (!bb) {
        bb = &bc->entry_basic_block;
    }
    
    Bc_Instruction* insn = arena_push_struct(&bc->arena, Bc_Instruction);
    insn->opcode = opcode;
    
    bc->curr_instruction = insn;
    
    bb->count++;
    if (!bb->first) {
        bb->first = insn;
    }
}

void
bc_push_operand(Bc_Builder* bc, Bc_Operand operand) {
    Bc_Instruction* insn = bc->curr_instruction;
    if (!insn) {
        assert(0 && "no current instruction, forgot to push_instruction() first?");
        return;
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

inline Bc_Operand
bc_get_unique_register_operand(Bc_Builder* bc, Bc_Type type) {
    Bc_Operand result;
    
    Bc_Register reg;
    reg.ident = bc->curr_declaration ? bc->curr_declaration->label.ident : 0;
    reg.index = bc->curr_local_count++;
    
    result.kind = BcOperand_Register;
    result.type = type;
    result.Register = reg;
    return result;
}

inline Bc_Register
bc_get_unique_register(Bc_Builder* bc) {
    Bc_Register result;
    result.ident = bc->curr_declaration ? bc->curr_declaration->label.ident : 0;
    result.index = bc->curr_local_count++;
    return result;
}

Bc_Basic_Block*
bc_push_basic_block(Bc_Builder* bc) {
    
    Bc_Basic_Block* block = arena_push_struct(&bc->arena, Bc_Basic_Block);
    if (bc->curr_basic_block) {
        bc->curr_basic_block->next = block;
    }
    bc->curr_basic_block = block;
    block->label = bc_get_unique_register(bc);
    
    Bc_Operand block_operand = {};
    block_operand.kind = BcOperand_Basic_Block;
    block_operand.Basic_Block = block;
    bc_push_instruction(bc, Bytecode_label);
    bc_push_operand(bc, block_operand);
    
    block->first = bc->curr_instruction;
    block->count = 1;
    
    Bc_Value decl;
    decl.basic_block = block;
    map_put(bc->declarations, block->label, decl);
    
    return block;
}

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
                    case Kw_int:  result.kind = BcTypeKind_s32; break; // Arch dep?
                    case Kw_s8:   result.kind = BcTypeKind_s8; break;
                    case Kw_s16:  result.kind = BcTypeKind_s16; break;
                    case Kw_s32:  result.kind = BcTypeKind_s32; break;
                    case Kw_s64:  result.kind = BcTypeKind_s64; break;
                    case Kw_smm:  result.kind = BcTypeKind_s64; break; // Arch dep.
                    case Kw_uint: result.kind = BcTypeKind_u32; break; // Arch dep?
                    case Kw_u8:   result.kind = BcTypeKind_u8; break;
                    case Kw_u16:  result.kind = BcTypeKind_u16; break;
                    case Kw_u32:  result.kind = BcTypeKind_u32; break;
                    case Kw_u64:  result.kind = BcTypeKind_u64; break;
                    case Kw_umm:  result.kind = BcTypeKind_u64; break; // Arch dep.
                    case Kw_f32:  result.kind = BcTypeKind_f32; break;
                    case Kw_f64:  result.kind = BcTypeKind_f64; break;
                    case Kw_char: result.kind = BcTypeKind_u8; break;
                    case Kw_bool: result.kind = BcTypeKind_s8; break;
                    case Kw_b32:  result.kind = BcTypeKind_s32; break;
                    case Kw_void: result.kind = BcTypeKind_None; break;
                    default: assert(0 && "invalid primitive type");
                }
            }
        } break;
    }
    
    return result;
}

Bc_Type
bc_build_type(Bc_Builder* bc, Type* type) {
    Bc_Type result = {};
    
    switch (type->kind) {
        case TypeKind_Primitive: {
            Primitive_Type_Kind primitive_kind = type->Primitive.kind;
            
            switch (primitive_kind) {
                case PrimitiveTypeKind_int:  result.kind = BcTypeKind_s32; break; // Arch dep?
                case PrimitiveTypeKind_s8:   result.kind = BcTypeKind_s8; break;
                case PrimitiveTypeKind_s16:  result.kind = BcTypeKind_s16; break;
                case PrimitiveTypeKind_s32:  result.kind = BcTypeKind_s32; break;
                case PrimitiveTypeKind_s64:  result.kind = BcTypeKind_s64; break;
                case PrimitiveTypeKind_smm:  result.kind = BcTypeKind_s64; break; // Arch dep.
                case PrimitiveTypeKind_uint: result.kind = BcTypeKind_u32; break; // Arch dep?
                case PrimitiveTypeKind_u8:   result.kind = BcTypeKind_u8; break;
                case PrimitiveTypeKind_u16:  result.kind = BcTypeKind_u16; break;
                case PrimitiveTypeKind_u32:  result.kind = BcTypeKind_u32; break;
                case PrimitiveTypeKind_u64:  result.kind = BcTypeKind_u64; break;
                case PrimitiveTypeKind_umm:  result.kind = BcTypeKind_u64; break; // Arch dep.
                case PrimitiveTypeKind_f32:  result.kind = BcTypeKind_f32; break;
                case PrimitiveTypeKind_f64:  result.kind = BcTypeKind_f64; break;
                case PrimitiveTypeKind_char: result.kind = BcTypeKind_u8; break;
                case PrimitiveTypeKind_bool: result.kind = BcTypeKind_s8; break;
                case PrimitiveTypeKind_b32:  result.kind = BcTypeKind_s32; break;
                case PrimitiveTypeKind_void: result.kind = BcTypeKind_None; break;
                default: assert(0 && "invalid primitive type");
            }
        } break;
    }
    
    return result;
}

Bc_Operand
bc_build_expression(Bc_Builder* bc, Ast* node) {
    Bc_Operand result = {};
    
    switch (node->kind) {
        case Ast_Ident: {
            result = map_get(bc->ident_to_operand, node->Ident);
            
            if (result.type.ptr_depth == 1) {
                Bc_Operand variable = result;
                
                Bc_Type loaded_type = result.type;
                loaded_type.ptr_depth--;
                result = bc_get_unique_register_operand(bc, loaded_type);
                Bc_Operand type_operand = {};
                type_operand.kind = BcOperand_Type;
                type_operand.type = loaded_type;
                
                bc_push_instruction(bc, Bytecode_load);
                bc_push_operand(bc, result);
                bc_push_operand(bc, type_operand);
                bc_push_operand(bc, variable);
                
            }
            
            assert(result.kind && "bug: failed to load operand from identifier");
        } break;
        
        case Ast_Value: {
            result.kind = BcOperand_Value;
            result.type = bc_build_type(bc, node->Value.type);
            
            switch (result.type.kind) {
                case BcTypeKind_s1:  
                case BcTypeKind_s8:  
                case BcTypeKind_s16: 
                case BcTypeKind_s32: 
                case BcTypeKind_s64: {
                    result.Value.signed_int = node->Value.value.signed_int;
                } break;
                
                case BcTypeKind_u8:  
                case BcTypeKind_u16: 
                case BcTypeKind_u32: 
                case BcTypeKind_u64: {
                    result.Value.unsigned_int = node->Value.value.unsigned_int;
                } break;
                
                case BcTypeKind_f32: 
                case BcTypeKind_f64: {
                    result.Value.floating = node->Value.value.floating;
                } break;
            }
        } break;
        
        case Ast_Unary_Expr: {
            Bc_Operand first = bc_build_expression(bc, node->Unary_Expr.first);
            
            switch (node->Unary_Expr.op) {
                case UnaryOp_Negate: {
                    bc_push_instruction(bc, Bytecode_neg);
                } break;
                
                case UnaryOp_Bitwise_Not: {
                    bc_push_instruction(bc, Bytecode_not);
                } break;
                
                case UnaryOp_Address_Of: {
                    bc_push_instruction(bc, Bytecode_not);
                } break;
                
                case UnaryOp_Dereference: {
                    bc_push_instruction(bc, Bytecode_not);
                } break;
                
                default: assert(0 && "bug: not a valid unary op");
            }
            
            result = bc_get_unique_register_operand(bc, first.type);
            
            bc_push_operand(bc, result);
            bc_push_operand(bc, first);
        } break;
        
        case Ast_Binary_Expr: {
            Bc_Operand first = bc_build_expression(bc, node->Binary_Expr.first);
            Bc_Operand second = bc_build_expression(bc, node->Binary_Expr.second);
            
            switch (node->Binary_Expr.op) {
#define BINOP(name, op, prec, assoc, is_comparator, bc_mnemonic) \
case BinaryOp_##name: bc_push_instruction(bc, Bytecode_##bc_mnemonic); break;
                DEF_BINARY_OPS
#undef BINOP
            }
            
            // Figure out the type of the result
            Bc_Type result_type;
            if (binary_is_comparator_table[node->Binary_Expr.op]) {
                result_type = { BcTypeKind_s1, 0 };
            } else {
                result_type = first.type;
            }
            
            Bc_Operand temp_register = bc_get_unique_register_operand(bc, result_type);
            bc_push_operand(bc, temp_register);
            bc_push_operand(bc, first);
            bc_push_operand(bc, second);
            
            if (is_binary_assign(node->Binary_Expr.op)) {
                string_id ident = ast_unwrap_ident(node->Binary_Expr.first);
                Bc_Operand dest_register = map_get(bc->ident_to_operand, ident);
                bc_push_instruction(bc, Bytecode_store);
                bc_push_operand(bc, dest_register);
                bc_push_operand(bc, temp_register);
                
            } else {
                result = temp_register;
            }
            
            
        } break;
        
        case Ast_Ternary_Expr: {
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
    
    if (cmp_code == Bytecode_noop) {
        cmp_code = Bytecode_cmpneq;
        first = bc_build_expression(bc, node);
        second.type = first.type;
        second.kind = BcOperand_Value;
        second.Value.signed_int = 0;
    } else {
        first = bc_build_expression(bc, node->Binary_Expr.first);
        second = bc_build_expression(bc, node->Binary_Expr.second);
    }
    
    Bc_Type dest_type = {};
    dest_type.kind = BcTypeKind_s1;
    Bc_Operand dest = bc_get_unique_register_operand(bc, dest_type);
    
    bc_push_instruction(bc, cmp_code);
    bc_push_operand(bc, dest);
    bc_push_operand(bc, first);
    bc_push_operand(bc, second);
    
    return dest;
}

void
bc_build_statement(Bc_Builder* bc, Ast* node) {
    
    switch (node->kind) {
        case Ast_Assign_Stmt: {
            Bc_Operand source = bc_build_expression(bc, node->Assign_Stmt.expr);
            
            string_id ident = ast_unwrap_ident(node->Assign_Stmt.ident);
            
            Bc_Instruction insn = {};
            Bc_Type type = bc_build_type(bc, node->Assign_Stmt.type);
            
            Bc_Operand dest = bc_get_unique_register_operand(bc, type);
            Bc_Operand alloc_type = dest;
            alloc_type.kind = BcOperand_Type;
            dest.type.ptr_depth++;
            map_put(bc->ident_to_operand, ident, dest);
            
            bc_push_instruction(bc, Bytecode_stack_alloc);
            bc_push_operand(bc, dest);
            bc_push_operand(bc, alloc_type);
            // TODO(Alexander): we should maybe also push the allignment 
            
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
            
            // Branch
            Bc_Operand empty_reg_op = {};
            empty_reg_op.kind = BcOperand_Register;
            bc_push_instruction(bc, Bytecode_branch);
            bc_push_operand(bc, cond);
            bc_push_operand(bc, empty_reg_op);
            bc_push_operand(bc, empty_reg_op);
            Bc_Instruction* branch_insn = bc->curr_instruction;
            
            
            branch_insn->src0.Register = bc_push_basic_block(bc)->label;
            
            bc_build_statement(bc, node->If_Stmt.then_block);
            
            bc_push_instruction(bc, Bytecode_branch);
            bc_push_operand(bc, empty_reg_op);
            Bc_Instruction* exit_insn = bc->curr_instruction;
            
            if (is_ast_none(node->If_Stmt.else_block)) {
                Bc_Basic_Block* exit_block = bc_push_basic_block(bc);
                branch_insn->src1.Register = exit_block->label;
                exit_insn->dest.Register = exit_block->label;
                
            } else {
                
                branch_insn->src1.Register = bc_push_basic_block(bc)->label;
                bc_build_statement(bc, node->If_Stmt.else_block);
                
                bc_push_instruction(bc, Bytecode_branch);
                bc_push_operand(bc, empty_reg_op);
                Bc_Instruction* exit2_insn = bc->curr_instruction;
                
                Bc_Basic_Block* exit_block = bc_push_basic_block(bc);
                exit_insn->dest.Register = exit_block->label;
                exit2_insn->dest.Register = exit_block->label;
            }
            
        } break;
        
        case Ast_For_Stmt: {
            unimplemented;
        } break;
        
        case Ast_While_Stmt: {
            unimplemented;
        } break;
        
        case Ast_Return_Stmt: {
            Bc_Operand source = bc_build_expression(bc, node->Return_Stmt.expr);
            
            bc_push_instruction(bc, Bytecode_ret);
            // HACK(Alexander): we can't set the operands directly
            bc->curr_instruction->src0 = source;
        } break;
        
        default: assert(0 && "bug: not an expression");
    }
}

void
bc_register_declaration(Bc_Builder* bc, string_id ident, Ast* type, Ast* decl) {
    switch (type->kind) {
        case Ast_Function_Type: {
            assert(decl->kind == Ast_Block_Stmt);
            
            Bc_Basic_Block* block = bc_push_basic_block(bc);
            block->label.ident = ident;
            bc->curr_declaration = block;
            
            bc->curr_local_count = 1;
            
            bc_build_statement(bc, decl);
            
            Bc_Register reg;
            reg.ident = ident;
            reg.index = 0;
            
            Bc_Value value;
            value.basic_block = block;
            
            map_put(bc->declarations, reg, value);
        } break;
    }
}

void
bc_build_from_top_level_declaration(Bc_Builder* bc, Ast* ast) {
    assert(ast->kind == Ast_Decl);
    
    Bc_Instruction result = {};
    Ast* stmt = ast->Decl.stmt;
    
    switch (stmt->kind) {
        case Ast_Decl_Stmt: {
            string_id ident = ast_unwrap_ident(stmt->Decl_Stmt.ident);
            
            Ast* type = stmt->Decl_Stmt.type;
            Ast* decl = stmt->Decl_Stmt.decl;
            bc_register_declaration(bc, ident, type, decl);
        } break;
        
        case Ast_Assign_Stmt: {
            unimplemented;
        } break;
    }
}

Bc_Basic_Block*
bc_build_from_ast(Bc_Builder* bc, Ast_File* ast_file) {
    // NOTE(Alexander): we want to minimize this as far as possible
    // to be able to compile large programs.
    pln("sizeof(Bc_Instruction) = %", f_umm(sizeof(Bc_Instruction)));
    pln("sizeof(Bc_Operand) = %\n", f_umm(sizeof(Bc_Operand)));
    
    
    for_map(ast_file->decls, decl) {
        bc_build_from_top_level_declaration(bc, decl->value);
    }
    
    Bc_Register entry_point_label = { Sym_main, 0 };
    Bc_Basic_Block* main_block = map_get(bc->declarations, entry_point_label).basic_block;
    
    pln("Bytecode size: % bytes", f_umm(bc->arena.curr_used));
    
    return main_block;
}