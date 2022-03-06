

struct Bc_Generator {
    Memory_Arena arena;
    
    Bc_Basic_Block entry_basic_block;
    
    // Current building block
    Bc_Instruction* curr_instruction;
    Bc_Basic_Block* curr_basic_block;
    u32 curr_local_count;
    map(string_id, Bc_Operand)* ident_to_operand;
};

// TODO(Alexander): we might want to use this approach, but keep it simple for now
// This approach essentially only allocates the number of operands that are actually needed
// given the opcode.
#if 0
Bc_Instruction
push_instruction(Bc_Generator* bc, Bc_Opcode opcode) {
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
bc_push_instruction(Bc_Generator* bc, Bc_Opcode opcode) {
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
bc_push_operand(Bc_Generator* bc, Bc_Operand operand) {
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
bc_get_unique_register(Bc_Generator* bc, Bc_Type type) {
    Bc_Operand result;
    
    Bc_Register reg;
    reg.ident = 0;
    reg.index = bc->curr_local_count++;
    
    result.kind = BcOperand_Register;
    result.type = type;
    result.Register = reg;
    return result;
}


Bc_Type
bc_generate_type(Bc_Generator* bc, Ast* type) {
    Bc_Type result = {};
    
    switch (type->kind) {
        case Ast_Named_Type: {
            string_id ident = ast_unwrap_ident(type->Named_Type);
            
            if (ident == Kw_string) {
                // TODO(Alexander): string type
            } else if (is_builtin_type_keyword(ident)) {
                // HACK(Alexander): this is a little bit hack of trying to convert between enums
                u32 primitive = (u32) ident - (u32) builtin_types_begin;
                assert(primitive > 0 && "invalid primitive type");
                result.kind = (Bc_Type_Kind) ((u32) BcTypeKind_int + (primitive - 1));
            }
        } break;
        
    }
    
    return result;
}

Bc_Type
bc_generate_type(Bc_Generator* bc, Type* type) {
    Bc_Type result = {};
    
    switch (type->kind) {
        case TypeKind_Primitive: {
            Primitive_Type_Kind primitive_kind = type->Primitive.kind;
            
            // HACK(Alexander): this is a little bit hack of trying to convert between enums
            result.kind = (Bc_Type_Kind) ((u32) BcTypeKind_int + (u32) primitive_kind);
        } break;
    }
    
    return result;
}

Bc_Operand
bc_generate_expression(Bc_Generator* bc, Ast* node) {
    Bc_Operand result = {};
    
    switch (node->kind) {
        case Ast_Ident: {
            result = map_get(bc->ident_to_operand, node->Ident);
            assert(result.kind && "bug: failed to load operand from identifier");
        } break;
        
        case Ast_Value: {
            result.kind = BcOperand_Const;
            result.type = bc_generate_type(bc, node->Value.type);
            result.Const.value = node->Value.value;
        } break;
        
        case Ast_Unary_Expr: {
            Bc_Operand first = bc_generate_expression(bc, node->Unary_Expr.first);
            
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
            
            result = bc_get_unique_register(bc, first.type);
            
            bc_push_operand(bc, result);
            bc_push_operand(bc, first);
        } break;
        
        case Ast_Binary_Expr: {
            bool is_assign = is_assignment_binary_operator(node->Binary_Expr.op);
            
            Bc_Operand first = bc_generate_expression(bc, node->Binary_Expr.first);
            Bc_Operand second = bc_generate_expression(bc, node->Binary_Expr.second);
            
            if (is_assign) {
                result = first;
            }
            
            pln("is_assign (%) = %\n", f_ast(node), f_bool(is_assign));
            
            switch (node->Binary_Expr.op) {
#define BINOP(name, op, prec, assoc, bc_mnemonic) \
case BinaryOp_##name: bc_push_instruction(bc, Bytecode_##bc_mnemonic); break;
                DEF_BINARY_OPS
#undef BINOP
            }
            
            
            if (!is_assign) {
                result = bc_get_unique_register(bc, {});
            }
            // TODO(Alexander): we don't have the type, however can be easily infered
            
            bc_push_operand(bc, result);
            bc_push_operand(bc, first);;
            bc_push_operand(bc, second);
        } break;
        
        case Ast_Ternary_Expr: {
            unimplemented;
        } break;
        
        default: assert(0 && "bug: not an expression");
    }
    
    return result;
}

void
bc_generate_statement(Bc_Generator* bc, Ast* node) {
    
    switch (node->kind) {
        case Ast_Assign_Stmt: {
            
            Bc_Operand source = bc_generate_expression(bc, node->Assign_Stmt.expr);
            
            string_id ident = ast_unwrap_ident(node->Assign_Stmt.ident);
            
            Bc_Instruction insn = {};
            Bc_Type type = bc_generate_type(bc, node->Assign_Stmt.type);
            
            bc_push_instruction(bc, Bytecode_stack_alloc);
            
            Bc_Operand operand = bc_get_unique_register(bc, type);
            map_put(bc->ident_to_operand, ident, operand);
            
            bc_push_operand(bc, operand);
            bc_push_operand(bc, source);
            
        } break;
        
        case Ast_Expr_Stmt: {
            bc_generate_expression(bc, node->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            unimplemented;
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
            unimplemented;
        } break;
        
        case Ast_For_Stmt: {
            unimplemented;
        } break;
        
        case Ast_While_Stmt: {
            unimplemented;
        } break;
        
        case Ast_Return_Stmt: {
            Bc_Operand source = bc_generate_expression(bc, node->Return_Stmt.expr);
            
            bc_push_instruction(bc, Bytecode_ret);
            // HACK(Alexander): we can't set the operands directly
            bc->curr_instruction->src0 = source;
        } break;
        
        default: assert(0 && "bug: not an expression");
    }
}

Bc_Basic_Block
bc_generate_basic_block(Bc_Generator* bc, Ast* stmts) {
    Bc_Basic_Block result = {};
    bc->curr_basic_block = &result;
    
    for_compound(stmts, stmt) {
        bc_generate_statement(bc, stmt);
    }
    
    String_Builder sb = {};
    
    Bc_Instruction* curr_insn = result.first;
    for (int i = 0; i < result.count; i++) {
        string_builder_push(&sb, curr_insn++);
        string_builder_push(&sb, "\n");
    }
    
    string str = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(str));
    string_builder_free(&sb);
    
    bc->curr_basic_block = 0;
    
    return result;
}

void
bc_register_declaration(Bc_Generator* bc, string_id ident, Ast* type, Ast* decl) {
    switch (type->kind) {
        case Ast_Function_Type: {
            assert(decl->kind == Ast_Block_Stmt);
            
            Ast* stmts = decl->Block_Stmt.stmts;
            bc_generate_basic_block(bc, stmts);
            
        } break;
    }
}

void
bc_generate_from_top_level_declaration(Bc_Generator* bc, Ast* ast) {
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
            
            
        } break;
    }
}

void
bc_generate_from_ast(Ast_File* ast_file) {
    Bc_Generator bc = {};
    
    pln("sizeof(Bc_Instruction) = %", f_umm(sizeof(Bc_Instruction)));
    pln("sizeof(Bc_Operand) = %\n", f_umm(sizeof(Bc_Operand)));
    
    String_Builder sb = {};
    
    for_map(ast_file->decls, decl) {
        bc_generate_from_top_level_declaration(&bc, decl->value);
    }
    
    string result = string_builder_to_string_nocopy(&sb);
    
    pln("%", f_string(result));
}
