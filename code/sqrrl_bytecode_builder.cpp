
int
emit_bytecode_for_function(Bytecode_Builder* bc, Ast_Function* func) {
    int func_index = bc->module.next_func_index++;
    
    Bc* prologue_inst = bc_label(bc, func);
    prologue_inst->label.function = func;
    bc->curr_func = func;
    
    emit_bytecode_for_expression(bc, func->body);
    
    Bc* epilogue_inst = bc_label(bc, func);
    epilogue_inst->label.function = func;
    epilogue_inst->label.epilogue_index = epilogue_inst->label.index;
    prologue_inst->label.epilogue_index = epilogue_inst->label.index;
    bc->curr_func = 0;
    
    return func_index;
}

void
emit_bytecode_for_declarations(Bytecode_Builder* bc, Ast_Block* block) {
    for_array_v(block->statements, ast, _) {
        if (ast->kind == AST_FUNCTION) {
            emit_bytecode_for_function(bc, (Ast_Function*) ast);
        }
    }
}


Bc*
emit_bytecode_for_conditional_jump(Bytecode_Builder* bc, Ast_Expression* cond) {
    Opcode opcode = BC_NOOP;
    if (cond->kind == AST_BINARY) {
        auto binop = (Ast_Binary*) cond;
        opcode = operator_to_inverse_jump_opcode(binop->op);
        if (opcode) {
            Bc_Arg left = emit_bytecode_for_expression(bc, binop->left);
            Bc_Arg right = emit_bytecode_for_expression(bc, binop->right);
            return bc_instruction(bc, opcode, bc_type(cond->inferred_type), {}, left, right);
        }
    }
    
    Bc_Arg cond_arg = emit_bytecode_for_expression(bc, cond);
    return bc_instruction(bc, BC_JZ, bc_type(cond->inferred_type), {}, cond_arg);
}


Bc_Arg
emit_bytecode_for_expression(Bytecode_Builder* bc, Ast_Expression* expr) {
    Bc_Arg result = {};
    
    switch (expr->kind) {
        case AST_LITERAL: {
            auto lit = (Ast_Literal*) expr;
            Ast_Type* type = lit->inferred_type;
            if (type->flags & TYPE_FLAG_INTEGER) {
                assert(type->size <= 4 && "unimplemented FIXME add 64-bit integers"); ;
                result = bc_immediate((s32) lit->value._u64);
                
            } else if (type->flags & TYPE_FLAG_FLOAT) {
                unimplemented;
                
            } else {
                assert(0 && "unknown literal type");
            }
        } break;
        
        case AST_BINARY: {
            auto binop = (Ast_Binary*) expr;
            Ast_Type* type = binop->inferred_type; 
            Bc_Arg left = emit_bytecode_for_expression(bc, binop->left);
            Bc_Arg right = emit_bytecode_for_expression(bc, binop->right);
            
            result = bc_register(BC_REG_ACCUMULATOR);
            Opcode opcode = operator_to_opcode(binop->op, type);
            bc_instruction(bc, opcode, bc_type(type), result, left, right);
        } break;
        
        case AST_IDENTIFIER: {
            auto ident = (Ast_Identifier*) expr;
            assert(ident->resolved_declaration && "cannot emit unresolved identifier");
            
            Ast_Declaration* decl = ident->resolved_declaration;
            result = bc_stack(decl->bytecode_stk_allocation);
        } break;
        
        case AST_BLOCK: {
            auto block = (Ast_Block*) expr;
            for_array_v(block->statements, stmt, _) {
                emit_bytecode_for_expression(bc, stmt);
            }
        } break;
        
        case AST_IF: {
            auto if_stmt = (Ast_If*) expr;
            Bc* jump = emit_bytecode_for_conditional_jump(bc, if_stmt->cond);
            emit_bytecode_for_expression(bc, if_stmt->then_stmt);
            
            jump->label.index = bc_label(bc)->label.index;
            if (if_stmt->else_stmt) {
                emit_bytecode_for_expression(bc, if_stmt->else_stmt);
            }
        } break;
        
        case AST_DECLARATION: {
            auto decl = (Ast_Declaration*) expr;
            Ast_Type* type = decl->inferred_type;
            if (type->size == 1 || type->size == 2 || type->size == 4 || type->size == 8) {
                // Small allocations can be stored directly inside register
                Bc_Arg src = emit_bytecode_for_expression(bc, decl->initializer);
                Bc_Arg dest = bc_stack_alloc(bc, type->size, type->align);
                bc_instruction(bc, BC_MOV, bc_type(type), {}, dest, src);
                decl->bytecode_stk_allocation = result.disp;
                
            } else {
                unimplemented;
            }
        } break;
        
        case AST_RETURN: {
            auto ret = (Ast_Return*) expr;
            
            if (ret->expression) {
                result = emit_bytecode_for_expression(bc, ret->expression); 
            }
            bc_instruction(bc, BC_RETURN, bc_type(expr->inferred_type), {}, result);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return result;
}
