
struct Interp {
    
    s32 block_depth;
    
    Arena stack;
    smm stack_pointer;
    smm base_pointer;
}

enum Interp_Result_Type {
    InterpResultType_Void,
    InterpResultType_Immediate,
    InterpResultType_Return,
    InterpResultType_Break,
    InterpResultType_Continue,
}

struct Interp_Value {
    Interp_Result_Type result_type;
    Value value;
    s32 block_depth;
    str_id label;
}

inline void*
interp_stack_push(Interp* interp, smm size) {
    void* memory = arena_push_size(&interp->stack, size, 1);
    interp->stack_pointer += size;
    return memory;
}

Interp_Value
interp_statement(Interp* interp, Ast* ast) {
    assert(ast->type > Ast_Stmt_Begin && ast->type < Ast_Stmt_End);
    
    Interp_Value result = {};
    
    switch (ast->type) {
        case Ast_Assign_Stmt: {
            
        } break;
        
        case Ast_Expr_Stmt: {
            result = interp_expression(interp, ast->Expr_Stmt);
        } break;
        
        case Ast_Block_Stmt: {
            result = interp_block(interp, ast->Block_Stmt.stmts);
        } break;
        
        case Ast_Break_Stmt: {
            result.result_type = InterpResultType_Break;
            result.label = ast->Break_Stmt.ident->Ident;
        } break;
        
        case Ast_Continue_Stmt: {
            result.result_type = InterpResultType_Continue;
            result.label = ast->Continue_Stmt.ident->Ident;
        } break;
        
        case Ast_Decl_Stmt: {
        } break;
        
        case Ast_If_Stmt: {
        } break;
        
        case Ast_For_Stmt: {
        } break;
        
        case Ast_While_Stmt: {
        } break;
        
        case Ast_Loop_Stmt: {
        } break;
        
        case Ast_Return_Stmt: {
        } break;
    }
    
    return result;
}

Interp_Value
interp_block(Interp* interp, Ast* ast) {
    interp->block_depth++;
    
    while (ast->type == Ast_Compound) {
        interp_statement(interp, ast->Compound.node);
        ast = ast->Compound.next;
    }
    
    interp->block_depth--;
}


Interp_Value
interp_function_call(Interp* interp, Ast* ast) {
    
}
