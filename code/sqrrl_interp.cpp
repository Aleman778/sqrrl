

void
register_compilation_units_from_ast(Interp* interp, Ast* decl) {
    
    switch (decl->kind) {
        case Ast_Compound: {
            for_compound(decl, it) {
                register_compilation_units_from_ast(interp, it);
            }
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(decl->Block_Stmt.stmts, it) {
                register_compilation_units_from_ast(interp, it);
            }
        } break;
        
        case Ast_Decl_Stmt: {
            Compilation_Unit cu = {};
            cu.ident = ast_unwrap_ident(decl->Decl_Stmt.ident);
            cu.ast = decl->Decl_Stmt.type;
            push_compilation_unit(interp, cu);
            
            cu.ast = decl;
            push_compilation_unit(interp, cu);
        } break;
        
        case Ast_Assign_Stmt: {
            Compilation_Unit cu = {};
            cu.ident = ast_unwrap_ident(decl->Assign_Stmt.ident);
            cu.ast = decl;
            push_compilation_unit(interp, cu);
        } break;
        
        case Ast_Typedef: {
            unimplemented;
        } break;
        
        default: {
            // TODO(Alexander): maybe don't accept everything but let's see
            if (is_valid_ast(decl)) {
                Compilation_Unit cu = {};
                cu.ast = decl;
                push_compilation_unit(interp, cu);
            }
        } break;
    }
}
