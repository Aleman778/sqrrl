
struct Type_Context {
    Memory_Arena* arena;
    
    Ast_Block* block;
};

inline void
begin_block(Type_Context* tcx, Ast_Block* block) {
    block->parent = tcx->block;
    tcx->block = block;
}

inline void
end_block(Type_Context* tcx) {
    assert(tcx->block);
    tcx->block = tcx->block->parent;
}


bool infer_procedure_signature(Type_Context* tcx, Ast_Procedure_Type* signature);

Ast_Type* infer_expression(Type_Context* tcx, Ast_Expression* expr);

Ast_Type* infer_binary_expression(Type_Context* tcx, Ast_Binary* binary);

bool infer_declaration(Type_Context* tcx, Ast_Declaration* decl);
