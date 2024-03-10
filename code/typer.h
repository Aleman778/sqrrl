
struct Type_Context {
    Memory_Arena* arena;
};

bool infer_procedure_signature(Type_Context* tcx, Ast_Procedure_Type* signature);
Ast_Type* infer_expression(Type_Context* tcx, Ast_Expression* expr);
Ast_Type* infer_binary_expression(Type_Context* tcx, Ast_Binary* binary);
bool infer_declaration(Type_Context* tcx, Ast_Declaration* decl);
