
enum Object_Kind {
    Object_Type_Definition,
    Object_Procedure,
    Object_Literal,
    Object_Variable,
};

struct Object {
    Object_Kind kind;
    union {
        Ast_Type_Declaration* type;
        Ast_Literal_Expression* literal;
        Ast_Type* variable;
    };
};

struct Scope {
    map(Identifier, Object)* objects;
    Scope* parent;
};

struct Type_Context {
    Memory_Arena* arena;
    Scope* scope;
};

bool infer_procedure_signature(Type_Context* tcx, Ast_Proc_Type* signature);

inline void
begin_block_scope(Type_Context* tcx, Scope* scope) {
    scope->parent = tcx->scope;
    tcx->scope = scope;
}

inline void
end_block_scope(Type_Context* tcx, Scope* scope) {
    map_free(scope->objects);
    tcx->scope = scope->parent;
}
