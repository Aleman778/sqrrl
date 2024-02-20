
struct Ast_Type;

struct Ast_Proc_Argument {
    Ast_Type* type;
    Identifier ident;
    Ast_Proc_Argument* next;
};

struct Ast_Proc_Type {
    Ast_Proc_Argument* args;
    Ast_Type* return_type;
};

enum Type_Kind {
    Type_None,
    
    // primitive types (order must match DEF_TYPE_KEYWORDS)
    Type_Void,
    Type_Bool,
    Type_S8,
    Type_S16,
    Type_S32,
    Type_S64,
    Type_Smm,
    Type_Int,
    Type_U8,
    Type_U16,
    Type_U32,
    Type_U64,
    Type_Umm,
    Type_Uint,
    Type_F32,
    Type_F64,
    Type_String,
    Type_Cstring,
    Type_Typeid,
    
    Type_Float,
    
    // other types
    Type_Alias,
    Type_Proc,
};

enum Type_Flags {
    TypeFlag_Const
};

struct Ast_Type {
    Type_Kind kind;
    u32 flags;
    union {
        Ast_Proc_Type proc;
    };
};


struct Ast_Expression;

struct Ast_Expression_Assign {
    Ast_Type* type;
    Ast_Expression* expr;
};

struct Ast_Expression_Literal {
    Type_Kind type;
    union {
        u64 u64_value;
        f32 f32_value;
        f64 f64_value;
    };
};

enum Ast_Expression_Kind {
    // Expressions
    Expr_Literal,
    Expr_Assign,
};

struct Ast_Expression {
    Ast_Expression_Kind kind;
    
    union {
        Ast_Expression_Literal literal;
        Ast_Expression_Assign assign;
    };
};

struct Ast_Proc_Declaration {
    Ast_Expression* block;
    Ast_Proc_Type signature;
    Identifier ident;
};

struct Ast_Type_Declaration {
    Ast_Type* type;
    Identifier ident;
};

enum Ast_Declaration_Kind {
    Decl_Procedure,
    Decl_Type,
};

struct Ast_Declaration {
    Ast_Declaration_Kind kind;
    
    union {
        Ast_Proc_Declaration proc;
        Ast_Type_Declaration type;
    };
};

inline Ast_Type*
push_ast_type(Lexer* lexer, Type_Kind kind) {
    Ast_Type* result = arena_push_struct(lexer->ast_arena, Ast_Type);
    result->kind = kind;
    return result;
}

inline Ast_Expression*
push_ast_expression(Lexer* lexer, Ast_Expression_Kind kind) {
    Ast_Expression* result = arena_push_struct(lexer->ast_arena, Ast_Expression);
    result->kind = kind;
    return result;
}

inline Ast_Declaration*
push_ast_declaration(Lexer* lexer, Ast_Declaration_Kind kind) {
    Ast_Declaration* result = arena_push_struct(lexer->ast_arena, Ast_Declaration);
    result->kind = kind;
    return result;
}
