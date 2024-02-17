
enum Ast_Type_Kind {
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
};

enum Type_Flags {
    TypeFlag_Const
};

struct Ast_Type {
    Ast_Type_Kind kind;
    u32 flags;
};

struct Ast_Expression;

struct Ast_Expression_Assign {
    Ast_Type* type;
    Ast_Expression* expr;
};

struct Ast_Expression_Literal {
    Ast_Type_Kind type;
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

inline Ast_Expression*
push_ast_expression(Lexer* lexer, Ast_Expression_Kind kind) {
    Ast_Expression* result = arena_push_struct(lexer->ast_arena, Ast_Expression);
    result->kind = kind;
    return result;
}

inline Ast_Type*
push_ast_type(Lexer* lexer, Ast_Type_Kind kind) {
    Ast_Type* result = arena_push_struct(lexer->ast_arena, Ast_Type);
    result->kind = kind;
    return result;
}
