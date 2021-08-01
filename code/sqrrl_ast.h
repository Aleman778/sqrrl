
// Define the different types of nodes in the Abstract Syntax Tree
#define DEF_AST_TYPES                          \
AST(Abi,               "abi", str)             \
AST(Literal,           "literal", Value)       \
AST(Ident,             "identifier", str_id)   \
AST(Argument,          "argument", struct {    \
Ast* type;                                     \
Ast* ident;                                    \
Ast* assign;                                   \
})                                             \
AST(Compound,          "compound", struct {    \
Ast* node;                                     \
Ast* next;                                     \
})                                             \
AST_GROUP(Expr_Begin,  "expression")           \
AST(Unary_Expr,        "unary", struct {       \
Unary_Op op;                                   \
Ast* first;                                    \
})                                             \
AST(Binary_Expr,       "binary", struct {      \
Unary_Op op;                                   \
Ast* first;                                    \
Ast* second;                                   \
})                                             \
AST(Ternary_Expr,      "ternary", struct {     \
Unary_Op op;                                   \
Ast* first;                                    \
Ast* second;                                   \
Ast* third;                                    \
})                                             \
AST(Call_Expr,         "call", struct {        \
Ast* ident;                                    \
Ast* args;                                     \
})                                             \
AST(Field_Expr,        "field", struct {       \
Ast* first;                                    \
Ast* second;                                   \
})                                             \
AST(Cast_Expr,         "cast", struct {        \
Ast* type;                                     \
Ast* expr;                                     \
})                                             \
AST(Paren_Expr,        "parentheses", struct { \
Ast* expr;                                     \
})                                             \
AST(Index_Expr,        "index", struct {       \
Ast* array;                                    \
Ast* index;                                    \
})                                             \
AST(Array_Expr,        "array", struct {       \
Ast* first;                                    \
Ast* second;                                   \
})                                             \
AST(Struct_Expr,       "struct", struct {      \
Ast* first;                                    \
Ast* second;                                   \
})                                             \
AST(Tuple_Expr,        "tuple", struct {       \
Ast* first;                                    \
Ast* second;                                   \
})                                             \
AST_GROUP(Expr_End,    "expression")           \
AST_GROUP(Stmt_Begin,  "statement")            \
AST(Assign_Stmt,       "assignment", struct {  \
Ast* first;                                    \
Ast* second;                                   \
})                                             \
AST(Block_Stmt,        "block", struct {       \
Ast* stmts;                                    \
})                                             \
AST(Break_Stmt,        "break", struct {       \
Ast* ident;                                    \
})                                             \
AST(Continue_Stmt,     "continue", struct {    \
Ast* ident;                                    \
})                                             \
AST(If_Stmt,           "if", struct {          \
Ast* cond;                                     \
Ast* then_block;                               \
Ast* else_block;                               \
})                                             \
AST(For_Stmt,          "for", struct {         \
Ast* label;                                    \
Ast* init;                                     \
Ast* cond;                                     \
Ast* update;                                   \
Ast* block;                                    \
})                                             \
AST(Loop_Stmt,         "loop", struct {        \
Ast* label;                                    \
})                                             \
AST(While_Stmt,        "while", struct {       \
Ast* label;                                    \
Ast* cond;                                     \
Ast* block;                                    \
})                                             \
AST(Return_Stmt,       "return", struct {      \
Ast* first;                                    \
Ast* second;                                   \
})                                             \
AST_GROUP(Stmt_End,    "statement")            \
AST_GROUP(Type_Begin,  "type")                 \
AST(Named_Type,        "named", str_id)        \
AST(Array_Type,        "array", struct {       \
Ast* elem_type;                                \
Ast* shape;                                    \
})                                             \
AST(Pointer_Type,      "pointer", Ast*)        \
AST(Tuple_Type,        "tuple", struct {       \
Ast* elem_types;                               \
})                                             \
AST(Infer_Type,        "infer", str_id)        \
AST(Function_Type,     "function", struct {    \
Ast* return_type;                              \
Ast* ident;                                    \
Ast* arg_types;                                \
})                                             \
AST(Struct_Type,       "struct", struct {      \
Ast* ident;                                    \
Ast* fields;                                   \
})                                             \
AST(Union_Type,        "union", struct {       \
Ast* ident;                                    \
Ast* fields;                                   \
})                                             \
AST(Enum_Type,         "enum", struct {        \
Ast* ident;                                    \
Ast* elem_type;                                \
Ast* fields;                                   \
})                                             \
AST(Typedef,           "typedef", struct {     \
Ast* type;                                     \
Ast* ident;                                     \
})                                             \
AST_GROUP(Type_End,    "type")                 \
AST_GROUP(Decl_Begin,  "declaration")          \
AST(Type_Decl,         "type", struct {        \
Ast* type;                                     \
Ast* stmt;                                     \
Ast_Decl_Mods mods;                            \
})                                             \
AST_GROUP(Decl_End,    "declaration")

global cstr ast_names[] = {
#define AST(symbol, name, decl) name,
#define AST_GROUP(symbol, name) name,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

global cstr ast_struct_names[] = {
#define AST(symbol, ...) "Ast_" #symbol,
#define AST_GROUP(symbol, ...) "Ast_" #symbol,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

typedef s32 Ast_Type;
enum {
#define AST(symbol, ...) Ast_##symbol,
#define AST_GROUP(symbol, ...) Ast_##symbol,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

typedef s32 Inlining;
enum {
    Inlining_Default,
    Inlining_Inline,
    Inlining_No_Inline,
};

typedef s32 Ast_Decl_Mods;
enum {
    AstDeclModified_None      = 0,
    AstDeclModified_Inline    = 1,
    AstDeclModified_No_Inline = 1 << 2,
    AstDeclModified_Internal  = 1 << 3,
};

union Span {
    struct {
        u32 base;
        u16 count;
        u16 index;
    };
    u64 packed;
};

global const Span empty_span = { 0 };

inline Span 
token_to_span(Token token) {
    return { (u32) token.offset, (u16) str_count(token.source), 0 }; // TODO(alexander): what should index be?
}

inline Span 
span_combine(Span span1, Span span2) {
    assert(span1.ctx == span2.ctx);
    
    Span span;
    span.base = span1.base;
    span.count = (u16) (span2.base - span1.base + (u32) span2.count);
    span.index = span1.index;
    return span;
}

struct Ast {
    Ast_Type type;
    union {
#define AST(symbol, name, decl) decl symbol;
#define AST_GROUP(...)
        DEF_AST_TYPES 
#undef AST_GROUP
#undef AST
    };
    Span span;
};

struct Ast_File {
    Ast* ast;
    
};
