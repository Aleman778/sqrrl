
// Define the different types of nodes in the Abstract Syntax Tree
#define DEF_AST_TYPES                            \
AST(Abi,               "abi", str)               \
AST(Literal,           "literal", Value)         \
AST(Ident,             "identifier", Symbol)     \
AST(Argument,          "argument", struct {      \
Ast_Node* type;                                  \
Ast_Node* ident;                                 \
Ast_Node* assign;                                \
})                                               \
AST(Compound,          "compound", struct {      \
Ast_Node* node;                                  \
Ast_Node* next;                                  \
})                                               \
AST_GROUP(Expr_Begin,  "expression")             \
AST(Unary_Expr,        "unary", struct {         \
Unary_Op op;                                     \
Ast_Node* first;                                 \
})                                               \
AST(Binary_Expr,       "binary", struct {        \
Unary_Op op;                                     \
Ast_Node* first;                                 \
Ast_Node* second;                                \
})                                               \
AST(Ternary_Expr,      "ternary", struct {       \
Unary_Op op;                                     \
Ast_Node* first;                                 \
Ast_Node* second;                                \
Ast_Node* third;                                 \
})                                               \
AST(Call_Expr,         "call", struct {          \
Ast_Node* ident;                                 \
Ast_Node* args;                                  \
})                                               \
AST(Field_Expr,        "field", struct {         \
Ast_Node* first;                                 \
Ast_Node* second;                                \
})                                               \
AST(Cast_Expr,         "cast", struct {          \
Ast_Node* type;                                  \
Ast_Node* expr;                                  \
})                                               \
AST(Paren_Expr,        "parentheses", struct {         \
Ast_Node* expr;                                \
})                                               \
AST(Index_Expr,        "index", struct {         \
Ast_Node* array;                                 \
Ast_Node* index;                                 \
})                                               \
AST(Array_Expr,        "array", struct {         \
Ast_Node* first;                                 \
Ast_Node* second;                                \
})                                               \
AST(Struct_Expr,       "struct", struct {        \
Ast_Node* first;                                 \
Ast_Node* second;                                \
})                                               \
AST(Tuple_Expr,        "tuple", struct {         \
Ast_Node* first;                                 \
Ast_Node* second;                                \
})                                               \
AST_GROUP(Expr_End,    "expression")             \
AST_GROUP(Stmt_Begin,  "statement")              \
AST(Assign_Stmt,       "assignment", struct {    \
Ast_Node* first;                                 \
Ast_Node* second;                                \
})                                               \
AST(Block_Stmt,        "block", struct {         \
Ast_Node* stmts;                                 \
})                                               \
AST(Break_Stmt,        "break", struct {         \
Ast_Node* ident;                                 \
})                                               \
AST(Continue_Stmt,     "continue", struct {      \
Ast_Node* ident;                                 \
})                                               \
AST(If_Stmt,           "if", struct {            \
Ast_Node* cond;                                  \
Ast_Node* then_block;                            \
Ast_Node* else_block;                            \
})                                               \
AST(For_Stmt,          "for", struct {           \
Ast_Node* label;                                 \
Ast_Node* init;                                  \
Ast_Node* cond;                                  \
Ast_Node* update;                                \
Ast_Node* block;                                 \
})                                               \
AST(Loop_Stmt,         "loop", struct {          \
Ast_Node* label;                                 \
})                                               \
AST(While_Stmt,        "while", struct {         \
Ast_Node* label;                                 \
Ast_Node* cond;                                  \
Ast_Node* block;                                 \
})                                               \
AST(Return_Stmt,       "return", struct {        \
Ast_Node* first;                                 \
Ast_Node* second;                                \
})                                               \
AST_GROUP(Stmt_End,    "statement")              \
AST_GROUP(Type,        "type")                   \
AST(Named_Type,        "named", struct {         \
Ast_Node* ident;                                 \
})                                               \
AST(Array_Type,        "array", struct {         \
Ast_Node* elem_type;                             \
Ast_Node* shape;                                 \
})                                               \
AST(Tuple_Type,        "tuple", struct {         \
Ast_Node* elem_types;                            \
})                                               \
AST(Infer_Type,        "infer", Symbol)          \
AST(Function_Type,     "function", struct {      \
Ast_Node* ident;                                 \
Ast_Node* arg_types;                             \
})                                               \
AST(Struct_Type,       "struct", struct {        \
Ast_Node* ident;                                 \
Ast_Node* fields;                                \
})                                               \
AST(Union_Type,        "union", struct {         \
Ast_Node* ident;                                 \
Ast_Node* fields;                                \
})                                               \
AST(Enum_Type,         "enum", struct {          \
Ast_Node* ident;                                 \
Ast_Node* elem_type;                             \
Ast_Node* fields;                                \
})                                               \
AST_GROUP(Type_End,    "type")                   \
AST_GROUP(Decl_Begin,  "declaration")            \
AST(Type_Decl,         "type", struct {          \
Ast_Node* type;                                  \
})                                               \
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
#define AST(symbol, ...) AstType_##symbol,
#define AST_GROUP(symbol, ...) AstType_##symbol,
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
    return { (u32) token.offset, (u16) token.source.count, 0 }; // TODO(alexander): what should index be?
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

struct Ast_Node {
    Ast_Type type;
    union {
#define AST(symbol, name, decl) decl Ast_##symbol;
#define AST_GROUP(...)
        DEF_AST_TYPES 
#undef AST_GROUP
#undef AST
    };
    Span span;
};
