
// Define the different types of nodes in the Abstract Syntax Tree
#define DEF_AST_TYPES \
AST(Abi,               "abi")                      \
AST(Literal,           "literal")                  \
AST(Ident,             "identifier")               \
AST(Ellipsis,          "ellipsis")                 \
AST_GROUP(Expr_Begin,  "expression")               \
AST(Unary_Expr,        "unary expression")         \
AST(Binary_Expr,       "binary expression")        \
AST(Ternary_Expr,      "ternary expression")       \
AST(Call_Expr,         "call expression")          \
AST(Field_Expr,        "field expression")         \
AST(Type_Cast_Expr,    "type cast expression")     \
AST(Paren_Expr,        "parenthesized expression") \
AST(Range_Expr,        "range expression")         \
AST(Index_Expr,        "index expression")         \
AST(Array_Expr,        "array expression")         \
AST(Struct_Expr,       "struct expression")        \
AST(Tuple_Expr,        "tuple expression")         \
AST_GROUP(Expr_End,    "expression")               \
AST_GROUP(Stmt_Begin,  "statement")                \
AST(Assign_Stmt,       "assign statement")         \
AST(Block_Stmt,        "block statement")          \
AST(Break_Stmt,        "break statement")          \
AST(Continue_Stmt,     "continue statement")       \
AST(If_Stmt,           "if statement")             \
AST(For_Stmt,          "for statement")            \
AST(Loop_Stmt,         "loop statement")           \
AST(While_Stmt,        "while statement")          \
AST(Return_Stmt,       "return statement")         \
AST_GROUP(Stmt_End,    "statement")                \
AST_GROUP(Type_Begin,  "type")                     \
AST(Typedef,           "typedef")                  \
AST(Primitive_Type,    "primitive type")           \
AST(Ident_Type,        "ident type")               \
AST(Pointer_Type,      "pointer type")             \
AST(Array_Type,        "array type")               \
AST(Tuple_Type,        "tuple type")               \
AST(Polymorphic_Type,  "polymorphic type")         \
AST(Struct_Type,       "struct type")              \
AST(Enum_Type,         "enum type")                \
AST(Union_Type,        "union type")               \
AST(Function_Type,     "function type")            \
AST_GROUP(Type_End,    "type")                     \
AST_GROUP(Decl_Begin,  "declaration")              \
AST(Import_Decl,       "import declaration")       \
AST(Extern_Block_Decl, "extern block declaration") \
AST(Type_Decl,         "type declaration")         \
AST_GROUP(Decl_End,    "declaration")

global const char* ast_type_names[] = {
#define AST(name, name2) name2,
#define AST_GROUP(name, name2) name2,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

global const char* ast_struct_names[] = {
#define AST(name, ...) "Ast_" #name,
#define AST_GROUP(name, ...) "Ast_" #name,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

typedef s32 Ast_Type;
enum {
#define AST(name, ...) AstType_##name,
#define AST_GROUP(name, ...) AstType_##name,
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

// Forward declare Ast_Node, see declaration at the bottom of this file
struct Ast_Node;

struct Ast_Abi {
    Ast_Node* name;
};

struct Ast_Literal {
    Ast_Node* type;
    Value value;
};

struct Ast_Ident {
    Symbol name;
};

struct Ast_Ellipsis {
    smm reserved;
};

struct Ast_Unary_Expr {
    Unary_Op op;
}

struct Ast_Node {
    Ast_Type type;
    union {
#define AST(name, ...) Ast_##name name;
#define AST_GROUP(...)
        DEF_AST_TYPES
#undef AST_GROUP
#undef AST
    };
    Span span;
};
