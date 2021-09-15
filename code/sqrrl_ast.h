
// Define the different types of nodes in the Abstract Syntax Tree
#define DEF_AST_TYPES                          \
AST_GROUP(None,        "none")                 \
AST(Abi,               "abi", str)             \
AST(Value,             "value", Value)       \
AST(Ident,             "identifier", str_id)   \
AST(Argument,          "argument", struct {    \
Ast* type;                                     \
Ast* ident;                                    \
Ast* assign;                                   \
})                                             \
AST(Compound,          "compound", struct {    \
Ast* next;                                     \
Ast* node;                                     \
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
Ast* var;                                      \
Ast* field;                                    \
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
Ast* type;                                      \
Ast* elements;                                  \
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
Ast* type;                                     \
Ast* expr;                                     \
})                                             \
AST(Expr_Stmt,         "expression", Ast*)     \
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
AST(While_Stmt,        "while", struct {       \
Ast* label;                                    \
Ast* cond;                                     \
Ast* block;                                    \
})                                             \
AST(Loop_Stmt,         "loop", struct {        \
Ast* label;                                    \
Ast* block;                                    \
})                                             \
AST(Return_Stmt,       "return", struct {      \
Ast* expr;                                     \
})                                             \
AST_GROUP(Stmt_End,    "statement")            \
AST_GROUP(Type_Begin,  "type")                 \
AST(Named_Type,        "named", Ast*)          \
AST(Array_Type,        "array", struct {       \
Ast* elem_type;                                \
Ast* shape;                                    \
})                                             \
AST(Pointer_Type,      "pointer", Ast*)        \
AST(Tuple_Type,        "tuple", struct {       \
Ast* elem_types;                               \
})                                             \
AST(Infer_Type,        "infer", void*)         \
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
Ast* ident;                                    \
})                                             \
AST_GROUP(Type_End,    "type")                 \
AST_GROUP(Decl_Begin,  "declaration")          \
AST(Type_Decl,         "type", struct {        \
Ast* type;                                     \
Ast* stmt;                                     \
Ast_Decl_Mods mods;                            \
})                                             \
AST_GROUP(Decl_End,    "declaration")

global cstr ast_strings[] = {
#define AST(symbol, name, decl) name,
#define AST_GROUP(symbol, name) name,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

global cstr ast_struct_strings[] = {
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

typedef s32 Ast_Decl_Mods;
enum {
    AstDeclModified_None      = 0,
    AstDeclModified_Inline    = bit(1),
    AstDeclModified_No_Inline = bit(2),
    AstDeclModified_Internal  = bit(3),
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
    assert(span1.index == span2.index);
    
    Span span;
    span.index = span1.index;
    if (span1.base < span2.base) {
        span.base = span1.base;
        span.count = (u16) (span2.base - span1.base + (u32) span2.count);
    } else {
        span.base = span2.base;
        span.count = (u16) (span1.base - span2.base + (u32) span1.count);
    }
    
    
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
        Ast* children[4];
    };
    Span span;
};

struct Ast_File {
    Ast* ast;
    
};

void
print_ast(Ast* node, Tokenizer* tokenizer, u32 spacing=0) {
    if (!node) {
        return;
    }
    
    printf("\n");
    for (u32 s = 0; s < spacing; s++) printf(" ");
    printf("(%s", ast_struct_strings[node->type]);
    
    spacing += 2;
    
    // some special cases
    if (node->type == Ast_Abi) {
        printf(" \"%s\")", node->Abi);
    } else if (node->type == Ast_Value) {
        print_value(&node->Value);
    } else if (node->type == Ast_Ident) {
        printf(" `%s`", vars_load_str(node->Ident));
    } else if (node->type == Ast_Binary_Expr) {
        assert_enum(BinaryOp, node->Binary_Expr.op);
        printf("(%s)", binary_op_strings[node->Binary_Expr.op]);
        print_ast(node->children[0], tokenizer, spacing);
        print_ast(node->children[1], tokenizer, spacing);
    } else if (node->type == Ast_Type_Decl) {
        print_ast(node->Type_Decl.type, tokenizer, spacing);
        print_ast(node->Type_Decl.stmt, tokenizer, spacing);
    } else {
        // otherwise parse all possible children
        print_ast(node->children[0], tokenizer, spacing);
        print_ast(node->children[1], tokenizer, spacing);
        print_ast(node->children[2], tokenizer, spacing);
        print_ast(node->children[3], tokenizer, spacing);
    }
    
    // HACK(alexander): this is for debugging spans
    Binary_Search_Result begin = binary_search(tokenizer->lines, (smm) node->span.base, compare_ints);
    Binary_Search_Result end = binary_search(tokenizer->lines, (smm) node->span.base + (smm) node->span.count, compare_ints);
    
    u32 beg_line = (u32) begin.index + 1;
    u32 beg_col = (u32) ((smm) node->span.base - *(smm*) begin.value) + 1;
    u32 end_line = (u32) end.index + 1;
    u32 end_col = (u32) ((smm) node->span.base + (smm) node->span.count - *(smm*) end.value) + 1;
    printf(" in examples/demo.sq:%u:%u to %u:%u", beg_line, beg_col, end_line, end_col);
    
    spacing -= 2;
    printf(")");
    if (spacing == 0) {
        printf("\n");
    }
}