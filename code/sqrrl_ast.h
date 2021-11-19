
// Define the different types of nodes in the Abstract Syntax Tree
#define DEF_AST_TYPES                           \
AST_GROUP(None,        "none")                  \
AST(Abi,               "abi", string)           \
AST(Value,             "value", Value)          \
AST(Ident,             "identifier", string_id) \
AST(Decl, "top level declaration", struct {  \
Ast* ident;                                     \
Ast* stmt;                                      \
Ast_Decl_Modifier mods;                         \
})                                              \
AST(Argument,          "argument", struct {     \
Ast* type;                                      \
Ast* ident;                                     \
Ast* assign;                                    \
})                                              \
AST(Compound,          "compound", struct {     \
Ast* node;                                      \
Ast* next;                                      \
})                                              \
AST_GROUP(Expr_Begin,  "expression")            \
AST(Unary_Expr,        "unary", struct {        \
Unary_Op op;                                    \
Ast* first;                                     \
})                                              \
AST(Binary_Expr,       "binary", struct {       \
Binary_Op op;                                   \
Ast* first;                                     \
Ast* second;                                    \
})                                              \
AST(Ternary_Expr,      "ternary", struct {      \
Ast* first;                                     \
Ast* second;                                    \
Ast* third;                                     \
})                                              \
AST(Call_Expr,         "call", struct {         \
Ast* ident;                                     \
Ast* args;                                      \
})                                              \
AST(Field_Expr,        "field", struct {        \
Ast* var;                                       \
Ast* field;                                     \
})                                              \
AST(Cast_Expr,         "cast", struct {         \
Ast* type;                                      \
Ast* expr;                                      \
})                                              \
AST(Paren_Expr,        "parentheses", struct {  \
Ast* expr;                                      \
})                                              \
AST(Index_Expr,        "index", struct {        \
Ast* array;                                     \
Ast* index;                                     \
})                                              \
AST(Array_Expr,        "array", struct {        \
Ast* elements;                                  \
})                                              \
AST(Struct_Expr,       "struct", struct {       \
Ast* ident;                                     \
Ast* fields;                                    \
})                                              \
AST(Tuple_Expr,        "tuple", struct {        \
Ast* values;                                    \
})                                              \
AST_GROUP(Expr_End,    "expression")            \
AST_GROUP(Stmt_Begin,  "statement")             \
AST(Assign_Stmt,       "assignment", struct {   \
Ast* type;                                      \
Ast* ident;                                     \
Ast* expr;                                      \
})                                              \
AST(Expr_Stmt,         "expression", Ast*)      \
AST(Block_Stmt,        "block", struct {        \
Ast* stmts;                                     \
})                                              \
AST(Break_Stmt,        "break", struct {        \
Ast* ident;                                     \
})                                              \
AST(Continue_Stmt,     "continue", struct {     \
Ast* ident;                                     \
})                                              \
AST(Decl_Stmt,         "declaration", struct {  \
Ast* ident;                                     \
Ast* type;                                      \
Ast* decl;                                      \
})                                              \
AST(If_Stmt,           "if", struct {           \
Ast* cond;                                      \
Ast* then_block;                                \
Ast* else_block;                                \
})                                              \
AST(For_Stmt,          "for", struct {          \
Ast* label;                                     \
Ast* init;                                      \
Ast* cond;                                      \
Ast* update;                                    \
Ast* block;                                     \
})                                              \
AST(While_Stmt,        "while", struct {        \
Ast* label;                                     \
Ast* cond;                                      \
Ast* block;                                     \
})                                              \
AST(Return_Stmt,       "return", struct {       \
Ast* expr;                                      \
})                                              \
AST_GROUP(Stmt_End,    "statement")             \
AST_GROUP(Type_Begin,  "type")                  \
AST(Named_Type,        "named", Ast*)           \
AST(Array_Type,        "array", struct {        \
Ast* elem_type;                                 \
Ast* shape;                                     \
})                                              \
AST(Pointer_Type,      "pointer", Ast*)         \
AST(Tuple_Type,        "tuple", struct {        \
Ast* elem_types;                                \
})                                              \
AST(Infer_Type,        "infer", void*)          \
AST(Function_Type,     "function", struct {     \
Ast* ident;                                     \
Ast* return_type;                               \
Ast* arg_types;                                 \
})                                              \
AST(Struct_Type,       "struct", struct {       \
Ast* ident;                                     \
Ast* fields;                                    \
})                                              \
AST(Union_Type,        "union", struct {        \
Ast* ident;                                     \
Ast* fields;                                    \
})                                              \
AST(Enum_Type,         "enum", struct {         \
Ast* ident;                                     \
Ast* elem_type;                                 \
Ast* fields;                                    \
})                                              \
AST(Typedef,           "typedef", struct {      \
Ast* type;                                      \
Ast* ident;                                     \
})                                              \
AST_GROUP(Type_End,    "type")

// NOTE(Alexander): iterate through a compound ast node, usage:
// Ast* compound = parse_compound(interp, ...)
// compound_iterator(compound, it) {
//     // `it` can be used as the ast node pointer
// }
#define compound_iterator(compound, it) \
for (Ast* it = compound->Compound.node; \
compound && compound->Compound.next->type == Ast_Compound; \
compound = compound->Compound.next, it = compound->Compound.node)

global cstring ast_strings[] = {
#define AST(symbol, name, decl) name,
#define AST_GROUP(symbol, name) name,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

global cstring ast_struct_strings[] = {
#define AST(symbol, ...) "Ast_" #symbol,
#define AST_GROUP(symbol, ...) "Ast_" #symbol,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

enum Ast_Type {
#define AST(symbol, ...) Ast_##symbol,
#define AST_GROUP(symbol, ...) Ast_##symbol,
    DEF_AST_TYPES
#undef AST_GROUP
#undef AST
};

typedef s32 Ast_Decl_Modifier;
enum {
    AstDeclModifier_None      = 0,
    AstDeclModifier_Inline    = bit(1),
    AstDeclModifier_No_Inline = bit(2),
    AstDeclModifier_Internal  = bit(3),
    AstDeclModifier_Global    = bit(4),
};

union Span {
    struct {
        u32 base;
        u16 count;
        u16 index;
    };
    u64 packed;
};

struct Span_Data {
    u32 begin_line;
    u32 begin_col;
    u32 end_line;
    u32 end_col;
};

Span_Data
calculate_span_data(smm* lines, Span span) {
    Binary_Search_Result begin = binary_search(lines, (smm) span.base, compare_smm);
    smm span_end = (smm) span.base + (smm) span.count;
    Binary_Search_Result end = binary_search(lines, end, compare_smm);
    
    Span_Data result;
    result.begin_line = (u32) begin.index + 1;
    result.begin_col = (u32) ((smm) span.base - *(smm*) begin.value) + 1;
    result.end_line = (u32) end.index + 1;
    result.end_col = (u32) ((smm) span.base + (smm) span.count - *(smm*) end.value) + 1;
    return result;
}

global const Span empty_span = { 0 };

inline Span 
token_to_span(Token token) {
    return { (u32) token.offset, (u16) string_count(token.source), 0 }; // TODO(alexander): what should index be?
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
        Ast* children[5];
    };
    Span span;
};

struct Ast_Decl_Entry {
    string_id key;
    Ast* value;
};

struct Ast_File {
    Ast_Decl_Entry* decls;
    s32 error_count;
};

inline bool
is_ast_decl(Ast* ast) {
    return ast->type == Ast_Decl;
}

inline bool
is_ast_expr(Ast* ast) {
    return ast->type > Ast_Expr_Begin && ast->type < Ast_Expr_End;
}

inline bool
is_ast_stmt(Ast* ast) {
    return ast->type > Ast_Stmt_Begin && ast->type < Ast_Stmt_End;
}

inline bool
is_ast_type(Ast* ast) {
    return ast->type > Ast_Type_Begin && ast->type < Ast_Type_End;
}

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
        printf(" ");
        print_value(&node->Value);
    } else if (node->type == Ast_Ident) {
        printf(" `%s`", vars_load_string(node->Ident));
    } else if (node->type == Ast_Unary_Expr) {
        assert_enum(UnaryOp, node->Unary_Expr.op);
        printf("(%s)", unary_op_strings[node->Binary_Expr.op]);
        print_ast(node->Unary_Expr.first, tokenizer, spacing);
    } else if (node->type == Ast_Binary_Expr) {
        assert_enum(BinaryOp, node->Binary_Expr.op);
        printf("(%s)", binary_op_strings[node->Binary_Expr.op]);
        print_ast(node->Binary_Expr.first, tokenizer, spacing);
        print_ast(node->Binary_Expr.second, tokenizer, spacing);
    } else if (node->type == Ast_Decl) {
        if (node->Decl.mods > 0) {
            printf("( ");
            if (is_bitflag_set(node->Decl.mods, AstDeclModifier_Inline)) {
                printf("inline ");
            }
            if (is_bitflag_set(node->Decl.mods, AstDeclModifier_No_Inline)) {
                printf("no_inline ");
            }
            if (is_bitflag_set(node->Decl.mods, AstDeclModifier_Internal)) {
                printf("internal ");
            }
            if (is_bitflag_set(node->Decl.mods, AstDeclModifier_Global)) {
                printf("global ");
            }
            printf(")");
        }
        print_ast(node->Decl.stmt, tokenizer, spacing);
    } else {
        // otherwise parse all possible children
        print_ast(node->children[0], tokenizer, spacing);
        print_ast(node->children[1], tokenizer, spacing);
        print_ast(node->children[2], tokenizer, spacing);
        print_ast(node->children[3], tokenizer, spacing);
    }
    
#if 0
    // HACK(alexander): this is for debugging spans
    Span_Data span = calculate_span_data(tokenizer->lines, node->span);
    printf(" in examples/demo.sq:%u:%u to %u:%u", span.begin_line, span.begin_col, span.end_line, span.end_col);
#endif
    
    spacing -= 2;
    printf(")");
    if (spacing == 0) {
        printf("\n");
    }
}
