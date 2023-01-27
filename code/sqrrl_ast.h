
// Define the different types of nodes in the Abstract Syntax Tree
#define DEF_AST                           \
AST_GROUP(None,        "none")                  \
AST(Abi,               "abi", string)           \
AST(Value,             "value", struct {        \
Value value;                                    \
})                                              \
AST(Ident,             "identifier", string_id) \
AST(Ident_Data,        "identifier", struct {   \
string_id ident;                                \
string contents;                                \
})                                              \
AST(Argument,          "argument", struct {     \
Ast* type;                                      \
Ast* ident;                                     \
Ast* assign;                                    \
})                                              \
AST(Switch_Case,       "case", struct {         \
Ast* cond;                                      \
Ast* stmt;                                     \
})                                              \
AST(Attribute,         "attribute", struct {    \
Ast* ident;                                     \
Ast* expr;                                      \
})                                              \
AST(Compound,          "compound", struct {     \
Ast* node;                                      \
Ast* next;                                      \
})                                              \
AST_GROUP(Expr_Begin,  "expression")            \
AST(Unary_Expr,        "unary", struct {        \
Ast* first;                                     \
Unary_Op op;                                    \
})                                              \
AST(Binary_Expr,       "binary", struct {       \
Ast* first;                                     \
Ast* second;                                    \
Binary_Op op;                                   \
Type* overload;                                 \
})                                              \
AST(Ternary_Expr,      "ternary", struct {      \
Ast* first;                                     \
Ast* second;                                    \
Ast* third;                                     \
})                                              \
AST(Call_Expr,         "call", struct {         \
Ast* ident;                                     \
Ast* args;                                      \
Type* function_type;                            \
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
AST(Aggregate_Expr, "aggregate initializer", struct { \
Ast* elements;                                  \
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
Ast_Decl_Modifier mods;                         \
})                                              \
AST(Expr_Stmt,         "expression", Ast*)      \
AST(Block_Stmt,        "block", struct {        \
Ast* stmts;                                     \
Ast* context;                                   \
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
Ast* stmt;                                      \
})                                              \
AST(If_Stmt,           "if", struct {           \
Ast* cond;                                      \
Ast* then_block;                                \
Ast* else_block;                                \
})                                              \
AST(Switch_Stmt,       "switch", struct {       \
Ast* cond;                                      \
Ast* cases;                                \
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
b32 is_dynamic;                                \
})                                              \
AST(Pointer_Type,      "pointer", Ast*)         \
AST(Tuple_Type,        "tuple", struct {        \
Ast* elem_types;                                \
})                                              \
AST(Infer_Type,        "infer", void*)          \
AST(Function_Type,     "function", struct {     \
Ast* ident;                                     \
Ast* return_type;                               \
Ast* arguments;                                 \
Ast* attributes;                                \
Binary_Op overload_operator;                    \
Ast_Decl_Modifier mods;                         \
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
AST(Const_Type,        "const", Ast*)           \
AST(Volatile_Type,     "volatile", Ast*)        \
AST(Declspec_Type,     "declspec", struct {     \
Ast* type; \
Ast* spec; \
})        \
AST(Typedef,           "typedef", struct {      \
Ast* type;                                      \
Ast* ident;                                     \
})                                              \
AST_GROUP(Type_End,    "type")

// NOTE(Alexander): iterate through a compound AST node, usage:
// Ast* compound = parse_compound(interp, ...)
// compound_iterator(compound, it) {
//     // `it` can be used as the ast node pointer
// }
// TODO(Alexander): this is really ugly and inefficient, try similar to C++ iterators.
#define for_compound(compound, it) \
Ast* compound_##it = compound; \
if (compound_##it) \
for (Ast* it = compound_##it->Compound.node; \
compound_##it&& compound_##it->kind == Ast_Compound && compound_##it->Compound.node && compound_##it->Compound.node->kind != Ast_None; \
compound_##it = compound_##it->Compound.next, it = compound_##it->Compound.node)

global cstring ast_strings[] = {
#define AST(symbol, name, decl) name,
#define AST_GROUP(symbol, name) name,
    DEF_AST
#undef AST_GROUP
#undef AST
};

global cstring ast_struct_strings[] = {
#define AST(symbol, ...) #symbol,
#define AST_GROUP(symbol, ...) #symbol,
    DEF_AST
#undef AST_GROUP
#undef AST
};

enum Ast_Kind {
#define AST(symbol, ...) Ast_##symbol,
#define AST_GROUP(symbol, ...) Ast_##symbol,
    DEF_AST
#undef AST_GROUP
#undef AST
};

typedef s32 Ast_Decl_Modifier;
enum {
    AstDeclModifier_None           = 0,
    AstDeclModifier_Inline         = bit(1),
    AstDeclModifier_Always_Inline  = bit(2),
    AstDeclModifier_No_Inline      = bit(3),
    AstDeclModifier_Internal       = bit(4),
    AstDeclModifier_External       = bit(5),
    AstDeclModifier_Global         = bit(6),
    AstDeclModifier_Const          = bit(7),
    AstDeclModifier_Cconv_cdecl    = bit(8),
    AstDeclModifier_Cconv_fastcall = bit(9),
    AstDeclModifier_Cconv_stdcall  = bit(10),
};

union Span {
    struct {
        u32 offset;
        u16 count;
        u16 file_index;
    };
    u64 packed;
};

inline Span
create_span(u32 offset, u16 count, u16 file_index) {
    Span span;
    span.offset = offset;
    span.count = count;
    span.file_index = file_index;
    return span;
}

struct Span_Data {
    u32 begin_line;
    u32 begin_column;
    u32 end_line;
    u32 end_column;
    u32 offset;
    u32 count;
    u32 file_index;
};

Span_Data
calculate_span_data(array(smm)* lines, Span span) {
    smm offset = span.offset;
    
    pln("line count = %", f_int(array_count(lines)));
    
    Binary_Search_Result begin = binary_search(lines, offset, compare_smm);
    smm span_end = (smm) span.offset + (smm) span.count;
    Binary_Search_Result end = binary_search(lines, span_end, compare_smm);
    
    if (begin.index > 0) {
        begin.value = (smm*) begin.value - 1;
    }
    
    Span_Data result;
    result.begin_line = (u32) begin.index;
    result.begin_column = (u32) ((smm) span.offset - *((smm*) begin.value));
    result.end_line = (u32) end.index;
    result.end_column = (u32) ((smm) span.offset + (smm) span.count - *(smm*) end.value);
    result.file_index = span.file_index;
    return result;
}

global const Span empty_span = { 0 };

inline Span 
token_to_span(Token token) {
    // TODO(Alexander): make sure that numbers fit in their size
    return { (u32) token.offset, (u16) token.source.count, (u16) token.file_index };
}

inline Span
span_combine(Span span1, Span span2) {
    assert(span1.file_index == span2.file_index);
    
    Span span;
    span.file_index = span1.file_index;
    if (span1.offset < span2.offset) {
        span.offset = span1.offset;
        span.count = (u16) (span2.offset - span1.offset + (u32) span2.count);
    } else {
        span.offset = span2.offset;
        span.count = (u16) (span1.offset - span2.offset + (u32) span1.count);
    }
    return span;
}

struct Ast {
    Ast_Kind kind;
    union {
#define AST(symbol, name, decl) decl symbol;
#define AST_GROUP(...)
        DEF_AST 
#undef AST_GROUP
#undef AST
        Ast* children[5];
    };
    Type* type;
    Token token;
    Span span;
};

typedef map(string_id, Ast*) Ast_Decl_Table;

struct Compilation_Unit {
    Ast* ast;
    string_id ident;
    
    Ic_Arg ic_return;
    Intermediate_Code *ic_first, *ic_last;
    Ic_Basic_Block *bb_first, *bb_last;
    Ic_Basic_Block *bb_return;
    Ic_Basic_Block *bb_data;
    Ic_Arg_Map* locals;
    Ic_Arg_Map* globals;
    
    s64 stk_args = 0;
    s64 stk_locals = 0;
    s64 stk_caller_args = 0;
    array(Ic_Stk_Entry)* stk_entries;
    s64 stk_usage;
};

struct Ast_File {
    Ast_Decl_Table* decls;
    Ast* static_block_first; // compound, start of linked list
    Ast* static_block_last; // compound, end of linked list
    s32 error_count;
    
    array(Compilation_Unit)* units;
};

inline string_id
ast_unwrap_ident(Ast* ast_ident) {
    assert(ast_ident->kind == Ast_Ident);
    return ast_ident->Ident;
}

inline string_id
try_unwrap_ident(Ast* ast_ident) {
    if (ast_ident && ast_ident->kind == Ast_Ident) {
        return ast_unwrap_ident(ast_ident);
    }
    return Kw_invalid;
}

inline bool
should_ast_stmt_end_with_semicolon(Ast* node) {
    return !(node->kind == Ast_Block_Stmt ||
             node->kind == Ast_Decl_Stmt ||
             node->kind == Ast_If_Stmt ||
             node->kind == Ast_Switch_Stmt ||
             node->kind == Ast_For_Stmt ||
             node->kind == Ast_While_Stmt);
}

inline bool
is_ast_none(Ast* ast) {
    return ast && ast->kind == Ast_None;
}

inline bool
is_valid_ast(Ast* ast) {
    return ast && ast->kind != Ast_None;
}

inline bool
is_ast_expr(Ast* ast) {
    return ast && ast->kind > Ast_Expr_Begin && ast->kind < Ast_Expr_End;
}

inline bool
is_ast_stmt(Ast* ast) {
    return ast && ast->kind > Ast_Stmt_Begin && ast->kind < Ast_Stmt_End;
}

inline bool
is_ast_type(Ast* ast) {
    return ast && ast->kind > Ast_Type_Begin && ast->kind < Ast_Type_End;
}

inline bool
is_ast_compound(Ast* ast) {
    return ast && ast->kind == Ast_Compound;
}

void
string_builder_push(String_Builder* sb, Ast_Decl_Modifier mods) {
    if (mods > 0) {
        string_builder_push(sb, "( ");
        if (is_bitflag_set(mods, AstDeclModifier_Inline)) {
            string_builder_push(sb, "inline ");
        }
        if (is_bitflag_set(mods, AstDeclModifier_No_Inline)) {
            string_builder_push(sb, "no_inline ");
        }
        if (is_bitflag_set(mods, AstDeclModifier_Internal)) {
            string_builder_push(sb, "internal ");
        }
        if (is_bitflag_set(mods, AstDeclModifier_Global)) {
            string_builder_push(sb, "global ");
        }
        string_builder_push(sb, ")");
    }
}

void
string_builder_push(String_Builder* sb, Ast* node, Tokenizer* tokenizer, u32 spacing=0) {
    if (!node) {
        return;
    }
    
    string_builder_push(sb, "\n");
    for (u32 s = 0; s < spacing; s++) string_builder_push(sb, " ");
    string_builder_push_format(sb, "(%", f_cstring(ast_struct_strings[node->kind]));
    
    if (node->type) {
        string_builder_push_format(sb, " <%>", f_type(node->type));
    }
    
    spacing += 2;
    
    // some special cases
    switch (node->kind) {
        case Ast_Abi: {
            string_builder_push_format(sb, " \"%\")", f_string(node->Abi));
        } break;
        
        case Ast_Value: {
            string_builder_push(sb, " ");
            string_builder_push(sb, &node->Value.value);
            
        } break;
        
        case Ast_Ident: {
            string_builder_push_format(sb, " `%` (%)", f_string(vars_load_string(node->Ident)),
                                       f_u32(node->Ident));
        } break;
        
        case Ast_Attribute: {
            string_builder_push(sb, node->Attribute.expr, tokenizer, spacing);
        } break;
        
        case Ast_Assign_Stmt: {
            string_builder_push(sb, node->Assign_Stmt.mods);
            string_builder_push(sb, node->Assign_Stmt.type, tokenizer, spacing);
            string_builder_push(sb, node->Assign_Stmt.ident, tokenizer, spacing);
            string_builder_push(sb, node->Assign_Stmt.expr, tokenizer, spacing);
        } break;
        
        case Ast_Unary_Expr: {
            assert_enum(UnaryOp, node->Unary_Expr.op);
            string_builder_push_format(sb, " (%)", f_cstring(unary_op_strings[node->Unary_Expr.op]));
            string_builder_push(sb, node->Unary_Expr.first, tokenizer, spacing);
        } break;
        
        case Ast_Binary_Expr: {
            assert_enum(BinaryOp, node->Binary_Expr.op);
            string_builder_push_format(sb, " (%)", f_cstring(binary_op_strings[node->Binary_Expr.op]));
            if (node->Binary_Expr.overload) { 
                string_builder_push(sb, "\n");
                for (u32 s = 0; s < spacing; s++) string_builder_push(sb, " ");
                string_builder_push_format(sb, "(");
                string_builder_push(sb, node->Binary_Expr.overload);
                string_builder_push_format(sb, ")");
            }
            string_builder_push(sb, node->Binary_Expr.first, tokenizer, spacing);
            string_builder_push(sb, node->Binary_Expr.second, tokenizer, spacing);
        } break;
        
        case Ast_Call_Expr: {
            string_builder_push(sb, node->Call_Expr.ident, tokenizer, spacing);
            string_builder_push(sb, node->Call_Expr.args, tokenizer, spacing);
        } break;
        
        case Ast_Compound: {
            for_compound(node, child_node) {
                string_builder_push(sb, child_node, tokenizer, spacing);
            }
        } break;
        
        case Ast_Function_Type: {
            string_builder_push(sb, node->Function_Type.mods);
            string_builder_push(sb, node->Function_Type.return_type, tokenizer, spacing);
            string_builder_push(sb, node->Function_Type.ident, tokenizer, spacing);
            string_builder_push(sb, node->Function_Type.attributes, tokenizer, spacing);
            string_builder_push(sb, node->Function_Type.arguments, tokenizer, spacing);
        } break;
        
        default: {
            // otherwise parse all possible children
            string_builder_push(sb, node->children[0], tokenizer, spacing);
            string_builder_push(sb, node->children[1], tokenizer, spacing);
            string_builder_push(sb, node->children[2], tokenizer, spacing);
            string_builder_push(sb, node->children[3], tokenizer, spacing);
            string_builder_push(sb, node->children[4], tokenizer, spacing);
        } break;
    }
    
#if 0
    // HACK(alexander): this is for debugging spans
    Span_Data span = calculate_span_data(tokenizer->lines, node->span);
    printf(" in examples/demo.sq:%u:%u to %u:%u", span.begin_line, span.begin_col, span.end_line, span.end_col);
#endif
    
    spacing -= 2;
    string_builder_push(sb, ")");
    if (spacing == 0) {
        string_builder_push(sb, "\n");
    }
}

void
print_ast(Ast* node, Tokenizer* t) {
    String_Builder sb = {};
    string_builder_alloc(&sb, 1000);
    string_builder_push(&sb, node, t);
    string result = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(result));
    string_builder_free(&sb);
}
