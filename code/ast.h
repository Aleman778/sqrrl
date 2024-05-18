
struct Ast_Type;
struct Ast_Block;
struct Ast_Declaration;


enum Ast_Kind {    
    AST_TYPE,
    AST_IDENTIFIER,
    AST_LITERAL,
    AST_ARGUMENT,
    AST_STRUCT_LITERAL,
    AST_UNARY,
    AST_BINARY,
    AST_CAST,
    AST_CALL,
    AST_RETURN,
    
    AST_DECLARATION,
    AST_FUNCTION,
    AST_STRUCT,
    
    AST_BLOCK,
};


struct Span {
    s32 l0, l1;
    s16 c0, c1;
};

struct Ast {
    Ast_Kind kind;
    
    Span span;
};

struct Ast_Expression : Ast {
    Ast_Type* inferred_type;
};

enum Type_Storage {
    TYPE_NONE,
    
    // Basic types (order must match DEF_TYPE_KEYWORDS)
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_S8,
    TYPE_S16,
    TYPE_S32,
    TYPE_S64,
    TYPE_SMM,
    TYPE_INT,
    TYPE_U8,
    TYPE_U16,
    TYPE_U32,
    TYPE_U64,
    TYPE_UMM,
    TYPE_UINT,
    TYPE_F32,
    TYPE_F64,
    TYPE_STRING,
    TYPE_CSTRING,
    TYPE_TYPEID,
    
    TYPE_FLOAT,
    TYPE_POINTER,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_ARRAY_FIXED,
    TYPE_ARRAY_RESIZABLE,
    TYPE_ARRAY_VIEW,
    TYPE_FUNCTION
};

enum {
    TYPE_FLAG_UNSIGNED = bit(0),
    TYPE_FLAG_INTEGER = bit(1),
    TYPE_FLAG_FLOAT = bit(2),
};

struct Ast_Type : Ast_Expression {
#define AST_KIND_Ast_Type AST_TYPE
    Ast_Type* subtype;
    Type_Storage storage;
    u32 flags;
    
    s32 size;
    s32 align;
};

inline umm
get_array_element_size(Ast_Type* elem_type) {
    return align_forward(elem_type->size, elem_type->align);
}

inline Ast_Type
create_basic_type(Type_Storage storage, u32 flags, int size) {
    Ast_Type result = {};
    result.storage = storage;
    result.flags = flags;
    result.size = size;
    result.align = size;
    return result;
}

bool
is_aggregate_type(Ast_Type* type) {
    return (type->storage == TYPE_STRING ||
            type->storage == TYPE_STRUCT ||
            type->storage == TYPE_UNION ||
            type->storage == TYPE_ARRAY_VIEW ||
            type->storage == TYPE_ARRAY_RESIZABLE);
}

string
type_to_string(Ast_Type* type) {
    if (type->storage <= TYPE_TYPEID) {
        return vars_load_string(builtin_types_begin + type->storage);
    } else {
        unimplemented;
        return string_lit("?");
    }
}

// TODO(Alexander): temporary, we need to fill in sizes later for int/ smm
// NOTE(Alexander): must have the same order as Type_Storage 
Ast_Type ast_basic_types[] = {
    create_basic_type(TYPE_NONE,    0, 0),
    create_basic_type(TYPE_VOID,    0, 0),
    create_basic_type(TYPE_BOOL,    0, 1),
    create_basic_type(TYPE_S8,      TYPE_FLAG_INTEGER, 1),
    create_basic_type(TYPE_S16,     TYPE_FLAG_INTEGER, 2),
    create_basic_type(TYPE_S32,     TYPE_FLAG_INTEGER, 4),
    create_basic_type(TYPE_S64,     TYPE_FLAG_INTEGER, 8),
    create_basic_type(TYPE_SMM,     TYPE_FLAG_INTEGER, 0),
    create_basic_type(TYPE_INT,     TYPE_FLAG_INTEGER, 4),
    create_basic_type(TYPE_U8,      TYPE_FLAG_UNSIGNED | TYPE_FLAG_INTEGER, 1),
    create_basic_type(TYPE_U16,     TYPE_FLAG_UNSIGNED | TYPE_FLAG_INTEGER, 2),
    create_basic_type(TYPE_U32,     TYPE_FLAG_UNSIGNED | TYPE_FLAG_INTEGER, 4),
    create_basic_type(TYPE_U64,     TYPE_FLAG_UNSIGNED | TYPE_FLAG_INTEGER, 8),
    create_basic_type(TYPE_UMM,     TYPE_FLAG_UNSIGNED | TYPE_FLAG_INTEGER, 0),
    create_basic_type(TYPE_UINT,    TYPE_FLAG_UNSIGNED | TYPE_FLAG_INTEGER, 4),
    create_basic_type(TYPE_F32,     TYPE_FLAG_FLOAT, 4),
    create_basic_type(TYPE_F64,     TYPE_FLAG_FLOAT, 8),
    create_basic_type(TYPE_STRING,  0, 0),
    create_basic_type(TYPE_CSTRING, 0, 0),
    create_basic_type(TYPE_TYPEID,  0, 0),
    create_basic_type(TYPE_FLOAT,   TYPE_FLAG_FLOAT, 0)
};

Ast_Type* t_void = &ast_basic_types[TYPE_VOID];

internal Ast_Type 
create_void_ptr_type_definition() {
    Ast_Type result = {};
    result.storage = TYPE_POINTER;
    result.subtype = &ast_basic_types[TYPE_VOID];
    return result;
}

Ast_Type t_void_ptr_definition = create_void_ptr_type_definition();
Ast_Type* t_void_ptr = &t_void_ptr_definition;


typedef Ast_Expression* Ast_Expression_List;

struct Ast_Identifier : Ast_Type {
#define AST_KIND_Ast_Identifier AST_IDENTIFIER
    
    Identifier identifier;
};

inline Identifier
unwrap_identifier(Ast* ast) {
    assert(ast->kind == AST_IDENTIFIER);
    return ((Ast_Identifier*) ast)->identifier;
}

inline Identifier
try_unwrap_identifier(Ast* ast) {
    if (ast->kind == AST_IDENTIFIER) {
        return unwrap_identifier(ast);
    }
    return Kw_invalid;
}

struct Ast_Literal : Ast_Expression {
#define AST_KIND_Ast_Literal AST_LITERAL
    
    Type_Storage type;
    Value value;
    bool u64_overflow;
};

struct Ast_Struct_Literal : Ast_Expression {
#define AST_KIND_Ast_Struct_Literal AST_STRUCT_LITERAL
    
    Identifier identifier;
    Ast_Block* block;
};

enum Operator_Kind {
    OP_NONE = 0,
    
    // Unary
    OP_NEG,
    
    // Binary
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_SCOPE_ACCESS,
    OP_SUBSCRIPT,
};

int
get_precedence(Operator_Kind op) {
    switch (op) {
        case OP_NEG: return 13;
        case OP_ADD:
        case OP_SUB: return 10;
        
        default: return 0;
    }
}

struct Ast_Unary : Ast_Expression {
#define AST_KIND_Ast_Unary AST_UNARY
    
    Ast_Expression* subexpression;
    Token token;
    Operator_Kind op;
};

struct Ast_Binary : Ast_Expression {
#define AST_KIND_Ast_Binary AST_BINARY
    
    Ast_Expression* left;
    Ast_Expression* right;
    Identifier access_identifier;
    Token token;
    Operator_Kind op;
};

struct Ast_Cast : Ast_Expression {
#define AST_KIND_Ast_Cast AST_CAST
    
    Ast_Type* type;
    Ast_Expression* expression;
};

struct Ast_Argument : Ast_Expression {
#define AST_KIND_Ast_Argument AST_ARGUMENT
    
    Ast_Expression* expression;
    Ast_Identifier* identifier;
};

struct Ast_Call : Ast_Expression {
#define AST_KIND_Ast_Call AST_CALL
    
    Ast_Expression* func;
    array(Ast_Argument)* args;
};

struct Ast_Return : Ast_Expression {
#define AST_KIND_Ast_Return AST_RETURN
    
    Ast_Expression* expression;
};

struct Ast_Block : Ast_Expression {
#define AST_KIND_Ast_Block AST_BLOCK
    
    Ast_Block* parent;
    array(Ast_Expression*)* statements;
    map(Identifier, Ast_Declaration*)* members;
};

void add_member(Ast_Block* block, Ast_Declaration* decl);


struct Ast_Declaration : Ast_Expression {
#define AST_KIND_Ast_Declaration AST_DECLARATION
    
    Ast_Expression* type;
    Ast_Expression* initializer;
    Identifier identifier;
};

struct Ast_Function : Ast_Declaration {
#define AST_KIND_Ast_Function AST_FUNCTION
    
    Ast_Type* return_type;
    Ast_Block* args;
    Ast_Block* body;
};

struct Ast_Struct : Ast_Declaration {
#define AST_KIND_Ast_Struct AST_STRUCT
    
    Ast_Block* args;
};

struct Ast_File {
    Ast_Block block;
    
    Source_File* source_file;
};

#define push_ast_node(lexer, T, ...) (T*) \
_push_ast_node(lexer, sizeof(T), alignof(T), AST_KIND_##T, __VA_ARGS__)

inline Span
token_to_span(Token token) {
    Span result = {};
    result.l0 = token.loc.line_number;
    result.l1 = result.l0;
    result.c0 = (s16) token.loc.column_number;
    result.c1 = (s16) (result.c0 + token.source.count);
    return result;
}

inline Ast*
_push_ast_node(Lexer* lexer, umm size, umm align, Ast_Kind kind, Token* token=0) {
    if (!token) {
        token = &lexer->curr_token;
    }
    
    Ast* result = (Ast*) arena_push_size(lexer->ast_arena, size, align);
    result->kind = kind;
    result->span = token_to_span(*token);
    return result;
}

void print_ast_expression(String_Builder* sb, Ast_Expression* expr, int indent=0, bool newline=true);

void print_ast_declaration(String_Builder* sb, Ast_Declaration* decl, int indent=0);

void print_ast_file(String_Builder* sb, Ast_File* file, int indent=0);
