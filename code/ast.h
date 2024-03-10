
enum Ast_Kind {    
    AST_TYPE,
    AST_IDENTIFIER,
    AST_LITERAL,
    AST_UNARY,
    AST_BINARY,
    AST_CAST,
    AST_DECLARATION,
    Ast_IDENT,
    AST_CALL,
    AST_RETURN,
    
    AST_BLOCK,
    
};

struct Ast {
    Ast_Kind kind;
};

struct Ast_Type;

struct Ast_Expression : Ast {
    Ast_Type* inferred_type;
};

enum Ast_Type_Kind {
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
    
    TYPE_ALIAS,
    TYPE_POINTER,
    TYPE_PROCEDURE,
    TYPE_LIKE_PROCEDURE
};

enum {
    TYPE_FLAG_CONST = bit(0),
    TYPE_FLAG_INTEGER = bit(1),
    TYPE_FLAG_UNSIGNED = bit(2),
    TYPE_FLAG_FLOAT = bit(3),
};

struct Ast_Type : Ast_Expression {
#define AST_KIND_Ast_Type AST_TYPE
    
    Ast_Type_Kind kind;
    u32 flags;
    Identifier alias;
    
    s32 size;
    s32 align;
};

struct Ast_Argument {
    Ast_Type* type;
    Identifier identifier;
    Ast_Expression* initializer;
};

typedef Ast_Argument Ast_Argument_List;

struct Ast_Procedure_Type : Ast_Type {
#define AST_KIND_Ast_Procedure_Type AST_TYPE
    
    Ast_Type* return_type;
    Ast_Argument_List* args;
};

inline Ast_Type
create_basic_type(Ast_Type_Kind kind, u32 flags, int size) {
    Ast_Type result = {};
    result.kind = kind;
    result.flags = flags;
    result.size = size;
    result.align = size;
    return result;
}

// TODO(Alexander): temporary, we need to fill in sizes later for int/ smm
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

typedef Ast_Expression* Ast_Expression_List;

struct Ast_Identifier : Ast_Expression {
#define AST_KIND_Ast_Identifier AST_IDENTIFIER
    
    Identifier identifier;
};

struct Ast_Literal : Ast_Expression {
#define AST_KIND_Ast_Literal AST_LITERAL
    
    Ast_Type_Kind type;
    union {
        u64 u64_value;
        f32 f32_value;
        f64 f64_value;
    };
};

enum {
    OP_NONE = 0,
    
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_SCOPE_ACCESS,
    OP_SUBSCRIPT,
};

struct Ast_Unary : Ast_Expression {
#define AST_KIND_Ast_Unary AST_UNARY
    
    Ast_Expression* left;
    Token token;
    int operator_type;
};

struct Ast_Binary : Ast_Expression {
#define AST_KIND_Ast_Binary AST_BINARY
    Ast_Expression* left;
    Ast_Expression* right;
    Identifier access_identifier;
    Token token;
    int operator_type;
};

struct Ast_Block : Ast_Expression {
#define AST_KIND_Ast_Block AST_BLOCK
    
    array(Ast_Expression*)* statements;
};

struct Ast_Cast : Ast_Expression {
#define AST_KIND_Ast_Cast AST_CAST
    
    Ast_Type* type;
    Ast_Expression* expr;
};

struct Ast_Call : Ast_Expression {
#define AST_KIND_Ast_Call AST_CALL
    
    Ast_Expression* proc;
    Ast_Argument_List* args;
};

struct Ast_Return : Ast_Expression {
#define AST_KIND_Ast_Return AST_RETURN
    
    Ast_Expression* expr;
};

struct Ast_Declaration : Ast_Expression {
#define AST_KIND_Ast_Declaration AST_DECLARATION
    
    Ast_Expression* type;
    Ast_Expression* initializer;
    Identifier identifier;
};

#define push_ast_node(lexer, T) (T*) \
_push_ast_node(lexer, sizeof(T), alignof(T), AST_KIND_##T)

inline Ast*
_push_ast_node(Lexer* lexer, umm size, umm align, Ast_Kind kind) {
    Ast* result = (Ast*) arena_push_size(lexer->ast_arena, size, align);
    result->kind = kind;
    return result;
}

void print_ast_expression(String_Builder* sb, Ast_Expression* expr, int indent=0, bool newline=true);

void print_ast_declaration(String_Builder* sb, Ast_Declaration* decl, int indent=0);
