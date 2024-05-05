
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
    Ast_IDENT,
    AST_PROCEDURE_CALL,
    AST_RETURN,
    
    AST_DECLARATION,
    AST_PROCEDURE,
    AST_STRUCT,
    
    AST_BLOCK,
    
};

struct Span {
    Location loc;
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
    TYPE_POINTER
};

enum {
    TYPE_FLAG_CONST = bit(0),
    TYPE_FLAG_INTEGER = bit(1),
    TYPE_FLAG_UNSIGNED = bit(2),
    TYPE_FLAG_FLOAT = bit(3),
};

struct Ast_Type : Ast_Expression {
#define AST_KIND_Ast_Type AST_TYPE
    Type_Storage storage;
    u32 flags;
    
    s32 size;
    s32 align;
};

inline Ast_Type
create_basic_type(Type_Storage storage, u32 flags, int size) {
    Ast_Type result = {};
    result.storage = storage;
    result.flags = flags;
    result.size = size;
    result.align = size;
    return result;
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
    union {
        u64 u64_value;
        f32 f32_value;
        f64 f64_value;
    };
};

struct Ast_Struct_Literal : Ast_Expression {
#define AST_KIND_Ast_Struct_Literal AST_STRUCT_LITERAL
    
    Identifier identifier;
    Ast_Block* block;
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

struct Ast_Procedure_Call : Ast_Expression {
#define AST_KIND_Ast_Procedure_Call AST_PROCEDURE_CALL
    
    Ast_Expression* proc;
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

struct Ast_Procedure : Ast_Declaration {
#define AST_KIND_Ast_Procedure AST_PROCEDURE
    
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

void print_ast_file(String_Builder* sb, Ast_File* file, int indent=0);
