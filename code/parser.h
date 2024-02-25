
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
    
    Type_Basic,
    Type_Alias,
    Type_Proc,
    Type_Aggregate,
};

enum Type_Flags {
    TypeFlag_Const
};

struct Ast_Type {
    Ast_Type_Kind kind;
    u32 flags;
};

struct Ast_Basic_Type : Ast_Type {
};

struct Ast_Alias_Type : Ast_Type {
    Identifier ident;
};

struct Ast_Proc_Argument {
    Ast_Type* type;
    Identifier ident;
    Ast_Proc_Argument* next;
};

struct Ast_Proc_Type : Ast_Type  {
    Ast_Type* return_type;
    Ast_Proc_Argument* args;
};

struct Ast_Aggregate_Type : Ast_Type {
    
};


// Expressions (statements are stored as expressions)
enum Ast_Expression_Kind {
    // Expressions
    Expr_Literal,
    Expr_Assign,
    Expr_Binary,
    Expr_Block,
    Expr_Call,
    Expr_Field,
    Expr_Ident,
    Expr_Index,
    Expr_Return,
};

struct Ast_Expression {
    Ast_Expression_Kind kind;
};

struct Ast_Expression_List {
    Ast_Expression* expr;
    Ast_Expression_List* next;
};

struct Ast_Assign_Expression : Ast_Expression {
    Ast_Type* type;
    Ast_Expression* expr;
    Identifier ident;
};

struct Ast_Literal_Expression : Ast_Expression {
    Ast_Type_Kind type;
    union {
        u64 u64_value;
        f32 f32_value;
        f64 f64_value;
    };
};

enum Binary_Operator {
    Binary_Add,
    Binary_Sub,
    Binary_Mul,
    Binary_Div,
};

struct Ast_Binary_Operator {
    int prec;
    bool is_valid;
};

struct Ast_Binary_Expression: Ast_Expression {
    Ast_Expression* left;
    Ast_Expression* right;
    Token token;
};

struct Ast_Block_Expression : Ast_Expression {
    Ast_Expression_List* list;
};

struct Ast_Call_Argument {
    Ast_Expression* expr;
    Ast_Call_Argument* next;
};

struct Ast_Call_Expression : Ast_Expression {
    Ast_Expression* proc;
    Ast_Call_Argument* args;
};

struct Ast_Field_Expression : Ast_Expression {
    Ast_Expression* expr;
    Identifier ident;
};

struct Ast_Ident_Expression : Ast_Expression {
    Identifier ident;
};

struct Ast_Index_Expression : Ast_Expression {
    Ast_Expression* expr;
    Ast_Expression* index;
};

struct Ast_Return_Expression : Ast_Expression {
    Ast_Expression* expr;
};

struct Ast_Proc_Declaration {
    Ast_Expression_List* block;
    Ast_Proc_Type signature;
    Identifier ident;
};

struct Ast_Type_Declaration {
    Ast_Type* type;
    Identifier ident;
};

enum Ast_Declaration_Kind {
    Decl_Procedure,
    Decl_Type,
};

struct Ast_Declaration {
    Ast_Declaration_Kind kind;
    
    union {
        Ast_Proc_Declaration proc;
        Ast_Type_Declaration type;
    };
};

#define push_ast_basic_type(lexer, kind) \
push_ast_type_node(lexer, sizeof(Ast_Type), alignof(Ast_Type), kind)


#define push_ast_type(lexer, kind) (Ast_##kind##_Type*) \
push_ast_type_node(lexer, sizeof(Ast_##kind##_Type), alignof(Ast_##kind##_Type), Type_##kind)

inline Ast_Type*
push_ast_type_node(Lexer* lexer, umm size, umm align, Ast_Type_Kind kind) {
    Ast_Type* result = (Ast_Type*) arena_push_size(lexer->ast_arena, size, align);
    result->kind = kind;
    return result;
}

#define push_ast_expression(lexer, kind) (Ast_##kind##_Expression*) \
push_ast_expression_node(lexer, sizeof(Ast_##kind##_Expression), alignof(Ast_##kind##_Expression), Expr_##kind)

inline Ast_Expression*
push_ast_expression_node(Lexer* lexer, umm size, umm align, Ast_Expression_Kind kind) {
    Ast_Expression* result = (Ast_Expression*) arena_push_size(lexer->ast_arena, size, align);
    result->kind = kind;
    return result;
}

inline Ast_Declaration*
push_ast_declaration(Lexer* lexer, Ast_Declaration_Kind kind) {
    Ast_Declaration* result = arena_push_struct(lexer->ast_arena, Ast_Declaration);
    result->kind = kind;
    return result;
}

Ast_Expression* parse_expression(Lexer* lexer, int min_prec=0);

//struct Ast_Proc_Argument {
//Ast_Type* type;
//Identifier ident;
//Ast_Proc_Argument* next;
//};

//struct Ast_Proc_Type {
//Ast_Proc_Argument* args;
//Ast_Type* return_type;
//};

inline void
string_builder_push_newline(String_Builder* sb, int num_spaces) {
    string_builder_push_char(sb, '\n');
    for (int i = 0; i < num_spaces; i++) string_builder_push_char(sb, ' ');
}

void
print_ast_type_kind(String_Builder* sb, Ast_Type_Kind kind) {
    switch (kind) {
        case Type_Void: {
            string_builder_push(sb, "void");
        } break;
        
        case Type_Int: {
            string_builder_push(sb, "int");
        } break;
    }
    
}

inline void
print_ast_type(String_Builder* sb, Ast_Type* type) {
    print_ast_type_kind(sb, type->kind);
}

inline void print_ast_expression(String_Builder* sb, Ast_Expression* expr, int indent=0, bool newline=true);

inline void
print_call_arguments(String_Builder* sb, Ast_Call_Argument* arg, int indent) {
    if (!arg) {
        string_builder_push(sb, " null");
    }
    while (arg) {
        string_builder_push_newline(sb, indent);
        string_builder_push(sb, "- ");
        print_ast_expression(sb, arg->expr, indent + 2, false);
        arg = arg->next;
    }
}

inline void
print_ast_expression(String_Builder* sb, Ast_Expression* expr, int indent, bool newline) {
    if (!expr) {
        string_builder_push(sb, "null");
        return;
    }
    
    if (newline) {
        string_builder_push_newline(sb, indent);
    }
    
    switch (expr->kind) {
        case Expr_Literal: {
            auto literal = (Ast_Literal_Expression*) expr;
            string_builder_push_format(sb, "Expr_Literal:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "type: ");
            print_ast_type_kind(sb, literal->type);
            
            // TODO(Alexander): float support
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "value: %", f_u64(literal->u64_value));
        } break;
        
        case Expr_Assign: {
            auto assign = (Ast_Assign_Expression*) expr;
            string_builder_push(sb, "Expr_Assign:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "type: ");
            print_ast_type(sb, assign->type);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "ident: %", f_ident(assign->ident));
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "expr: ");
            print_ast_expression(sb, assign->expr, indent + 4);
        } break;
        
        case Expr_Call: {
            auto call = (Ast_Call_Expression*) expr;
            string_builder_push(sb, "Expr_Call:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "proc: ");
            print_ast_expression(sb, call->proc, indent + 4);
            
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "args: ");
            print_call_arguments(sb, call->args, indent + 4);
        } break;
        
        case Expr_Ident: {
            auto ident = (Ast_Ident_Expression*) expr;
            string_builder_push(sb, "Expr_Ident:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "ident: %", f_ident(ident->ident));
        } break;
        
        case Expr_Return: {
            auto ret = (Ast_Return_Expression*) expr;
            string_builder_push(sb, "Expr_Return:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "expr: ");
            print_ast_expression(sb, ret->expr, indent + 4);
        } break;
    }
}

inline void
print_ast_proc_signature(String_Builder* sb, Ast_Proc_Type* proc, int indent=0) {
    string_builder_push_newline(sb, indent);
    string_builder_push(sb, "return_type: ");
    print_ast_type(sb, proc->return_type);
    
    string_builder_push_newline(sb, indent);
    string_builder_push(sb, "args:");
    
    Ast_Proc_Argument* arg = proc->args;
    if (!arg) {
        string_builder_push(sb, " null");
    }
    while (arg) {
        string_builder_push_newline(sb, indent + 2);
        string_builder_push_format(sb, "- %: ", f_ident(arg->ident));
        print_ast_type(sb, arg->type);
        arg = arg->next;
    }
    
#if 0
    print_ast_type(sb, proc->return_type);
    string_builder_push_format(sb, " %(", f_ident(ident));
    
    Ast_Proc_Argument* arg = proc->args;
    while (arg) {
        print_ast_type(sb, arg->type);
        arg = arg->next;
        if (arg) {
            string_builder_push(sb, ", ");
        }
    }
    string_builder_push(sb, ")");
#endif
}

inline void
print_ast_expression_list(String_Builder* sb, Ast_Expression_List* list, int indent=0) {
    while (list) {
        string_builder_push_newline(sb, indent);
        string_builder_push(sb, "- ");
        print_ast_expression(sb, list->expr, indent + 2, false);
        list = list->next;
    }
}

void
print_ast_declaration(String_Builder* sb, Ast_Declaration* decl) {
    switch (decl->kind) {
        case Decl_Procedure: {
            Ast_Proc_Declaration proc = decl->proc;
            string_builder_push(sb, "Decl_Procedure:");
            
            string_builder_push_newline(sb, 2);
            string_builder_push_format(sb, "ident: %", f_ident(proc.ident));
            
            string_builder_push_newline(sb, 2);
            string_builder_push(sb, "signature:");
            print_ast_proc_signature(sb, &proc.signature, 4);
            
            string_builder_push_newline(sb, 2);
            string_builder_push(sb, "stmts:");
            print_ast_expression_list(sb, proc.block, 4);
        } break;
        
        case Decl_Type: {
            
        } break;
    }
}