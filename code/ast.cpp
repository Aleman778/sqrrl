
inline void
print_argument_list(String_Builder* sb, Ast_Argument_List* args, int indent) {
    if (!args) {
        string_builder_push(sb, " null");
    }
    for_array_it(args, arg) {
        string_builder_push_newline(sb, indent);
        string_builder_push(sb, "- ");
        print_ast_expression(sb, arg->initializer, indent + 2, false);
        // TODO(Alexander): print type and identifier when applicable
    }
}

void
print_ast_basic_type(String_Builder* sb, Ast_Type_Kind kind) {
    switch (kind) {
        case TYPE_VOID:
        case TYPE_BOOL:
        case TYPE_S8:
        case TYPE_S16:
        case TYPE_S32:
        case TYPE_S64:
        case TYPE_SMM:
        case TYPE_INT:
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
        case TYPE_UMM:
        case TYPE_UINT:
        case TYPE_F32:
        case TYPE_F64:
        case TYPE_STRING:
        case TYPE_CSTRING:
        case TYPE_TYPEID:
        case TYPE_FLOAT: {
            string s = vars_load_string(builtin_types_begin + kind);
            string_builder_push_format(sb, "%", f_string(s));
        } break;
        
        default: {
            string_builder_push(sb, "unknown");
        } break;
    }
    
}

inline void
print_ast_type(String_Builder* sb, Ast_Type* type, int indent=0) {
    if (!type) {
        string_builder_push(sb, "null");
        return;
    }
    
    
    switch (type->kind) {
        case TYPE_ALIAS: {
            string_builder_push_newline(sb, indent);
            string_builder_push(sb, "Type_Alias:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "alias: %", f_ident(type->alias));
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "inferred_type: ");
            print_ast_type(sb, type->inferred_type, indent + 4);
        } break;
        
        case TYPE_PROCEDURE:
        case TYPE_LIKE_PROCEDURE: {
            auto proc = (Ast_Procedure_Type*) type;
            string_builder_push_newline(sb, indent);
            string_builder_push(sb, "Type_Maybe_Proc:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "proc: ");
            print_ast_type(sb, proc->return_type, indent + 4);
            
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "args: ");
            print_argument_list(sb, proc->args, indent + 4);
        } break;
        
        default: {
            print_ast_basic_type(sb, type->kind);
        } break;
    }
}

void
print_ast_expression(String_Builder* sb, Ast_Expression* expr, int indent, bool newline) {
    if (!expr) {
        string_builder_push(sb, "null");
        return;
    }
    
    if (newline) {
        string_builder_push_newline(sb, indent);
    }
    
    switch (expr->kind) {
        case AST_TYPE: {
            string_builder_push_format(sb, "Ast_Type: ");
            print_ast_type(sb, (Ast_Type*) expr, indent + 2);
        } break;
        
        case AST_LITERAL: {
            auto literal = (Ast_Literal*) expr;
            string_builder_push_format(sb, "Ast_Literal:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "type: ");
            print_ast_basic_type(sb, literal->type);
            
            // TODO(Alexander): float support
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "value: %", f_u64(literal->u64_value));
        } break;
        
        case AST_DECLARATION: {
            print_ast_declaration(sb, (Ast_Declaration*) expr, indent);
        } break;
        
        case AST_CALL: {
            auto call = (Ast_Call*) expr;
            string_builder_push(sb, "Ast_Call:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "proc: ");
            print_ast_expression(sb, call->proc, indent + 4);
            
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "args: ");
            print_argument_list(sb, call->args, indent + 4);
        } break;
        
        case AST_IDENTIFIER: {
            auto ident = (Ast_Identifier*) expr;
            string_builder_push(sb, "Ast_Identifier:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "identifier: %", f_ident(ident->identifier));
        } break;
        
        case AST_BINARY: {
            auto binary = (Ast_Binary*) expr;
            string_builder_push(sb, "Ast_Binary:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "op: %", f_token(binary->token));
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "left:");
            print_ast_expression(sb, binary->left, indent + 4);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "right:");
            print_ast_expression(sb, binary->right, indent + 4);
        } break;
        
        case AST_RETURN: {
            auto ret = (Ast_Return*) expr;
            string_builder_push(sb, "Ast_Return:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "expr: ");
            print_ast_expression(sb, ret->expr, indent + 4);
        } break;
        
        default: {
            string_builder_push_format(sb, "Ast_Unknown (%)", f_int(expr->kind));
        } break;
    }
    
    if (expr->kind != AST_TYPE) {
        string_builder_push_newline(sb, indent + 2);
        string_builder_push(sb, "inferred_type: ");
        print_ast_type(sb, expr->inferred_type, indent + 4);
    }
}

inline void
print_ast_proc_signature(String_Builder* sb, Ast_Procedure_Type* sig, int indent=0) {
    string_builder_push_newline(sb, indent);
    string_builder_push(sb, "return_type: ");
    print_ast_type(sb, sig->return_type);
    
    string_builder_push_newline(sb, indent);
    string_builder_push(sb, "args:");
    
    if (!sig->args) {
        string_builder_push(sb, " null");
    }
    for_array_it(sig->args, arg) {
        string_builder_push_newline(sb, indent + 2);
        string_builder_push_format(sb, "- %: ", f_ident(arg->identifier));
        print_ast_type(sb, arg->type, indent + 4);
    }
}

inline void
print_ast_expression_list(String_Builder* sb, Ast_Expression_List* list, int indent=0) {
    if (!list) {
        string_builder_push(sb, "null");
    }
    for_array_v(list, expr, _) {
        string_builder_push_newline(sb, indent);
        string_builder_push(sb, "- ");
        print_ast_expression(sb, expr, indent + 2, false);
    }
}

void
print_ast_declaration(String_Builder* sb, Ast_Declaration* decl, int indent) {
    string_builder_push(sb, "Ast_Declaration:");
    
    string_builder_push_newline(sb, indent + 2);
    string_builder_push(sb, "type: ");
    print_ast_expression(sb, decl->type, indent + 4);
    
    string_builder_push_newline(sb, indent + 2);
    string_builder_push_format(sb, "identifier: %", f_ident(decl->identifier));
    
    string_builder_push_newline(sb, indent + 2);
    string_builder_push(sb, "initializer: ");
    print_ast_expression(sb, decl->initializer, indent + 4);
}
