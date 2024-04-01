
inline void
print_argument_list(String_Builder* sb, Ast_Argument_List* args, int indent) {
    if (!args) {
        string_builder_push(sb, "null");
    }
    for_array_it(args, arg) {
        string_builder_push_newline(sb, indent);
        string_builder_push(sb, "- ");
        print_ast_expression(sb, arg->initializer, indent + 2, false);
        // TODO(Alexander): print type and identifier when applicable
    }
}

void
print_ast_type_storage(String_Builder* sb, Type_Storage kind) {
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

void
print_ast_expression(String_Builder* sb, Ast_Expression* expr, int indent, bool newline) {
    if (!expr) {
        string_builder_push(sb, "null");
        return;
    }
    
    if (newline && expr->kind != AST_TYPE) {
        string_builder_push_newline(sb, indent);
    }
    
    switch (expr->kind) {
        case AST_TYPE: {
            print_ast_type_storage(sb, ((Ast_Type*) expr)->storage);
        } break;
        
        case AST_ALIAS_TYPE: {
            auto type = (Ast_Alias_Type*) expr;
            string_builder_push(sb, "Ast_Alias_Type:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "alias: %", f_ident(type->alias));
        } break;
        
        case AST_STRUCT_TYPE: {
            auto type = (Ast_Struct_Type*) expr;
            string_builder_push(sb, "Ast_Struct_Type:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "alias: %", f_ident(type->alias));
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "declarations:");
            for_array_v(type->declarations, it, _) {
                string_builder_push_newline(sb, indent + 4);
                string_builder_push_format(sb, "- ");
                print_ast_declaration(sb, it, indent + 6);
            }
        } break;
        
        case AST_PROCEDURE_TYPE: {
            auto proc = (Ast_Procedure_Type*) expr;
            string_builder_push(sb, "Ast_Procedure_Type:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "return_type: ");
            print_ast_expression(sb, proc->return_type, indent + 4);
            
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "args: ");
            print_argument_list(sb, proc->args, indent + 4);
        } break;
        
        case AST_LITERAL: {
            auto literal = (Ast_Literal*) expr;
            string_builder_push_format(sb, "Ast_Literal:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "type: ");
            print_ast_type_storage(sb, literal->type);
            
            // TODO(Alexander): float support
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "value: %", f_u64(literal->u64_value));
        } break;
        
        case AST_STRUCT_LITERAL: {
            auto literal = (Ast_Struct_Literal*) expr;
            string_builder_push_format(sb, "Ast_Struct_Literal:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "identifier: %", f_ident(literal->identifier));
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "initializers: ");
            if (!array_count(literal->initializers)) {
                string_builder_push(sb, "null");
            }
            for_array_v(literal->initializers, it, _) {
                string_builder_push_newline(sb, indent + 4);
                string_builder_push(sb, "- ");
                print_ast_declaration(sb, it, indent + 6);
            }
        } break;
        
        case AST_BLOCK: {
            auto block = (Ast_Block*) expr;
            string_builder_push_format(sb, "Ast_Block:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "statements: ");
            if (!array_count(block->statements)) {
                string_builder_push(sb, "null");
            }
            for_array_v(block->statements, it, _) {
                string_builder_push_newline(sb, indent + 2);
                string_builder_push(sb, "- ");
                print_ast_expression(sb, it, indent + 4, false);
            }
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
        print_ast_expression(sb, expr->inferred_type, indent + 4);
    }
}

inline void
print_ast_proc_signature(String_Builder* sb, Ast_Procedure_Type* sig, int indent=0) {
    string_builder_push_newline(sb, indent);
    string_builder_push(sb, "return_type: ");
    print_ast_expression(sb, sig->return_type);
    
    string_builder_push_newline(sb, indent);
    string_builder_push(sb, "args:");
    
    if (!sig->args) {
        string_builder_push(sb, " null");
    }
    for_array_it(sig->args, arg) {
        string_builder_push_newline(sb, indent + 2);
        string_builder_push_format(sb, "- %: ", f_ident(arg->identifier));
        print_ast_expression(sb, arg->type, indent + 4);
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
