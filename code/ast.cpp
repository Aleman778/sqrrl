
void
add_statement(Ast_Block* block, Ast_Expression* statement) {
    array_push(block->statements, statement);
    
    if (statement->kind == AST_DECLARATION) {
        auto decl = (Ast_Declaration*) statement;
        
        if (decl->identifier && map_get_index(block->members, decl->identifier) == -1) {
            map_put(block->members, decl->identifier, decl);
            
        } else if (decl->kind == AST_FUNCTION) {
            // TODO(Alexander): create a set of overloaded functions
            unimplemented;
        }
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
print_ast_block(String_Builder* sb, Ast_Block* block, int indent) {
    string_builder_push_newline(sb, indent);
    string_builder_push_format(sb, "Ast_Block:");
    
    string_builder_push_newline(sb, indent + 2);
    string_builder_push(sb, "statements: ");
    if (!array_count(block->statements)) {
        string_builder_push(sb, "null");
    }
    for_array_v(block->statements, it, _) {
        string_builder_push_newline(sb, indent + 2);
        string_builder_push(sb, "- ");
        print_ast_expression(sb, (Ast_Expression*) it, indent + 4, false);
    }
}

void
print_ast_expression(String_Builder* sb, Ast_Expression* expr, int indent, bool newline) {
    if (!expr) {
        string_builder_push(sb, "null");
        return;
    }
    
    if (newline && expr->kind != AST_TYPE && expr->kind != AST_BLOCK) {
        string_builder_push_newline(sb, indent);
    }
    
    switch (expr->kind) {
        case AST_TYPE: {
            print_ast_type_storage(sb, ((Ast_Type*) expr)->storage);
        } break;
        
        case AST_IDENTIFIER: {
            auto ident = (Ast_Identifier*) expr;
            string_builder_push(sb, "AST_IDENTIFIER:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "identifier: %", f_ident(ident->identifier));
        } break;
        
        case AST_LITERAL: {
            auto literal = (Ast_Literal*) expr;
            string_builder_push_format(sb, "Ast_Literal:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "type: ");
            print_ast_type_storage(sb, literal->type);
            
            // TODO(Alexander): float support
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "value: %", f_u64(literal->value._u64));
        } break;
        
        case AST_STRUCT_LITERAL: {
            auto literal = (Ast_Struct_Literal*) expr;
            string_builder_push_format(sb, "Ast_Struct_Literal:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "identifier: %", f_ident(literal->identifier));
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "block: ");
            print_ast_block(sb, literal->block, indent + 4);
        } break;
        
        case AST_BLOCK: {
            print_ast_block(sb, (Ast_Block*) expr, indent);
        } break;
        
        case AST_CALL: {
            auto call = (Ast_Call*) expr;
            string_builder_push(sb, "Ast_Call:");
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "proc: ");
            print_ast_expression(sb, call->func, indent + 4);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "args: ");
            print_ast_expression(sb, call->args, indent + 4);
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
        
        case AST_IF: {
            auto if_stmt = (Ast_If*) expr;
            string_builder_push(sb, "Ast_If:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "cond: ");
            print_ast_expression(sb, if_stmt->cond, indent + 2);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "then: ");
            print_ast_expression(sb, if_stmt->then_stmt, indent + 2);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "else_if: ");
            print_ast_expression(sb, if_stmt->else_stmt, indent + 2);
        } break;
        
        case AST_RETURN: {
            auto ret = (Ast_Return*) expr;
            string_builder_push(sb, "Ast_Return:");
            string_builder_push_newline(sb, indent + 2);
            string_builder_push(sb, "expr: ");
            print_ast_expression(sb, ret->expression, indent + 4);
        } break;
        
        case AST_DECLARATION: {
            string_builder_push(sb, "Ast_Declaration:");
            print_ast_declaration(sb, (Ast_Declaration*) expr, indent);
        } break;
        
        case AST_FUNCTION: {
            auto func = (Ast_Function*) expr;
            string_builder_push(sb, "Ast_Procedure:");
            print_ast_declaration(sb, (Ast_Declaration*) expr, indent);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "return_type: ");
            print_ast_expression(sb, func->return_type, indent + 4);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "args: ");
            print_ast_expression(sb, func->args, indent + 4);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "body: ");
            print_ast_expression(sb, func->body, indent + 4);
        } break;
        
        case AST_STRUCT: {
            auto struct_node = (Ast_Struct*) expr;
            string_builder_push(sb, "Ast_Struct:");
            print_ast_declaration(sb, (Ast_Declaration*) expr, indent);
            
            string_builder_push_newline(sb, indent + 2);
            string_builder_push_format(sb, "identifier: %", f_ident(struct_node->identifier));
        } break;
        
        default: {
            string_builder_push_format(sb, "Ast_Unknown (%)", f_int(expr->kind));
        } break;
    }
    
    if (expr->inferred_type != expr && expr->kind != AST_TYPE) {
        string_builder_push_newline(sb, indent + 2);
        string_builder_push(sb, "inferred_type: ");
        print_ast_expression(sb, expr->inferred_type, indent + 4);
    }
}

void
print_ast_declaration(String_Builder* sb, Ast_Declaration* decl, int indent) {
    string_builder_push_newline(sb, indent + 2);
    string_builder_push(sb, "type: ");
    print_ast_expression(sb, decl->type, indent + 4);
    
    string_builder_push_newline(sb, indent + 2);
    string_builder_push_format(sb, "identifier: %", f_ident(decl->identifier));
    
    string_builder_push_newline(sb, indent + 2);
    string_builder_push(sb, "initializer: ");
    print_ast_expression(sb, decl->initializer, indent + 4);
}

void
print_ast_file(String_Builder* sb, Ast_File* file, int indent) {
    string_builder_push(sb, "Ast_File:");
    
    string_builder_push_newline(sb, indent + 2);
    string_builder_push(sb, "block: ");
    print_ast_block(sb, &file->block, indent + 4);
}