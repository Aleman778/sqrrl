
Ast_Type*
resolve_type_definition(Type_Context* tcx, Ast_Block* block, Identifier ident) {
    if (is_builtin_type_keyword(ident)) {
        return &ast_basic_types[ident - builtin_types_begin];
        
    } else {
        Ast_Type* result = 0;
        
        Ast_Scope_Member* member = map_get(block->members, ident);
        if (member) {
            result = member->type_def;
        }
        
        if (!result && block->parent) {
            result = resolve_type_definition(tcx, block->parent, ident);
        }
        
        return result;
    }
}


Ast_Type*
infer_type(Type_Context* tcx, Ast_Type* type) {
    if (type->inferred_type) {
        return type->inferred_type;
    }
    
    Ast_Type* result = 0;
    switch (type->kind) {
        case AST_ALIAS_TYPE: {
            result = resolve_type_definition(tcx, tcx->block, type->alias);
        } break;
        
        case AST_PROCEDURE_TYPE: {
            if (infer_procedure_signature(tcx, (Ast_Procedure_Type*) type)) {
                result = type;
            }
        } break;
        
        default: {
            result = type;
        } break;
    }
    
    type->inferred_type = result;
    return result;
}

bool
infer_procedure_signature(Type_Context* tcx, Ast_Procedure_Type* signature) {
    
    // infer types used in signature (return + args) make sure they are valid types
    bool result = true;
    result = result && infer_type(tcx, signature->return_type);
    for_array_it(signature->args, arg) {
        
        Ast_Type* type = arg->type;
        if (!type) {
            type = (Ast_Type*) arg->initializer;
        }
        
        if (!type) {
            result = false;
            break;
        }
        
        result = result && infer_type(tcx, type);
    }
    return result;
}

Ast_Type*
infer_expression(Type_Context* tcx, Ast_Expression* expr) {
    Ast_Type* result = 0;
    
    switch (expr->kind) {
        case AST_TYPE:
        case AST_ALIAS_TYPE: 
        case AST_PROCEDURE_TYPE:
        case AST_STRUCT_TYPE: {
            result = infer_type(tcx, (Ast_Type*) expr);
        } break;
        
        case AST_LITERAL: {
            auto literal = (Ast_Literal*) expr;
            result = &ast_basic_types[literal->type];
        } break;
        
        case AST_IDENTIFIER: {
            auto ident = (Ast_Identifier*) expr;
            unimplemented;
            //result = resolve_variable_type(tcx, ident->identifier);
        } break;
        
        case AST_DECLARATION: {
            if (infer_declaration(tcx, (Ast_Declaration*) expr)) {
                result = &ast_basic_types[1];
            }
        } break;
        
        case AST_UNARY: {
            unimplemented;
        } break;
        
        case AST_BINARY: {
            result = infer_binary_expression(tcx, (Ast_Binary*) expr);
        } break;
        
        case AST_BLOCK: {
            auto block = (Ast_Block*) expr;
            result = &ast_basic_types[1];
            begin_block(tcx, block);
            for_array_v(block->statements, it, _) {
                if (!infer_expression(tcx, (Ast_Expression*) it)) {
                    result = 0;
                    break;
                }
            }
            end_block(tcx);
        } break;
        
        case AST_RETURN: {
            auto ret = (Ast_Return*) expr;
            Ast_Type* found = infer_expression(tcx, ret->expr);
            result = found;
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    expr->inferred_type = result;
    return result;
}

Ast_Type*
infer_binary_expression(Type_Context* tcx, Ast_Binary* binary) {
    Ast_Type* left = infer_expression(tcx, binary->left);
    Ast_Type* right = infer_expression(tcx, binary->right);
    Ast_Type* result = 0;
    
    if (!left || !right) {
        return 0;
    }
    
    // TODO(Alexander): operator overloading
    
    // TODO(Alexander): this only applies to simple operations such as +, -, / and *, add more later
    if (left->kind == TYPE_POINTER) {
        result = left;
        unimplemented; // TODO(Alexander): check legal operators and types on right
        
    } else if ((left->flags & TYPE_FLAG_INTEGER) && (right->flags & TYPE_FLAG_INTEGER)) {
        result = (left->size >= right->size) ? left : right;
        
    } else if ((left->flags & TYPE_FLAG_FLOAT) && (right->flags & TYPE_FLAG_FLOAT)) {
        result = (left->size >= right->size) ? left : right;
        
    } else if ((left->flags & TYPE_FLAG_INTEGER) && (right->flags & TYPE_FLAG_FLOAT)) {
        result = right;
        
        // Add implicit cast 
        //Ast_Cast* cast = push_ast_expression(tcx->lexer, Cast);
        //cast->expr = left;
        //cast->inferred_type = right;
        //binary->left = cast;
        
    } else if ((left->flags & TYPE_FLAG_FLOAT) && (right->flags & TYPE_FLAG_INTEGER)) {
        result = left;
        unimplemented; // TODO(Alexander): add implicit conversion on right to f32/f64
    }
    
    return result;
}

bool
infer_function_declaration(Type_Context* tcx, Ast_Procedure_Type* sig, Ast_Block* block) {
    bool success = true;
    
    begin_block(tcx, block);
    
    // Push arguments to block
    for_array_it(sig->args, arg) {
        //register_variable(tcx, arg->ident, arg->type);
    }
    
    if (success) {
        for_array_v(block->statements, it, _) {
            if (!infer_expression(tcx, (Ast_Expression*) it)) {
                success = false;
                break;
            }
        }
    }
    
    end_block(tcx);
    return success;
}

bool
infer_declaration(Type_Context* tcx, Ast_Declaration* decl) { 
    Ast_Type* type = infer_expression(tcx, decl->type);
    //assert(type); // TODO: probably return?
    
    
    if (!type) return false;
    
    // TODO(Alexander): register type in type table
    
    switch (type->kind) {
        case AST_PROCEDURE_TYPE: {
            if (decl->initializer && decl->initializer->kind == AST_BLOCK) {
                infer_function_declaration(tcx,
                                           (Ast_Procedure_Type*) type,
                                           (Ast_Block*) decl->initializer);
            } else {
                
            }
        } break;
        
        case AST_STRUCT_TYPE: {
            if (decl->initializer && decl->initializer->kind == AST_BLOCK) {
                
            }
        } break;
    }
    
    if (decl->initializer) {
        Ast_Type* actual_type = infer_expression(tcx, decl->initializer);
        if (type && actual_type) {
            return true;
        }
    } else {
        return true;
    }
    
    return false;
}