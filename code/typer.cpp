
#if 0
void
register_variable(Type_Context* tcx, Identifier ident, Ast_Type* type) {
    // TODO(Alexander): check collisions
    Object obj = {};
    obj.kind = Object_Variable;
    obj.variable = type;
    map_put(tcx->scope->objects, ident, obj);
}

inline Object
resolve_object(Type_Context* tcx, Identifier ident, Object_Kind kind) {
    Object obj = map_get(tcx->scope->objects, ident);
    if (obj.kind != kind) {
        // TODO: search deeper!
        unimplemented;
    }
    
    return obj;
}

Ast_Type*
resolve_type_definition(Type_Context* tcx, Identifier ident) {
    if (is_builtin_type_keyword(ident)) {
        return &ast_basic_types[ident - builtin_types_begin];
    } else {
        Object obj = resolve_object(tcx, ident, Object_Type_Definition);
        if (obj.kind == Object_Type_Definition) {
            return obj.type->type;
        }
    }
    
    return 0;
}

Ast_Type*
resolve_variable_type(Type_Context* tcx, Identifier ident) {
    Object obj = resolve_object(tcx, ident, Object_Variable);
    if (obj.kind == Object_Variable) {
        return obj.variable;
    }
    
    return 0;
}
#endif

Ast_Type*
infer_type(Type_Context* tcx, Ast_Type* type) {
    if (type->inferred_type) {
        return type->inferred_type;
    }
    
    Ast_Type* result = 0;
    switch (type->kind) {
        case TYPE_ALIAS: {
            unimplemented;
            //result = resolve_type_definition(tcx, type->alias);
        } break;
        
        case TYPE_PROCEDURE: {
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
        result = result && infer_type(tcx, arg->type);
    }
    return result;
}

Ast_Type*
infer_expression(Type_Context* tcx, Ast_Expression* expr) {
    Ast_Type* result = 0;
    
    switch (expr->kind) {
        case AST_TYPE: {
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
            infer_declaration(tcx, (Ast_Declaration*) expr);
        } break;
        
        case AST_UNARY: {
            unimplemented;
        } break;
        
        case AST_BINARY: {
            result = infer_binary_expression(tcx, (Ast_Binary*) expr);
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
infer_block(Type_Context* tcx, Ast_Block* block) {
    for_array_v(block->statements, expr, _) {
        if (!infer_expression(tcx, expr)) {
            return false;
        }
    }
    return true;
}

bool
infer_declaration(Type_Context* tcx, Ast_Declaration* decl) { 
    
    Ast_Type* type = infer_expression(tcx, decl->type);
    assert(type); // TODO: probably return?
    
    // TODO(Alexander): register type in type table
    
    if (type->kind == TYPE_PROCEDURE && decl->initializer->kind == AST_BLOCK) {
        auto sig = (Ast_Procedure_Type*) type;
        
        // Push arguments to scope
        for_array_it(sig->args, arg) {
            unimplemented;
            //register_variable(tcx, arg->ident, arg->type);
        }
        if (infer_block(tcx, (Ast_Block*) decl->initializer)) {
            return true;
        }
        
    } else {
        if (decl->initializer) {
            Ast_Type* actual_type = infer_expression(tcx, decl->initializer);
            if (type && actual_type) {
                return true;
            } else {
                // report error!
                unimplemented;
            }
        } else {
            return true;
        }
    }
    
    return false;
}