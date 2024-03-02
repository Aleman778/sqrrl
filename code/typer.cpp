
void
register_variable(Type_Context* tcx, Identifier ident, Ast_Type* type) {
    // TODO(Alexander): check collisions
    Object obj = {};
    obj.kind = Object_Variable;
    obj.variable = type;
    map_put(tcx->scope->objects, ident, obj);
}

Ast_Type*
infer_type(Type_Context* tcx, Ast_Type* type) {
    Ast_Type* result = 0;
    
    switch (type->kind) {
        case Type_Alias: {
            auto alias = (Ast_Alias_Type*) type;
            if (!alias->resolved) {
                //alias->resolved = resolve_type_definition_by_identifier(tcx, alias->ident);
            }
            
            result = alias->resolved;
        } break;
        
        case Type_Proc: {
            if (infer_procedure_signature(tcx, (Ast_Proc_Type*) type)) {
                result = type;
            }
        } break;
        
        default: {
            result = type;
        } break;
    }
    
    return result;
}

bool
infer_procedure_signature(Type_Context* tcx, Ast_Proc_Type* signature) {
    
    // infer types used in signature (return + args) make sure they are valid types
    bool result = true;
    result = result && infer_type(tcx, signature->return_type);
    Ast_Proc_Argument* arg = signature->args;
    while (arg) {
        result = result && infer_type(tcx, arg->type);
        arg = arg->next;
    }
    
    return result;
}

void
infer_expression_list(Type_Context* tcx, Ast_Expression_List* list) {
    while (list) {
        list = list->next;
    }
}

void
infer_declaration(Type_Context* tcx, Ast_Declaration* decl) { 
    
    switch (decl->kind) {
        case Decl_Procedure: {
            
            Ast_Proc_Type* sig = &decl->proc.signature;
            if (infer_type(tcx, sig)) {
                
                Scope scope = {};
                begin_block_scope(tcx, &scope);
                
                // Push arguments to scope
                Ast_Proc_Argument* arg = sig->args;
                while (arg) {
                    register_variable(tcx, arg->ident, arg->type);
                    arg = arg->next;
                }
                
                infer_expression_list(tcx, decl->proc.block);
                
                end_block_scope(tcx, &scope);
                
            } else {
                unimplemented;
            }
            
        } break;
        
        case Decl_Type: {
            unimplemented;
        } break;
    }
    
}