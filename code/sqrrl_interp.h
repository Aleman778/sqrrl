
enum Compilation_Unit_Status {
    CUnitStatus_Parsing_Finished,
    
    CUnitStatus_Type_Inference_Failed,
    CUnitStatus_Type_Inference_Finished,
    
    
    CUnitStatus_Finished,
};

struct Compilation_Unit {
    Ast_Module* module;
    Ast_File* file;
    Ast* ast;
    
    Type* type;
    Value value;
    Bytecode_Function* bytecode_function;
    s64 external_address;
    
    string_id ident;
    
    Compilation_Unit_Status status;
    bool is_main;
};

struct Included_File {
    Source_File* file;
    Ast_Module* module;
};

struct Interp {
    Scope global_scope;
    
    Memory_Arena ast_arena;
    array(Compilation_Unit)* compilation_units;
    
    array(Included_File)* included_files;
};

void
interp_add_source_file(Interp* interp, Ast_Module* module, Source_File* file) {
    Included_File included_file = {};
    included_file.file = file;
    included_file.module = module;
    array_push(interp->included_files, included_file);
}

void
register_compilation_units_from_ast_decl(Interp* interp, Ast_Module* module, Ast_File* file, Ast* decl) {
    switch (decl->kind) {
        case Ast_Compound: {
            for_compound(decl, it) {
                register_compilation_units_from_ast_decl(interp, module, file, it);
            }
        } break;
        
        case Ast_Block_Stmt: {
            for_compound(decl->Block_Stmt.stmts, it) {
                register_compilation_units_from_ast_decl(interp, module, file, it);
            }
        } break;
        
        case Ast_Decl_Stmt: {
            Compilation_Unit cu = {};
            cu.module = module;
            cu.file = file;
            cu.ident = ast_unwrap_ident(decl->Decl_Stmt.ident);
            cu.ast = decl->Decl_Stmt.type;
            array_push(interp->compilation_units, cu);
            
            cu.ast = decl;
            array_push(interp->compilation_units, cu);
        } break;
        
        case Ast_Assign_Stmt: {
            Compilation_Unit cu = {};
            cu.module = module;
            cu.file = file;
            cu.ident = ast_unwrap_ident(decl->Assign_Stmt.ident);
            cu.ast = decl;
            array_push(interp->compilation_units, cu);
        } break;
        
        case Ast_Typedef: {
            unimplemented;
        } break;
        
        default: {
            // TODO(Alexander): maybe don't accept everything but let's see
            if (is_valid_ast(decl)) {
                Compilation_Unit cu = {};
                cu.module = module;
                cu.file = file;
                cu.ast = decl;
                array_push(interp->compilation_units, cu);
            }
        } break;
    }
}

inline void
register_compilation_units_from_ast_file(Interp* interp, Ast_Module* module, Ast_File* file) {
    for_array_v(file->declarations, decl, _) {
        register_compilation_units_from_ast_decl(interp, module, file, decl);
    }
}

void
add_file_to_module(Interp* interp, Ast_Module* module, Ast_File* file) {
    bool new_file = true;
    for_array_v(module->files, existing_file, _) {
        if (existing_file == file) {
            new_file = false;
        }
    }
    
    if (new_file) {
        array_push(module->files, file);
        register_compilation_units_from_ast_file(interp, module, file);
    }
}

inline void*
scope_get_data_pointer(Scope* interp, string_id ident) {
    unimplemented;
    return 0;
}

inline Entity
create_constant_value(Type* type, Value value) {
    Entity entity = {};
    entity.kind = EntityKind_Constant;
    entity.type = type;
    entity.value = value;
    return entity;
}

inline Entity
create_variable(Type* type) {
    Entity entity = {};
    entity.kind = EntityKind_Variable;
    entity.type = type;
    return entity;
}

inline Entity
create_typedef(Type* type) {
    Entity entity = {};
    entity.kind = EntityKind_Typedef;
    entity.type = type;
    return entity;
}

inline Entity
create_macro(Ast* ast) {
    Entity entity = {};
    entity.kind = EntityKind_Macro;
    entity.ast = ast;
    return entity;
}

inline Entity
create_function(Type* type) {
    Entity entity = {};
    entity.kind = EntityKind_Function;
    entity.type = type;
    return entity;
}

inline Value
scope_get_constant_value(Scope* scope, string_id ident) {
    Entity entity = map_get(scope->entities, ident);
    if (entity.kind == EntityKind_Constant) {
        return entity.value;
    }
    
    return {};
}

inline void
interp_put_global(Interp* interp, string_id ident, Type* type, Value value) {
    Entity entity = create_constant_value(type, value);
    map_put(interp->global_scope.entities, ident, entity);
}

inline Value
interp_get_value(Interp* interp, string_id ident) {
    return scope_get_constant_value(&interp->global_scope, ident);
}

inline void*
interp_get_data_pointer(Interp* interp, string_id ident) {
    // TODO(Alexander)
    unimplemented;
    return 0;
}
