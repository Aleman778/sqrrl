
// TODO(Alexander): entity is a confusing name, can be data or declaration.
struct Interp_Entity {
    void* data;
    Type* type;
    b32 is_valid;
};

struct Symbol_Table {
    string_id key; 
    Interp_Entity value;
};

struct Interp_Scope {
    map(string_id, Interp_Entity)* locals;
    array(string_id)* local_stack;
    string_id name;
    Span span;
};

struct Interp {
    array(Interp_Scope)* scopes;
    map(string_id, Interp_Entity)* globals;
    
    map(string_id, Type*) global_types;
    
    Memory_Arena stack;
    smm base_pointer;
    
    s32 block_depth;
    s32 error_count;
    
    b32 set_undeclared_to_zero; // TODO(Alexander): maybe turn this into a bitflag later...
};

enum Interp_Value_Mod {
    InterpValueMod_None,
    InterpValueMod_Return,
    InterpValueMod_Break,
    InterpValueMod_Continue,
};

struct Interp_Value {
    Value value;
    Type type;
    void* data;
    Interp_Value_Mod modifier;
    string_id label;
    s32 block_depth;
    b32 is_error;
};

// TODO(alexander): better diagnostic, this will do for now!
inline void
interp_error(Interp* interp, string message) {
    if (interp->error_count == 0) {
        // TODO: need the line numbers to calculate this!
        //Span_Data span = calculate_span_data(tokenizer->lines, node->span);
        Span_Data span = {};
        string filename = string_lit("examples/demo.sq"); // TODO: store names of source files somewhere!
        pln("%:%:%: error: %\n", f_string(filename), f_smm(span.begin_line), f_smm(span.begin_column), f_string(message));
        DEBUG_log_backtrace();
        interp->error_count++;
    }
}

inline void
interp_unresolved_identifier_error(Interp* interp, string_id ident) {
    interp_error(interp, string_format("unresolved identifier `%`", f_string(vars_load_string(ident))));
}

inline void
interp_mismatched_types(Interp* interp, Type* expected, Type* found) {
    interp_error(interp, string_format("mismatched types, expected `%` found `%`", 
                                       f_type(expected), f_type(found)));
}

inline Interp_Value
create_interp_value(Interp* interp) {
    Interp_Value result = {};
    result.block_depth = interp->block_depth;
    return result;
}

Interp_Value
value_to_interp_value(Interp* interp, Value value) {
    Interp_Value result = create_interp_value(interp);
    result.value = value;
    
    return result;
}

inline Interp_Entity
interp_load_entity_from_current_scope(Interp* interp, string_id ident) {
    Interp_Entity result = {};
    
    if (array_count(interp->scopes) > 0) {
        Interp_Scope* current_scope = interp->scopes + (array_count(interp->scopes) - 1);
        result = map_get(current_scope->locals, ident);
    }
    
    if (result.is_valid) {
        return result;
    }
    
    result = map_get(interp->globals, ident);
    return result;
}

inline void
interp_push_entity_to_scope(Interp_Scope* scope, string_id ident, void* data, Type* type) {
    Interp_Entity entity;
    entity.data = data;
    entity.type = type;
    entity.is_valid = true;
    map_put(scope->locals, ident, entity);
    array_push(scope->local_stack, ident);
}

inline Interp_Scope*
interp_get_current_scope(Interp* interp) {
    if (array_count(interp->scopes) == 0) {
        return 0;
    }
    return interp->scopes + (array_count(interp->scopes) - 1);
}

inline void
interp_push_entity_to_current_scope(Interp* interp, string_id ident, void* data, Type* type) {
    Interp_Scope* current_scope = interp_get_current_scope(interp);
    if (current_scope) {
        interp_push_entity_to_scope(current_scope, ident, data, type);
    } else {
        Interp_Entity entity;
        entity.data = data;
        entity.type = type;
        entity.is_valid = true;
        map_put(interp->globals, ident, entity);
    }
}

void interp_save_value(Interp* interp, Type* type, void* storage, Value_Data data);
Interp_Value interp_load_value(Interp* interp, Type* type, void* data);

inline void*
interp_push_value(Interp* interp, Type* type, Value_Data data) {
    assert(type->cached_size > 0 && "bad size");
    assert(type->cached_align > 0 && "bad align");
    
    void* result = arena_push_size(&interp->stack, type->cached_size, type->cached_align);
    interp_save_value(interp, type, result, data);
    
    return result;
}

inline bool
interp_entity_is_assigned(Interp* interp, Interp_Entity* entity, string_id ident) {
    if (!entity->data || !entity->type) {
        if (entity->type) {
            
            // HACK(Alexander): maybe need to rethink what is legal to do here.
            if (entity->type->kind == Type_Enum) {
                return true;
            }
            
            interp_error(interp, string_format("`%`: declared but not assigned", f_string(vars_load_string(ident))));
        } else {
            interp_error(interp, string_format("`%`: undeclared identifier ", f_string(vars_load_string(ident))));
        }
        return false;
    }
    
    return true;
}

inline bool
interp_entity_is_declared(Interp* interp, Interp_Entity* entity, string_id ident) {
    if (!entity->type) {
        interp_error(interp, string_format("`%`: undeclared identifier ", f_string(vars_load_string(ident))));
        // TODO(Alexander): what about int maybe primitives and types should get its own symbol table?
        return false;
    }
    return true;
}

inline Interp_Value
interp_load_value(Interp* interp, string_id ident) {
    Interp_Value result = {};
    
    Interp_Entity entity = interp_load_entity_from_current_scope(interp, ident);
    if (!entity.is_valid) {
        return result;
    }
    
    if (entity.data == 0) {
        result.type = *entity.type;
        return result;
    }
    
    if (interp_entity_is_assigned(interp, &entity, ident)) {
        result = interp_load_value(interp, entity.type, entity.data);
    }
    
    return result;
}

Interp_Value interp_expression(Interp* interp, Ast* ast);
Interp_Value interp_field_expr(Interp* interp, Interp_Value var, string_id ident);
Interp_Value interp_function_call(Interp* interp, string_id ident, Ast* args, Type* function_type=0);
Interp_Value interp_statement(Interp* interp, Ast* ast);
Interp_Value interp_block(Interp* interp, Ast* ast);

void interp_declaration_statement(Interp* interp, Ast* ast);

void interp_ast_declarations(Interp* interp, Ast_Decl_Table* decls);

// Interpreter intrinsics
Value interp_intrinsic_pln(Interp* interp, array(Interp_Value)* var_args);
Value interp_intrinsic_debug_break(Interp* interp, array(Interp_Value)* var_args);
Value interp_intrinsic_assert(Interp* interp, array(Interp_Value)* var_args);
