
struct Entity {
    void* data;
    Type* type;
};

struct Symbol_Table {
    string_id key; 
    Entity value;
};

struct Interp {
    Symbol_Table* symbol_table;
    
    s32 block_depth;
    
    Arena stack;
    smm base_pointer;
};

enum Interp_Value_Mod {
    InterpValueMod_None,
    InterpValueMod_Return,
    InterpValueMod_Break,
    InterpValueMod_Continue,
};

struct Interp_Value {
    Value value;
    Type* type;
    void* data;
    s32 block_depth;
    Interp_Value_Mod modifier;
    string_id label;
};

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

// TODO(alexander): better diagnostic, this will do for now!
inline void
interp_error(Interp* interp, string message) {
    // TODO: need the line numbers to calculate this!
    // Span_Data span = calculate_span_data(tokenizer->lines, node->span);
    Span_Data span = {};
    string filename = string_lit("examples/demo.sq"); // TODO: store names of source files somewhere!
    pln("%:%:%: error: %\n", f_string(filename), f_smm(span.begin_line), f_smm(span.begin_col), f_string(message));
    DEBUG_log_backtrace();
}

inline void
interp_register_type(Interp* interp, string_id ident, Type* type, void* data=0) {
    Entity entity;
    entity.data = 0;
    entity.type = type;
    map_put(interp->symbol_table, ident, entity);
}

inline void
interp_unresolved_identifier_error(Interp* interp, string_id ident) {
    interp_error(interp, string_format("unresolved identifier `%`", f_string(vars_load_string(ident))));
}

void interp_save_value(Interp* interp, Type* type, void* storage, Value value);
Interp_Value interp_load_value(Interp* interp, Type* type, void* data);

inline void*
interp_push_value(Interp* interp, Type* type, Value value) {
    assert(type->cached_size > 0 && "bad size");
    assert(type->cached_align > 0 && "bad align");
    
    void* result = arena_push_size(&interp->stack, type->cached_size, type->cached_align);
    interp_save_value(interp, type, result, value);
    
    return result;
}

inline bool
interp_entity_is_assigned(Interp* interp, Entity* entity, string_id ident) {
    if (!entity->data || !entity->type) {
        if (entity->type) {
            
            // HACK(Alexander): maybe need to rethink what is legal to do here.
            if (entity->type->kind == TypeKind_Enum) {
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
interp_entity_is_declared(Interp* interp, Entity* entity, string_id ident) {
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
    
    Entity entity = map_get(interp->symbol_table, ident);
    if (!entity.data) {
        result.type = entity.type;
        return result;
    }
    
    if (interp_entity_is_assigned(interp, &entity, ident)) {
        result = interp_load_value(interp, entity.type, entity.data);
    }
    
    return result;
}

Interp_Value interp_expression(Interp* interp, Ast* ast);
Interp_Value interp_function_call(Interp* interp, string_id ident, Ast* args);
Interp_Value interp_statement(Interp* interp, Ast* ast);
Interp_Value interp_block(Interp* interp, Ast* ast);

Type* interp_type(Interp* interp, Ast* ast);

void interp_declaration_statement(Interp* interp, Ast* ast);

void interp_ast_declarations(Interp* interp, Ast_Decl_Entry* decls);
