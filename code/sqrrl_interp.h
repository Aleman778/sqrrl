
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
    InterpValueMod_Return,
    InterpValueMod_Break,
    InterpValueMod_Continue,
};

struct Interp_Value {
    Value value;
    Type* type;
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
interp_unresolved_identifier_error(Interp* interp, string_id ident) {
    interp_error(interp, string_format("unresolved identifier `%`", f_string(vars_load_string(ident))));
}

void* interp_push_value(Interp* interp, Type* type, Value value);
Interp_Value interp_resolve_value(Interp* interp, Type* type, void* data);

inline Interp_Value
interp_resolve_value(Interp* interp, string_id ident) {
    Entity entity = map_get(interp->symbol_table, ident);
    if (!entity.data || !entity.type) {
        if (entity.type) {
            interp_error(interp, string_format("`%`: declared but not assigned", f_string(vars_load_string(ident))));
        } else {
            interp_error(interp, string_format("`%`: undeclared identifier ", f_string(vars_load_string(ident))));
        }
    }
    
    return interp_resolve_value(interp, entity.type, entity.data);
}

Interp_Value interp_expression(Interp* interp, Ast* ast);
Interp_Value interp_function_call(Interp* interp, string_id ident, Ast* args);
Interp_Value interp_statement(Interp* interp, Ast* ast);
Interp_Value interp_block(Interp* interp, Ast* ast);

Type* interp_type(Interp* interp, Ast* ast);

void interp_ast_declarations(Interp* interp, Ast_Decl_Entry* decls);
