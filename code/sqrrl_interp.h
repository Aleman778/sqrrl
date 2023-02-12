
struct Interp_Scope {
    map(string_id, void*)* locals;
    array(string_id)* local_stack;
    string_id name;
    Span span;
    Interp_Scope* prev_scope;
};

struct Interp {
    Interp_Scope* global_scope;
    Interp_Scope* curr_scope;
    
    Memory_Arena stack;
    smm base_pointer;
    
    s32 block_depth;
    s32 error_count;
    
    // TODO(Alexander): maybe turn this into a bitflag later...
    b32 set_undeclared_to_zero;
    b32 flag_running_in_bytecode;
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

inline Interp_Value
value_to_interp_value(Interp* interp, Value value, void* data=0) {
    Interp_Value result = create_interp_value(interp);
    result.value = value;
    result.data = data;
    return result;
}

inline Interp_Value
interp_value_load_from_memory(Interp* interp, Type* type, void* data) {
    Value value = value_load_from_memory(type, data);
    return value_to_interp_value(interp, value, data);
}

inline bool
interp_value_exists(Interp* interp, string_id ident) {
    if (!interp->curr_scope) {
        return false;
    }
    return map_key_exists(interp->curr_scope->locals, ident);
}

inline void*
get_interp_value_pointer(Interp* interp, string_id ident) {
    if (interp_value_exists(interp, ident)) {
        void* data = map_get(interp->curr_scope->locals, ident);
        return data;
    } else {
        // TODO(Alexander): implement globals support
        //unimplemented;
    }
    
    return 0;
}

Interp_Value
get_interp_value(Interp* interp, Type* type, string_id ident) {
    if (interp->curr_scope) {
        void* data = get_interp_value_pointer(interp, ident);
        if (data) {
            Value value = value_load_from_memory(type, data);
            return value_to_interp_value(interp, value, data);
        }
    }
    
    return {};
}

void*
push_interp_value(Interp* interp, Type* type, string_id ident, Interp_Value value) {
    if (!interp->curr_scope) {
        Interp_Scope* first_scope = arena_push_struct(&interp->stack, Interp_Scope);
        interp->curr_scope = first_scope;
    }
    
    if (ident > 0 && interp_value_exists(interp, ident)) {
        interp_error(interp, string_format("cannot redeclare previous local variable `%`",
                                           f_var(ident)));
        return 0;
    }
    
    void* data = arena_push_size(&interp->stack, type->size, type->align);
    if (!is_void(value.value)) {
        value_store_in_memory(type, data, value.value.data);
    }
    
    if (ident > 0) {
        array_push(interp->curr_scope->local_stack, ident);
        map_put(interp->curr_scope->locals, ident, data);
    }
    
    return data;
}

Interp_Value interp_expression(Interp* interp, Ast* ast);
Interp_Value interp_field_expr(Interp* interp, Interp_Value var, string_id ident);
Interp_Value interp_function_call(Interp* interp, Ast* args, Type* function_type);
Interp_Value interp_statement(Interp* interp, Ast* ast);
Interp_Value interp_block(Interp* interp, Ast* ast);

// Interpreter intrinsics
Value interp_intrinsic_print_format(Interp* interp, array(Interp_Value)* var_args);
Value interp_intrinsic_debug_break(Interp* interp, array(Interp_Value)* var_args);
Value interp_intrinsic_assert(Interp* interp, array(Interp_Value)* var_args);
