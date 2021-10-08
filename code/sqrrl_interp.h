
struct Interp {
    struct { string_id key; Value* value; }* symbol_table;
    
    s32 block_depth;
    
    Arena stack;
    smm stack_pointer;
    smm base_pointer;
};

enum Interp_Value_Type {
    InterpValueType_Void,
    InterpValueType_Numeric,
    InterpValueType_Return,
    InterpValueType_Break,
    InterpValueType_Continue,
};

struct Interp_Value {
    Interp_Value_Type type;
    Value value;
    s32 block_depth;
    string_id label;
};

inline Interp_Value
create_interp_value(Interp* interp) {
    Interp_Value result = {};
    result.block_depth = interp->block_depth;
    return result;
}

// TODO(alexander): better diagnostic, this will do for now!
inline void
interp_error(Interp* parser, string message) {
    // TODO: need the line numbers to calculate this!
    // Span_Data span = calculate_span_data(tokenizer->lines, node->span);
    Span_Data span = {};
    string filename = string_lit("examples/demo.sq"); // TODO: store names of source files somewhere!
    pln("%:%:%: error: %\n", f_string(filename), f_smm(span.begin_line), f_smm(span.begin_col), f_string(message));
    DEBUG_log_backtrace();
}

inline void*
interp_stack_push(Interp* interp, smm size) {
    void* memory = arena_push_size(&interp->stack, size, 1);
    interp->stack_pointer += size;
    return memory;
}

Interp_Value interp_resolve_identifier(Interp* interp, string_id ident);

Interp_Value interp_expression(Interp* interp, Ast* ast);
Interp_Value interp_function_call(Interp* interp, Ast* ast);
Interp_Value interp_statement(Interp* interp, Ast* ast);
Interp_Value interp_block(Interp* interp, Ast* ast);
