
struct Interp {
    
    s32 block_depth;
    
    Arena stack;
    smm stack_pointer;
    smm base_pointer;
};

enum Interp_Result_Type {
    InterpResultType_Void,
    InterpResultType_Numeric,
    InterpResultType_Return,
    InterpResultType_Break,
    InterpResultType_Continue,
};

struct Interp_Value {
    Interp_Result_Type result_type;
    Value value;
    s32 block_depth;
    str_id label;
};

// TODO(alexander): better diagnostic, this will do for now!
inline void
interp_error(Interp* parser, Token token, str message) {
    pln("%:%:%: error: %\n", f_str(token.file), f_smm(token.line + 1), f_smm(token.column + 1), f_str(message));
    DEBUG_log_backtrace();
}

inline void*
interp_stack_push(Interp* interp, smm size) {
    void* memory = arena_push_size(&interp->stack, size, 1);
    interp->stack_pointer += size;
    return memory;
}

Interp_Value interp_expression(Interp* interp, Ast* ast);
Interp_Value interp_function_call(Interp* interp, Ast* ast);
Interp_Value interp_statement(Interp* interp, Ast* ast);
Interp_Value interp_block(Interp* interp, Ast* ast);
