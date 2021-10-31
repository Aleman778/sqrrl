
enum Entity_Kind {
    EntityKind_Value,
    EntityKind_Type,
};

struct Entity {
    Entity_Kind kind;
    union  {
        Value* value;
        Type* type;
        b32 is_initialized;
    };
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

enum Interp_Value_Type {
    InterpValueType_Void,
    InterpValueType_Numeric,
    InterpValueType_Array,
    InterpValueType_Struct,
    InterpValueType_String,
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

Interp_Value
value_to_interp_value(Interp* interp, Value value) {
    Interp_Value result = create_interp_value(interp);
    result.value = value;
    
    switch (value.type) {
        case Value_void: {
            result.type = InterpValueType_Void;
        } break;
        
        case Value_boolean:
        case Value_signed_int:
        case Value_unsigned_int: 
        case Value_floating:
        case Value_pointer: {
            result.type = InterpValueType_Numeric;
        } break;
        
        case Value_array: {
            result.type = InterpValueType_Array;
        } break;
        
        case Value_string: {
            result.type = InterpValueType_String;
        } break;
    }
    
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

inline Entity symbol_table_resolve_identifier(Symbol_Table* table, string_id ident);
inline Value* symbol_table_resolve_value(Symbol_Table* table, string_id ident);
inline Type* symbol_table_resolve_type(Symbol_Table* table, string_id ident);
Value* symbol_table_store_value(Symbol_Table* table, Arena* arena, string_id ident);
Type* symbol_table_store_type(Symbol_Table* table, Arena* arena, string_id ident);

Interp_Value interp_expression(Interp* interp, Ast* ast);
Interp_Value interp_function_call(Interp* interp, string_id ident);
Interp_Value interp_statement(Interp* interp, Ast* ast);
Interp_Value interp_block(Interp* interp, Ast* ast);

Type* interp_type(Interp* interp, Ast* ast);

void interp_ast_declarations(Interp* interp, Ast_Decl_Entry* decls);
