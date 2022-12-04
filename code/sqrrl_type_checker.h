
struct Type_Scope {
    array(string_id)* locals;
};

struct Type_Context {
    Memory_Arena type_arena;
    
    map(string_id, Type*)* locals;
    map(string_id, Type*)* globals;
    
    map(string_id, Type*)* local_type_table;
    map(string_id, Type*)* global_type_table;
    
    array(Type_Scope)* scopes;
    Type_Scope* active_scope;
    
    Type* return_type;
    
    // TODO(Alexander): this can be made into a bitflag later
    b32 set_undeclared_to_s64;
    
    s32 block_depth;
    s32 error_count;
    s32 warning_count;
};

inline void
type_error(Type_Context* tcx, string message, Token* token=0) {
    Span_Data span = {};
    string filename = string_lit("examples/demo.sq"); // TODO: store names of source files somewhere!
    
    if (token) {
        pln("%:%:%: ", f_string(token->file), f_smm(token->line + 1), f_smm(token->column + 1));
    }
    
    pln("error: %\n", f_string(message));
    DEBUG_log_backtrace();
    tcx->error_count++;
}

inline void
type_error_mismatch(Type_Context* tcx, Type* expected, Type* found, Token* token=0) {
    type_error(tcx, string_format("mismatched types expected `%`, found `%`",
                                  f_type(expected), f_type(found)), token);
}

inline void
type_warning(Type_Context* tcx, string message) {
    Span_Data span = {};
    string filename = string_lit("examples/demo.sq"); // TODO: store names of source files somewhere!
    pln("%:%:%: warning: %\n", f_string(filename), f_smm(span.begin_line), f_smm(span.begin_column), f_string(message));
    DEBUG_log_backtrace();
    tcx->warning_count++;
}

bool
push_local(Type_Context* tcx, string_id ident, Type* type, bool report_error, Token* token=0) {
    assert(tcx->block_depth > 0);
    assert(tcx->active_scope);
    
    if (map_get(tcx->locals, ident)) {
        if (report_error) {
            type_error(tcx,
                       string_format("cannot redeclare previous local variable `%`",
                                     f_string(vars_load_string(ident))), token);
        }
        return false;
        
    } else {
        map_put(tcx->locals, ident, type);
        array_push(tcx->active_scope->locals, ident);
        return true;
    }
}

void
push_type_scope(Type_Context* tcx) {
    tcx->block_depth++;
    
    Type_Scope scope = {};
    array_push(tcx->scopes, scope);
    tcx->active_scope = &array_last(tcx->scopes);
}

void
pop_type_scope(Type_Context* tcx) {
    assert(tcx->active_scope);
    for_array_v(tcx->active_scope->locals, ident, _) {
        if (!map_remove(tcx->locals, ident)) {
            assert(0 && "compiler bug; local variable couldn't be freed");
        }
    }
    array_free(tcx->active_scope->locals);
    array_pop(tcx->scopes);
    
    if (array_count(tcx->scopes) > 0) {
        tcx->active_scope = &array_last(tcx->scopes);
    } else {
        tcx->active_scope = 0;
    }
    
    tcx->block_depth--;
}

bool
type_equals(Type* a, Type* b) {
    assert(a && b);
    
    if (a->kind != b->kind) {
        return false;
    }
    
    switch (a->kind) {
        case TypeKind_Void: {
            return true;
        } break;
        
        case TypeKind_Basic: {
            if (a->Basic.kind != b->Basic.kind) {
                return false;
            }
        } break;
        
        case TypeKind_Array: {
            if (!type_equals(a->Array.type, b->Array.type)) {
                return false;
            }
            if (a->Array.capacity != b->Array.capacity) {
                return false;
            }
        } break;
        
        case TypeKind_Union: {
            if (a->kind != b->kind) {
                return false;
            }
            
            Type_Struct* sa = &a->Struct;
            Type_Struct* sb = &a->Struct;
            
            if (array_count(sa->types) != array_count(sb->types)) {
                return false;
            }
            
        } break;
        
        case TypeKind_Struct: {
            if (a->kind != b->kind) {
                return false;
            }
            
            Type_Struct* sa = &a->Struct;
            Type_Struct* sb = &a->Struct;
            
            if (array_count(sa->types) != array_count(sb->types)) {
                return false;
            }
            
            // TODO(Alexander): check that entries in the struct/unions match
            //for_array(table_a->) {
            //}
            
        } break;
        
        case TypeKind_Pointer: {
            return type_equals(a->Pointer, b->Pointer);
        } break;
        
        default: {
            pln("%", f_string(string_format("%", f_type(a))));
            assert(0 && "not implemented");
        } break;
    }
    
    return true;
}

// NOTE(Alexander): forward declare
struct Ast_File;

Type* type_infer_expression(Type_Context* tcx, Ast* expr, Type* parent_type, bool report_error);

s32 type_check_ast_file(Ast_File* ast_file);

Type* create_type_from_ast(Type_Context* tcx, Ast* ast, bool report_error);

Type* load_type_declaration(Type_Context* tcx, string_id ident, bool report_error);
