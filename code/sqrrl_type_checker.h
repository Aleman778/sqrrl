
struct Operator_Overload {
    Operator op;
    Type* rhs;
    Type* func;
    // NOTE(Alexander): lhs is used to locate this struct
};

struct Overloaded_Operator_List {
    array(Operator_Overload)* ops;
    bool is_valid;
};

struct Overloaded_Function_List {
    array(Type*)* functions;
    bool is_valid;
};

// TODO(Alexander): Dynamic Library stuff is a hack and needs to be improved
struct Library_Function {
    void* pointer;
    Type* type;
    string_id name;
    u32 relative_ptr;
    u32 bc_func_index;
    
};

struct Dynamic_Function {
    void* pointer;
    string name;
    //Type_Info* type_info; // TODO: maybe we want the type info (we don't support function type info yet)
};

struct Dynamic_Library {
    Dynamic_Function* functions;
    int count;
};

struct Library_Imports {
    array(Library_Function)* functions;
    bool resolve_at_runtime;
    
    bool is_valid;
};

struct Library_Import_Table {
    map(string_id, Library_Imports)* libs;
};

enum Entity_Kind {
    EntityKind_None,
    
    EntityKind_Constant,
    EntityKind_Variable,
    EntityKind_Macro,
    EntityKind_Function,
    EntityKind_Function_Overloads,
    EntityKind_Typedef,
};

struct Entity {
    Entity_Kind kind;
    Ast* ast;
    Type* type;
    Value value;
    
    array(Type*)* overloads;
};

struct Scope {
    Scope* parent;
    
    map(string_id, Entity)* entities;
};

struct Type_Context {
    Interp* interp;
    
    Ast_Module* module;
    Ast_File* file;
    Scope* scope;
    
    Memory_Arena type_arena;
    
    Data_Packer* data_packer;
    
    Type* entry_point;
    
    map(Type*, Type*)* type_to_pointer;
    
    map(Type*, Overloaded_Operator_List)* overloaded_operators;
    map(string_id, Overloaded_Function_List)* overloaded_functions;
    
    Library_Import_Table import_table;
    
    Backend_Type target_backend;
    
    Type* return_type;
    
    s32 block_depth;
    s32 error_count;
    s32 warning_count;
};

void
print_span_location(Span span) {
    if (span.file_index > 0) {
        Source_File* file = get_source_file_by_index(span.file_index);
        if (file && file->lines) {
            Span_Data result = calculate_span_data(file->lines, span);
            print("%:%:%: ", f_string(file->abspath), f_u32(result.begin_line + 1), f_u32(result.begin_column + 1));
        }
    }
}

inline void
type_error(Type_Context* tcx, string message, Span span) {
    
    print_span_location(span);
    pln("error: %", f_string(message));
    if (tcx->error_count < 3) {
        DEBUG_log_backtrace();
        print("\n");
    }
    tcx->error_count++;
}

inline void
type_error_mismatch(Type_Context* tcx, Type* expected, Type* found, Span span) {
    type_error(tcx, string_print("mismatched types expected `%`, found `%`",
                                 f_type(expected), f_type(found)), span);
}

inline void
type_warning(Type_Context* tcx, string message, Span span) {
    Span_Data spand = {};
    print_span_location(span);
    pln("warning: %\n", f_string(message));
    if (tcx->error_count < 3) {
        DEBUG_log_backtrace();
        print("\n");
    }
    DEBUG_log_backtrace();
    tcx->warning_count++;
}

Type*
type_wrap_pointer(Type_Context* tcx, Type* type) {
    Type* result = 0;
    
    result = map_get(tcx->type_to_pointer, type);
    if (!result) {
        result = arena_push_struct(&tcx->type_arena, Type);
        result->kind = TypeKind_Pointer;
        result->Pointer = type;
        result->size = t_void_ptr->size;
        result->align = t_void_ptr->align;
        map_put(tcx->type_to_pointer, type, result);
    }
    
    return result;
}

#if 0
bool
push_local(Type_Context* tcx, string_id ident, Type* type, Span span, bool report_error) {
    assert(tcx->block_depth > 0);
    assert(tcx->active_scope);
    
    if (map_get(tcx->locals, ident)) {
        if (report_error) {
            type_error(tcx,
                       string_print("cannot redeclare previous local variable `%`",
                                    f_string(vars_load_string(ident))), span);
        }
        return false;
        
    } else {
        map_put(tcx->locals, ident, type);
        array_push(tcx->active_scope->locals, ident);
        return true;
    }
}
#endif

inline void
begin_block_scope(Type_Context* tcx, Scope* scope) {
    scope->parent = tcx->scope;
    tcx->scope = scope;
    tcx->block_depth++;
}

inline void
end_block_scope(Type_Context* tcx, Scope* scope) {
    map_free(scope->entities);
    tcx->scope = scope->parent;
    tcx->block_depth--;
}

bool
type_equals(Type* a, Type* b) {
    assert(a && b);
    
    if (a->kind == TypeKind_Unresolved || b->kind == TypeKind_Unresolved) {
        return false;
    }
    
    
    if (a->kind == TypeKind_Enum) {
        a = a->Enum.type;
    }
    
    if (b->kind == TypeKind_Enum) {
        b = b->Enum.type;
    }
    
    if (a->kind != b->kind) {
        return false;
    }
    
    switch (a->kind) {
        case TypeKind_Any:
        case TypeKind_Type:
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
        
        case TypeKind_Struct:
        case TypeKind_Union: {
            if (a->kind != b->kind) {
                return false;
            }
            
            Struct_Like_Info* sa = &a->Struct_Like;
            Struct_Like_Info* sb = &a->Struct_Like;
            
            if (array_count(sa->types) != array_count(sb->types)) {
                return false;
            }
            
            // TODO(Alexander): we can improve this later, doesn't work for anonymous structs
            if (sa != sb) {
                return false;
            }
            
        } break;
        
        case TypeKind_Pointer: {
            return type_equals(a->Pointer, b->Pointer);
        } break;
        
        case TypeKind_Function: {
            Type_Function* proc_a = &a->Function;
            Type_Function* proc_b = &b->Function;
            
            if (!type_equals(proc_a->return_type, proc_b->return_type)) {
                return false;
            }
            
            if (array_count(proc_a->arg_types) != array_count(proc_b->arg_types)) {
                return false;
            }
            
            for (int arg_index = 0; 
                 arg_index < array_count(proc_a->arg_types); 
                 arg_index++) {
                Type* arg_a = proc_a->arg_types[arg_index];
                Type* arg_b = proc_b->arg_types[arg_index];
                
                if (!type_equals(arg_a, arg_b)) {
                    return false;
                }
            }
        } break;
        
        default: {
            pln("%", f_string(string_print("%", f_type(a))));
            assert(0 && "not implemented");
        } break;
    }
    
    return true;
}

// NOTE(Alexander): forward declare
struct Ast_File;

bool match_struct_like_args(Type_Context* tcx, Type* formal_type, int first_field, int last_field, Ast* args, bool report_error);
Type* type_infer_statement(Type_Context* tcx, Ast* stmt, bool report_error);
Type* type_infer_expression(Type_Context* tcx, Ast* expr, Type* parent_type, bool report_error);

bool type_check_assignment(Type_Context* tcx, Type* lhs, Type* rhs, bool rhs_is_value, Span span,
                           Operator op=Op_Assign, bool report_error=true);

struct Create_Type_From_Ast_Result {
    Type* type;
    Ast_Decl_Modifier mods;
};


Create_Type_From_Ast_Result create_type_from_ast(Type_Context* tcx, Ast* ast, bool report_error);

Entity resolve_entity_from_identifier(Type_Context* tcx, string_id ident, Span span, bool report_error);
Type* resolve_typedef_from_identifier(Type_Context* tcx, string_id ident, Span span, bool report_error);

Type* save_operator_overload(Type_Context* tcx, Type* type, Operator op, Span span, bool report_error);
