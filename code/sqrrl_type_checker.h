
struct Compilation_Unit {
    Ast* ast;
    string_id ident;
};

struct Type_Context {
    Memory_Arena* type_arena;
    
    map(string_id, Type*)* locals;
    map(string_id, Type*)* globals;
    
    map(string_id, Type*)* local_type_table;
    map(string_id, Type*)* global_type_table;
    
    Type* return_type;
    
    b32 block_depth;
    s32 error_count;
    s32 warning_count;
};


bool
type_equals(Type* a, Type* b) {
    if (a->kind != b->kind) {
        return false;
    }
    
    switch (a->kind) {
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
        
        default: {
            pln("%", f_string(string_format("%", f_type(a))));
            assert(0 && "not implemented");
        } break;
    }
    
    return true;
}

// NOTE(Alexander): forward declare
struct Ast_File;

s32 type_check_ast_file(Ast_File* ast_file);