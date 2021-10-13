
enum Primitive_Type_Kind {
    PrimitiveTypeKind_int,
    PrimitiveTypeKind_s8,
    PrimitiveTypeKind_s16,
    PrimitiveTypeKind_s32,
    PrimitiveTypeKind_s64,
    PrimitiveTypeKind_smm,
    PrimitiveTypeKind_uint,
    PrimitiveTypeKind_u8,
    PrimitiveTypeKind_u16,
    PrimitiveTypeKind_u32,
    PrimitiveTypeKind_u64,
    PrimitiveTypeKind_umm,
    PrimitiveTypeKind_f32,
    PrimitiveTypeKind_f64,
    PrimitiveTypeKind_char,
    PrimitiveTypeKind_string,
    PrimitiveTypeKind_bool,
    PrimitiveTypeKind_void
};

enum Type_Kind {
    TypeKind_Void,
    TypeKind_Primitive,
    TypeKind_Array,
    TypeKind_Tuple,
    TypeKind_Struct,
    TypeKind_Union,
    TypeKind_Function,
    TypeKind_Pointer,
    TypeKind_Unresolved
};

typedef struct { string_id key; Type* value; }* Type_Table;

struct Type {
    Type_Kind kind;
    union {
        struct {
            Primitive_Type_Kind kind;
            s32 size;
            b32 signedness;
            Value max_value;
            Value min_value;
        } Primitive;
        
        struct {
            Type* type;
            smm capacity;
        } Array;
        
        struct {
            Type* types;
        } Tuple;
        
        struct {
            Type_Table fields;
        } Struct;
        
        struct {
            Type_Table fields;
        } Union;
        
        struct {
            Type_Table arguments;
            Type* return_value;
        } Function;
        
        struct {
            Type* type;
        } Pointer;
        
        struct {
            string_id ident;
        } Unresolved;
    };
    
    s32 cached_size;
    s32 cached_align;
};

// TODO(alexander): thesea are globals for now, this code is temporary
global Type_Table global_type_table = 0;

void
put_type_definition(string_id ident, Type type) {
    map_put(ident, type);
}

Type*
get_type_definition(string_id ident) {
    return map_get(ident);
}
