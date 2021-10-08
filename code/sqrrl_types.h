
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
    PrimitiveTypeKind_char,
    PrimitiveTypeKind_string,
};

enum Type_Kind {
    TypeKind_Void,
    TypeKind_Primitive, // size in bytes/bits, signedness, min, max value.
    TypeKind_Array, // is it dynamic or does it have a max count if any
    TypeKind_Struct, // types for each field
    TypeKind_Union, // types for each field
    TypeKind_Function, // return type and argument types
    TypeKind_Pointer, // referenced type
};

struct Primitive_Type {
    Primitive_Type_Kind kind;
    s32 size;
    b32 signedness;
    Value max_value;
    Value min_value;
};

struct Type {
    Type_Kind kind;
    union {
        Primitive_Type Primitive;
        
    };
    s32 cached_size;
    s32 cached_align;
};

global struct { string_id key; Type* value; }* global_type_table = 0;

Type*
get_type_definition(string_id ident) {
    return map_get(global_type_table, ident);
}
