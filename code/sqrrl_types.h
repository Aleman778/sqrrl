
// NOTE(Alexander): this has to be synched with the keywords in sqrrl_vars.h
#define DEF_PRIMITIVE_TYPES \
PRIMITIVE(int, 4, signed, INT_MAX, INT_MIN) \
PRIMITIVE(s8, 1, signed, S8_MAX, S8_MIN) \
PRIMITIVE(s16, 2, signed, S16_MAX, S16_MIN) \
PRIMITIVE(s32, 4, signed, S32_MAX, S32_MIN) \
PRIMITIVE(s64, 8, signed, S64_MAX, S64_MIN) \
PRIMITIVE(smm, 0, signed, 0, 0) \
PRIMITIVE(uint, 4, unsigned, UINT_MAX, 0) \
PRIMITIVE(u8, 1, unsigned, U8_MAX, 0) \
PRIMITIVE(u16, 2, unsigned, U16_MAX, 0) \
PRIMITIVE(u32, 4, unsigned, U32_MAX, 0) \
PRIMITIVE(u64, 8, unsigned, U64_MAX, 0) \
PRIMITIVE(umm, 0, unsigned, 0, 0) \
PRIMITIVE(f32, 4, signed, 0, 0) \
PRIMITIVE(f64, 8, signed, 0, 0) \
PRIMITIVE(char, 1, unsigned, CHAR_MAX, CHAR_MIN) \
PRIMITIVE(bool, 1, signed, BOOL_MAX, BOOL_MIN) \
PRIMITIVE(b32, 1, signed, S32_MAX, S32_MIN) \
PRIMITIVE(void, 0, unsigned, 0, 0)


enum Primitive_Type_Kind {
#define PRIMITIVE(symbol, ...) PrimitiveTypeKind_##symbol,
    DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
};

enum Type_Kind {
    TypeKind_Void,
    TypeKind_Primitive,
    TypeKind_Array,
    TypeKind_String,
    TypeKind_Tuple,
    TypeKind_Struct,
    TypeKind_Union,
    TypeKind_Enum,
    TypeKind_Function,
    TypeKind_Pointer,
    TypeKind_Unresolved
};

struct Type_Table {
    struct { string_id key; Type* value; }* ident_to_type;
    struct { string_id key; smm value; }* ident_to_offset;
    string_id* idents;
    int count;
};

// NOTE(Alexander): forward declare
struct Ast;

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
            smm capacity; // zero means infinite
        } Array;
        
        struct {
            Type* types;
        } Tuple;
        
        Type_Table Struct_Or_Union;
        
        struct {
            Type_Table fields;
        } Union;
        
        struct {
            Type* type;
            struct { string_id key; Value value; }* values;
        } Enum;
        
        struct {
            Type_Table arguments;
            Type* return_value;
            Ast* block;
            string_id ident;
        } Function;
        
        Type* Pointer;
    };
    
    s32 cached_size;
    s32 cached_align;
};

global Type global_primitive_types[] = {
#define S_signed true
#define S_unsigned false
#define VAL(signedness, val) create_##signedness##_int_value(val)
#define PRIMITIVE(symbol, size, sign, max, min) { \
TypeKind_Primitive, { \
PrimitiveTypeKind_##symbol, size, S_##sign, VAL(sign, max), VAL(sign, min)  \
}, \
size, size \
},
    DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
#undef VAL
};

bool
type_equals(Type* a, Type* b) {
    if (a->kind != b->kind) {
        return false;
    }
    
    switch (a->kind) {
        case TypeKind_Primitive: {
            if (a->Primitive.kind != b->Primitive.kind) {
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
        
        default: {
            assert(0 && "not implemented");
        } break;
    }
    
    return true;
}