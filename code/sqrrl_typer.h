
// NOTE(Alexander): this has to be synched with the keywords in sqrrl_vars.h
#define DEF_PRIMITIVE_TYPES \
PRIMITIVE(int, 4, signed, INT_MAX, INT_MIN) \
PRIMITIVE(s8, 1, signed, S8_MAX, S8_MIN) \
PRIMITIVE(s16, 2, signed, S16_MAX, S16_MIN) \
PRIMITIVE(s32, 4, signed, S32_MAX, S32_MIN) \
PRIMITIVE(s64, 8, signed, S64_MAX, S64_MIN) \
PRIMITIVE(smm, 8, signed, SMM_MAX, SMM_MIN) \
PRIMITIVE(uint, 4, unsigned, UINT_MAX, 0) \
PRIMITIVE(u8, 1, unsigned, U8_MAX, 0) \
PRIMITIVE(u16, 2, unsigned, U16_MAX, 0) \
PRIMITIVE(u32, 4, unsigned, U32_MAX, 0) \
PRIMITIVE(u64, 8, unsigned, U64_MAX, 0) \
PRIMITIVE(umm, 8, unsigned, SMM_MAX, 0) \
PRIMITIVE(f32, 4, signed, 0, 0) \
PRIMITIVE(f64, 8, signed, 0, 0) \
PRIMITIVE(char, 1, unsigned, CHAR_MAX, CHAR_MIN) \
PRIMITIVE(bool, 1, signed, BOOL_MAX, BOOL_MIN) \
PRIMITIVE(b32, 1, signed, S32_MAX, S32_MIN) \
PRIMITIVE(void, 0, unsigned, 0, 0)


enum Primitive_Type_Kind {
#define PRIMITIVE(symbol, ...) PrimitiveType_##symbol,
    DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
};

enum Type_Kind {
    Type_None,
    Type_Void,
    Type_Primitive,
    Type_Array,
    Type_String,
    Type_Tuple,
    Type_Struct,
    Type_Union,
    Type_Enum,
    Type_Function,
    Type_Pointer,
    Type_Unresolved
};

struct Type_Table {
    map(string_id, Type*)* ident_to_type;
    map(string_id, smm)* ident_to_offset;
    array(string_id)* idents;
    int count;
};

// NOTE(Alexander): forward declare
struct Ast;
struct Interp;
struct Interp_Value;

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
            array(Type)* types;
        } Tuple;
        
        Type_Table Struct_Or_Union;
        
        struct {
            Type* type;
            map(string_id, Value)* values;
        } Enum;
        
        struct {
            Type_Table arguments;
            Type* return_value;
            Ast* block;
            Value (*intrinsic)(Interp*, array(Interp_Value)*); // TODO(Alexander): temporary intrinsic definition
            string_id ident;
            b32 is_variadic;
        } Function;
        
        Type* Pointer;
    };
    
    string name;
    s32 cached_size;
    s32 cached_align;
};

global Type global_void_type = { Type_Void };
global Type global_unresolved_type = { Type_Unresolved };

global Type global_primitive_types[] = {
#define S_signed true
#define S_unsigned false
#define VAL(signedness, val) create_##signedness##_int_value(val)
#define PRIMITIVE(symbol, size, sign, max, min) { \
Type_Primitive, { \
PrimitiveType_##symbol, size, S_##sign, VAL(sign, max), VAL(sign, min)  \
}, \
string_lit(#symbol), \
size, size \
},
    DEF_PRIMITIVE_TYPES
#undef PRIMITIVE
#undef VAL
};

// TODO(Alexander): the sizeof/ alignof is only useful info for specific build of compiler
// need to update these for other build targets
global Type global_string_type = { Type_String, {}, {}, sizeof(string), alignof(string) };

bool
type_equals(Type* a, Type* b) {
    if (a->kind != b->kind) {
        return false;
    }
    
    switch (a->kind) {
        case Type_Primitive: {
            if (a->Primitive.kind != b->Primitive.kind) {
                return false;
            }
        } break;
        
        case Type_Array: {
            if (!type_equals(a->Array.type, b->Array.type)) {
                return false;
            }
            if (a->Array.capacity != b->Array.capacity) {
                return false;
            }
        } break;
        
        case Type_Union:
        case Type_Struct: {
            if (a->kind != b->kind) {
                return false;
            }
            
            Type_Table* table_a = &a->Struct_Or_Union;
            Type_Table* table_b = &a->Struct_Or_Union;
            
            if (table_a->count != table_b->count) {
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

void
string_builder_push(String_Builder* sb, Type* type) {
    switch (type->kind) {
        case Type_Void: {
            string_builder_push(sb, "void");
        } break;
        
        case Type_Primitive: {
            string_builder_push(sb, type->name); 
        } break;
        
        case Type_Array: {
            string_builder_push(sb, type->Array.type);
            if (type->Array.capacity == 0) {
                string_builder_push_format(sb, "[%]", f_smm(type->Array.capacity));
            } else {
                string_builder_push(sb, "[..]");
            }
        } break;
        
        case Type_Pointer: {
            string_builder_push(sb, type->Pointer);
            string_builder_push(sb, "*");
        } break;
        
        case Type_String: {
            string_builder_push(sb, "string");
        } break;
        
        case Type_Tuple: {
            string_builder_push(sb, "(");
            for_array(type->Tuple.types, it, _) {
                string_builder_push(sb, it);
            }
            
            string_builder_push(sb, ")");
        } break;
        
        case Type_Struct: {
            string_builder_push(sb, "struct");
        } break;
        
        case Type_Union: {
            string_builder_push(sb, "union");
        } break;
        
        case Type_Enum: {
            string_builder_push(sb, "enum");
        } break;
        case Type_Function: {
            string_builder_push(sb, "function");
        } break;
        
        default: {
            string_builder_push(sb, "invalid");
        }break;
    }
}
