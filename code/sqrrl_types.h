struct Interp;
struct Interp_Value;
struct Type;
struct Ast;



union Limits {
    struct {
        s64 min_value;
        s64 max_value;
    };
    struct {
        u64 min_unsigned_value;
        u64 max_unsigned_value;
    };
};

inline Limits
s64_limits(s64 min_value, s64 max_value) {
    Limits result;
    result.min_value = min_value;
    result.max_value = max_value;
    return result;
}

inline Limits
u64_limits(u64 max_value) {
    Limits result;
    result.min_unsigned_value = 0;
    result.max_unsigned_value = max_value;
    return result;
}


#define S64Limits(v) (s64) v
#define U64Value(v) (u64) v

#define DEF_BASIC_TYPES \
BASIC(Invalid, BasicFlag_Invalid,                      Kw_invalid, 0, {}) \
BASIC(bool,    BasicFlag_Integer,                      Kw_bool, 1, s64_limits(0, 1)) \
BASIC(s8,      BasicFlag_Integer,                      Kw_s8, 1,   s64_limits(S8_MIN, S8_MAX)) \
BASIC(s16,     BasicFlag_Integer,                      Kw_s16, 2,  s64_limits(S16_MIN, S16_MAX)) \
BASIC(s32,     BasicFlag_Integer,                      Kw_s32, 4,  s64_limits(S32_MIN, S32_MAX)) \
BASIC(s64,     BasicFlag_Integer,                      Kw_s64, 8,  s64_limits(S64_MIN, S64_MAX)) \
BASIC(smm,     BasicFlag_Integer,                      Kw_smm, -1, s64_limits(SMM_MIN, SMM_MAX)) \
BASIC(int,     BasicFlag_Integer,                      Kw_int, 4,  s64_limits(INT_MIN, INT_MAX)) \
BASIC(u8,      BasicFlag_Unsigned | BasicFlag_Integer, Kw_u8, 1,   u64_limits(U8_MAX)) \
BASIC(u16,     BasicFlag_Unsigned | BasicFlag_Integer, Kw_u16, 2,  u64_limits(U16_MAX)) \
BASIC(u32,     BasicFlag_Unsigned | BasicFlag_Integer, Kw_u32, 4,  u64_limits(U32_MAX)) \
BASIC(u64,     BasicFlag_Unsigned | BasicFlag_Integer, Kw_u64, 8,  u64_limits(U64_MAX)) \
BASIC(umm,     BasicFlag_Unsigned | BasicFlag_Integer, Kw_umm, -1, u64_limits(UMM_MAX)) \
BASIC(uint,    BasicFlag_Unsigned | BasicFlag_Integer, Kw_uint, 4, u64_limits(UINT_MAX)) \
BASIC(f32,     BasicFlag_Floating,                     Kw_f32, 4,  {}) \
BASIC(f64,     BasicFlag_Floating,                     Kw_f64, 8,  {}) \
BASIC(string,  BasicFlag_String,                       Kw_string, -1, {}) \
BASIC(cstring, BasicFlag_String,                       Kw_cstring, -1, {})


enum Basic_Type {
#define BASIC(basic_ident, ...) Basic_##basic_ident,
    DEF_BASIC_TYPES
#undef BASIC
};


enum {
    BasicFlag_Invalid = 0,
    BasicFlag_Integer = bit(1),
    BasicFlag_Unsigned = bit(2),
    BasicFlag_Floating = bit(3),
    BasicFlag_String = bit(4),
    
    BasicFlag_Numeric = BasicFlag_Integer | BasicFlag_Floating,
};


enum Type_Kind {
    TypeKind_Unresolved,
    TypeKind_Void,
    TypeKind_Basic,
    TypeKind_Array,
    TypeKind_Struct,
    TypeKind_Union,
    TypeKind_Enum,
    TypeKind_Function,
    TypeKind_Pointer,
};

typedef map(string_id, s32) Ident_Mapper;

struct Type_Struct {
    array(Type*)* types;
    array(string_id)* idents;
    array(umm)* offsets;
    Ident_Mapper* ident_to_index;
};

struct Struct_Field_Info {
    Type* type;
    smm offset;
};

inline Struct_Field_Info
get_field_info(Type_Struct* t_struct, string_id ident) {
    Struct_Field_Info result;
    int field_index = map_get(t_struct->ident_to_index, ident);
    result.type = t_struct->types[field_index];
    result.offset = t_struct->offsets[field_index];
    return result;
}


struct Type_Union {
    array(Type*)* types;
    array(string_id)* idents;
};

struct Type_Enum {
    map(string_id, Value)* values;
    Type* type;
};

struct Type_Function {
    array(string_id)* arg_idents;
    array(Type*)* arg_types;
    Ident_Mapper* ident_to_index;
    Type* return_type;
    Ast* block;
    Value (*interp_intrinsic)(Interp*, array(Interp_Value)*); // TODO(Alexander): temporary intrinsic definition
    void* intrinsic;
    string_id ident;
    b32 is_variadic;
};

struct Type {
    Type_Kind kind;
    
    union {
        struct {
            Basic_Type kind;
            u32 flags;
            Limits limits;
        } Basic;
        
        struct {
            Type* type;
            smm capacity;
            b32 is_dynamic;
        } Array;
        
        Type_Struct Struct;
        Type_Union Union;
        Type_Enum Enum;
        Type_Function Function;
        
        Type* Pointer;
    };
    
    string_id ident;
    s32 size;
    s32 align;
};

Type basic_type_definitions[] = {
#define BASIC(ident, flags, keyword, size, limits) \
{ TypeKind_Basic, { Basic_##ident, flags, limits }, keyword, size, size },
    DEF_BASIC_TYPES
#undef BASIC
};
Type unresolved_type_definition = { TypeKind_Unresolved };
Type void_type_definition = { TypeKind_Void };


global Type* t_unresolve = &unresolved_type_definition;
global Type* t_void = &void_type_definition;
#define BASIC(ident, ...) global Type* t_##ident = &basic_type_definitions[Basic_##ident];
DEF_BASIC_TYPES
#undef BASIC


void
string_builder_push(String_Builder* sb, Type* type) {
    if (!type) return;
    
    switch (type->kind) {
        case TypeKind_Unresolved: {
            string_builder_push(sb, "unresolved");
        } break;
        
        case TypeKind_Void: {
            string_builder_push(sb, "void");
        } break;
        
        case TypeKind_Basic: {
            string_builder_push(sb, vars_load_string(type->ident));
        } break;
        
        case TypeKind_Array: {
            if (type->Array.capacity > 0) {
                string_builder_push_format(sb, "[%]", f_smm(type->Array.capacity));
            } else {
                string_builder_push(sb, "[..]");
            }
            string_builder_push(sb, type->Array.type);
        } break;
        
        case TypeKind_Pointer: {
            string_builder_push(sb, type->Pointer);
            string_builder_push(sb, "*");
        } break;
        
        case TypeKind_Struct: {
            string_builder_push(sb, type->ident);
        } break;
        
        case TypeKind_Union: {
            string_builder_push(sb, "union");
        } break;
        
        case TypeKind_Enum: {
            string_builder_push(sb, "enum");
        } break;
        
        case TypeKind_Function: {
            string_builder_push(sb, type->Function.return_type);
            string_builder_push(sb, " ");
            if (type->Function.ident > 0) {
                string_builder_push(sb, vars_load_string(type->Function.ident));
            }
            string_builder_push(sb, "(");
            
            Type_Function* func = &type->Function;
            for_array_v(func->arg_idents, arg_ident, arg_index) {
                Type* arg_type = func->arg_types[arg_index];
                
                string_builder_push(sb, arg_type);
                string_builder_push(sb, " ");
                string_builder_push(sb, vars_load_string(arg_ident));
                if (arg_index < array_count(func->arg_idents) - 1) {
                    string_builder_push(sb, ", ");
                }
            }
            string_builder_push(sb, ")");
        } break;
        
        default: {
            string_builder_push(sb, "invalid");
        } break;
    }
}

inline Type*
type_deref(Type* type) {
    assert(type && type->kind == TypeKind_Pointer);
    return type->Pointer;
}

void
print_type(Type* type) {
    String_Builder sb = {};
    string_builder_alloc(&sb, 20);
    string_builder_push(&sb, type);
    string result = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(result));
    string_builder_free(&sb);
}
