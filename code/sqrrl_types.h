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

#define DEF_BASIC_TYPES \
BASIC(Invalid, BasicFlag_Invalid,                      Kw_invalid, 0, {}) \
BASIC(bool,    BasicFlag_Integer | BasicFlag_Boolean,  Kw_bool, 1, s64_limits(0, 1)) \
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
    BasicFlag_Boolean = bit(1),
    BasicFlag_Integer = bit(2),
    BasicFlag_Unsigned = bit(3),
    BasicFlag_Floating = bit(4),
    BasicFlag_String = bit(5),
    
    BasicFlag_Numeric = BasicFlag_Integer | BasicFlag_Floating,
};

enum Type_Kind {
    TypeKind_Unresolved,
    TypeKind_Void,
    TypeKind_Any,
    TypeKind_Type,
    TypeKind_Basic,
    TypeKind_Array,
    TypeKind_Struct,
    TypeKind_Union,
    TypeKind_Enum,
    TypeKind_Function,
    TypeKind_Pointer,
};

typedef map(string_id, s32) Ident_Mapper;

struct Struct_Like_Info {
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
get_field_info_by_index(Struct_Like_Info* struct_info, int field_index) {
    assert(field_index < array_count(struct_info->types));
    Struct_Field_Info result = {};
    result.type = struct_info->types[field_index];
    result.offset = struct_info->offsets[field_index];
    return result;
}

inline Struct_Field_Info
get_field_info(Struct_Like_Info* struct_info, string_id ident) {
    int field_index = map_get(struct_info->ident_to_index, ident);
    return get_field_info_by_index(struct_info, field_index);
}

struct Type_Struct {
    Struct_Like_Info info;
};

struct Type_Union {
    Struct_Like_Info info;
};

struct Type_Enum {
    map(string_id, Value)* values;
    Type* type;
};

enum Calling_Convention {
    //CConv_Sqrrl = 0, // TODO(Alexander): implement custom calling convention
    CConv_Windows_X64,
};

// NOTE(Alexander): forward declare
struct Compilation_Unit;

struct Type_Function {
    array(string_id)* arg_idents;
    array(Type*)* arg_types;
    Ident_Mapper* ident_to_index;
    array(Ast*)* default_args;
    Type* return_type;
    Calling_Convention cconv;
    Compilation_Unit* unit;
    Value (*interp_intrinsic)(Interp*, array(Interp_Value)*); // TODO(Alexander): temporary intrinsic definition
    void* intrinsic; // TODO: intrinsic is a bad name, used as normal function pointer too
    string_id ident;
    s32 first_default_arg_index;
    b32 is_variadic;
    b32 dump_bytecode;
};

struct Type_Array {
    Type* type;
    smm capacity;
    b32 is_dynamic;
    b32 is_inplace; // NOTE(Alexander): meaning we store array directly rather than a pointer
};

struct Type {
    Type_Kind kind;
    
    union {
        struct {
            Basic_Type kind;
            u32 flags;
            Limits limits;
        } Basic;
        
        Type_Array Array;
        Struct_Like_Info Struct_Like;
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

inline bool 
is_valid_type(Type* type) {
    return (type &&
            type->kind != TypeKind_Unresolved &&
            type->kind != TypeKind_Void);
}

Type basic_type_definitions[] = {
#define BASIC(ident, flags, keyword, size, limits) \
{ TypeKind_Basic, { Basic_##ident, flags, limits }, keyword, size, size },
    DEF_BASIC_TYPES
#undef BASIC
};
Type unresolved_type_definition = { TypeKind_Unresolved };
Type void_type_definition = { TypeKind_Void };
Type any_type_definition = { TypeKind_Any };
Type type_definition = { TypeKind_Type };


internal Type 
create_void_ptr_type_definition() {
    Type t = {};
    t.kind = TypeKind_Pointer;
    t.Pointer = &void_type_definition;
    return t;
}
Type void_ptr_type_definition = create_void_ptr_type_definition();

global Type* t_unresolve = &unresolved_type_definition;
global Type* t_void = &void_type_definition;
global Type* t_void_ptr = &void_ptr_type_definition;
global Type* t_any = &any_type_definition;
global Type* t_type = &type_definition;
#define BASIC(ident, ...) global Type* t_##ident = &basic_type_definitions[Basic_##ident];
DEF_BASIC_TYPES
#undef BASIC

Format_Type
convert_type_to_format_type(Type* type) {
    switch (type->kind) {
        case TypeKind_Basic: {
            switch (type->Basic.kind) {
                case Basic_bool: return FormatType_bool;
                case Basic_s8: return FormatType_s8;
                case Basic_s16: return FormatType_s16;
                case Basic_s32: return FormatType_s32;
                case Basic_s64: return FormatType_s64;
                case Basic_int: return FormatType_int;
                
                case Basic_u8: return FormatType_u8;
                case Basic_u16: return FormatType_u16;
                case Basic_u32: return FormatType_u32;
                case Basic_u64: return FormatType_u64;
                case Basic_uint: return FormatType_uint;
                
                case Basic_f32: return FormatType_f32;
                case Basic_f64: return FormatType_f64;
                
                case Basic_string: return FormatType_string;
                case Basic_cstring: return FormatType_cstring;
            }
        } break;
        
        
        case TypeKind_Type:
        case TypeKind_Pointer: {
            return FormatType_u64_HEX;
        } break;
        
        case TypeKind_Enum: {
            return convert_type_to_format_type(type->Enum.type);
        } break;
        
        default: {
            unimplemented;
        } break;
    }
    
    return FormatType_None;
}

Format_Type 
convert_value_type_to_format_type(Value_Type type) {
    switch (type) {
        case Value_boolean: return FormatType_bool;
        case Value_signed_int: return FormatType_s64;
        case Value_unsigned_int: return FormatType_u64;
        case Value_floating: return FormatType_f64;
        case Value_pointer: return FormatType_smm;
        //Value_array,
        case Value_string: return FormatType_string;
    }
    
    return FormatType_None;
}

void
string_builder_push(String_Builder* sb, Type* type) {
    if (!type) {
        string_builder_push(sb, "null");
        return;
    }
    
    switch (type->kind) {
        case TypeKind_Unresolved: {
            string_builder_push(sb, "unresolved");
        } break;
        
        case TypeKind_Void: {
            string_builder_push(sb, "void");
        } break;
        
        case TypeKind_Any: {
            string_builder_push(sb, "any");
        } break;
        
        case TypeKind_Type: {
            string_builder_push(sb, "Type");
        } break;
        
        case TypeKind_Basic: {
            string_builder_push(sb, vars_load_string(type->ident));
        } break;
        
        case TypeKind_Array: {
            string_builder_push(sb, "[");
            if (type->Array.capacity > 0) {
                string_builder_push_format(sb, "%", f_smm(type->Array.capacity));
            }
            if (type->Array.is_dynamic) {
                string_builder_push(sb, "..");
            }
            string_builder_push(sb, "]");
            
            string_builder_push(sb, type->Array.type);
        } break;
        
        case TypeKind_Pointer: {
            string_builder_push(sb, type->Pointer);
            string_builder_push(sb, "*");
        } break;
        
        case TypeKind_Struct: 
        case TypeKind_Union:
        case TypeKind_Enum: {
            string_builder_push(sb, type->ident);
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
                if (arg_ident > 0) {
                    string_builder_push(sb, " ");
                    string_builder_push(sb, vars_load_string(arg_ident));
                }
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

typedef umm intrin_type_def(Type*);

umm
type_sizeof(Type* type) {
    return type->size;
}

umm
type_alignof(Type* type) {
    return type->align;
}

//

// Type introspection
//

enum TI_Basic_Type_Info {
    TI_Bool,
    TI_S8,
    TI_S16,
    TI_S32,
    TI_S64,
    TI_U8,
    TI_U16,
    TI_U32,
    TI_U64,
    TI_F32,
    TI_F64,
    TI_String,
    TI_CString
};

struct Type_Info;

struct TI_Struct_Field_Info {
    Type_Info* type;
    string ident;
    s64 offset;
};

struct TI_Struct_Type_Info {
    string ident;
    TI_Struct_Field_Info* fields;
    smm count;
};

struct TI_Enum_Type_Info {
    string ident;
    Type_Info* type;
    string* names;
    smm count;
};

struct TI_Array_Type_Info {
    Type_Info* elem_type;
    smm elem_size;
    smm fixed_count; // -1 => if value stores the count
};

struct Var_Arg {
    Type_Info* type;
    smm data_size;
};

struct Var_Args {
    u8* data;
    Var_Arg* types;
    smm count;
};

struct Type_Info {
    Type_Kind kind;
    
    union {
        TI_Basic_Type_Info Basic;
        TI_Struct_Type_Info Struct;
        TI_Enum_Type_Info Enum;
        TI_Array_Type_Info Array;
    };
    
    s32 size;
    s32 align;
};

//enum Section {
//Text_Section,
//Read_Only_Data_Section,
//Read_Write_Data_Section,
//Relocation_Section,
//}

Exported_Data export_var_args_info(Data_Packer* packer, int var_arg_start, Ast* actual_arguments);

Exported_Data export_type_info(Data_Packer* packer, Type* type);

void print_type(Type* type);

inline Type*
type_deref(Type* type) {
    assert(type && type->kind == TypeKind_Pointer);
    return type->Pointer;
}

// TODO(Alexander): dummy functions
inline s64
type_of(Type* type) {
    return (s64) type;
}
