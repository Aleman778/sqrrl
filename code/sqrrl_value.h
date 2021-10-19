
#define DEF_UNARY_OPS \
UNOP(None,        "") \
UNOP(Negate,      "-") \
UNOP(Not,         "!") \
UNOP(Bitwise_Not, "~") \
UNOP(Address_Of,  "&") \
UNOP(Dereference,   "*")

#define DEF_BINARY_OPS \
BINOP(None,               "",   0,  Assoc_Left) \
BINOP(Multiply,           "*",  11, Assoc_Left) \
BINOP(Divide,             "/",  11, Assoc_Left) \
BINOP(Modulo,             "%",  11, Assoc_Left) \
BINOP(Add,                "+",  10, Assoc_Left) \
BINOP(Subtract,           "-",  10, Assoc_Left) \
BINOP(Shift_Left,         "<<", 9,  Assoc_Left) \
BINOP(Shift_Right,        ">>", 9,  Assoc_Left) \
BINOP(Less_Than,          "<",  8,  Assoc_Left) \
BINOP(Less_Equals,        "<=", 8,  Assoc_Left) \
BINOP(Greater_Than,       ">",  8,  Assoc_Left) \
BINOP(Greater_Equals,     ">=", 8,  Assoc_Left) \
BINOP(Equals,             "==", 7,  Assoc_Left) \
BINOP(Not_Equals,         "!=", 7,  Assoc_Left) \
BINOP(Bitwise_And,        "&",  6,  Assoc_Left) \
BINOP(Bitwise_Or,         "|",  5,  Assoc_Left) \
BINOP(Bitwise_Xor,        "^",  4,  Assoc_Left) \
BINOP(Logical_And,        "&&", 3,  Assoc_Left) \
BINOP(Logical_Or,         "||", 2,  Assoc_Left) \
BINOP(Assign,             "=",  1,  Assoc_Right) \
BINOP(Add_Assign,         "+=", 1,  Assoc_Right) \
BINOP(Subtract_Assign,    "-=", 1,  Assoc_Right) \
BINOP(Multiply_Assign,    "*=", 1,  Assoc_Right) \
BINOP(Divide_Assign,      "/=", 1,  Assoc_Right) \
BINOP(Modulo_Assign,      "%=", 1,  Assoc_Right) \
BINOP(Bitwise_And_Assign, "&=", 1,  Assoc_Right) \
BINOP(Bitwise_Or_Assign,  "|=", 1,  Assoc_Right) \
BINOP(Bitwise_Xor_Assign, "^=", 1,  Assoc_Right) \
BINOP(Shift_Left_Assign,  "<<", 1,  Assoc_Right) \
BINOP(Shift_Right_Assign, ">>", 1,  Assoc_Right)

enum Assoc {
    Assoc_Left,
    Assoc_Right,
};

enum Unary_Op {
#define UNOP(symbol, name) UnaryOp_##symbol,
    DEF_UNARY_OPS
#undef UNOP
};

enum Binary_Op {
#define BINOP(symbol, name, prec, assoc) BinaryOp_##symbol,
    DEF_BINARY_OPS
#undef BINOP
};

global cstring unary_op_strings[] = {
#define UNOP(symbol, name) name,
    DEF_UNARY_OPS
#undef UNOP
};

global cstring binary_op_strings[] = {
#define BINOP(symbol, name, prec, assoc) name,
    DEF_BINARY_OPS
#undef BINOP
};

u8
binary_get_prec(Binary_Op op) {
    switch (op) {
#define BINOP(symbol, name, prec, assoc) case BinaryOp_##symbol: return prec;
        DEF_BINARY_OPS
#undef BINOP
    }
    assert(0 && "bug");
    return 0;
}

Assoc
binary_get_assoc(Binary_Op op) {
    switch (op) {
#define BINOP(symbol, name, prec, assoc) case BinaryOp_##symbol: return assoc;
        DEF_BINARY_OPS
#undef BINOP
    }
    
    assert(0 && "bug");
    return Assoc_Left;
}

enum Ternary_Op {
    TernaryOp_Conditional, // expr ? true : false
};

// Forward declare type
struct Type;

struct Pointer_Value {
    smm address;
    Type* type;
};

// NOTE(alexander): forward declare.
struct Value;

struct Array_Value {
    Type* element_type;
    Value* elements;
    smm count;
    smm capacity;
};

// TODO(alexander): add more value types
typedef s32 Value_Type;
enum {
    Value_boolean,
    Value_signed_int,
    Value_unsigned_int,
    Value_floating,
    Value_pointer,
    Value_array,
    Value_string,
};

struct Value {
    Value_Type type;
    union {
        bool boolean;
        s64 signed_int;
        u64 unsigned_int;
        f64 floating;
        Pointer_Value pointer;
        Array_Value array;
        string str;
    };
};

inline u64
value_to_u64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.boolean == true ? 1 : 0;
        case Value_signed_int: return (u64) value.signed_int;
        case Value_floating: return (u64) value.floating;
        default: return (u64) value.unsigned_int;
    }
}

inline Value
create_boolean_value(bool value) {
    Value result;
    result.type = Value_boolean;
    result.boolean = (u64) value;
    return result;
}

inline Value
create_signed_int_value(s64 value) {
    Value result;
    result.type = Value_signed_int;
    result.signed_int = value;
    return result;
}

inline Value
create_unsigned_int_value(u64 value) {
    Value result;
    result.type = Value_unsigned_int;
    result.unsigned_int = value;
    return result;
}

inline Value
create_floating_value(f64 value) {
    Value result;
    result.type = Value_floating;
    result.floating = value;
    return result;
}

inline Value
create_pointer_value(smm value) {
    Value result;
    result.type = Value_pointer;
    result.pointer = { value };
    return result;
}

inline Value
create_array_value(Value* values, smm count, smm capacity) {
    Value result;
    result.type = Value_array;
    result.array = { 0, values, count, capacity };
    return result;
}

inline Value
create_string_value(string value) {
    Value result;
    result.type = Value_string;
    result.str = value;
    return result;
}

void print_value(Value* val) {
    switch (val->type) {
        case Value_boolean: {
            printf(val->boolean ? "true" : "false");
        } break;
        
        case Value_signed_int: {
            printf("%lld", val->signed_int);
        } break;
        
        case Value_unsigned_int: {
            printf("%llu", val->signed_int);
        } break;
        
        case Value_floating: {
            printf("%f", val->floating);
        } break;
        
        case Value_pointer: {
            printf("0x%I64X", val->pointer.address);
        } break;
        
        case Value_array: {
            printf("[");
            Array_Value* arr = &val->array;
            for (smm index = 0; index < arr->count; index++) {
                if (index > 0) printf(", ");
                print_value(&arr->elements[index]);
            }
            printf("[");
        } break;
        
        case Value_string: {
            printf("%s", val->str);
        } break;
    }
}
