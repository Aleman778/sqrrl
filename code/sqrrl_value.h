
typedef s32 Unary_Op;
enum {
    UnaryOp_Neg,
    UnaryOp_Not,
    UnaryOp_Bit_Not,
    UnaryOp_Addr_Of,
    UnaryOp_Deref,
};


global cstr unary_op_strings[] = { "-", "!", "~", "&", "*" };

typedef s32 Binary_Op;
enum {
    BinaryOp_Add,
    BinaryOp_Subtract,
    BinaryOp_Multiply,
    BinaryOp_Divide,
    BinaryOp_Modulo,
    BinaryOp_Bitwise_And,
    BinaryOp_Logical_And,
    BinaryOp_Bitwise_Or,
    BinaryOp_Logical_Or,
    BinaryOp_Bitwise_Xor,
    BinaryOp_Shift_Left,
    BinaryOp_Shift_Right,
    BinaryOp_Equals,
    BinaryOp_Not_Equals,
    BinaryOp_Less_Than,
    BinaryOp_Less_Equals,
    BinaryOp_Greater_Than,
    BinaryOp_Greater_Equals,
    BinaryOp_Assign,
    BinaryOp_Add_Assign,
    BinaryOp_Subtract_Assign,
    BinaryOp_Multiply_Assign,
    BinaryOp_Divide_Assign,
    BinaryOp_Modulo_Assign,
    BinaryOp_Bitwise_And_Assign,
    BinaryOp_Bitwise_Or_Assign,
    BinaryOp_Bitwise_Xor_Assign,
    BinaryOp_Shift_Left_Assign,
    BinaryOp_Shift_Right_Assign,
    BinaryOp_Count,
};

global cstr binary_op_strings[] = { 
    "+", "-", "*", "/", "%", "&", "&&", "|", "||", "^", "<<", ">>", "==", "!=", "<", 
    "<=", ">", ">=", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=" };


typedef s32 Associativity;
enum {
    Associative_Left,
    Associative_Right,
};

u8
binary_get_precedence(Binary_Op op) {
    switch (op) {
        case BinaryOp_Multiply:            return 11;
        case BinaryOp_Divide:             return 11;
        case BinaryOp_Modulo:             return 11;
        case BinaryOp_Add:                return 10;
        case BinaryOp_Subtract:           return 10;
        case BinaryOp_Shift_Left:         return 9;
        case BinaryOp_Shift_Right:        return 9;
        case BinaryOp_Less_Than:          return 8;
        case BinaryOp_Less_Equals:        return 8;
        case BinaryOp_Greater_Than:       return 8;
        case BinaryOp_Greater_Equals:     return 8;
        case BinaryOp_Equals:             return 7;
        case BinaryOp_Not_Equals:         return 7;
        case BinaryOp_Bitwise_And:        return 6;
        case BinaryOp_Bitwise_Or:         return 5;
        case BinaryOp_Bitwise_Xor:        return 4;
        case BinaryOp_Logical_And:        return 3;
        case BinaryOp_Logical_Or:         return 2;
        case BinaryOp_Assign:             return 1;
        case BinaryOp_Add_Assign:         return 1;
        case BinaryOp_Subtract_Assign:    return 1;
        case BinaryOp_Multiply_Assign:    return 1;
        case BinaryOp_Divide_Assign:      return 1;
        case BinaryOp_Modulo_Assign:      return 1;
        case BinaryOp_Bitwise_And_Assign: return 1;
        case BinaryOp_Bitwise_Or_Assign:  return 1;
        case BinaryOp_Bitwise_Xor_Assign: return 1;
        case BinaryOp_Shift_Left_Assign:  return 1;
        case BinaryOp_Shift_Right_Assign: return 1;
    }
    assert(0 && "bug"); 
    return 0;
}

Associativity
binary_get_associativity(Binary_Op op) {
    switch (op) {
        case BinaryOp_Multiply:
        case BinaryOp_Divide:
        case BinaryOp_Modulo:
        case BinaryOp_Add:
        case BinaryOp_Subtract:
        case BinaryOp_Shift_Left:
        case BinaryOp_Shift_Right:
        case BinaryOp_Less_Than:
        case BinaryOp_Less_Equals:
        case BinaryOp_Greater_Than:
        case BinaryOp_Greater_Equals:
        case BinaryOp_Equals:
        case BinaryOp_Not_Equals:
        case BinaryOp_Bitwise_And:
        case BinaryOp_Bitwise_Or:
        case BinaryOp_Bitwise_Xor:
        case BinaryOp_Logical_And:
        case BinaryOp_Logical_Or: {
            return Associative_Left;
        }
        
        case BinaryOp_Assign:
        case BinaryOp_Add_Assign:
        case BinaryOp_Subtract_Assign:
        case BinaryOp_Multiply_Assign:
        case BinaryOp_Divide_Assign:
        case BinaryOp_Modulo_Assign:
        case BinaryOp_Bitwise_And_Assign:
        case BinaryOp_Bitwise_Or_Assign:
        case BinaryOp_Bitwise_Xor_Assign:
        case BinaryOp_Shift_Left_Assign:
        case BinaryOp_Shift_Right_Assign: {
            return Associative_Right;
        }
    }
    assert(0 && "bug");
    return Associative_Left;
}

typedef s32 Ternary_Op;
enum {
    TernaryOp_Conditional, // expr ? true : false
};


struct Pointer_Value {
    smm address;
    //Type* type; // TODO(alexander): missing type definition
};

// NOTE(alexander): forward declare.
struct Value;

struct Array_Value {
    //Type* element_type; // TODO(alexander): missing type definition
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
        str string;
    };
};

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
    result.signed_int = (u64) value;
    return result;
}

inline Value
create_unsigned_int_value(u64 value) {
    Value result;
    result.type = Value_unsigned_int;
    result.unsigned_int = (u64) value;
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
    result.array = { values, count, capacity };
    return result;
}

inline Value
create_string_value(str value) {
    Value result;
    result.type = Value_string;
    result.string = value;
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
            printf("%s", val->string);
        } break;
    }
}
