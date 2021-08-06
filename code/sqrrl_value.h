
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
    BinaryOp_Shift_left,
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
};

global cstr binary_op_strings[] = { 
    "+", "-", "*", "/", "%", "&", "&&", "|", "||", "^", "<<", ">>", "==", "!=", "<", 
    "<=", ">", ">=", "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=" };


u8
binary_get_precedence(Binary_Op op) {
    switch (op) {
        case BinaryOp_Mul:                return 11;
        case BinaryOp_Div:                return 11;
        case BinaryOp_Mod:                return 11;
        case BinaryOp_Add:                return 10;
        case BinaryOp_Sub:                return 10;
        case BinaryOp_Shl:                return 9;
        case BinaryOp_Shr:                return 9;
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

Assoc
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
            return Assoc::Left;
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
            return Assoc::Right;
        }
    }
    assert(0 && "bug");
    return Assoc::Left;
}

typedef s32 Ternary_Op;
enum {
    TernaryOp_Conditional, // expr ? true : false
};


struct Pointer_Value {
    smm address;
    //Type* type; // TODO(alexander): missing type definition
};

struct Array_Value {
    //Type* element_type; // TODO(alexander): missing type definition
    void* elements;
    smm count;
    smm capacity;
};

typedef s32 Value_Type;
enum {
    Value_boolean,
    Value_sint,
    Value_uint,
    Value_floating,
    Value_pointer,
    Value_array,
};

struct Value {
    Value_Type type;
    union {
        bool boolean;
        s64 sint;
        u64 uint;
        f64 floating;
        Pointer_Value pointer;
        Array_Value array;
    };
};

void print_value(Value* val) {
    switch (val->type) {
        case Value_boolean: {
            printf(val->boolean ? "true" : "false");
        } break;
        
        case Value
    }
}
