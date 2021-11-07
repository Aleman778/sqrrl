
#define DEF_UNARY_OPS \
UNOP(None,        "") \
UNOP(Negate,      "-") \
UNOP(Not,         "!") \
UNOP(Bitwise_Not, "~") \
UNOP(Address_Of,  "&") \
UNOP(Dereference, "*")

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

bool
is_binary_assign(Binary_Op op) {
    // HACK(Alexander): using assoicativity to figure this out isn't guaranteed to be
    // correct if we modify this later.
    return binary_get_assoc(op) == Assoc_Right;
}

enum Ternary_Op {
    TernaryOp_Conditional, // expr ? true : false
};

// Forward declare type
struct Type;

// NOTE(alexander): forward declare.
struct Value;

struct Array_Value {
    void* elements;
    smm count;
};

enum Value_Type {
    Value_void,
    Value_boolean,
    Value_signed_int,
    Value_unsigned_int,
    Value_floating,
    Value_pointer,
    Value_array,
    Value_string,
    Value_ast_node,
};

// NOTE(Alexander): forward declare
struct Ast;

struct Value {
    Value_Type type;
    union {
        bool boolean;
        s64 signed_int;
        u64 unsigned_int;
        f64 floating;
        smm pointer;
        Array_Value array;
        string str;
        Ast* ast;
        void* data;
    };
};

inline bool
is_void(Value value) {
    return value.type == Value_void;
}

inline bool
is_integer(Value value) {
    switch (value.type) {
        case Value_boolean:
        case Value_signed_int:
        case Value_unsigned_int:
        case Value_pointer:
        return true;
    }
    return false;
}

inline bool
is_floating(Value value) {
    return value.type == Value_floating;
}

inline bool
is_numeric(Value value) {
    return is_integer(value) && is_floating(value);
}

inline bool
is_ast_node(Value value) {
    return value.type == Value_ast_node;
}

inline u64
value_to_u64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.boolean == true ? 1 : 0;
        case Value_signed_int: return (u64) value.signed_int;
        case Value_floating: return (u64) value.floating;
        case Value_pointer: return (u64) value.pointer;
        default: return (u64) value.unsigned_int;
    }
}

inline smm
value_to_smm(Value value) {
    switch (value.type) {
        case Value_boolean: return value.boolean == true ? 1 : 0;
        case Value_signed_int: return (smm) value.signed_int;
        case Value_floating: return (smm) value.floating;
        case Value_pointer: return value.pointer;
        default: return (smm) value.unsigned_int;
    }
}

inline f64
value_to_f64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.boolean == true ? 1.0 : 0.0;
        case Value_signed_int: return (f64) value.signed_int;
        case Value_unsigned_int: return (f64) value.unsigned_int;
        case Value_pointer: return (f64) value.pointer;
        default: return value.floating;
    }
}

inline s64
value_integer_binary_operation(Value first, Value second, Binary_Op op) {
    switch (op) {
        case BinaryOp_Multiply: 
        case BinaryOp_Multiply_Assign: {
            return first.signed_int * second.signed_int;
        }
        
        case BinaryOp_Divide:
        case BinaryOp_Divide_Assign:{
            return first.signed_int / second.signed_int;
        }
        
        case BinaryOp_Modulo:
        case BinaryOp_Modulo_Assign:{
            return first.signed_int % second.signed_int;
        }
        
        case BinaryOp_Add:
        case BinaryOp_Add_Assign:{
            return first.signed_int + second.signed_int;
        }
        
        case BinaryOp_Subtract:
        case BinaryOp_Subtract_Assign:{
            return first.signed_int - second.signed_int;
        }
        
        case BinaryOp_Shift_Left:
        case BinaryOp_Shift_Left_Assign: {
            return first.signed_int << second.signed_int;
        }
        
        case BinaryOp_Shift_Right:
        case BinaryOp_Shift_Right_Assign:{
            return first.signed_int >> second.signed_int;
        }
        
        case BinaryOp_Bitwise_And:
        case BinaryOp_Bitwise_And_Assign:{
            return first.signed_int & second.signed_int;
        }
        
        case BinaryOp_Bitwise_Or:
        case BinaryOp_Bitwise_Or_Assign:{
            return first.signed_int | second.signed_int;
        }
        
        case BinaryOp_Bitwise_Xor:
        case BinaryOp_Bitwise_Xor_Assign:{
            return first.signed_int ^ second.signed_int;
        }
        
        case BinaryOp_Less_Than: {
            return first.signed_int < second.signed_int;
        }
        
        case BinaryOp_Less_Equals: {
            return first.signed_int <= second.signed_int;
        }
        
        case BinaryOp_Greater_Than: {
            return first.signed_int > second.signed_int;
        }
        
        case BinaryOp_Greater_Equals: {
            return first.signed_int >= second.signed_int;
        }
        
        case BinaryOp_Equals: {
            return first.signed_int == second.signed_int;
        }
        
        case BinaryOp_Not_Equals: {
            return first.signed_int != second.signed_int;
        }
        
        case BinaryOp_Assign: {
            return second.signed_int;
        }
        
        default: {
            assert(0 && "unimplemented");
        }
    }
    
    return 0;
}


inline f64
value_floating_binary_operation(Value first, Value second, Binary_Op op) {
    switch (op) {
        case BinaryOp_Multiply: 
        case BinaryOp_Multiply_Assign: {
            return first.floating * second.floating;
        }
        
        case BinaryOp_Divide:
        case BinaryOp_Divide_Assign:{
            return first.floating / second.floating;
        }
        
        case BinaryOp_Add:
        case BinaryOp_Add_Assign:{
            return first.floating + second.floating;
        }
        
        case BinaryOp_Subtract:
        case BinaryOp_Subtract_Assign:{
            return first.floating - second.floating;
        }
        
        case BinaryOp_Less_Than: {
            return first.floating < second.floating;
        }
        
        case BinaryOp_Less_Equals: {
            return first.floating <= second.floating;
        }
        
        case BinaryOp_Greater_Than: {
            return first.floating > second.floating;
        }
        
        case BinaryOp_Greater_Equals: {
            return first.floating >= second.floating;
        }
        
        case BinaryOp_Equals: {
            return first.floating == second.floating;
        }
        
        case BinaryOp_Not_Equals: {
            return first.floating != second.floating;
        }
        
        case BinaryOp_Assign: {
            return second.floating;
        }
        
        default: {
            assert(0 && "unimplemented");
        }
    }
    
    return 0;
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
create_string_value(string value) {
    Value result;
    result.type = Value_string;
    result.str = value;
    return result;
}

// TODO(Alexander): print actual types from memory by specifiying the type as well
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
            printf("0x%I64X", val->pointer);
        } break;
        
        case Value_array: {
            printf("0x%I64X", (smm) val->array.elements);
            // TODO(Alexander): can't know what elements there are without the type!
            //printf("[");
            //Array_Value* arr = &val->array;
            //for (smm index = 0; index < arr->count; index++) {
            //if (index > 0) printf(", ");
            //print_value(&arr->elements[index]);
            //}
            //printf("]");
        } break;
        
        case Value_string: {
            printf("%s", val->str);
        } break;
    }
}
