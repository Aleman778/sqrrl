
typedef s32 Unary_Op;
enum {
    UnaryOp_Neg,
    UnaryOp_Not,
    UnaryOp_Bit_Not,
    UnaryOp_Addr_Of,
    UnaryOp_Deref,
};

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
    BinaryOp_Addition_Assign,
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
    ValueType_boolean,
    ValueType_sint,
    ValueType_uint,
    ValueType_floating,
    ValueType_pointer,
    ValueType_array,
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
