
// BINOP(name, op precedence, associativiy
#define DEF_UNARY_OPS \
UNOP(None,         ) \
UNOP(Negate,      -) \
UNOP(Not,         !) \
UNOP(Bitwise_Not, ~) \
UNOP(Address_Of,  &) \
UNOP(Dereference, *) \
UNOP(Count,        )

// BINOP(name, op, prec, assoc, bc_mnemonic)
#define DEF_BINARY_OPS \
BINOP(None,               !,   0,  Assoc_Left,  noop) \
BINOP(Multiply,           *,  11, Assoc_Left,  mul) \
BINOP(Divide,             /,  11, Assoc_Left,  div) \
BINOP(Modulo,             %,  11, Assoc_Left,  mod) \
BINOP(Add,                +,  10, Assoc_Left,  add) \
BINOP(Subtract,           -,  10, Assoc_Left,  sub) \
BINOP(Shift_Left,         <<, 9,  Assoc_Left,  shl) \
BINOP(Shift_Right,        >>, 9,  Assoc_Left,  shr) \
BINOP(Less_Than,          <,  8,  Assoc_Left,  lt) \
BINOP(Less_Equals,        <=, 8,  Assoc_Left,  le) \
BINOP(Greater_Than,       >,  8,  Assoc_Left,  gt) \
BINOP(Greater_Equals,     >=, 8,  Assoc_Left,  ge) \
BINOP(Equals,             ==, 7,  Assoc_Left,  eq) \
BINOP(Not_Equals,         !=, 7,  Assoc_Left,  neq) \
BINOP(Bitwise_And,        &,  6,  Assoc_Left,  and) \
BINOP(Bitwise_Or,         |,  5,  Assoc_Left,  or) \
BINOP(Bitwise_Xor,        ^,  4,  Assoc_Left,  xor) \
BINOP(Logical_And,        &&, 3,  Assoc_Left,  land) \
BINOP(Logical_Or,         ||, 2,  Assoc_Left,  lor) \
BINOP(Assign,             =,  1,  Assoc_Right, store) \
BINOP(Add_Assign,         +=, 1,  Assoc_Right, add) \
BINOP(Subtract_Assign,    -=, 1,  Assoc_Right, sub) \
BINOP(Multiply_Assign,    *=, 1,  Assoc_Right, mul) \
BINOP(Divide_Assign,      /=, 1,  Assoc_Right, div) \
BINOP(Modulo_Assign,      %=, 1,  Assoc_Right, mod) \
BINOP(Bitwise_And_Assign, &=, 1,  Assoc_Right, and) \
BINOP(Bitwise_Or_Assign,  |=, 1,  Assoc_Right, or) \
BINOP(Bitwise_Xor_Assign, ^=, 1,  Assoc_Right, xor) \
BINOP(Shift_Left_Assign,  <<, 1,  Assoc_Right, shl) \
BINOP(Shift_Right_Assign, >>, 1,  Assoc_Right, shr) \
BINOP(Count,               !, 0,  Assoc_Left,  noop)

enum Assoc {
    Assoc_Left,
    Assoc_Right,
};

enum Unary_Op {
#define UNOP(symbol,...) UnaryOp_##symbol,
    DEF_UNARY_OPS
#undef UNOP
};

enum Binary_Op {
#define BINOP(symbol,...) BinaryOp_##symbol,
    DEF_BINARY_OPS
#undef BINOP
};

global cstring unary_op_strings[] = {
#define UNOP(name, op) #op,
    DEF_UNARY_OPS
#undef UNOP
};

global cstring binary_op_strings[] = {
#define BINOP(symbol, name, prec, assoc, bc_mnemonic) #name,
    DEF_BINARY_OPS
#undef BINOP
};

inline bool
is_assignment_binary_operator(Binary_Op op) {
    return op >= BinaryOp_Assign;
}

u8
binary_get_prec(Binary_Op op) {
    switch (op) {
#define BINOP(symbol, name, prec,...) case BinaryOp_##symbol: return prec;
        DEF_BINARY_OPS
#undef BINOP
    }
    assert(0 && "bug");
    return 0;
}

Assoc
binary_get_assoc(Binary_Op op) {
    switch (op) {
#define BINOP(symbol, name, prec, assoc,...) case BinaryOp_##symbol: return assoc;
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

inline bool
value_to_bool(Value value) {
    switch (value.type) {
        case Value_boolean: return value.boolean;
        case Value_unsigned_int: return value.unsigned_int != 0;
        case Value_signed_int: return value.signed_int != 0;
        case Value_pointer: return value.pointer != 0;
        default: return false;
    }
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

inline s64
value_to_s64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.boolean == true ? 1 : 0;
        case Value_signed_int: return value.signed_int;
        case Value_floating: return (s64) value.floating;
        case Value_pointer: return (s64) value.pointer;
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

Value value_cast(Value value, Type* type);

inline s64 value_integer_binary_operation(Value first, Value second, Binary_Op op);
inline Value value_floating_binary_operation(Value first, Value second, Binary_Op op);

void string_builder_push(String_Builder* sb, Value* value);
void print_value(Value* value);