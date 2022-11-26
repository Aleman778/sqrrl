struct Ast;
struct Type;
struct Bc_Basic_Block;


// BINOP(name, op precedence, associativiy
#define DEF_UNARY_OPS \
UNOP(None,         ) \
UNOP(Negate,      -) \
UNOP(Logical_Not, !) \
UNOP(Bitwise_Not, ~) \
UNOP(Address_Of,  &) \
UNOP(Dereference, *) \
UNOP(Count,        )

// BINOP(name, op, prec, assoc, is_comparator, bc_mnemonic)
#define DEF_BINARY_OPS \
BINOP(None,               !,   0, Assoc_Left,  false, noop) \
BINOP(Multiply,           *,  11, Assoc_Left,  false, mul) \
BINOP(Divide,             /,  11, Assoc_Left,  false, div) \
BINOP(Modulo,             %,  11, Assoc_Left,  false, mod) \
BINOP(Add,                +,  10, Assoc_Left,  false, add) \
BINOP(Subtract,           -,  10, Assoc_Left,  false, sub) \
BINOP(Shift_Left,         <<, 9,  Assoc_Left,  false, shl) \
BINOP(Shift_Right,        >>, 9,  Assoc_Left,  false, shr) \
BINOP(Less_Than,          <,  8,  Assoc_Left,  true,  cmplt) \
BINOP(Less_Equals,        <=, 8,  Assoc_Left,  true,  cmple) \
BINOP(Greater_Than,       >,  8,  Assoc_Left,  true,  cmpgt) \
BINOP(Greater_Equals,     >=, 8,  Assoc_Left,  true,  cmpge) \
BINOP(Equals,             ==, 7,  Assoc_Left,  true,  cmpeq) \
BINOP(Not_Equals,         !=, 7,  Assoc_Left,  true,  cmpneq) \
BINOP(Bitwise_And,        &,  6,  Assoc_Left,  false, and) \
BINOP(Bitwise_Or,         |,  5,  Assoc_Left,  false, or) \
BINOP(Bitwise_Xor,        ^,  4,  Assoc_Left,  false, xor) \
BINOP(Logical_And,        &&, 3,  Assoc_Left,  false, and) \
BINOP(Logical_Or,         ||, 2,  Assoc_Left,  false, or) \
BINOP(Assign,             =,  1,  Assoc_Right, false, copy) \
BINOP(Add_Assign,         +=, 1,  Assoc_Right, false, add) \
BINOP(Subtract_Assign,    -=, 1,  Assoc_Right, false, sub) \
BINOP(Multiply_Assign,    *=, 1,  Assoc_Right, false, mul) \
BINOP(Divide_Assign,      /=, 1,  Assoc_Right, false, div) \
BINOP(Modulo_Assign,      %=, 1,  Assoc_Right, false, mod) \
BINOP(Bitwise_And_Assign, &=, 1,  Assoc_Right, false, and) \
BINOP(Bitwise_Or_Assign,  |=, 1,  Assoc_Right, false, or) \
BINOP(Bitwise_Xor_Assign, ^=, 1,  Assoc_Right, false, xor) \
BINOP(Shift_Left_Assign,  <<=, 1,  Assoc_Right, false, shl) \
BINOP(Shift_Right_Assign, >>=, 1,  Assoc_Right, false, shr) \
BINOP(Count,               !, 0,  Assoc_Left,  false, noop)

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
#define BINOP(symbol, name, ...) #name,
    DEF_BINARY_OPS
#undef BINOP
};

u8 binary_prec_table[] = {
#define BINOP(symbol, name, prec,...) prec,
    DEF_BINARY_OPS
#undef BINOP
};

Assoc binary_assoc_table[] = {
#define BINOP(symbol, name, prec, assoc,...) assoc,
    DEF_BINARY_OPS
#undef BINOP
};

bool binary_is_comparator_table[] = {
#define BINOP(symbol, name, prec, assoc, is_comparator,...) is_comparator,
    DEF_BINARY_OPS
#undef BINOP
};

#define binary_is_comparator(binop) (binary_is_comparator_table[binop])

inline bool
is_binary_assign(Binary_Op op) {
    return op >= BinaryOp_Assign;
}

enum Ternary_Op {
    TernaryOp_Conditional, // expr ? true : false
};

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
    Value_cstring,
    Value_memory_string,
    
    // TODO(Alexander): these don't really belong here, should be moved.
    Value_ast_node,
};

union Value_Data {
    bool boolean;
    s64 signed_int;
    u64 unsigned_int;
    f64 floating;
    smm pointer;
    Array_Value array;
    string str;
    cstring cstr;
    Memory_String mstr;
    Ast* ast; // TODO(Alexander): does it make sense to store this here?
    Bc_Basic_Block* basic_block; // TODO(Alexander): does it make sense to store this here?
    void* data;
};

struct Value {
    Value_Type type;
    Value_Data data;
};


void value_store_in_memory(Type* type, void* dest, Value_Data src);
Value value_load_from_memory(Type* type, void* data);
Value_Type value_type_from_basic_flags(u32 flags);

inline Value
create_boolean_value(bool value) {
    Value result;
    result.type = Value_boolean;
    result.data.boolean = (u64) value;
    return result;
}

inline Value
create_signed_int_value(s64 value) {
    Value result;
    result.type = Value_signed_int;
    result.data.signed_int = value;
    return result;
}

inline Value
create_unsigned_int_value(u64 value) {
    Value result;
    result.type = Value_unsigned_int;
    result.data.unsigned_int = value;
    return result;
}

inline Value
create_floating_value(f64 value) {
    Value result;
    result.type = Value_floating;
    result.data.floating = value;
    return result;
}

inline Value
create_pointer_value(smm value) {
    Value result;
    result.type = Value_pointer;
    result.data.pointer = { value };
    return result;
}

inline Value
create_string_value(string value) {
    Value result;
    result.type = Value_string;
    result.data.str = value;
    return result;
}

inline Value
create_memory_string_value(Memory_String value) {
    Value result;
    result.type = Value_memory_string;
    result.data.mstr = value;
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
is_string(Value value) {
    return value.type == Value_string;
}

inline bool
is_cstring(Value value) {
    return value.type == Value_cstring;
}

inline bool
is_ast_node(Value value) {
    return value.type == Value_ast_node;
}

inline bool
value_to_bool(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean;
        case Value_unsigned_int: return value.data.unsigned_int != 0;
        case Value_signed_int: return value.data.signed_int != 0;
        case Value_pointer: return value.data.pointer != 0;
        default: return false;
    }
}

inline u64
value_to_u64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean == true ? 1 : 0;
        case Value_signed_int: return (u64) value.data.signed_int;
        case Value_floating: return (u64) value.data.floating;
        case Value_pointer: return (u64) value.data.pointer;
        default: return (u64) value.data.unsigned_int;
    }
}

inline s64
value_to_s64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean == true ? 1 : 0;
        case Value_signed_int: return value.data.signed_int;
        case Value_floating: return (s64) value.data.floating;
        case Value_pointer: return (s64) value.data.pointer;
        default: return (u64) value.data.unsigned_int;
    }
}

inline smm
value_to_smm(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean == true ? 1 : 0;
        case Value_signed_int: return (smm) value.data.signed_int;
        case Value_floating: return (smm) value.data.floating;
        case Value_pointer: return value.data.pointer;
        default: return (smm) value.data.unsigned_int;
    }
}

inline f64
value_to_f64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean == true ? 1.0 : 0.0;
        case Value_signed_int: return (f64) value.data.signed_int;
        case Value_unsigned_int: return (f64) value.data.unsigned_int;
        case Value_pointer: return (f64) value.data.pointer;
        default: return value.data.floating;
    }
}

// TODO(Alexander): circular dependency for Primitive_Type_Kind
//Value value_cast(Value value, Primitive_Type_Kind type_kind);

inline s64 value_integer_binary_operation(Value first, Value second, Binary_Op op);
inline Value value_floating_binary_operation(Value first, Value second, Binary_Op op);

void string_builder_push(String_Builder* sb, Value* value);
void print_value(Value* value);