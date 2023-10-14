struct Ast;
struct Type;
struct Bc_Basic_Block;


// OP(symbol, prec, assoc, is_comparator, signed_opcode, unsigned_opcode)
#define DEF_OPERATORS \
OP(None,                !,  0, Assoc_Left,  false, BC_NOOP, BC_NOOP) \
OP(Post_Increment,     a++, 14, Assoc_Left,  false, BC_NOOP, BC_NOOP) \
OP(Post_Decrement,     a--, 14, Assoc_Left,  false, BC_NOOP, BC_NOOP) \
OP(Negate,              -, 13, Assoc_Right, false, BC_NEG, BC_NEG) \
OP(Logical_Not,         !, 13, Assoc_Right, false, BC_NOT, BC_NOT) \
OP(Bitwise_Not,         ~, 13, Assoc_Right, false, BC_NOT, BC_NOT) \
OP(Address_Of,          &, 13, Assoc_Right, false, BC_NOOP, BC_NOOP) \
OP(Dereference,         *, 13, Assoc_Right, false, BC_NOOP, BC_NOOP) \
OP(Pre_Increment,      ++a, 13, Assoc_Right, false, BC_NOOP, BC_NOOP) \
OP(Pre_Decrement,      --a, 13, Assoc_Right, false, BC_NOOP, BC_NOOP) \
OP(Multiply,           *,  11, Assoc_Left,  false, BC_MUL, BC_MUL) \
OP(Divide,             /,  11, Assoc_Left,  false, BC_DIV_S, BC_DIV_U) \
OP(Modulo,             %,  11, Assoc_Left,  false, BC_MOD_S, BC_MOD_U) \
OP(Add,                +,  10, Assoc_Left,  false, BC_ADD, BC_ADD) \
OP(Subtract,           -,  10, Assoc_Left,  false, BC_SUB, BC_SUB) \
OP(Shift_Left,         <<, 9,  Assoc_Left,  false, BC_SHL, BC_SHL) \
OP(Shift_Right,        >>, 9,  Assoc_Left,  false, BC_SAR, BC_SHR) \
OP(Less_Than,          <,  8,  Assoc_Left,  true, BC_LT_S, BC_LT_U) \
OP(Less_Equals,        <=, 8,  Assoc_Left,  true, BC_LE_S, BC_LE_U) \
OP(Greater_Than,       >,  8,  Assoc_Left,  true, BC_GT_S, BC_GT_U) \
OP(Greater_Equals,     >=, 8,  Assoc_Left,  true, BC_GE_S, BC_GE_U) \
OP(Equals,             ==, 7,  Assoc_Left,  true, BC_EQ, BC_EQ) \
OP(Not_Equals,         !=, 7,  Assoc_Left,  true, BC_NEQ, BC_NEQ) \
OP(Bitwise_And,        &,  6,  Assoc_Left,  false, BC_AND, BC_AND) \
OP(Bitwise_Or,         |,  5,  Assoc_Left,  false, BC_OR, BC_OR) \
OP(Bitwise_Xor,        ^,  4,  Assoc_Left,  false, BC_XOR, BC_XOR) \
OP(Logical_And,        &&, 3,  Assoc_Left,  false, BC_NOOP, BC_NOOP) \
OP(Logical_Or,         ||, 2,  Assoc_Left,  false, BC_NOOP, BC_NOOP) \
OP(Assign,             =,  1,  Assoc_Right, false, BC_COPY, BC_COPY) \
OP(Add_Assign,         +=, 1,  Assoc_Right, false, BC_ADD, BC_ADD) \
OP(Subtract_Assign,    -=, 1,  Assoc_Right, false, BC_SUB, BC_SUB) \
OP(Multiply_Assign,    *=, 1,  Assoc_Right, false, BC_MUL, BC_MUL) \
OP(Divide_Assign,      /=, 1,  Assoc_Right, false, BC_DIV_S, BC_DIV_U) \
OP(Modulo_Assign,      %=, 1,  Assoc_Right, false, BC_MOD_S, BC_MOD_U) \
OP(Bitwise_And_Assign, &=, 1,  Assoc_Right, false, BC_AND, BC_AND) \
OP(Bitwise_Or_Assign,  |=, 1,  Assoc_Right, false, BC_OR, BC_OR) \
OP(Bitwise_Xor_Assign, ^=, 1,  Assoc_Right, false, BC_XOR, BC_XOR) \
OP(Shift_Left_Assign,  <<=, 1, Assoc_Right, false, BC_SHL, BC_SHL) \
OP(Shift_Right_Assign, >>=, 1, Assoc_Right, false, BC_SAR, BC_SHR) \
OP(Count,                !, 0, Assoc_Left,  false, BC_NOOP, BC_NOOP)

enum Assoc {
    Assoc_Left,
    Assoc_Right,
};

enum Operator {
#define OP(symbol,...) Op_##symbol,
    DEF_OPERATORS
#undef OP
};

global cstring operator_strings[] = {
#define OP(name, op, ...) #op,
    DEF_OPERATORS
#undef OP
};

u8 operator_prec_table[] = {
#define OP(symbol, name, prec,...) prec,
    DEF_OPERATORS
#undef OP
};

Assoc operator_assoc_table[] = {
#define OP(symbol, name, prec, assoc,...) assoc,
    DEF_OPERATORS
#undef OP
};

bool operator_is_comparator_table[] = {
#define OP(symbol, name, prec, assoc, is_comparator,...) is_comparator,
    DEF_OPERATORS
#undef OP
};

Bytecode_Operator bytecode_operator_table[] = {
#define OP(symbol, name, prec, assoc, is_comparator, sop, uop) sop, uop,
    DEF_OPERATORS
#undef OP
};


#define operator_is_comparator(binop) (operator_is_comparator_table[binop])

inline bool
operator_is_assign(Operator op) {
    return op >= Op_Assign;
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
    Value_character,
    Value_signed_int,
    Value_unsigned_int,
    Value_floating,
    Value_pointer,
    Value_array,
    Value_string,
    Value_cstring,
    Value_memory_string,
};

struct Character_Value {
    union {
        u8 bytes[4];
        u32 unicode;
    };
    u32 num_bytes;
};

union Value_Data {
    bool boolean;
    Character_Value character;
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
void convert_aggregate_literal_to_memory(Ast* expr, void* dest);
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
create_character_value(u32 unicode, u32 num_bytes) {
    Value result;
    result.type = Value_character;
    result.data.character.unicode = unicode;
    result.data.character.num_bytes = num_bytes;
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
        case Value_character:
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
value_to_bool(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean;
        case Value_character: return value.data.character.bytes[0] != 0;
        case Value_unsigned_int: return value.data.unsigned_int != 0;
        case Value_signed_int: return value.data.signed_int != 0;
        case Value_pointer: return value.data.pointer != 0;
        case Value_string: return value.data.data != 0;
        case Value_cstring: return value.data.cstr != 0;
        default: return false;
    }
}

inline u64
value_to_u64(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean == true ? 1 : 0;
        case Value_character: return (u64) value.data.character.bytes[0];
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
        case Value_character: return (s64) value.data.character.bytes[0];
        case Value_unsigned_int: return (s64) value.data.unsigned_int;
        case Value_floating: return (s64) value.data.floating;
        case Value_pointer: return (s64) value.data.pointer;
        default: return (s64) value.data.signed_int;
    }
}

inline smm
value_to_smm(Value value) {
    switch (value.type) {
        case Value_boolean: return value.data.boolean == true ? 1 : 0;
        case Value_character: return (smm) value.data.character.bytes[0];
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
        case Value_character: return (f64) value.data.character.bytes[0];
        case Value_signed_int: return (f64) value.data.signed_int;
        case Value_unsigned_int: return (f64) value.data.unsigned_int;
        case Value_pointer: return (f64) value.data.pointer;
        default: return value.data.floating;
    }
}

// TODO(Alexander): circular dependency for Primitive_Type_Kind
//Value value_cast(Value value, Primitive_Type_Kind type_kind);

inline s64 value_integer_binary_operation(Value first, Value second, Operator op);
inline Value value_floating_binary_operation(Value first, Value second, Operator op);

void string_builder_push(String_Builder* sb, Value* value);
void print_value(Value* value);