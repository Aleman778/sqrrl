
// Predefined keywords variables that are reserved by the compiler.
#define DEF_KEYWORDS \
VAR(invalid)  \
VAR_GROUP(builtin_keywords_begin) \
VAR(asm)      \
VAR(break)    \
VAR(case)     \
VAR(cast)     \
VAR(continue) \
VAR(volatile) \
VAR(defer)    \
VAR(do)       \
VAR(else)     \
VAR(enum)     \
VAR(export)   \
VAR(false)    \
VAR(for)      \
VAR(global)   \
VAR(extern)   \
VAR(internal) \
VAR(if)       \
VAR(in)       \
VAR(inline)   \
VAR(always_inline) \
VAR(no_inline)\
VAR(local_persist) \
VAR(operator) \
VAR(return)   \
VAR(static)   \
VAR(struct)   \
VAR(switch)   \
VAR(true)     \
VAR(typedef)  \
VAR(union)    \
VAR(while)    \
VAR_GROUP(builtin_keywords_end)

#define DEF_TYPE_KEYWORDS \
VAR_GROUP(builtin_types_begin) \
VAR(bool)     \
VAR(s8)       \
VAR(s16)      \
VAR(s32)      \
VAR(s64)      \
VAR(smm)      \
VAR(int)      \
VAR(u8)       \
VAR(u16)      \
VAR(u32)      \
VAR(u64)      \
VAR(umm)      \
VAR(uint)     \
VAR(f32)      \
VAR(f64)      \
VAR(string)   \
VAR(cstring)  \
VAR(void)     \
VAR(Type)     \
VAR_GROUP(builtin_types_end)

// OP(symbol, prec, assoc, is_comparator, signed_opcode, unsigned_opcode)
#define Op_None 0
#define DEF_OPERATORS \
VAR_GROUP(builtin_operators_begin) \
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
OP(Assign,             =,  1,  Assoc_Right, false, BC_NOOP, BC_NOOP) \
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
VAR_GROUP(builtin_operators_end)

enum Assoc {
    Assoc_Left,
    Assoc_Right,
};

//global cstring operator_strings[] = {
//#define OP(name, op, ...) #op,
//DEF_OPERATORS
//#undef OP
//};

u8 operator_prec_table[] = {
#define OP(symbol, name, prec,...) prec,
#define VAR_GROUP(...) 0,
    DEF_OPERATORS
#undef VAR_GROUP
#undef OP
};

Assoc operator_assoc_table[] = {
#define OP(symbol, name, prec, assoc,...) assoc,
#define VAR_GROUP(...) Assoc_Left,
    DEF_OPERATORS
#undef VAR_GROUP
#undef OP
};

bool operator_is_comparator_table[] = {
#define OP(symbol, name, prec, assoc, is_comparator,...) is_comparator,
#define VAR_GROUP(...) false,
    DEF_OPERATORS
#undef VAR_GROUP
#undef OP
};


Bytecode_Operator bytecode_operator_table[] = {
#define OP(symbol, name, prec, assoc, is_comparator, sop, uop) sop, uop,
#define VAR_GROUP(...) BC_NOOP, BC_NOOP,
    DEF_OPERATORS
#undef VAR_GROUP
#undef OP
};

#define operator_is_comparator(binop) (operator_is_comparator_table[binop])


#define DEF_SYMBOLS \
VAR(__VA_ARGS__) \
VAR(__COUNTER__) \
VAR(__FILE__) \
VAR(__FUNCTION__) \
VAR(__LINE__) \
VAR(DUMP_AST) \
VAR(DUMP_BYTECODE) \
VAR(DUMP_DISASM) \
VAR(define)   \
VAR(defined)  \
VAR(elif)     \
VAR(endif)    \
VAR(include)  \
VAR(import)   \
VAR(__assert) \
VAR(ifdef)    \
VAR(ifndef)   \
VAR(expand)   \
VAR(pragma)   \
VAR(once)     \
VAR(line)     \
VAR(undef)    \
VAR(error)    \
VAR(main)     \
VAR(print)    \
VAR(__string) \
VAR(__const)  \
VAR(test_proc) \
VAR(unsigned) \
VAR(signed)   \
VAR(long)     \
VAR(short)    \
VAR(float)    \
VAR(double)   \
VAR(char)     \
VAR(__int8)   \
VAR(__int16)  \
VAR(__int32)  \
VAR(__int64)  \
VAR(__ptr32)  \
VAR(__ptr64)  \
VAR(__pragma) \
VAR(__inline) \
VAR(__forceinline) \
VAR(__unaligned) \
VAR(__declspec)  \
VAR(__cdecl)  \
VAR(__fastcall) \
VAR(__stdcall) \
VAR(__debugbreak) \
VAR(__start) \
VAR(pack)     \
VAR(push)     \
VAR(pop)      \
VAR(data)     \
VAR(count)    \
VAR(capacity) \
VAR(pln)      \
VAR(format)   \
VAR(expr)     \
VAR(link)     \
VAR(link_dynamic) \
VAR(extern_name) \
VAR(intrinsic) \
VAR(debug_break) \
VAR(rdtsc) \
VAR(dump_bytecode) \
VAR(dump_ast) \
VAR(Var_Args) \
VAR(Dynamic_Library) \

void
initialize_keywords_and_symbols(String_Interner* interner) {
    if (!array_count(interner)) {
        return;
    }
    
    string_map_new_arena(interner->str_to_id);
#define VAR(symbol) save_cstring(interner, #symbol);
#define VAR_GROUP(symbol) VAR(symbol)
#define OP(name, symbol, ...) array_push(interner->id_to_str, string_lit(#symbol));
    DEF_KEYWORDS DEF_TYPE_KEYWORDS DEF_OPERATORS DEF_SYMBOLS
#undef OP
#undef VAR_GROUP
#undef VAR
}

typedef string_id Var;
typedef string_id Operator;
enum {
#define VAR_GROUP(symbol) symbol,
#define VAR(symbol) Kw_##symbol,
    DEF_KEYWORDS DEF_TYPE_KEYWORDS
#undef VAR
    
#define OP(name, ...) Op_##name,
    DEF_OPERATORS
#undef OP
    
#define VAR(symbol) Sym_##symbol,
    DEF_SYMBOLS
#undef VAR
#undef VAR_GROUP
};


inline void
vars_initialize_keywords_and_symbols() {
    initialize_keywords_and_symbols(&global_vars);
}

inline bool
is_builtin_keyword(string_id id) {
    return id > builtin_keywords_begin && id < builtin_keywords_end;
}

inline bool
is_builtin_type_keyword(string_id id) {
    return id > builtin_types_begin && id < builtin_types_end;
}

inline bool
is_not_builtin_keyword(string_id id) {
    return id > builtin_keywords_end;
}

inline bool
is_builtin_operator(string_id id) {
    return id > builtin_operators_begin && id < builtin_operators_end;
}

inline void
string_builder_push(String_Builder* sb, string_id ident) {
    string_builder_push(sb, vars_load_string(ident));
}

inline bool
operator_is_assign(Operator op) {
    return op >= Op_Assign;
}

inline u8
operator_get_precedence(Operator op) {
    return operator_prec_table[op - builtin_operators_begin];
}

inline Assoc
operator_get_associativity(Operator op) {
    return operator_assoc_table[op - builtin_operators_begin];
} 