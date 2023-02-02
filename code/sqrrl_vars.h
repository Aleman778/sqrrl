
// Predefined keywords variables that are reserved by the compiler.
#define DEF_KEYWORDS \
VAR(invalid)  \
VAR_GROUP(builtin_keywords_begin) \
VAR(asm)      \
VAR(break)    \
VAR(case)     \
VAR(cast)     \
VAR(continue) \
VAR(const)    \
VAR(volatile) \
VAR(defer)    \
VAR(do)       \
VAR(else)     \
VAR(enum)     \
VAR(false)    \
VAR(for)      \
VAR(global)   \
VAR(extern)   \
VAR(intern)   \
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
VAR(while)

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
VAR(auto)     \
VAR_GROUP(builtin_types_end) \
VAR_GROUP(builtin_keywords_end)

#define DEF_SYMBOLS \
VAR(__VA_ARGS__) \
VAR(__COUNTER__) \
VAR(__FILE__) \
VAR(__FUNCTION__) \
VAR(__LINE__) \
VAR(PRINT_AST) \
VAR(RUN_AST_INTERP) \
VAR(PRINT_BYTECODE) \
VAR(RUN_BYTECODE_INTERP) \
VAR(PRINT_ASM) \
VAR(PRINT_ASM_VREG) \
VAR(define)   \
VAR(defined)  \
VAR(elif)     \
VAR(endif)    \
VAR(include)  \
VAR(ifdef)    \
VAR(ifndef)   \
VAR(pragma)   \
VAR(once)     \
VAR(line)     \
VAR(undef)    \
VAR(error)    \
VAR(main)     \
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
VAR(pack)     \
VAR(push)     \
VAR(pop)      \
VAR(data)     \
VAR(count)    \
VAR(capacity) \
VAR(pln)      \
VAR(format)   \
VAR(link)

typedef u32 string_id;

struct String_Interner {
    string_map(string_id)* str_to_id = 0;
    string* id_to_str = 0;
    u32 id_counter = 0;
};

global String_Interner global_vars;


string_id
save_cstring(String_Interner* interner, cstring s) {
    string_id id = string_map_get(interner->str_to_id, s);
    if (!id) {
        id = interner->id_counter++;
        string_map_put(interner->str_to_id, s, id);
        array_push(interner->id_to_str, string_lit(s));
    }
    return id;
}

inline string_id
vars_save_cstring(cstring s) {
    return save_cstring(&global_vars, s);
}

inline string_id
save_string(String_Interner* interner, string s) {
    cstring cs = string_to_cstring(s);
    return save_cstring(interner, cs);
}

inline string_id
vars_save_string(string s) {
    return save_string(&global_vars, s);
}

string
load_string(String_Interner* interner, string_id id) {
    string result = {};
    if (id < array_count(interner->id_to_str)) {
        result = interner->id_to_str[id];
    }
    return result;
}

inline string
vars_load_string(string_id id) {
    return load_string(&global_vars, id);
}

void
initialize_keywords_and_symbols(String_Interner* interner) {
    if (interner->id_counter != 0) {
        return;
    }
    
    string_map_new_arena(interner->str_to_id);
#define VAR(symbol) save_cstring(interner, #symbol);
#define VAR_GROUP(symbol) VAR(symbol)
    DEF_KEYWORDS DEF_TYPE_KEYWORDS DEF_SYMBOLS
#undef VAR_GROUP
#undef VAR
}

typedef string_id Var;
enum {
#define VAR_GROUP(symbol) symbol,
#define VAR(symbol) Kw_##symbol,
    DEF_KEYWORDS DEF_TYPE_KEYWORDS
#undef VAR
    
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
    return id > builtin_keywords_begin && id <= builtin_keywords_end;
}

inline bool
is_builtin_type_keyword(string_id id) {
    return id > builtin_types_begin && id <= builtin_types_end;
}

inline bool
is_not_builtin_keyword(string_id id) {
    return id > builtin_keywords_end;
}

inline void
string_builder_push(String_Builder* sb, string_id ident) {
    string_builder_push(sb, vars_load_string(ident));
}