
// Predefined keywords variables that are reserved by the compiler.
#define DEF_VARS  \
VAR(invalid)  \
VAR(asm)      \
VAR(break)    \
VAR(cast)     \
VAR(continue) \
VAR(defer)    \
VAR(do)       \
VAR(else)     \
VAR(enum)     \
VAR(extern)   \
VAR(false)    \
VAR(for)      \
VAR(global)   \
VAR(internal) \
VAR(if)       \
VAR(include)  \
VAR(in)       \
VAR(inline)   \
VAR(no_inline)\
VAR(local_persist) \
VAR(return)   \
VAR(static)   \
VAR(struct)   \
VAR(true)     \
VAR(typedef)  \
VAR(union)    \
VAR(while)    \
VAR(int)      \
VAR(s8)       \
VAR(s16)      \
VAR(s32)      \
VAR(s64)      \
VAR(smm)      \
VAR(uint)     \
VAR(u8)       \
VAR(u16)      \
VAR(u32)      \
VAR(u64)      \
VAR(umm)      \
VAR(f32)      \
VAR(f64)      \
VAR(char)     \
VAR(bool)     \
VAR(b32)      \
VAR(void)     \
VAR(string)   \
VAR(pointer)  \
VAR(array)    \
VAR(tuple)    \
VAR(function) \
VAR(infer)

// NOTE(alexander): interning strings into ids
typedef u32 string_id;
global struct { cstring key; string_id value; }* vars_str_to_id = 0; // stb_ds hashmap maps strings to ids
global string* vars_id_to_str = 0; // stb_ds array of strings where the index is the string id
global u32 vars_id_counter = 0;

string_id
vars_save_cstring(cstring s) {
    string_id id = string_map_get(vars_str_to_id, s);
    if (!id) {
        id = vars_id_counter++;
        string_map_put(vars_str_to_id, s, id);
        array_push(vars_id_to_str, string_lit(s));
    }
    return id;
}

inline string_id
vars_save_string(string s) {
    cstring cs = string_to_cstring(s);
    return vars_save_cstring(cs);
}

string
vars_load_string(string_id id) {
    string result = {};
    if (id < array_count(vars_id_to_str)) {
        result = vars_id_to_str[id];
    }
    return result;
}

void
vars_initialize() {
    string_map_new_arena(vars_str_to_id);
#define VAR(symbol) vars_save_cstring(#symbol);
    DEF_VARS
#undef VAR
}

typedef string_id Keyword;
enum {
#define VAR(symbol) Kw_##symbol,
    DEF_VARS
#undef VAR
};

global const u32 builtin_types_begin = Kw_int;
global const u32 builtin_types_end = Kw_void;
global const u32 builtin_keywords_begin = Kw_infer;
global const u32 builtin_keywords_end = builtin_types_end;
global const u32 keyword_first = Kw_invalid;
global const u32 keyword_last = Kw_infer;

inline bool
is_keyword(string_id id) {
    return id > keyword_first && id <= keyword_last;
}
