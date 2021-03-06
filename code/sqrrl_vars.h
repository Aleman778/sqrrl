
// Predefined keyword variables that are reserved by the compiler.
#define DEF_VARS  \
VAR(invalid)  \
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
VAR(loop)     \
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
VAR(b32)      \
VAR(char)     \
VAR(str)      \
VAR(bool)     \
VAR(void)     \
VAR(pointer)  \
VAR(array)    \
VAR(tuple)    \
VAR(function) \
VAR(infer)

// NOTE(alexander): interning strings into ids
typedef u32 str_id;
global struct { cstr key; str_id value; }* vars_str_to_id = 0; // stb_ds hashmap maps strings to ids
global cstr* vars_id_to_str = 0; // stb_ds array of strings where the index is the string id
global u32 vars_id_counter = 0;

str_id
vars_save_str(cstr s) {
    str_id id = str_map_get(vars_str_to_id, s);
    if (!id) {
        id = vars_id_counter++;
        str_map_put(vars_str_to_id, s, id);
        arr_push(vars_id_to_str, s);
    }
    return id;
}

cstr
vars_load_str(str_id id) {
    if (id < arrlen(vars_id_to_str)) {
        return vars_id_to_str[id];
    }
    return 0;
}

void
vars_initialize() {
    sh_new_arena(vars_str_to_id);
#define VAR(symbol) vars_save_str(#symbol);
    DEF_VARS
#undef VAR
}

typedef str_id Keyword;
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
is_keyword(str_id id) {
    return id > keyword_first && id <= keyword_last;
}