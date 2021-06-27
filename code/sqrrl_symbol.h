
// Predefined keyword symbols that are reserved by the compiler.
#define DEF_SYMBOLS  \
SYMBOL(invalid)  \
SYMBOL(break)    \
SYMBOL(cast)     \
SYMBOL(continue) \
SYMBOL(defer)    \
SYMBOL(do)       \
SYMBOL(else)     \
SYMBOL(enum)     \
SYMBOL(extern)   \
SYMBOL(false)    \
SYMBOL(for)      \
SYMBOL(hidden)   \
SYMBOL(if)       \
SYMBOL(include)  \
SYMBOL(in)       \
SYMBOL(inline)   \
SYMBOL(loop)     \
SYMBOL(return)   \
SYMBOL(static)   \
SYMBOL(struct)   \
SYMBOL(true)     \
SYMBOL(typedef)  \
SYMBOL(union)    \
SYMBOL(while)    \
SYMBOL(int)      \
SYMBOL(s8)       \
SYMBOL(s16)      \
SYMBOL(s32)      \
SYMBOL(s64)      \
SYMBOL(smm)      \
SYMBOL(uint)     \
SYMBOL(u8)       \
SYMBOL(u16)      \
SYMBOL(u32)      \
SYMBOL(u64)      \
SYMBOL(umm)      \
SYMBOL(f32)      \
SYMBOL(f64)      \
SYMBOL(b32)      \
SYMBOL(char)     \
SYMBOL(str)      \
SYMBOL(bool)     \
SYMBOL(void)     \
SYMBOL(pointer)  \
SYMBOL(array)    \
SYMBOL(tuple)    \
SYMBOL(function) \
SYMBOL(infer)

struct Symbol {
    str s;
    u32 index;
};

enum Keyword {
#define SYMBOL(symbol) Kw_##symbol,
    DEF_SYMBOLS
#undef SYMBOL
};

global const u32 builtin_keywords_begin = Kw_break;
global const u32 builtin_keywords_end = Kw_bool;
global const u32 builtin_types_begin = Kw_int;
global const u32 builtin_types_end = Kw_void;

global Symbol builtin_keyword_symbols[] = {
#define SYMBOL(atom) { str_lit(#atom), Kw_##atom },
    DEF_SYMBOLS
#undef SYMBOL
};