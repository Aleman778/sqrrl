
// Predefined keyword symbols that are reserved by the compiler.
#define DEF_KEYWORDS    \
KEYWORD(invalid)    \
KEYWORD(break)      \
KEYWORD(cast)       \
KEYWORD(continue)   \
KEYWORD(defer)      \
KEYWORD(do)         \
KEYWORD(else)       \
KEYWORD(enum)       \
KEYWORD(extern)     \
KEYWORD(false)      \
KEYWORD(for)        \
KEYWORD(hidden)     \
KEYWORD(if)         \
KEYWORD(import)     \
KEYWORD(in)         \
KEYWORD(loop)       \
KEYWORD(return)     \
KEYWORD(struct)     \
KEYWORD(true)       \
KEYWORD(union)      \
KEYWORD(while)      \
KEYWORD(int)        \
KEYWORD(s8)         \
KEYWORD(s16)        \
KEYWORD(s32)        \
KEYWORD(s64)        \
KEYWORD(uint)       \
KEYWORD(u8)         \
KEYWORD(u16)        \
KEYWORD(u32)        \
KEYWORD(u64)        \
KEYWORD(f32)        \
KEYWORD(f64)        \
KEYWORD(b32)        \
KEYWORD(char)       \
KEYWORD(str)        \
KEYWORD(bool)       \
KEYWORD(void)

struct Symbol {
    str s;
    u32 index;
};

enum Keyword {
#define KEYWORD(symbol) Kw_##symbol,
    DEF_KEYWORDS
#undef KEYWORD
};
