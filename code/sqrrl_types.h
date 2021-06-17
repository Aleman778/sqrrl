
#define internal static
#define global static
#define local_persist static

#define array_count(array) (sizeof(array) / sizeof((array)[0]))

#define kilobytes(value) (1024LL * (value))
#define megabytes(value) (1024LL * kilobytes(value))
#define gigabytes(value) (1024LL * megabytes(value))
#define terabytes(value) (1024LL * gigabytes(value))

#if BUILD_DEBUG
void
__assert(const char* expression, const char* file, int line) {
    // TODO(alexander): improve assertion printing.
    fprintf(stderr, "%s:%d: Assertion failed: %s\n", file, line, expression);
    *(int *)0 = 0; // NOTE(alexander): purposefully trap the program
}
#define assert(expression) (void)((expression) || (__assert(#expression, __FILE__, __LINE__), 0))
#else
#define assert(expression)
#endif

typedef int8_t    s8;
typedef uint8_t   u8;
typedef int16_t   s16;
typedef uint16_t  u16;
typedef int32_t   s32;
typedef uint32_t  u32;
typedef int64_t   s64;
typedef uint64_t  u64;
typedef uintptr_t umm;
typedef intptr_t  smm;
typedef float     f32;
typedef double    f64;
typedef int32_t   b32;

struct str {
    u8* data;
    smm count;
};

inline str
str_lit(const char* string) {
    str result;
    result.data = (u8*) string;
    result.count = strlen(string);
    return result;
}

inline char*
lit(str string) {
    return (char*) string.data;
}
