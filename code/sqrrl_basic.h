// Basic defines useful types and functions similar to C++ standard library

#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <cstdarg>

// NOTE(alexander): rename static to better reflect its actual meaning
#define internal static
#define global static
#define local_persist static

// NOTE(alexander): count the number of elements in a fixed size array
#define array_count(array) (sizeof(array) / sizeof((array)[0]))

// NOTE(alexander): specify file size macros
#define kilobytes(value) (1024LL * (value))
#define megabytes(value) (1024LL * kilobytes(value))
#define gigabytes(value) (1024LL * megabytes(value))
#define terabytes(value) (1024LL * gigabytes(value))

// NOTE(alexander): bit stuff
#define bit(x) (1 << (x))

// NOTE(alexander): define assestion macro on debug mode
#ifdef assert
#undef assert
#endif

#if BUILD_DEBUG
void
__assert(cstr expression, cstr file, int line) {
    // TODO(alexander): improve assertion printing.
    fprintf(stderr, "%s:%d: Assertion failed: %s\n", file, line, expression);
    *(int *)0 = 0; // NOTE(alexander): purposefully trap the program
}
#define assert(expression) (void)((expression) || (__assert(#expression, __FILE__, __LINE__), 0))
#else
#define assert(expression)
#endif

// TODO(alexander): special asserts
#define assert_enum(T, v) assert((v) > 0 && (v) < T##_Count && "enum value out of range")
#define assert_power_of_two(x) assert((((x) & ((x) - 1)) == 0) && "x is not power of two")

// NOTE(alexander): define more convinient types
typedef unsigned int uint;
typedef int8_t       s8;
typedef uint8_t      u8;
typedef int16_t      s16;
typedef uint16_t     u16;
typedef int32_t      s32;
typedef uint32_t     u32;
typedef int64_t      s64;
typedef uint64_t     u64;
typedef uintptr_t    umm;
typedef intptr_t     smm;
typedef float        f32;
typedef double       f64;
typedef int32_t      b32;
typedef char*        str;
typedef const char*  cstr;

// TODO(alexander): lazy!!!! don't use malloc for this, put in arena later...

inline str
str_alloc(u32 count) {
    char* result = (char*) malloc(count + 5) + 4;
    result[count] = '\0';
    *((u32*) result - 1) = count;
    return (str) result;
}

inline str
str_lit(str string, u32 count) {
    str result = str_alloc(count);
    memcpy(result, string, count);
    return result;
}

inline str
str_lit(cstr string) {
    u32 count = (u32) strlen(string);
    return str_lit((str) string, count);
}

#define str_count(s) *((u32*) s - 1)

inline u32
count(str string) {
    return str_count(string);
}

// NOTE(alexander): improved string formatting and printf
typedef int Format_Type;
enum { // TODO(alexander): add more types
    FormatType_int,
    FormatType_uint,
    FormatType_smm,
    FormatType_umm,
    FormatType_str,
    FormatType_cstr,
};

// TODO(alexander): add more types
#define f_int(x) FormatType_int, (int) (x)
#define f_smm(x) FormatType_smm, (smm) (x)
#define f_uint(x) FormatType_uint, (uint) (x)
#define f_umm(x) FormatType_umm, (umm) (x)
#define f_str(x) FormatType_str, (int) str_count(x), x
#define f_cstr(x) FormatType_cstr, (cstr) (x)

void pln(cstr format...);
str str_format(cstr format...);

// TODO(alexander): implement this later, we use stb_ds for now!
// NOTE(alexander): dynamic arrays, usage:
//     i32* array = 0;
//     arr_push(array, 5);

//struct Array_Header {
//smm count;
//smm capacity;
//};

//#define arr_push(a, x) _arr_push(a, sizeof((a)[0]), )
//#define arr_count(a) ((Array_Header*) (a) - 1)->count
//#define arr_capacity(a) ((Array_Header*) (a) - 1)->capacity

//void
//_arr_alloc(void** array, smm elem_size, smm capacity) {
//if (*array) {
//Array_Header* header = (Array_Header*) *array - 1;
//smm new_capacity = header->capacity*2;
//
//} else {
//Array_Header* header = (Array_Header*) malloc(sizeof(Array_Header) + capacity*elem_size);
//header->count = 0;
//header->capacity = capacity;
//*array = header + 1;
//}
//}

//void
//_arr_push(void* array, smm elem_size, void* data) {

//}

// NOTE(alexander): change the naming convention of stb_ds
#define arr_free(a) arrfree(a)
#define arr_push(a, x) arrput(a, x)
#define arr_pop(a, x) arrpop(a, x)
#define arr_insert(a, x, p) arrins(a, p, x)
#define arr_remove(a, p) arrdel(a, p)
#define arr_set_capacity(a, c) arrsetcap(a, c)
#define arr_get_capacity(a) arrcap(a)
#define arr_count(a) arrlen(a)

#define map_put(m, k, v) hmput(m, k, v)
#define str_map_put(m, k, v) shput(m, k, v)

int 
compare_ints(void* a, void* b) {
    return *(int*) a - *(int*) b;
}

int
compare_smm(void* a, void* b) {
    return (int) (*(smm*) a - *(smm*) b);
}

struct Binary_Search_Result  {
    void* value;
    smm index;
    b32 exact_match;
};

Binary_Search_Result
_binary_search(void* arr, void* val, smm count, smm size, 
               int (*compare)(void*, void*)) {
    Binary_Search_Result result = {};
    smm low = 0, high = count - 1;
    
    while (low <= high) {
        smm mid = (high - low) / 2 + low;
        result.index = mid;
        
        void* elem = (u8*) arr + mid*size;
        int cmp = compare(val, elem);
        if (cmp > 0) {
            high = mid - 1;
        } else if (cmp < 0) {
            low = mid + 1;
        } else {
            result.exact_match = true;
            break;
        }
    }
    
    result.value = (u8*) arr + result.index*size;
    return result;
}

#define binary_search(arr, val, compare) _binary_search(arr, &val, arr_count(arr), sizeof(arr), compare)

// NOTE(alexander): hash map

// NOTE(alexander): memory arena
#ifndef DEFAULT_ALIGNMENT
#define DEFAULT_ALIGNMENT (2*alignof(smm))
#endif
#define ARENA_DEFAULT_BLOCK_SIZE kilobytes(10)

// NOTE(alexander): align has to be a power of two.
inline umm
align_forward(umm address, umm align) {
    assert_power_of_two(align);
    umm modulo = address & (align - 1);
    if (modulo != 0) {
        address += align - modulo;
    }
    return address;
}

// NOTE(alexander): memory arena
struct Arena {
    u8* base;
    umm size;
    umm curr_used;
    umm prev_used;
    umm min_block_size;
};

inline void
arena_initialize(Arena* arena, void* base, umm size) {
    arena->base = (u8*) base;
    arena->size = size;
    arena->curr_used = 0;
    arena->prev_used = 0;
    arena->min_block_size = 0;
}

inline void
arena_initialize(Arena* arena, umm min_block_size) {
    arena->base = 0;
    arena->size = 0;
    arena->curr_used = 0;
    arena->prev_used = 0;
    arena->min_block_size = min_block_size;
}

void*
arena_push_size(Arena* arena, umm size, umm align=DEFAULT_ALIGNMENT, umm flags=0) {
    umm current = (umm) (arena->base + arena->curr_used);
    umm offset = align_forward(current, align) - (umm) arena->base;
    
    if (offset + size > arena->size) {
        if (arena->min_block_size == 0) {
            arena->min_block_size = ARENA_DEFAULT_BLOCK_SIZE;
        }
        
        arena->base = (u8*) calloc(1, arena->min_block_size);
        arena->curr_used = 0;
        arena->prev_used = 0;
        arena->size = arena->min_block_size;
        
        current = (umm) arena->base + arena->curr_used;
        offset = align_forward(current, align) - (umm) arena->base;
        // TODO(alexander): we need to also store the previous memory block so we can eventually free it.
    }
    
    void* result = arena->base + offset;
    arena->prev_used = arena->curr_used;
    arena->curr_used = offset + size;
    
    // TODO(alexander): add memory clear to zero flag
    
    return result;
}

#define arena_push_struct(arena, type, flags) (type*) arena_push_size(arena, (umm) sizeof(type), (umm) alignof(type), flags)

inline void
arena_rewind(Arena* arena) {
    arena->curr_used = arena->prev_used;
}

inline void
arena_clear(Arena* arena) {
    arena->curr_used = 0;
    arena->prev_used = 0;
}
