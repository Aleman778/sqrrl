// Basic defines useful types and functions similar to C++ standard library

#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <cstdarg>

// NOTE(Alexander): rename static to better reflect its actual meaning
#define internal static
#define global static
#define local_persist static

// NOTE(Alexander): count the number of elements in a fixed size array
#define fixed_array_count(array) (sizeof(array) / sizeof((array)[0]))

// NOTE(Alexander): specify file size macros
#define kilobytes(value) (1024LL * (value))
#define megabytes(value) (1024LL * kilobytes(value))
#define gigabytes(value) (1024LL * megabytes(value))
#define terabytes(value) (1024LL * gigabytes(value))

// NOTE(Alexander): minimum and maximum value
#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

// NOTE(Alexander): bit stuff
#define bit(x) (1 << (x))
#define is_bit_set(var, x) ((var) & (1 << (x)))
#define is_bitflag_set(var, flag) ((var) & (flag))

// NOTE(Alexander): define more convinient types
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
typedef const char*  cstring;

// NOTE(Alexander): define type min and max values
#undef INT_MIN
#undef INT_MAX
#undef UINT_MAX
#undef CHAR_MIN
#undef CHAR_MAX

#define S8_MIN (-128)
#define S8_MAX 127
#define S16_MIN (-32768)
#define S16_MAX 32767
#define S32_MIN (-2147483647 - 1)
#define S32_MAX 2147483647
#define S64_MAX 9223372036854775807ll
#define S64_MIN (-9223372036854775807ll - 1)
#define INT_MIN S32_MIN
#define INT_MAX S32_MAX
#define U8_MAX 0xffU
#define U16_MAX 0xffffU
#define U32_MAX 0xffffffffU
#define U64_MAX 0xffffffffffffffffull
#define UINT_MAX U32_MAX
#define BOOL_MAX S8_MAX
#define BOOL_MIN S8_MIN
#define CHAR_MAX U8_MAX
#define CHAR_MIN 0U

// NOTE(Alexander): define assestion macro on debug mode
#ifdef assert
#undef assert
#endif
#if BUILD_DEBUG
void
__assert(cstring expression, cstring file, int line) {
    // TODO(Alexander): improve assertion printing.
    fprintf(stderr, "%s:%d: Assertion failed: %s\n", file, line, expression);
    *(int *)0 = 0; // NOTE(Alexander): purposefully trap the program
}
#define assert(expression) (void)((expression) || (__assert(#expression, __FILE__, __LINE__), 0))
#else
#define assert(expression)
#endif

#define unimplemented assert(0 && "this code is not implemented yet")
#define invalid_code_path assert(0 && "invalid code path, this is likely a bug")

// TODO(Alexander): special asserts
#define assert_enum(T, v) assert((v) > 0 && (v) < T##_Count && "enum value out of range")
#define assert_power_of_two(x) assert((((x) & ((x) - 1)) == 0) && "x is not power of two")

inline umm
cstring_count(cstring str) {
    return (umm) strlen(str);
}

// NOTE(Alexander): strings
struct string {
    umm count;
    u8* data;
};

inline string
create_string(umm count, u8* data) {
    string result;
    result.count = count;
    result.data = data;
    return result;
};

// TODO(Alexander): lazy!!!! don't use malloc for this, put in arena later...
inline string
string_alloc(umm count) {
    string result;
    result.count = count;
    result.data = (u8*) malloc(count);
    return (string) result;
}

inline string
string_lit(cstring str) {
    string result;
    result.data = (u8*) str;
    result.count = cstring_count(str);
    return result;
}

// NOTE(Alexander): allocates a new cstring that is null terminated
inline cstring
string_to_cstring(string str) {
    char* result = (char*) malloc(str.count + 1);
    memcpy(result, str.data, str.count);
    result[str.count] = 0;
    return (cstring) result;
}

inline string
string_copy(string str) {
    string result = string_alloc(str.count);
    memcpy(result.data, str.data, str.count);
    return result;
}

inline string
string_view(u8* begin, u8* end) {
    assert(begin < end);
    
    string result;
    result.count = (umm) (end - begin);
    result.data = begin;
    return result;
}

struct String_Builder {
    u8* data;
    umm size;
    umm curr_used;
};

inline void
string_builder_free(String_Builder* sb) {
    free(sb->data);
    sb->data = 0;
    sb->curr_used = 0;
    sb->size = 0;
}

inline void
string_builder_alloc(String_Builder* sb, umm new_size) {
    void* new_data = realloc(sb->data, new_size);
    if (!new_data) {
        free(sb->data);
        sb->data = (u8*) malloc(new_size);
    }
    sb->data = (u8*) new_data;
    sb->size = new_size;
}

inline void
string_builder_ensure_capacity(String_Builder* sb, umm capacity) {
    umm min_size = sb->curr_used + capacity;
    if (min_size > sb->size) {
        umm new_size = max(sb->size * 2, min_size);
        string_builder_alloc(sb, new_size);
    }
}

void
string_builder_push(String_Builder* sb, string str) {
    string_builder_ensure_capacity(sb, str.count);
    
    memcpy(sb->data + sb->curr_used, str.data, str.count);
    sb->curr_used += str.count;
}


void
string_builder_push(String_Builder* sb, cstring str) {
    string_builder_push(sb, string_lit(str));
}

void string_builder_push_format(String_Builder* sb, cstring format...);

string
string_builder_to_string(String_Builder* sb) {
    string result;
    result.data = (u8*) malloc(sb->curr_used + 1);
    result.count = sb->curr_used;
    memcpy(result.data, sb->data, sb->curr_used);
    result.data[result.count] = 0;
    return result;
}

string
string_builder_to_string_nocopy(String_Builder* sb) {
    string result;
    result.data = sb->data;
    result.count = sb->curr_used;
    return result;
}

// NOTE(Alexander): improved string formatting and printf
typedef int Format_Type;
enum { // TODO(Alexander): add more types
    FormatType_char,
    FormatType_u8,
    FormatType_int,
    FormatType_uint,
    FormatType_s64,
    FormatType_u64,
    FormatType_smm,
    FormatType_umm,
    FormatType_float,
    FormatType_string,
    FormatType_cstring,
};

// TODO(Alexander): add more types
#define f_char(x) FormatType_char, (char) (x)
#define f_int(x) FormatType_int, (int) (x)
#define f_uint(x) FormatType_uint, (uint) (x)
#define f_s64(x) FormatType_s64, (s64) (x)
#define f_u64(x) FormatType_u64, (u64) (x)
#define f_smm(x) FormatType_smm, (smm) (x)
#define f_umm(x) FormatType_umm, (umm) (x)
#define f_float(x) FormatType_float, (double) (x)
#define f_string(x) FormatType_string, (int) (x).count, (char*) (x).data
#define f_cstring(x) FormatType_cstring, (cstring) (x)

void pln(cstring format...);
string string_format(cstring format...);

// TODO(Alexander): implement this later, we use stb_ds for now!
// NOTE(Alexander): dynamic arrays, usage:
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

// NOTE(Alexander): change the naming convention of stb_ds
// Usage:
// array(int)* values = 0;           // Don't need to allocate memory, then don't forget to set it to null (0)
// array_push(values, 10);           // Will allocate memory here
// int count = array_count(values);  // count = 1
// int x = array_pop(values);        // x = 10
#define array(V) V
#define array_free(a) arrfree(a)
#define array_push(a, x) arrput(a, x)
#define array_pop(a) arrpop(a)
#define array_insert(a, x, p) arrins(a, p, x)
#define array_remove(a, p) arrdel(a, p)
#define array_set_capacity(a, c) arrsetcap(a, c)
#define array_get_capacity(a) arrcap(a)
#define array_count(a) arrlen(a)
#define array_set_count(a, c) arrsetlen(a, c)

#define array_iterator(arr, it, it_index) \
int it_index = 0; \
for (auto it = arr[0]; \
it_index < arr_count(arr); \
it_index++, it = arr[it_index])

// NOTE(Alexander): hash maps
// Usage:
// map(int, int)* map = 0;                 // Don't need to allocate memory, then don't forget to set it to null (0)
// map_put(map, 10, 20);                   // Will allocate memory here
// int x = map_get(map, 10);               // x = 20
// int count = map_count(map);             // count = 1
#define map(K, V) struct { K key; V value; }
#define map_free(m) hmfree(m)
#define map_put(m, k, v) hmput(m, k, v)
#define map_get(m, k) hmget(m, k)
#define map_remove(m, k) hmdel(m, k)
#define map_count(m) hmlen(m)

// NOTE(Alexander): hash map iterator
// Usage: continuing from previous example...
//
// int result = 0;
// map_iterator(map, it, it_index) {
//     result += it;
// }
// pln("%d", f_int(result)); // 10
#define map_iterator(map, it, it_index) \
int it_index = 0; \
for (auto it = map[0]; \
it_index < map_count(map); \
it_index++, it = map[it_index])

// NOTE(Alexander): string hash maps
#define string_map(V) struct { cstring key, V value }
#define string_map_free(m) smfree(m)
#define string_map_count(m) shlen(m)
#define string_map_put(m, k, v) shput(m, k, v)
#define string_map_get(m, k) shget(m, k)
#define string_map_remove(m, k) shdel(m, k)
#define string_map_new_arena(m) sh_new_arena(m)

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

#define binary_search(arr, val, compare) _binary_search(arr, &(val), array_count(arr), sizeof(arr), compare)

// NOTE(Alexander): hash map

// NOTE(Alexander): memory arena
#ifndef DEFAULT_ALIGNMENT
#define DEFAULT_ALIGNMENT (2*alignof(smm))
#endif
#define ARENA_DEFAULT_BLOCK_SIZE kilobytes(10)

// NOTE(Alexander): align has to be a power of two.
inline umm
align_forward(umm address, umm align) {
    assert_power_of_two(align);
    umm modulo = address & (align - 1);
    if (modulo != 0) {
        address += align - modulo;
    }
    return address;
}

// NOTE(Alexander): memory arena
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
        // TODO(Alexander): we need to also store the previous memory block so we can eventually free it.
    }
    
    void* result = arena->base + offset;
    arena->prev_used = arena->curr_used;
    arena->curr_used = offset + size;
    
    // TODO(Alexander): add memory clear to zero flag
    
    return result;
}

#define arena_push_struct(arena, type, ...) (type*) arena_push_size(arena, (umm) sizeof(type), (umm) alignof(type), __VA_ARGS__)

inline void
arena_rewind(Arena* arena) {
    arena->curr_used = arena->prev_used;
}

inline void
arena_clear(Arena* arena) {
    arena->curr_used = 0;
    arena->prev_used = 0;
}
