// Basic defines useful types and functions similar to C++ standard library

#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <cstdarg>
#include <cmath>

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

#define is_power_of_two(x) (((x) & ((x) - 1)) == 0)

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
#define SMM_MIN S64_MIN // TODO(Alexander): we need to detect and set this correctly later
#define SMM_MAX S64_MAX // TODO(Alexander): we need to detect and set this correctly later
#define INT_MIN S32_MIN
#define INT_MAX S32_MAX
#define U8_MAX 0xffU
#define U16_MAX 0xffffU
#define U32_MAX 0xffffffffU
#define U64_MAX 0xffffffffffffffffull
#define UMM_MAX U64_MAX // TODO(Alexander): we need to detect and set this correctly later
#define UINT_MAX U32_MAX
#define BOOL_MAX S8_MAX
#define BOOL_MIN S8_MIN
#define CHAR_MAX U8_MAX
#define CHAR_MIN 0U

// NOTE(Alexander): memory operations
// TODO(Alexander): implement memcpy ourselves
#define copy_memory memcpy

void DEBUG_log_backtrace();

#if BUILD_DEBUG
#define ptrace() \
DEBUG_log_backtrace();
#else
#define ptrace()
#endif

// NOTE(Alexander): define assestion macro on debug mode
#ifdef assert
#undef assert
#endif
#if BUILD_DEBUG
void
__assert(cstring expression, cstring file, smm line) {
    // TODO(Alexander): improve assertion printing.
    fprintf(stderr, "%s:%zd: Assertion failed: %s\n", file, line, expression);
    DEBUG_log_backtrace();
    
    // Flush the standard streams make sure we get all debug data
    fflush(stdout);
    fflush(stderr);
    
    *(int *)0 = 0; // NOTE(Alexander): purposefully trap the program
}
#define assert(expression) (void)((expression) || (__assert(#expression, __FILE__, __LINE__), 0))
#else
#define assert(expression)
#endif

#if !BUILD_TEST
extern "C" bool
intrinsic_assert(int test, cstring msg, cstring file, smm line) {
    if (test == 0) {
        pln("%:%: Assertion failed: %", f_cstring(file), f_smm(line), f_cstring(msg));
        fflush(stdout);
        *(int *)0 = 0;
        return false;
    }
    return true;
}
#endif

#define unimplemented assert(0 && "this code is not implemented yet")
#define invalid_code_path assert(0 && "invalid code path, this is likely a bug")
#define compiler_bug(m) assert(0 && "Compiler Bug!"#m)

// TODO(Alexander): special asserts
#define assert_enum(T, v) assert((v) > 0 && (v) < T##_Count && "enum value out of range")
#define assert_power_of_two(x) assert(is_power_of_two(x) && "x is not power of two")


// TODO(Alexander): create proper allocators
void*
allocate(umm size) {
    return malloc(size);
}

void*
allocate_zeros(umm size) {
    return calloc(1, size);
}

s32
abs_s32(s32 s) {
    return s < 0 ? -s : s;
}

f32
random_f32() {
    return (f32) rand() / (RAND_MAX + 1.0f);
}

inline u32
safe_truncate_u64(u64 value) {
    assert(value <= U32_MAX && "u64 cannot fit in u32");
    return (u32) value;
}

inline s32
safe_truncate_s64(s64 value) {
    assert(value >= S32_MIN && value <= S32_MAX && "s64 cannot fit in s32");
    return (s32) value;
}

inline u32
safe_cast_u32(s32 value) {
    assert(value < 0 && "s32 value was negative");
    return (u32) value;
}

inline umm
cstring_count(cstring str) {
    return (umm) strlen(str);
}

inline cstring
cstring_copy(cstring str) {
    umm count = cstring_count(str);
    cstring result = (cstring) malloc(count + 1);
    copy_memory((void*) result, (void*) str, count + 1);
    return result;
}

inline void
cstring_free(cstring str) {
    free((void*) str);
}

cstring
cstring_to_lower_ascii_nocopy(char* dest, cstring src, umm count) {
    for (umm i = 0; i < count; i++) {
        char c = src[i];
        char is_upper = (u8) (c >= 'A' && c <= 'Z');
        dest[i] = is_upper * (c - 'A' + 'a') + !is_upper * c;
    }
    dest[count] = 0;
    return (cstring) dest;
}

inline cstring
cstring_to_lower_ascii_nocopy(cstring str) {
    return cstring_to_lower_ascii_nocopy((char*) str, str, cstring_count(str));
}


inline cstring
cstring_to_lower_ascii(cstring str) {
    umm count = cstring_count(str);
    char* result = (char*) malloc(count + 1);
    return cstring_to_lower_ascii_nocopy(result, str, count);
}

inline cstring
cstring_concat(cstring a_data, umm a_count,
               cstring b_data, umm b_count) {
    
    char* result = (char*) malloc(a_count + b_count + 1);
    
    if (result) {
        copy_memory((void*) result, a_data, a_count);
        copy_memory((void*) (result + a_count), b_data, b_count);
        
        result[a_count + b_count] = '\0';
    }
    
    return (cstring) result;
}

inline cstring
cstring_concat(cstring a, cstring b) {
    return cstring_concat(a, cstring_count(a), 
                          b, cstring_count(b));
}

// NOTE(Alexander): strings
struct string {
    u8* data;
    smm count;
};


inline string
create_string(smm count, u8* data) {
    string result;
    result.count = count;
    result.data = data;
    return result;
};

// TODO(Alexander): lazy!!!! don't use malloc for this, put in arena later...
inline string
string_alloc(smm count) {
    string result;
    result.count = count;
    result.data = (u8*) malloc(count);
    return (string) result;
}

inline void
string_free(string str) {
    free(str.data);
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
string_to_cstring(string str, char* dest=0) {
    char* result = dest ? dest : (char*) malloc(str.count + 1);
    copy_memory(result, str.data, str.count);
    result[str.count] = 0;
    return (cstring) result;
}

inline string
string_copy(string str) {
    string result = string_alloc(str.count);
    copy_memory(result.data, str.data, str.count);
    return result;
}

inline string
string_view(u8* begin, u8* end) {
    assert(begin <= end);
    
    string result;
    result.count = (smm) (end - begin);
    result.data = begin;
    return result;
}

inline int
string_compare(string a, string b) {
    smm count = min(a.count, b.count);
    for (smm i = 0; i < count; i++) {
        if (a.data[i] != b.data[i]) {
            return b.data[i] - a.data[i];
        }
    }
    
    return (int) (b.count - a.count);
}
#define string_equals(a, b) (string_compare(a, b) == 0)

inline bool
string_begins_with(string str, string substr) {
    if (str.count < substr.count) {
        return false;
    }
    str.count = substr.count;
    return string_equals(str, substr);
}

inline bool
string_ends_with(string str, string substr) {
    if (str.count < substr.count) {
        return false;
    }
    u8* end = str.data + str.count;
    str = string_view(end - substr.count, end);
    return string_equals(str, substr);
}

inline void
string_to_lower_ascii_no_copy(string* str) {
    for (smm i = 0; i < str->count; i++) {
        u8 c = str->data[i];
        u8 is_upper = (u8) (c >= 'A' && c <= 'Z');
        str->data[i] = is_upper * (c - 'A' + 'a') + !is_upper * c;
    }
}

inline string
string_concat(string a, string b) {
    string concat = string_alloc(a.count + b.count);
    copy_memory(concat.data, a.data, a.count);
    copy_memory(concat.data + a.count, b.data, b.count);
    return concat;
}

inline string
string_concat(string a, cstring b) {
    smm b_count = cstring_count(b);
    string concat = string_alloc(a.count + b_count);
    copy_memory(concat.data, a.data, a.count);
    copy_memory(concat.data + a.count, b, b_count);
    return concat;
}

string
string_unquote_nocopy(string s) {
    string result = s;
    
    if (s.data[0] == '"') {
        result.data++;
        result.count--;
    }
    
    if (s.data[s.count - 1] == '"') {
        result.count--;
    }
    
    return result;
}

// Memory string stores count and cstring next to each other in memory
typedef cstring Memory_String;

#define memory_string_count(s) *((smm*) (s) - 1)

inline string
mstring_to_string(Memory_String str) {
    string result;
    result.count = memory_string_count(str);
    result.data = (u8*) str;
    return result;
}

inline Memory_String
string_copy_to_memory(string str, void* dest) {
    *(smm*) dest = str.count;
    memcpy((u8*)((smm*) dest + 1), str.data, str.count);
    return (Memory_String) ((smm*) dest + 1);
}

struct String_Builder {
    u8* data;
    smm size;
    smm curr_used;
};

inline void
string_builder_clear(String_Builder* sb) {
    // TODO(Alexander): maybe we want to shink the buffer if it's very large?
    sb->curr_used = 0;
}

inline void
string_builder_free(String_Builder* sb) {
    free(sb->data);
    sb->data = 0;
    sb->curr_used = 0;
    sb->size = 0;
}

inline void
string_builder_alloc(String_Builder* sb, smm new_size) {
    void* new_data = realloc(sb->data, new_size);
    if (!new_data) {
        new_data = malloc(new_size);
        copy_memory(new_data, sb->data, sb->size);
        free(sb->data);
    }
    sb->data = (u8*) new_data;
    sb->size = new_size;
}

inline void
string_builder_ensure_capacity(String_Builder* sb, smm capacity) {
    smm min_size = sb->curr_used + capacity;
    if (min_size > sb->size) {
        smm new_size = max(sb->size * 2, min_size);
        string_builder_alloc(sb, new_size);
    }
    
}

void
string_builder_push(String_Builder* sb, string str) {
    string_builder_ensure_capacity(sb, str.count);
    
    copy_memory(sb->data + sb->curr_used, str.data, str.count);
    sb->curr_used += str.count;
}

inline void
string_builder_push_char(String_Builder* sb, u8 c) {
    string_builder_ensure_capacity(sb, 1);
    *(sb->data + sb->curr_used) = c;
    sb->curr_used += 1;
}

void
string_builder_push(String_Builder* sb, cstring str) {
    string_builder_push(sb, string_lit(str));
}

void string_builder_push_format(String_Builder* sb, cstring format...);
void string_builder_push_cformat(String_Builder* sb, cstring format...);

string
string_builder_to_string(String_Builder* sb) {
    string result;
    result.data = (u8*) malloc(sb->curr_used + 1);
    result.count = sb->curr_used;
    copy_memory(result.data, sb->data, sb->curr_used);
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
enum Format_Type { // TODO(Alexander): add more types
    FormatType_None,
    FormatType_void,
    FormatType_char,
    FormatType_int,
    FormatType_s8,
    FormatType_s16,
    FormatType_s32,
    FormatType_s64,
    FormatType_smm,
    FormatType_uint,
    FormatType_u8,
    FormatType_u16,
    FormatType_u32,
    FormatType_u64,
    FormatType_umm,
    FormatType_bool,
    FormatType_b32,
    FormatType_f32,
    FormatType_f64,
    FormatType_string,
    FormatType_cstring,
    FormatType_memory_string,
    
    FormatType_u64_HEX,
    
    FormatType_ast,
    FormatType_value,
    FormatType_type,
    FormatType_intermediate_code,
};

// TODO(Alexander): add more types
#define f_bool(x) FormatType_cstring, (x) ? "true" : "false"
#define f_char(x) FormatType_char, (char) (x)
#define f_int(x) FormatType_int, (int) (x)
#define f_uint(x) FormatType_uint, (uint) (x)
#define f_s64(x) FormatType_s64, (s64) (x)
#define f_u32(x) FormatType_u32, (u32) (x)
#define f_u64(x) FormatType_u64, (u64) (x)
#define f_u64_HEX(x) FormatType_u64_HEX, (u64) (x)
#define f_smm(x) FormatType_smm, (smm) (x)
#define f_umm(x) FormatType_umm, (umm) (x)
#define f_float(x) FormatType_f64, (double) (x)
#define f_var(x) FormatType_string, (string) vars_load_string(x)
#define f_string(x) FormatType_string, (string) (x)
#define f_mstring(x) FormatType_memory_string, (Memory_String) (x)
#define f_cstring(x) FormatType_cstring, (cstring) (x)
#define f_ast(x) FormatType_ast, (Ast*) (x)
#define f_value(x) FormatType_value, (Value*) (x)
#define f_type(x) FormatType_type, (Type*) (x)
#define f_ic(x) FormatType_intermediate_code, (Intermediate_Code*) (x)

void print_format(cstring format...);
string string_format(cstring format...);

// NOTE(Alexander): print formatted string with new line
#define pln(format, ...) print_format(format##"\n", ##__VA_ARGS__)
//#define pln(...)


// TODO(Alexander): implement this later, we use stb_ds for now!
// NOTE(Alexander): dynamic arrays, usage:
//     i32* array = 0;
//     arr_push(array, 5);

//struct Array_,Header {
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
#define array_first(a) (array_count(a) ? (a) : 0)
#define array_last(a) arrlast(a)
#define array_insert(a, x, p) arrins(a, p, x)
#define array_remove(a, p) arrdel(a, p)
#define array_remove_n(a, p, n) arrdeln(a, p, n)
#define array_swap_remove(a, p) arrdelswap(a, p)
#define array_set_capacity(a, c) arrsetcap(a, c)
#define array_get_capacity(a) arrcap(a)
#define array_count(a) arrlen(a)
#define array_set_count(a, c) arrsetlen(a, c)

#define for_array(arr, it, it_index) \
int it_index = 0; \
for (auto it = arr; \
it && it_index < array_count(arr); \
it_index++, it++)

#define for_array_reverse(arr, it, it_index) \
int it_index = (int) array_count(arr) - 1; \
if (arr) \
for (auto it = &array_last(arr); \
it && it_index >= 0; \
it_index--, it--)

#define for_array_v(arr, it, it_index) \
int it_index = 0; \
if (arr) \
for (auto it = arr[it_index]; \
it_index < array_count(arr); \
it = arr[++it_index < array_count(arr) ? it_index : 0])

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
#define map_get_index(m, k) hmgeti(m, k)
#define map_key_exists(m, k) ((hmgeti(m, k)) != -1)
#define map_remove(m, k) hmdel(m, k)
#define map_count(m) hmlen(m)

// NOTE(Alexander): hash map iterator
// Usage: continuing from previous example...
//
// int result = 0;
// for_map(map, it) {
//     result += it->value;
// }
// pln("%d", f_int(result)); // 10
#define for_map(map, it) \
for (auto it = map; it < map + map_count(map); it++)

// NOTE(Alexander): string hash maps
#define string_map(V) struct { cstring key; V value; }
#define string_map_free(m) smfree(m)
#define string_map_count(m) shlen(m)
#define string_map_put(m, k, v) shput(m, k, v)
#define string_map_get(m, k) shget(m, k)
#define string_map_get_index(m, k) shgeti(m, k)
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
    //pln("low = %, high = %", f_smm(low), f_smm(high));
    
    while (low <= high) {
        smm mid = (high - low) / 2 + low;
        result.index = mid;
        
        void* elem = (u8*) arr + mid*size;
        //pln("low = %, high = %, *val = %", f_smm(low), f_smm(high), f_smm(*((smm*) val)));
        
        int cmp = compare(val, elem);
        //pln("% - % = %", f_smm(*((smm*) val)), f_smm(*((smm*) elem)), f_int(cmp));
        if (cmp > 0) {
            low = mid + 1;
        } else if (cmp < 0) {
            high = mid - 1;
            //if (low > high) {
            //result.index = max(mid - 1, 0);
            //}
        } else {
            result.exact_match = true;
            break;
        }
    }
    
    result.value = (u8*) arr + result.index*size;
    return result;
}

#define binary_search(arr, val, compare) \
_binary_search(arr, &(val), array_count(arr), sizeof(arr), compare)

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
struct Memory_Arena {
    u8* base; // TODO(Alexander): we need to keep track of allocated blocks!
    umm size;
    umm curr_used;
    umm prev_used;
    umm min_block_size;
};

struct Temporary_Memory {
    Memory_Arena* arena;
    // TODO(Alexander): we need to keep track of allocated blocks!
    umm curr_used;
    umm prev_used;
};

inline void
arena_initialize(Memory_Arena* arena, void* base, umm size) {
    arena->base = (u8*) base;
    arena->size = size;
    arena->curr_used = 0;
    arena->prev_used = 0;
    arena->min_block_size = 0;
}

inline void
arena_initialize(Memory_Arena* arena, umm min_block_size) {
    arena->base = 0;
    arena->size = 0;
    arena->curr_used = 0;
    arena->prev_used = 0;
    arena->min_block_size = min_block_size;
}

inline Temporary_Memory
begin_temporary_memory(Memory_Arena* arena) {
    Temporary_Memory result = {};
    result.arena = arena;
    result.curr_used = arena->curr_used;
    result.prev_used = arena->prev_used;
    return result;
}

inline void
end_temporary_memory(Temporary_Memory* temp_memory) {
    Memory_Arena* arena = temp_memory->arena;
    arena->curr_used = temp_memory->curr_used;
    arena->prev_used = temp_memory->prev_used;
}


inline void
arena_grow(Memory_Arena* arena, umm block_size = 0) {
    if (block_size == 0) {
        if (arena->min_block_size == 0) {
            arena->min_block_size = ARENA_DEFAULT_BLOCK_SIZE;
        }
        
        block_size = arena->min_block_size;
    }
    
    arena->base = (u8*) calloc(1, arena->min_block_size);
    arena->curr_used = 0;
    arena->prev_used = 0;
    arena->size = block_size;
}


void*
arena_push_size(Memory_Arena* arena, umm size, umm align=DEFAULT_ALIGNMENT, umm flags=0) {
    umm current = (umm) (arena->base + arena->curr_used);
    umm offset = align_forward(current, align) - (umm) arena->base;
    
    if (offset + size > arena->size) {
        arena_grow(arena);
        
        current = (umm) arena->base + arena->curr_used;
        offset = align_forward(current, align) - (umm) arena->base;
    }
    
    void* result = arena->base + offset;
    arena->prev_used = arena->curr_used;
    arena->curr_used = offset + size;
    
    memset(result, 0, size);
    
    // TODO(Alexander): add memory clear to zero flag
    
    return result;
}

inline Memory_String
arena_push_flat_string(Memory_Arena* arena, string str) {
    void* memory = arena_push_size(arena, sizeof(umm) + str.count, alignof(umm));
    return string_copy_to_memory(str, memory);
}

#define arena_can_fit(arena, type) \
arena_can_fit_size(arena, sizeof(type), alignof(type))

inline bool
arena_can_fit_size(Memory_Arena* arena, umm size, umm align) {
    umm current = (umm) (arena->base + arena->curr_used);
    umm offset = align_forward(current, align) - (umm) arena->base;
    return offset + size <= arena->size;
}


#define arena_push_struct(arena, type, ...) \
(type*) arena_push_size(arena, (umm) sizeof(type), (umm) alignof(type), ##__VA_ARGS__)

#define arena_push_array_of_structs(arena, count, type, ...) \
(type*) arena_push_size(arena, (umm) count*sizeof(type), (umm) alignof(type), ##__VA_ARGS__)

#define arena_get_data(arena, byte_offset) \
(void*) ((u8*) (arena)->base + (byte_offset))

#define arena_get_struct(arena, type, byte_offset) \
(type*) ((u8*) (arena)->base + (byte_offset))


inline void
arena_rewind(Memory_Arena* arena) {
    arena->curr_used = arena->prev_used;
}

inline void
arena_clear(Memory_Arena* arena) {
    arena->curr_used = 0;
    arena->prev_used = 0;
}
