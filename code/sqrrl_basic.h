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

// NOTE(alexander): define assestion macro on debug mode
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
typedef const char*  cstr;

// NOTE(alexander): string definition
struct str {
    u8* data;
    smm count;
};

// NOTE(alexander): converts C string to str type
inline str
str_lit(const char* string) {
    str result;
    result.data = (u8*) string;
    result.count = strlen(string);
    return result;
}

// NOTE(alexander): for converting str back to char* (not completely the same as cstr)
#define lit(string) ((char*) (string).data)

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
#define f_uint(x) FormatType_uint, (uint) (x)
#define f_str(x) FormatType_str, (int) ((x).count), (char*) ((x).data)
#define f_cstr(x) FormatType_cstr, (cstr) (x)

void
pln(const char* format...) {
    va_list args;
    va_start(args, format);
    
    const char* format_at_prev_percent = format;
    int count_until_percent = 0;
    while (*format != '\0') {
        if (*format == '%') {
            if (count_until_percent > 0) {
                printf("%.*s", count_until_percent, format_at_prev_percent);
                count_until_percent = 0;
            }
            
            format_at_prev_percent = format + 1;
            
            Format_Type type = (Format_Type) va_arg(args, int);
            switch (type) {
                case FormatType_int: {
                    printf("%d", va_arg(args, int));
                } break;
                
                case FormatType_uint: {
                    printf("%u", va_arg(args, uint));
                } break;
                
                case FormatType_smm: {
                    printf("%zd", va_arg(args, smm));
                } break;
                
                case FormatType_umm: {
                    printf("%zu", va_arg(args, umm));
                } break;
                
                case FormatType_str: {
                    int count = va_arg(args, int);
                    char* string = va_arg(args, char*);
                    printf("%.*s", count, string);
                } break;
                
                case FormatType_cstr: {
                    printf("%s", va_arg(args, char*));
                } break;
                
                default: {
                    printf("%c", *format);
                } break;
            }
        } else {
            count_until_percent++;
        }
        
        format++;
    }
    
    if (count_until_percent > 0) {
        printf("%.*s", count_until_percent, format_at_prev_percent);
    }
}

// TODO(alexander): maybe move this to sqrrl_str.h later...
str // NOTE(alexander): this string has to be manually freed at the moment!!!
str_format(const char* format...) { // TODO(alexander): replace snprintf with custom implementation later...
    va_list args;
    va_start(args, format);
    
    umm size_remaining = 1000;
    str result;
    result.data = (u8*) malloc(size_remaining); // TODO(alexander): use a scratch buffer instead
    result.count = 0;
    char* buffer = (char*) result.data;
    
    while (*format != '\0') {
        if (*format == '%') {
            int count;
            Format_Type type = va_arg(args, Format_Type);
            switch (type) {
                case FormatType_int: {
                    count = snprintf(buffer, size_remaining, "%d", va_arg(args, int));
                } break;
                
                case FormatType_uint: {
                    count = snprintf(buffer, size_remaining, "%u", va_arg(args, uint));
                } break;
                
                case FormatType_smm: {
                    count = snprintf(buffer, size_remaining, "%zd", va_arg(args, smm));
                } break;
                
                case FormatType_umm: {
                    count = snprintf(buffer, size_remaining, "%zu", va_arg(args, umm));
                } break;
                
                case FormatType_str: {
                    count = snprintf(buffer, size_remaining, "%.*s", va_arg(args, int), va_arg(args, char*));
                } break;
                
                case FormatType_cstr: {
                    count = snprintf(buffer, size_remaining, "%s", va_arg(args, char*));
                } break;
                
                default: {
                    *buffer = *format;
                    count = 1; 
                } break;
            }
            
            if (count == 0) {
                assert(0 && "buffer overflow"); // TODO(alexander): increase buffer size
            }
            size_remaining -= count;
            buffer += count;
            result.count += count;
            
        } else {
            size_remaining--;
            *buffer++ = *format;
        }
        
        format++;
    }
    
    return result;
}