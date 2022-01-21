
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
                case FormatType_char: {
                    printf("%c", va_arg(args, char));
                } break;
                
                case FormatType_int: {
                    printf("%d", va_arg(args, int));
                } break;
                
                case FormatType_uint: {
                    printf("%u", va_arg(args, uint));
                } break;
                
                case FormatType_s64: {
                    printf("%lld", va_arg(args, s64));
                } break;
                
                case FormatType_u64: {
                    printf("%llu", va_arg(args, u64));
                } break;
                
                case FormatType_smm: {
                    printf("%zd", va_arg(args, smm));
                } break;
                
                case FormatType_umm: {
                    printf("%zu", va_arg(args, umm));
                } break;
                
                case FormatType_float: {
                    printf("%f", va_arg(args, double));
                } break;
                
                case FormatType_string: {
                    int count = va_arg(args, int);
                    char* str = va_arg(args, char*);
                    printf("%.*s", count, str);
                } break;
                
                case FormatType_cstring: {
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
        printf("%.*s\n", count_until_percent, format_at_prev_percent);
    } else {
        printf("\n");
    }
}

struct Format_Sprintf_Result {
    int count;
    va_list next_args;
};

inline internal Format_Sprintf_Result
format_sprintf(char* dst, umm dst_size, Format_Type type, va_list args) {
    Format_Sprintf_Result result;
    
    switch (type) {
        case FormatType_char: {
            if (dst_size >= 1) {
                char value = va_arg(args, char);
                *dst = (u8) value;
                result.count = 1;
            }
        } break;
        
        case FormatType_int: {
            int value = va_arg(args, int);
            result.count = snprintf(dst, dst_size, "%d", value);
        } break;
        
        case FormatType_uint: {
            uint value = va_arg(args, uint);
            result.count= snprintf(dst, dst_size, "%u", value);
        } break;
        
        case FormatType_s64: {
            s64 value = va_arg(args, s64);
            result.count = snprintf(dst, dst_size, "%lld", value);
        } break;
        
        case FormatType_u64: {
            u64 value = va_arg(args, u64);
            result.count= snprintf(dst, dst_size, "%llu", value);
        } break;
        
        case FormatType_smm: {
            smm value = va_arg(args, smm);
            result.count = snprintf(dst, dst_size, "%zd", value);
        } break;
        
        case FormatType_umm: {
            umm value = va_arg(args, umm);
            result.count = snprintf(dst, dst_size, "%zu", value);
        } break;
        
        case FormatType_float: {
            double value = va_arg(args, double);
            result.count = snprintf(dst, dst_size, "%f", value);
        } break;
        
        case FormatType_string: {
            int str_count = va_arg(args, int);
            char* str = va_arg(args, char*);
            result.count = snprintf(dst, dst_size, "%.*s", str_count, str);
        } break;
        
        case FormatType_cstring: {
            char* cstr = va_arg(args, char*);
            result.count = snprintf(dst, dst_size, "%s", cstr);
        } break;
        
        default: {
            assert(0 && "Incorrect formatting option");
        } break;
    }
    
    result.next_args = args;
    return result;
}

internal va_list
string_builder_push_data_format(String_Builder* sb, Format_Type type, va_list args) {
    va_list result = args;
    
    for (;;) {
        umm size_remaining = sb->size - sb->curr_used;
        Format_Sprintf_Result fmt = format_sprintf((char*) sb->data + sb->curr_used, size_remaining, type, args);
        
        if (fmt.count >= size_remaining) {
            string_builder_ensure_capacity(sb, fmt.count + 1);
        } else {
            sb->curr_used += fmt.count;
            result = fmt.next_args;
            break;
        }
    }
    
    return result;
}

void
string_builder_push_cformat(String_Builder* sb, cstring format...) {
    va_list args;
    va_start(args, format);
    
    for (;;) {
        umm size_remaining = sb->size - sb->curr_used;
        int count = vsnprintf((char*) sb->data, size_remaining, format, args);
        
        if (count >= size_remaining) {
            string_builder_ensure_capacity(sb, count + 1);
        } else {
            sb->curr_used += count;
            break;
        }
    }
    
    va_end(args);
}

internal void
_string_builder_push_format(String_Builder* sb, cstring format, va_list args) {
    u8* scan = (u8*) format;
    u8* last_push = scan;
    
    while (*scan) {
        if (*scan == '%') {
            if (*(scan + 1) == '%') {
                continue;
            }
            
            if (last_push != scan) {
                string_builder_push(sb, string_view(last_push, scan));
            }
            last_push = scan + 1;
            
            Format_Type format_type = va_arg(args, Format_Type);
            
            args = string_builder_push_data_format(sb, format_type, args);
        }
        
        scan++;
    }
    
    if (last_push != scan) {
        string_builder_push(sb, string_view(last_push, scan));
    }
}

void
string_builder_push_format(String_Builder* sb, cstring format...) {
    va_list args;
    va_start(args, format);
    _string_builder_push_format(sb, format, args);
    va_end(args);
}

string // NOTE(alexander): this string has to be manually freed at the moment!!!
string_format(cstring format...) { // TODO(alexander): replace snprintf with custom implementation later...
    va_list args;
    va_start(args, format);
    
    String_Builder sb = {};
    string_builder_alloc(&sb, 1000);
    _string_builder_push_format(&sb, format, args);
    
    return string_builder_to_string_nocopy(&sb);
}
