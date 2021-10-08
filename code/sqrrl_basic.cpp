
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

string // NOTE(alexander): this string has to be manually freed at the moment!!!
string_format(const char* format...) { // TODO(alexander): replace snprintf with custom implementation later...
    va_list args;
    va_start(args, format);
    
    umm size_remaining = 1000;
    string result = (string) malloc(size_remaining + 5) + 4; // TODO(alexander): use a scratch buffer instead
    char* buffer = (char*) result;
    u32 resulting_count = 0;
    
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
                
                case FormatType_string: {
                    int str_count = va_arg(args, int);
                    char* str = va_arg(args, char*);
                    count = snprintf(buffer, size_remaining, "%.*s", str_count, str);
                } break;
                
                case FormatType_cstring: {
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
            resulting_count += count;
            
        } else {
            size_remaining--;
            *buffer++ = *format;
            resulting_count++;
        }
        
        format++;
    }
    
    string_count(result) = resulting_count;
    
    return result;
}
