
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

internal void
string_builder_push_data_format(String_Builder* sb, Format_Type type, va_list args) {
    int count = -1;
    
    while (count < 0) {
        umm size_remaining = sb->size - sb->curr_used;
        
        switch (type) {
            case FormatType_char: {
                if (sb->curr_used < sb->size) {
                    char value = va_arg(args, char);
                    sb->data[sb->curr_used++] = (u8) value;
                    count = 1;
                }
            } break;
            
            case FormatType_int: {
                int value = va_arg(args, int);
                count = snprintf((char*) sb->data + sb->curr_used, size_remaining, "%d", value);
            } break;
            
            case FormatType_uint: {
                uint value = va_arg(args, uint);
                count= snprintf((char*) sb->data + sb->curr_used, size_remaining, "%u", value);
            } break;
            
            case FormatType_smm: {
                smm value = va_arg(args, smm);
                count = snprintf((char*) sb->data + sb->curr_used, size_remaining, "%zd", value);
            } break;
            
            case FormatType_umm: {
                umm value = va_arg(args, umm);
                count = snprintf((char*) sb->data + sb->curr_used, size_remaining, "%zu", value);
            } break;
            
            case FormatType_string: {
                int str_count = va_arg(args, int);
                char* str = va_arg(args, char*);
                count = snprintf((char*) sb->data + sb->curr_used, size_remaining, "%.*s", str_count, str);
            } break;
            
            case FormatType_cstring: {
                char* cstr = va_arg(args, char*);
                count = snprintf((char*) sb->data + sb->curr_used, size_remaining, "%s", cstr);
            } break;
            
            default: {
                assert(0 && "Incorrect formatting option");
            } break;
        }
        
        if (count >= 0) {
            sb->curr_used += count;
        } else {
            assert(0 && "buffer overflow");
            //string_builder_ensure_capacity(sb, 1);
        }
    }
}

void
string_builder_push_format(String_Builder* sb, cstring format...) {
    u8* scan = (u8*) format;
    u8* last_push = scan;
    
    va_list args;
    va_start(args, format);
    
    while (*scan) {
        if (*scan == '%') {
            if (*(scan + 1) == '%') {
                continue;
            }
            
            if (last_push != scan) {
                string_builder_push(sb, string_view(last_push, scan));
                last_push = scan + 1;
            }
            Format_Type format_type = va_arg(args, Format_Type);
            
            string_builder_push_data_format(sb, format_type, args);
        }
        
        scan++;
    }
    
    if (last_push != scan) {
        string_builder_push(sb, string_view(last_push, scan));
    }
    
    va_end(args);
}

string // NOTE(alexander): this string has to be manually freed at the moment!!!
string_format(const char* format...) { // TODO(alexander): replace snprintf with custom implementation later...
    va_list args;
    va_start(args, format);
    
    String_Builder sb = {};
    string_builder_alloc(&sb, 1000);
    
    return string_builder_to_string_nocopy(&sb);
}
