

enum WASM_Section {
    WASMSection_Custom,
    WASMSection_Type,
    WASMSection_Import,
    WASMSection_Function,
    WASMSection_Table,
    WASMSection_Memory,
    WASMSection_Global,
    WASMSection_Export,
    WASMSection_Start,
    WASMSection_Element,
    WASMSection_Code,
    WASMSection_Data,
    WASMSection_Data_Count,
    
    WASMSection_Count,
};

const cstring wasm_section_names[] = {
    "custom", "type", "import", "function", "table", "memory", "global",
    "export", "start", "element", "code", "data", "data count", "count"
};

void
convert_type_to_wasm(Type* type, Buffer* buf) {
    
    switch (type->kind) {
        case TypeKind_Basic: {
            switch (type->Basic.kind) {
                case Basic_bool:
                case Basic_s8:
                case Basic_u8:
                case Basic_s16:
                case Basic_u16:
                case Basic_s32:
                case Basic_u32:
                case Basic_int:
                case Basic_uint: push_u8(buf, 0x7F); break;
                
                case Basic_s64: 
                case Basic_u64: 
                case Basic_smm: 
                case Basic_umm: 
                case Basic_string:
                case Basic_cstring: push_u8(buf, 0x7E); break;
                
                case Basic_f32: push_u8(buf, 0x7D); break;
                case Basic_f64: push_u8(buf, 0x7C); break;
                
                default: unimplemented; break;
            }
        } break;
        case TypeKind_Struct:
        case TypeKind_Union: // TODO(Alexander): should structs/ unions be a different type than 64-bit int?
        case TypeKind_Type:
        case TypeKind_Pointer: {
            push_u8(buf, 0x7E);
        } break;
        
        case TypeKind_Enum: {
            convert_type_to_wasm(type->Enum.type, buf);
        } break;
        
        case TypeKind_Function: {
            push_u8(buf, 0x60); // functype tag
            
            // arguments
            push_leb128_u32(buf, (u32) array_count(type->Function.arg_types));
            for_array_v(type->Function.arg_types, arg_type, arg_index) {
                convert_type_to_wasm(arg_type, buf);
            }
            
            // return values
            // TODO: multiple return types
            bool has_return_type = is_valid_type(type->Function.return_type);
            push_leb128_u32(buf, has_return_type);
            if (has_return_type) {
                convert_type_to_wasm(type->Function.return_type, buf);
            }
        } break;
        
        default: {
            unimplemented;
        } break;
    }
}

void
wasm_mov(Intermediate_Code* ic, Buffer* buf,
         Ic_Type t1, s64 r1, s64 d1,
         Ic_Type t2, s64 r2, s64 d2, s64 rip) {
    
    
    switch (t1 & IC_TF_MASK) {
        case IC_REG: {
            
            if (t2 & IC_DISP) {
                push_u8(buf, 0x41); // 0x41=i32.const
                push_leb128_s32(buf, (s32) d2);
            }
        } break;
    }
}

s64
convert_ic_to_wasm(Intermediate_Code* ic, Buffer* buf, s64 rip) {
    switch (ic->opcode) {
        case IC_MOV: {
            wasm_mov(ic, buf,
                     ic->src0.type, ic->src0.reg, ic->src0.disp,
                     ic->src1.type, ic->src1.reg, ic->src1.disp, rip);
        } break;
    }
    
    return rip;
}

void
wasm_push_string(Buffer* buf, string str) {
    push_leb128_u32(buf, (u32) str.count);
    memmove(&buf->data[buf->curr_used], str.data, str.count);
    buf->curr_used += str.count;
}

void
string_builder_pad(String_Builder* sb, smm from_byte, smm width) {
    smm used = sb->curr_used - from_byte;
    for (smm i = used; i < width; i++) {
        string_builder_push(sb, " ");
    }
}

void
string_builder_pad_string(String_Builder* sb, cstring str, smm width) { 
    smm start = sb->curr_used;
    string_builder_push(sb, str);
    string_builder_pad(sb, start, width);
}

// TODO: find a better place for this
void
print_bytes(String_Builder* sb, u8* bytes, int count, int pad=0) {
    for (int byte_index = 0; byte_index < count; byte_index++) {
        if (byte_index != 0 && byte_index % 16 == 0) {
            string_builder_push(sb, "\n");
            string_builder_pad(sb, sb->curr_used, pad);
        }
        
        u8 byte = ((u8*) bytes)[byte_index];
        if (byte > 0xF) {
            string_builder_push_cformat(sb, "%hhx ", byte);
        } else {
            string_builder_push_cformat(sb, "0%hhx ", byte);
        }
    }
}



struct WASM_Section_Marker {
    WASM_Section section;
    u32 offset;
    u32 size;
};

struct WASM_Debug {
    WASM_Section_Marker markers[WASMSection_Count];
    int marker_count;
};

void
DEBUG_wasm_begin_section(WASM_Debug* debug, WASM_Section section, Buffer* buf) {
    if (debug->marker_count < fixed_array_count(debug->markers)) {
        int marker_index = debug->marker_count;
        debug->markers[marker_index].section = section;
        debug->markers[marker_index].offset = (u32) buf->curr_used;
        
        if (marker_index > 0) {
            int last_offset = debug->markers[marker_index - 1].offset;
            debug->markers[marker_index - 1].size = (u32) (buf->curr_used - last_offset);
        }
        
        debug->marker_count++;
    }
}

void
DEBUG_wasm_end(WASM_Debug* debug, Buffer* buf) {
    String_Builder sb = {};
    int left_panel_width = 15;
    
    string_builder_push_format(&sb, "\nWASM (% bytes):\n\n", f_umm(buf->curr_used));
    
    string_builder_pad_string(&sb, "Signature: ", left_panel_width);
    print_bytes(&sb, buf->data, 8);
    
    if (debug->marker_count > 0) {
        int marker_index = debug->marker_count - 1;
        int last_offset = debug->markers[marker_index].offset;
        debug->markers[marker_index].size = (u32) (buf->curr_used - last_offset);
    }
    
    
    
    for (int marker_index = 0; 
         marker_index < debug->marker_count; 
         marker_index++) {
        
        WASM_Section_Marker marker = debug->markers[marker_index];
        smm start = sb.curr_used + 1;
        string_builder_push_format(&sb, "\n%: ", f_cstring(wasm_section_names[marker.section]));
        string_builder_pad(&sb, start, left_panel_width);
        print_bytes(&sb, buf->data + marker.offset, marker.size, left_panel_width);
        
    }
    
    string s = string_builder_to_string_nocopy(&sb);
    pln("%", f_string(s));
    string_builder_free(&sb);
}

void
wasm_set_vec_size(Buffer* buf, smm vec_first_byte) {
    // TODO: this is perhaps a bit of a hack to push the leb128 size
    smm size = buf->curr_used - vec_first_byte - 4;
    buf->curr_used = vec_first_byte;
    push_leb128_u32(buf, (u32) size);
    memmove(&buf->data[buf->curr_used], 
            &buf->data[vec_first_byte + 4], size);
    buf->curr_used += size;
}


void
convert_to_wasm_module(Bytecode* bytecode, s64 stk_usage, Buffer* buf) {
    
    // Define module
    push_u32(buf, 0x6D736100); // Signature (.asm)
    push_u32(buf, 0x1); // Version
    
    WASM_Debug debug = {};
    
    // Type section (1)
    DEBUG_wasm_begin_section(&debug, WASMSection_Type, buf);
    push_u8(buf, WASMSection_Type);
    smm type_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    
#if 0
    u32 num_types = 0;
    for_array(ast_file->units, cu, _) {
        if (cu->ast->kind == Ast_Decl_Stmt) {
            num_types++;
        }
    }
    
    push_leb128_u32(buf, num_types); // number of types
    for_array(ast_file->units, cu, _1) {
        if (cu->ast->kind == Ast_Decl_Stmt) {
            
            Type* type = cu->ast->type;
            if (type->kind == TypeKind_Function) {
                
                smm first_byte = buf->curr_used;
                pln("Compiling function: %", f_type(type));
                convert_type_to_wasm(type, buf);
                
                for (smm byte_index = first_byte; byte_index < buf->curr_used; byte_index++) {
                    u8 byte = buf->data[byte_index];
                    if (byte > 0xF) {
                        printf("%hhX ", byte);
                    } else {
                        printf("0%hhX ", byte);
                    }
                    
                    if ((byte_index - first_byte) % 40 == 39) {
                        printf("\n");
                    }
                }
                printf("\n\n");
            }
        }
    }
#endif
    wasm_set_vec_size(buf, type_section_start);
    
    // Import section (2)
    DEBUG_wasm_begin_section(&debug, WASMSection_Import, buf);
    push_u8(buf, WASMSection_Import);
    push_leb128_u32(buf, 1); // section size
    push_leb128_u32(buf, 0); // num imports: empty, for now
    // TODO: implement imports
    
    // Function section (3)
    DEBUG_wasm_begin_section(&debug, WASMSection_Function, buf);
    push_u8(buf, WASMSection_Function);
    push_leb128_u32(buf, 2); // section size
    push_leb128_u32(buf, 1); // number of function
    push_leb128_u32(buf, 0); // typeidx (funcidx[0] = typeidx[0])
    
    // Export section (7)
    DEBUG_wasm_begin_section(&debug, WASMSection_Export, buf);
    push_u8(buf, WASMSection_Export);
    push_leb128_u32(buf, 14); // section size
    push_leb128_u32(buf, 1); // number of exports
    
    // - export[0] (for funcidx 1)
    wasm_push_string(buf, string_lit("helloworld")); // name of export
    push_u8(buf, 0x00); // 0x00 = funcidx
    push_leb128_u32(buf, 0); // funcidx
    
    // Code section (10)
    DEBUG_wasm_begin_section(&debug, WASMSection_Code, buf);
    push_u8(buf, WASMSection_Code);
    smm code_section_start = buf->curr_used;
    push_u32(buf, 0); // reserve space for the size
    push_leb128_u32(buf, 1); // number of functions
    
#if 0
    s64 rip = 0;
    for_array(ast_file->units, cu, _2) {
        Type* type = cu->ast->type;
        if (cu->ast->kind == Ast_Decl_Stmt && type->kind == TypeKind_Function) {
            
            smm function_start = buf->curr_used;
            push_u32(buf, 0); // reserve space for size
            push_leb128_u32(buf, 0); // number of locals
            //push_leb128_u32(buf, 0); // TODO: add locals
            
            Intermediate_Code* curr = cu->ic_first;
            while (curr) {
                rip = convert_ic_to_wasm(curr, buf, rip);
                curr = curr->next;
            }
            
            if (type->Function.return_type) {
                // TODO(Alexander): should we only call return on functions with return type?
                push_u8(buf, 0x0F); // return
            }
            
            push_u8(buf, 0x0B); // end marker
            
            wasm_set_vec_size(buf, function_start);
        }
    }
#endif
    wasm_set_vec_size(buf, code_section_start);
    
    DEBUG_wasm_end(&debug, buf);
}