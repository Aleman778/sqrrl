

enum WASM_Section {
    WASMSection_custom,
    WASMSection_type,
    WASMSection_import,
    WASMSection_function,
    WASMSection_table,
    WASMSection_memory,
    WASMSection_global,
    WASMSection_export,
    WASMSection_start,
    WASMSection_element,
    WASMSection_code,
    WASMSection_data,
    WASMSection_data_count,
};

void
convert_type_to_wasm(Type* type, Buffer* buffer) {
    
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
                case Basic_uint:
                case Basic_string:
                case Basic_cstring: push_u8(buffer, 0x7F); break;
                
                case Basic_s64: 
                case Basic_u64: 
                case Basic_smm: 
                case Basic_umm: push_u8(buffer, 0x7E); break;
                
                case Basic_f32: push_u8(buffer, 0x7D); break;
                case Basic_f64: push_u8(buffer, 0x7C); break;
                
                default: unimplemented; break;
            }
        } break;
        
        case TypeKind_Function: {
            push_u8(buffer, 0x60); // functype tag
            
            // arguments
            push_leb128_u32(buffer, (u32) array_count(type->Function.arg_types));
            for_array_v(type->Function.arg_types, arg_type, arg_index) {
                convert_type_to_wasm(arg_type, buffer);
            }
            
            // return values
            // TODO: multiple return types
            bool has_return_type = is_valid_type(type->Function.return_type);
            push_leb128_u32(buffer, has_return_type);
            if (has_return_type) {
                convert_type_to_wasm(type->Function.return_type, buffer);
            }
        } break;
    }
    
}


void
convert_to_wasm_module(Intermediate_Code* ic, s64 stk_usage, Buffer* buffer) {
    
    // Define module
    push_u32(buffer, 0x6D736100); // Signature (.asm)
    push_u32(buffer, 0x1); // Version
    
    
    // Type section (1)
    push_u8(buffer, WASMSection_type);
    //push_size();
    
}