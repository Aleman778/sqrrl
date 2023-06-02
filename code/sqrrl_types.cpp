
internal inline Relocation
push_relocation(Type_Info_Packer* packer, void* from_ptr, u32 relative_from, u32 relative_to) {
    Relocation relocation = {};
    relocation.from_ptr = from_ptr;
    relocation.from = relative_from;
    relocation.to = relative_to;
    array_push(packer->relocations, relocation);
    return relocation;
}

internal inline string
export_string(Type_Info_Packer* packer, string_id ident, string* from_ptr, u32 relative_from) {
    Exported_String result = map_get(packer->exported_strings, ident);
    if (!result.is_valid) {
        string str = vars_load_string(ident);
        void* data = arena_push_size(&packer->arena, str.count, 1);
        u32 relative_data = (u32) arena_relative_pointer(&packer->arena, data);
        memcpy(data, str.data, str.count);
        
        result.str.data = (u8*) data;
        result.str.count = str.count;
        result.relative_ptr = relative_data;
        result.is_valid = true;
        
        map_put(packer->exported_strings, ident, result);
    }
    
    //pln("export \"%\", rel: %, from: %", f_string(result.str), f_u32(result.relative_ptr),
    //f_u32(relative_from));
    push_relocation(packer, from_ptr, relative_from, result.relative_ptr);
    *from_ptr = result.str;
    
    return result.str;
}

Exported_Type
export_var_args_info(Type_Info_Packer* packer, int var_arg_start, Ast* actual_arguments) {
    Exported_Type result = {};
    
    int var_arg_count = -var_arg_start;
    for_compound(actual_arguments, _) var_arg_count++;
    
    if (var_arg_count <= 0) {
        return result;
    }
    
    Var_Args* var_args = arena_push_struct(&packer->arena, Var_Args);
    result.var_args = var_args;
    result.relative_ptr = (u32) arena_relative_pointer(&packer->arena, var_args);
    
    Var_Arg* types = arena_push_array_of_structs(&packer->arena, var_arg_count, Var_Arg);
    var_args->types = types;
    var_args->count = var_arg_count;
    u32 curr_type_relative_ptr = (u32) arena_relative_pointer(&packer->arena, types);
    push_relocation(packer, &var_args->types, result.relative_ptr + 8, curr_type_relative_ptr);
    
    Var_Arg* curr_type = types;
    int arg_index = 0;
    for_compound(actual_arguments, argument) {
        if (arg_index >= var_arg_start) {
            
            Exported_Type exported = export_type_info(packer, argument->type);
            push_relocation(packer, curr_type, curr_type_relative_ptr, exported.relative_ptr);
            curr_type->data_size = argument->type->size;
            curr_type->type = exported.type_info;
            
            var_args->count++;
            curr_type++;
            curr_type_relative_ptr += sizeof(Var_Arg);
        }
        
        arg_index++;
    }
    
    return result;
}

Exported_Type
export_type_info(Type_Info_Packer* packer, Type* type) {
    Exported_Type result = map_get(packer->mapper, type);
    
    if (!result.type_info) {
        Type_Info* type_info = arena_push_struct(&packer->arena, Type_Info);
        type_info->kind = type->kind;
        result.type_info = type_info;
        result.relative_ptr = (u32) arena_relative_pointer(&packer->arena, type_info);
        map_put(packer->mapper, type, result);
        
        // Serialize structure into memory
        switch (type->kind) {
            case TypeKind_Void: break;
            
            case TypeKind_Basic: {
                switch (type->Basic.kind) {
                    case Basic_bool: type_info->Basic = TI_Bool; break;
                    case Basic_s8: type_info->Basic = TI_S8; break;
                    case Basic_s16: type_info->Basic = TI_S16; break;
                    case Basic_s32: type_info->Basic = TI_S32; break;
                    case Basic_s64: type_info->Basic = TI_S64; break;
                    case Basic_u8: type_info->Basic = TI_U8; break;
                    case Basic_u16: type_info->Basic = TI_U16; break;
                    case Basic_u32: type_info->Basic = TI_U32; break;
                    case Basic_u64: type_info->Basic = TI_U64; break;
                    case Basic_f32: type_info->Basic = TI_F32; break;
                    case Basic_f64: type_info->Basic = TI_F64; break;
                    case Basic_string: type_info->Basic = TI_String; break;
                    case Basic_cstring: type_info->Basic = TI_CString; break;
                    default: unimplemented; // TODO(Alexander): maybe should be an error?
                }
            } break;
            
            case TypeKind_Union:
            case TypeKind_Struct: {
                smm count = (smm) array_count(type->Struct_Like.types);
                u32 offset = (u32) ((u8*) &type_info->Struct.ident - (u8*) result.type_info);
                export_string(packer, type->ident, 
                              &type_info->Struct.ident, result.relative_ptr + offset);
                
                type_info->Struct.fields =
                    arena_push_array_of_structs(&packer->arena, count, TI_Struct_Field_Info);
                type_info->Struct.count = count;
                u32 relative_fields = (u32) arena_relative_pointer(&packer->arena, 
                                                                   type_info->Struct.fields);
                
                
                offset = (u32) ((u8*) &type_info->Struct.fields - (u8*) result.type_info);
                push_relocation(packer, &type_info->Struct.fields, result.relative_ptr + offset, relative_fields);
                
                TI_Struct_Field_Info* curr_field = type_info->Struct.fields;
                for_array_v(type->Struct_Like.types, field_type, field_index) {
                    string_id field_ident = type->Struct_Like.idents[field_index];
                    
                    Exported_Type exported = export_type_info(packer, field_type);
                    curr_field->type = exported.type_info;
                    offset = (u32) ((u8*) &curr_field->type - (u8*) curr_field);
                    push_relocation(packer, &curr_field->type, relative_fields + offset, exported.relative_ptr);
                    
                    offset = (u32) ((u8*) &curr_field->ident - (u8*) curr_field);
                    export_string(packer, field_ident, &curr_field->ident, relative_fields + offset);
                    
                    curr_field->offset = type->Struct_Like.offsets[field_index];
                    
                    curr_field++;
                    relative_fields += sizeof(TI_Struct_Field_Info);
                }
            } break;
            
            case TypeKind_Array: {
                Exported_Type exported = export_type_info(packer, type->Array.type);
                type_info->Array.elem_type = exported.type_info;
                u32 offset = (u32) ((u8*) &type_info->Array.elem_type - (u8*) result.type_info);
                push_relocation(packer, &type_info->Array.elem_type, result.relative_ptr + offset,
                                exported.relative_ptr);
                
                type_info->Array.elem_size = type->Array.type->size;
                type_info->Array.fixed_count = type->Array.is_inplace ? type->Array.capacity : -1;
            } break;
            
            case TypeKind_Enum: {
                u32 offset = (u32) ((u8*) &type_info->Enum.ident - (u8*) result.type_info);
                export_string(packer, type->ident, 
                              &type_info->Enum.ident, result.relative_ptr + offset);
                
                Exported_Type exported = export_type_info(packer, type->Enum.type);
                type_info->Enum.type = exported.type_info;
                offset = (u32) ((u8*) &type_info->Enum.type - (u8*) result.type_info);
                push_relocation(packer, &type_info->Enum.type, 
                                result.relative_ptr + offset,
                                exported.relative_ptr);
                
                smm count = map_count(type->Enum.values);
                string* names = arena_push_array_of_structs(&packer->arena, count, string);
                u32 relative_names = (u32) arena_relative_pointer(&packer->arena, names);
                offset = (u32) ((u8*) &type_info->Enum.names - (u8*) result.type_info);
                push_relocation(packer, &type_info->Enum.names, result.relative_ptr + offset, relative_names);
                type_info->Enum.names = names;
                type_info->Enum.count = count;
                
                for_map(type->Enum.values, it) {
                    s64 value = value_to_s64(it->value);
                    //pln("% = %", f_var(it->key), f_s64(value));
                    
                    // TODO(Alexander): find a way to store all the enum names
                    if (value >= 0 && value < count) {
                        offset = (u32) value*sizeof(string);
                        export_string(packer, it->key, &names[value], 
                                      relative_names + offset);
                    }
                }
            } break;
            
            case TypeKind_Pointer: {
                type_info->kind = TypeKind_Pointer;
            } break;
            
            default: {
                unimplemented;
            } break;
        }
    }
    
    return result;
}

void
print_type(Type* type) {
    String_Builder sb = {};
    string_builder_alloc(&sb, 20);
    string_builder_push(&sb, type);
    string result = string_builder_to_string_nocopy(&sb);
    print("%", f_string(result));
    string_builder_free(&sb);
}
