
Exported_Data
export_var_args_info(Data_Packer* packer, int var_arg_start, Ast* actual_arguments) {
    int var_arg_count = -var_arg_start;
    for_compound(actual_arguments, _) var_arg_count++;
    
    Exported_Data result = {};
    
    result = export_struct(packer, Var_Args, Data_Section);
    Var_Args* var_args = (Var_Args*) result.data;
    
    Exported_Data args_ptr = add_offset(result, 8);
    Exported_Data actual_args = {};
    if (var_arg_count > 0) {
        actual_args = export_array(packer, var_arg_count, Var_Arg, Data_Section);
        push_relocation(packer, args_ptr, actual_args);
    }
    var_args->types = (Var_Arg*) actual_args.data;
    var_args->count = var_arg_count;
    
    //push_relocation(packer, &var_args->types, result.relative_ptr + 8, curr_type_relative_ptr);
    
    Var_Arg* curr_type = var_args->types;
    int arg_index = 0;
    for_compound(actual_arguments, argument) {
        if (arg_index >= var_arg_start) {
            Exported_Data exported = export_type_info(packer, argument->type);
            push_relocation(packer, actual_args, exported);
            curr_type->type = (Type_Info*) exported.data;
            curr_type->data_size = argument->type->size;
            
            curr_type++;
            actual_args = add_offset(actual_args, sizeof(Var_Arg));
        }
        
        arg_index++;
    }
    
    return result;
}

Exported_Data
export_type_info(Data_Packer* packer, Type* type) {
    Exported_Data result = map_get(packer->exported_types, type);
    
    if (!result.data) {
        result = export_struct(packer, Type_Info, Data_Section);
        Type_Info* type_info = (Type_Info*) result.data;
        type_info->kind = type->kind;
        type_info->size = type->size;
        type_info->align = type->align;
        assert(type_info->size && "bad size");
        assert(type_info->align && "bad align");
        map_put(packer->exported_types, type, result);
        
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
                
                Exported_Data name = export_string(packer, type->ident);
                type_info->Struct.ident = name.str;
                u32 offset = (u32) ((u8*) &type_info->Struct.ident - (u8*) type_info);
                push_relocation(packer, add_offset(result, offset), name);
                //pln("export \"%\", rel: %, from: %", f_string(name.str), f_u32(result.relative_ptr),
                //f_u32(name.relative_ptr));
                
                Exported_Data fields = export_array(packer, count, TI_Struct_Field_Info, Data_Section);
                type_info->Struct.fields = (TI_Struct_Field_Info*) fields.data;
                type_info->Struct.count = count;
                offset = (u32) ((u8*) &type_info->Struct.fields - (u8*) type_info);
                push_relocation(packer, add_offset(result, offset), fields);
                
                Exported_Data curr_field = fields;
                for_array_v(type->Struct_Like.types, field_type, field_index) {
                    TI_Struct_Field_Info* field_info = (TI_Struct_Field_Info*) curr_field.data;
                    
                    // Type info
                    Exported_Data exported_type = export_type_info(packer, field_type);
                    field_info->type = (Type_Info*) exported_type.data;
                    field_info->offset = type->Struct_Like.offsets[field_index];
                    offset = (u32) ((u8*) &field_info->type - (u8*) field_info);
                    push_relocation(packer, add_offset(curr_field, offset), exported_type);
                    
                    // Field name
                    string_id field_ident = type->Struct_Like.idents[field_index];
                    Exported_Data field_name = export_string(packer, field_ident);
                    field_info->ident = field_name.str;
                    offset = (u32) ((u8*) &field_info->ident - (u8*) field_info);
                    push_relocation(packer, add_offset(curr_field, offset), field_name);
                    
                    curr_field = add_offset(curr_field, sizeof(TI_Struct_Field_Info));
                }
            } break;
            
            case TypeKind_Array: {
                Exported_Data exported_type = export_type_info(packer, type->Array.type);
                type_info->Array.elem_type = (Type_Info*) exported_type.data;
                u32 offset = (u32) ((u8*) &type_info->Array.elem_type - (u8*) type_info);
                push_relocation(packer, add_offset(result, offset), exported_type);
                
                type_info->Array.elem_size = type->Array.type->size;
                type_info->Array.fixed_count = (type->Array.kind == ArrayKind_Fixed_Inplace ?
                                                type->Array.capacity : -1);
            } break;
            
            case TypeKind_Enum: {
                Exported_Data name = export_string(packer, type->ident);
                type_info->Enum.ident = name.str;
                u32 offset = (u32) ((u8*) &type_info->Enum.ident - (u8*) type_info);
                push_relocation(packer, add_offset(result, offset), name);
                
                Exported_Data exported_type = export_type_info(packer, type->Enum.type);
                type_info->Enum.type = (Type_Info*) exported_type.data;
                offset = (u32) ((u8*) &type_info->Enum.type - (u8*) type_info);
                push_relocation(packer, add_offset(result, offset), exported_type);
                
                smm count = map_count(type->Enum.values);
                Exported_Data enum_names = export_array(packer, count, string, Data_Section);
                type_info->Enum.names = (string*) enum_names.data;
                type_info->Enum.count = count;
                offset = (u32) ((u8*) &type_info->Enum.names - (u8*) type_info);
                push_relocation(packer, add_offset(result, offset), enum_names);
                
                for_map(type->Enum.values, it) {
                    s64 value = value_to_s64(it->value);
                    //pln("% = %", f_var(it->key), f_s64(value));
                    
                    // TODO(Alexander): find a way to store all the enum names
                    if (value >= 0 && value < count) {
                        offset = (u32) value*sizeof(string);
                        Exported_Data enum_name = export_string(packer, it->key);
                        type_info->Enum.names[value] = enum_name.str; 
                        push_relocation(packer, add_offset(enum_names, offset), enum_name);
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
