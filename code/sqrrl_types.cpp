
Exported_Type
export_var_args_info(Type_Info_Packer* packer, int var_arg_start, Ast* actual_arguments) {
    Exported_Type result = {};
    
    Type_Info* type_info = arena_push_struct(&packer->arena, Type_Info);
    type_info->kind = TypeKind_Var_Args;
    result.type_info = type_info;
    result.relative_ptr = (u32) arena_relative_pointer(&packer->arena, type_info);
    
    
    int var_arg_count = -var_arg_start;
    for_compound(actual_arguments, _) var_arg_count++;
    
    Type_Info** var_args = arena_push_array_of_structs(&packer->arena, var_arg_count, Type_Info*);
    u32 curr_var_arg_relative_ptr = (u32) arena_relative_pointer(&packer->arena, var_args);
    type_info->Var_Args.args = var_args;
    
    {
        Relocation relocation = {};
        relocation.from_ptr = &type_info->Var_Args.args;
        relocation.from = result.relative_ptr;
        relocation.to = curr_var_arg_relative_ptr;
        array_push(packer->relocations, relocation);
    }
    
    Type_Info* curr_var_arg = *var_args;
    int arg_index = 0;
    for_compound(actual_arguments, argument) {
        if (arg_index >= var_arg_start) {
            
            Exported_Type exported = export_type_info(packer, argument->type);
            
            Relocation relocation = {};
            relocation.from_ptr = &curr_var_arg;
            relocation.from = result.relative_ptr;
            relocation.to = exported.relative_ptr;
            array_push(packer->relocations, relocation);
            
            curr_var_arg++;
            curr_var_arg_relative_ptr += sizeof(Type_Info*);
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
                type_info->Struct.ident = vars_load_string(type->ident);
                type_info->Struct.fields =
                    arena_push_array_of_structs(&packer->arena, count, TI_Struct_Field_Info);
                type_info->Struct.count = count;
                
                for_array_v(type->Struct_Like.types, field_type, field_index) {
                    Exported_Type exported = export_type_info(packer, field_type);
                    TI_Struct_Field_Info* field = type_info->Struct.fields + field_index;
                    field->type = exported.type_info;
                    field->ident = vars_load_string(type->Struct_Like.idents[field_index]);
                    field->offset = type->Struct_Like.offsets[field_index];
                    
                    Relocation relocation = {};
                    relocation.from_ptr = &field->type;
                    relocation.from = result.relative_ptr;
                    relocation.to = exported.relative_ptr;
                    array_push(packer->relocations, relocation);
                }
            } break;
            
            case TypeKind_Array: {
                Exported_Type exported = export_type_info(packer, type->Array.type);
                type_info->Array.elem_type = exported.type_info;
                type_info->Array.elem_size = type->Array.type->size;
                type_info->Array.fixed_count = type->Array.is_inplace ? type->Array.capacity : 0;
                
                Relocation relocation = {};
                relocation.from_ptr = &type_info->Array.elem_type;
                relocation.from = result.relative_ptr;
                relocation.to = exported.relative_ptr;
                array_push(packer->relocations, relocation);
            } break;
            
            case TypeKind_Enum: {
                Exported_Type exported = export_type_info(packer, type->Enum.type);
                type_info->Enum.ident = vars_load_string(type->ident);
                type_info->Enum.type = exported.type_info;
                
                Relocation relocation = {};
                relocation.from_ptr = &type_info->Enum.type;
                relocation.from = result.relative_ptr;
                relocation.to = exported.relative_ptr;
                array_push(packer->relocations, relocation);
                
                smm count = map_count(type->Enum.values);
                string* names = arena_push_array_of_structs(&packer->arena, count, string);
                type_info->Enum.names = names;
                type_info->Enum.count = count;
                for_map(type->Enum.values, it) {
                    s64 value = value_to_s64(it->value);
                    //pln("% = %", f_var(it->key), f_s64(value));
                    if (value >= 0 && value < count) {
                        names[value] = vars_load_string(it->key);
                    } else {
                        names[value] = string_lit("?");
                    }
                }
            } break;
            
            case TypeKind_Pointer: {
                type_info->kind = TypeKind_Basic;
                type_info->Basic = TI_S64; // TODO(Alexander): temporary
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

