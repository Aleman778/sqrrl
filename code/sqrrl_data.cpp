
inline Relocation 
push_relocation(Data_Packer* packer, Exported_Data from, Exported_Data to) {
    Relocation relocation = {};
    relocation.from_ptr = from.data;
    relocation.from = from.relative_ptr;
    relocation.from_section = from.section;
    relocation.to = to.relative_ptr;
    relocation.to_section = to.section;
    array_push(packer->relocations, relocation);
    return relocation;
}

Exported_Data
export_string(Data_Packer* packer, string_id ident) {
    Exported_Data result = map_get(packer->exported_strings, ident);
    if (!result.str.data) {
        string str = vars_load_string(ident);
        void* data = arena_push_size(&packer->rdata_arena, str.count, 1);
        u32 relative_data = (u32) arena_relative_pointer(&packer->rdata_arena, data);
        memcpy(data, str.data, str.count);
        
        result.str.data = (u8*) data;
        result.str.count = str.count;
        result.relative_ptr = relative_data;
        result.section = Read_Data_Section;
        map_put(packer->exported_strings, ident, result);
    }
    return result;
}
