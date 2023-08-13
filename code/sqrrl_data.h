
 enum Section {
    Read_Data_Section,
    Data_Section,
    
    Section_Count,
};

struct Relocation {
    void* from_ptr;
    u32 from;
    u32 to;
    
    Section from_section;
    Section to_section;
};

struct Exported_String {
    string str;
    u32 relative_ptr;
    b32 is_valid;
};

// Forward declare
struct Type_Info;
struct Var_Args;

struct Exported_Data {
    union {
        void* data;
        string str;
    };
    u32 relative_ptr;
    Section section;
};

inline Exported_Data
add_offset(Exported_Data result, int offset) {
    result.data = (u8*) result.data + offset;
    result.relative_ptr += offset;
    return result;
}

struct Data_Packer {
    
    union {
        struct {
            Memory_Arena rdata_arena;
            Memory_Arena data_arena;
        };
        Memory_Arena section_arenas[2];
    };
    
    array(Relocation)* relocations;
    
    map(string_id, Exported_Data)* exported_strings;
    map(Type*, Exported_Data)* exported_types;
};

Exported_Data export_string(Data_Packer* packer, string_id ident);

inline Relocation push_relocation(Data_Packer* packer, Exported_Data from, Exported_Data to);

#define export_struct(packer, type, section, ...) \
export_size(packer, section, sizeof(type), alignof(type), __VA_ARGS__)

#define export_array(packer, count, type, section, ...) \
export_size(packer, section, (umm) count*sizeof(type), (umm) alignof(type), __VA_ARGS__)

inline Exported_Data
export_size(Data_Packer* packer, Section section, umm size, umm align=DEFAULT_ALIGNMENT) {
    Exported_Data result = {};
    result.data = arena_push_size(&packer->section_arenas[section], size, align);
    result.relative_ptr = (u32) arena_relative_pointer(&packer->section_arenas[section], result.data);
    result.section = section;
    return result;
}
