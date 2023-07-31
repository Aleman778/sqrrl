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

const u8 wasm_binary_opcodes[] = {
    /*
i32,  i64,  f32,  f64*/
    0x6A, 0x7C, 0x92, 0xA0, // BC_ADD
    0x6B, 0x7D, 0x93, 0xA1, // BC_SUB
    0x6C, 0x7E, 0x94, 0xA2, // BC_MUL
    0x6D, 0x7F, 0x00, 0x00, // BC_DIV_S
    0x6E, 0x80, 0x95, 0xA3, // BC_DIV_U
    0x6F, 0x81, 0x00, 0x00, // BC_MOD_S
    0x70, 0x82, 0x00, 0x00, // BC_MOD_U
    0x71, 0x83, 0x00, 0x00, // BC_AND
    0x72, 0x84, 0x00, 0x00, // BC_OR
    0x73, 0x85, 0x00, 0x00, // BC_XOR
    0x74, 0x86, 0x00, 0x00, // BC_SHL
    0x75, 0x87, 0x00, 0x00, // BC_SAR
    0x76, 0x88, 0x00, 0x00, // BC_SHR
};

const u8 wasm_comparator_opcodes[] = {
    /*
    i32,  i64,  f32,  f64*/
    0x46, 0x51, 0x5B, 0x61, // BC_EQ
    0x4A, 0x55, 0x00, 0x00, // BC_GT_S
    0x4B, 0x56, 0x5E, 0x64, // BC_GT_U
    0x4E, 0x59, 0x00, 0x00, // BC_GE_S
    0x4F, 0x5A, 0x60, 0x66, // BC_GE_U
    0x49, 0x54, 0x5D, 0x63, // BC_LT_U
    0x48, 0x53, 0x00, 0x00, // BC_LT_S
    0x4D, 0x58, 0x5F, 0x65, // BC_LE_U
    0x4C, 0x57, 0x00, 0x00, // BC_LE_S
    0x47, 0x52, 0x5C, 0x62, // BC_NEQ
};

struct WASM_Assembler {
    u32* stack_offsets;
    u32 stack_usage;
    
    u32 tmp_local_i32;
    u32 tmp_local_i64;
};

struct WASM_Block {
    u32 begin;
    u32 end;
    u32 label_index;
    bool is_loop;
};

struct WASM_Section_Marker {
    WASM_Section section;
    u32 offset;
    u32 size;
};

struct WASM_Debug {
    WASM_Section_Marker markers[WASMSection_Count];
    int marker_count;
};
