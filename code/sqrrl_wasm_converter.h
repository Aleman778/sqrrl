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

struct WASM_Slot {
    Bytecode_Type type;
};

struct WASM_Assembler {
    Memory_Arena arena;
    
    s64 stack_size;
    s64 stack_offset_for_locals;
    
    WASM_Slot* slots; // bytecode registers == wasm slots atm.
    
    u32 rdata_offset;
    u32 data_offset;
    
    u32 tmp_local_i32;
    u32 tmp_local_i64;
    u32 tmp_local_f32;
    u32 tmp_local_f64;
};

Bytecode_Type
wasm_register_type(WASM_Assembler* wasm, int register_index) {
    return wasm->slots[register_index].type;
}

inline u32
wasm_tmp_local(WASM_Assembler* wasm, Bytecode_Type type) {
    if (type.kind & BC_TYPE_FLOAT) {
        return (type.size == 8) ? wasm->tmp_local_f64 : wasm->tmp_local_f32;
    } else {
        return wasm->tmp_local_i64;
    }
}

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

#define WASM_MAGIC_NUMBER 0x6D736100
#define WASM_VERSION 0x01

#define DEF_WASM_INSTRUCTIONS \
WASM_VAR("local.get", 0x20) \
WASM_VAR("local.set", 0x21) \
WASM_VAR("local.tee", 0x22) \
WASM_VAR("global.get", 0x23) \
WASM_VAR("global.set", 0x24) \
\
WASM_MEM("i32.load", 0x28) \
WASM_MEM("i64.load", 0x29) \
WASM_MEM("f32.load", 0x2A) \
WASM_MEM("f64.load", 0x2B) \
WASM_MEM("i32.load8_s", 0x2C) \
WASM_MEM("i32.load8_u", 0x2D) \
WASM_MEM("i32.load16_s", 0x2E) \
WASM_MEM("i32.load16_u", 0x2F) \
WASM_MEM("i64.load8_s", 0x30) \
WASM_MEM("i64.load8_u", 0x31) \
WASM_MEM("i64.load16_s", 0x32) \
WASM_MEM("i64.load16_u", 0x33) \
WASM_MEM("i64.load32_s", 0x34) \
WASM_MEM("i64.load32_u", 0x35) \
WASM_MEM("i32.store", 0x36) \
WASM_MEM("i64.store", 0x37) \
WASM_MEM("f32.store", 0x38) \
WASM_MEM("f64.store", 0x39) \
WASM_MEM("i32.store8", 0x3A) \
WASM_MEM("i32.store16", 0x3B) \
WASM_MEM("i64.store8", 0x3C) \
WASM_MEM("i64.store16", 0x3D) \
WASM_MEM("i64.store32", 0x3E) \
\
WASM_BIN("i64.add", 0x7C) \
WASM_BIN("i64.sub", 0x7D) \
WASM_BIN("i64.mul", 0x7E) \
WASM_BIN("i64.div_s", 0x7F) \
WASM_BIN("i64.div_u", 0x80) \
WASM_BIN("i64.rem_s", 0x81) \
WASM_BIN("i64.rem_u", 0x82) \
WASM_BIN("i64.and", 0x83) \
WASM_BIN("i64.or", 0x84) \
WASM_BIN("i64.xor", 0x85) \
WASM_BIN("i64.shl", 0x86) \
WASM_BIN("i64.shr_s", 0x87) \
WASM_BIN("i64.shr_u", 0x88) \
WASM_BIN("i64.rotl", 0x89) \
WASM_BIN("i64.rotr", 0x8A)
