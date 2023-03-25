#define DEF_PE_MACHINE \
PE_MACHINE(MACHINE_UNKNOWN, \
0x0, \
"The content of this field is assumed to be applicable to any machine type") \
PE_MACHINE(MACHINE_ALPHA, \
0x184, \
"Alpha AXP, 32-bit address space") \
PE_MACHINE(MACHINE_ALPHA64,  \
0x284, \
"Alpha 64, 64-bit address space") \
PE_MACHINE(MACHINE_AM33, \
0x1d3, \
"Matsushita AM33") \
PE_MACHINE(MACHINE_AMD64, \
0x8664, \
"x64") \
PE_MACHINE(MACHINE_ARM, \
0x1c0, \
"ARM little endian") \
PE_MACHINE(MACHINE_ARM64, \
0xaa64, \
"ARM64 little endian") \
PE_MACHINE(MACHINE_ARMNT, \
0x1c4, \
"ARM Thumb-2 little endian") \
PE_MACHINE(MACHINE_AXP64, \
0x284, \
"AXP 64 (Same as Alpha 64)") \
PE_MACHINE(MACHINE_EBC, \
0xebc, \
"EFI byte code") \
PE_MACHINE(MACHINE_I386, \
0x14c, \
"Intel 386 or later processors and compatible processors") \
PE_MACHINE(MACHINE_IA64, \
0x200, \
"Intel Itanium processor family") \
PE_MACHINE(MACHINE_LOONGARCH32, \
0x6232, \
"LoongArch 32-bit processor family") \
PE_MACHINE(MACHINE_LOONGARCH64, \
0x6264, \
"LoongArch 64-bit processor family") \
PE_MACHINE(MACHINE_M32R, \
0x9041, \
"Mitsubishi M32R little endian") \
PE_MACHINE(MACHINE_MIPS16, \
0x266, \
"MIPS16") \
PE_MACHINE(MACHINE_MIPSFPU, \
0x366, \
"MIPS with FPU") \
PE_MACHINE(MACHINE_MIPSFPU16, \
0x466, \
"MIPS16 with FPU") \
PE_MACHINE(MACHINE_POWERPC, \
0x1f0, \
"Power PC little endian") \
PE_MACHINE(MACHINE_POWERPCFP, \
0x1f1, \
"Power PC with floating point support") \
PE_MACHINE(MACHINE_R4000, \
0x166, \
"MIPS little endian") \
PE_MACHINE(MACHINE_RISCV32, \
0x5032, \
"RISC-V 32-bit address space") \
PE_MACHINE(MACHINE_RISCV64, \
0x5064, \
"RISC-V 64-bit address space") \
PE_MACHINE(MACHINE_RISCV128, \
0x5128, \
"RISC-V 128-bit address space") \
PE_MACHINE(MACHINE_SH3, \
0x1a2, \
"Hitachi SH3") \
PE_MACHINE(MACHINE_SH3DSP, \
0x1a3, \
"Hitachi SH3 DSP") \
PE_MACHINE(MACHINE_SH4, \
0x1a6, \
"Hitachi SH4") \
PE_MACHINE(MACHINE_SH5, \
0x1a8, \
"Hitachi SH5") \
PE_MACHINE(MACHINE_THUMB, \
0x1c2, \
"Thumb") \
PE_MACHINE(MACHINE_WCEMIPSV2, \
0x169, \
"MIPS little-endian WCE v2")

enum {
#define PE_MACHINE(name, value, ...) COFF_##name = value,
    DEF_PE_MACHINE
#undef PE_MACHINE
};
typedef u16 PE_Machine_Type;

const int pe_machine_values[] {
#define PE_MACHINE(name, value, ...) value,
    DEF_PE_MACHINE
#undef PE_MACHINE
};
const cstring pe_machine_descriptions[] {
#define PE_MACHINE(name, value, description) description,
    DEF_PE_MACHINE
#undef PE_MACHINE
};

#define DEF_PE_CHAR \
PE_CHAR(RELOCS_STRIPPED         , 0x0001, "Image only uses relative addresses") \
PE_CHAR(EXECUTABLE_IMAGE        , 0x0002, "Image is executable") \
PE_CHAR(LINE_NUMS_STRIPPED      , 0x0004, "Line numbers are stripped") \
PE_CHAR(LOCAL_SYMS_STRIPPED     , 0x0008, "Local symbols are stripped") \
PE_CHAR(AGGRESSIVE_WS_TRIM      , 0x0010, "Aggressively trim working set") \
PE_CHAR(LARGE_ADDRESS_AWARE     , 0x0020, "Supports > 2GB addresses") \
PE_CHAR(RESERVED_1              , 0x0040, "Reserved") \
PE_CHAR(BYTES_REVERSED_LO       , 0x0080, "Bytes of machine word are reversed (LSB first)") \
PE_CHAR(32BIT_MACHINE           , 0x0100, "32-bit machine") \
PE_CHAR(DEBUG_STRIPPED          , 0x0200, "Debugging information is stripped") \
PE_CHAR(REMOVABLE_RUN_FROM_SWAP , 0x0400, "Image can be run from removable media") \
PE_CHAR(NET_RUN_FROM_SWAP       , 0x0800, "Image can run from the network") \
PE_CHAR(SYSTEM                  , 0x1000, "System file") \
PE_CHAR(DLL                     , 0x2000, "DLL file") \
PE_CHAR(UP_SYSTEM_ONLY          , 0x4000, "File should only be run on a UP machine") \
PE_CHAR(BYTES_REVERSED_HI       , 0x8000, "Bytes of machine word are reversed (MSB first)")

enum {
#define PE_CHAR(name, value, ...) COFF_##name = value,
    DEF_PE_CHAR
#undef PE_CHAR
};
typedef u16 PE_Characteristics;

const u16 pe_characteristics_values[] {
#define PE_CHAR(name, value, ...) value,
    DEF_PE_CHAR
#undef PE_CHAR
};
const cstring pe_characteristics_descriptions[] {
#define PE_CHAR(name, value, description) description,
    DEF_PE_CHAR
#undef PE_CHAR
};

#define DEF_WINDOWS_SUBSYSTEMS \
SUBSYSTEM(UNKNOWN, 0, "Unknown subsystem") \
SUBSYSTEM(NATIVE, 1, "Native Windows subsystem") \
SUBSYSTEM(WINDOWS_GUI, 2, "Windows GUI subsystem") \
SUBSYSTEM(WINDOWS_CUI, 3, "Windows character subsystem") \
SUBSYSTEM(OS2_CUI, 5, "OS/2 character subsystem") \
SUBSYSTEM(POSIX_CUI, 7, "POSIX character subsystem") \
SUBSYSTEM(NATIVE_WINDOWS, 8, "Native Windows 9x driver subsystem") \
SUBSYSTEM(WINDOWS_CE_GUI, 9, "Windows CE subsystem") \
SUBSYSTEM(EFI_APPLICATION, 10, "EFI application subsystem") \
SUBSYSTEM(EFI_BOOT_SERVICE_DRIVER, 11, "EFI boot service driver subsystem") \
SUBSYSTEM(EFI_RUNTIME_DRIVER, 12, "EFI runtime driver subsystem") \
SUBSYSTEM(EFI_ROM, 13, "EFI ROM image subsystem") \
SUBSYSTEM(XBOX, 14, "Xbox subsystem") \
SUBSYSTEM(WINDOWS_BOOT_APPLICATION, 16, "Windows boot application subsystem")

enum {
#define SUBSYSTEM(name, value, desc) COFF_SUBSYSTEM_##name = value,
    DEF_WINDOWS_SUBSYSTEMS
#undef SUBSYSTEM
};
typedef u16 COFF_Windows_Subsystem;

const int coff_subsystem_values[] = {
#define SUBSYSTEM(name, value, description) value,
    DEF_WINDOWS_SUBSYSTEMS
#undef SUBSYSTEM
};

const char* coff_subsystem_descriptions[] = {
#define SUBSYSTEM(name, value, description) description,
    DEF_WINDOWS_SUBSYSTEMS
#undef SUBSYSTEM
};

#define DEF_DLL_CHARACTERISTICS_FLAGS \
FLAG(IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA, 0x0020, "Image can handle a high entropy 64-bit virtual address space.") \
FLAG(IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE, 0x0040, "DLL can be relocated at load time.") \
FLAG(IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY, 0x0080, "Code Integrity checks are enforced.") \
FLAG(IMAGE_DLLCHARACTERISTICS_NX_COMPAT, 0x0100, "Image is NX compatible.") \
FLAG(IMAGE_DLLCHARACTERISTICS_NO_ISOLATION, 0x0200, "Isolation aware, but do not isolate the image.") \
FLAG(IMAGE_DLLCHARACTERISTICS_NO_SEH, 0x0400, "Does not use structured exception (SE) handling.") \
FLAG(IMAGE_DLLCHARACTERISTICS_NO_BIND, 0x0800, "Do not bind the image.") \
FLAG(IMAGE_DLLCHARACTERISTICS_APPCONTAINER, 0x1000, "Image should execute in an AppContainer.") \
FLAG(IMAGE_DLLCHARACTERISTICS_WDM_DRIVER, 0x2000, "Driver uses WDM model.") \
FLAG(IMAGE_DLLCHARACTERISTICS_GUARD_CF, 0x4000, "Image supports Control Flow Guard.") \
FLAG(IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE, 0x8000, "Terminal Server aware.")

enum {
#define FLAG(name, value, description) COFF_##name = value,
    DEF_DLL_CHARACTERISTICS_FLAGS
#undef FLAG
};
typedef u16 COFF_DLL_Characteristics_Flags;

const u16 coff_dll_characteristics_values[] = {
#define FLAG(name, value, description) value,
    DEF_DLL_CHARACTERISTICS_FLAGS
#undef FLAG
};

const char* coff_dll_characteristics_descriptions[] = {
#define FLAG(name, value, description) description,
    DEF_DLL_CHARACTERISTICS_FLAGS
#undef FLAG
};

#define DEF_SECTION_FLAGS \
SECTION_FLAG(SCN_RESERVED, 0x00000000, "Reserved for future use.") \
SECTION_FLAG(SCN_RESERVED_1, 0x00000001, "Reserved for future use.") \
SECTION_FLAG(SCN_RESERVED_2, 0x00000002, "Reserved for future use.") \
SECTION_FLAG(SCN_RESERVED_3, 0x00000004, "Reserved for future use.") \
SECTION_FLAG(SCN_TYPE_NO_PAD, 0x00000008, "The section should not be padded to the next boundary. This flag is obsolete and is replaced by SCN_ALIGN_1BYTES. This is valid only for object files.") \
SECTION_FLAG(SCN_RESERVED_4, 0x00000010, "Reserved for future use.") \
SECTION_FLAG(SCN_CNT_CODE, 0x00000020, "The section contains executable code.") \
SECTION_FLAG(SCN_CNT_INITIALIZED_DATA, 0x00000040, "The section contains initialized data.") \
SECTION_FLAG(SCN_CNT_UNINITIALIZED_DATA, 0x00000080, "The section contains uninitialized data.") \
SECTION_FLAG(SCN_LNK_OTHER, 0x00000100, "Reserved for future use.") \
SECTION_FLAG(SCN_LNK_INFO, 0x00000200, "The section contains comments or other information. The .drectve section has this type. This is valid for object files only.") \
SECTION_FLAG(SCN_RESERVED_5, 0x00000400, "Reserved for future use.") \
SECTION_FLAG(SCN_LNK_REMOVE, 0x00000800, "The section will not become part of the image. This is valid only for object files.") \
SECTION_FLAG(SCN_LNK_COMDAT, 0x00001000, "The section contains COMDAT data. For more information, see COMDAT Sections (Object Only). This is valid only for object files.") \
SECTION_FLAG(SCN_GPREL, 0x00008000, "The section contains data referenced through the global pointer (GP).") \
SECTION_FLAG(SCN_MEM_PURGEABLE, 0x00020000, "Reserved for future use.") \
SECTION_FLAG(SCN_MEM_16BIT, 0x00020000, "Reserved for future use.") \
SECTION_FLAG(SCN_MEM_LOCKED, 0x00040000, "Reserved for future use.") \
SECTION_FLAG(SCN_MEM_PRELOAD, 0x00080000, "Reserved for future use.") \
SECTION_FLAG(SCN_ALIGN_1BYTES, 0x00100000, "Align data on a 1-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_2BYTES, 0x00200000, "Align data on a 2-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_4BYTES, 0x00300000, "Align data on a 4-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_8BYTES, 0x00400000, "Align data on an 8-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_16BYTES, 0x00500000, "Align data on a 16-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_32BYTES, 0x00600000, "Align data on a 32-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_64BYTES, 0x00700000, "Align data on a 64-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_128BYTES, 0x00800000, "Align data on a 128-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_256BYTES, 0x00900000, "Align data on a 256-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_512BYTES, 0x00A00000, "Align data on a 512-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_1024BYTES, 0x00B00000, "Align data on a 1024-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_2048BYTES, 0x00C00000, "Align data on a 2048-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_4096BYTES, 0x00D00000, "Align data on a 4096-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_ALIGN_8192BYTES, 0x00E00000, "Align data on an 8192-byte boundary. Valid only for object files.") \
SECTION_FLAG(SCN_LNK_NRELOC_OVFL, 0x01000000, "The section contains extended relocations.") \
SECTION_FLAG(SCN_MEM_DISCARDABLE, 0x02000000, "The section can be discarded as needed.") \
SECTION_FLAG(SCN_MEM_NOT_CACHED, 0x04000000, "The section cannot be cached.") \
SECTION_FLAG(SCN_MEM_NOT_PAGED, 0x08000000, "The section is not pageable.") \
SECTION_FLAG(SCN_MEM_SHARED, 0x10000000, "The section can be shared in memory.") \
SECTION_FLAG(SCN_MEM_EXECUTE, 0x20000000, "The section can be executed as code.") \
SECTION_FLAG(SCN_MEM_READ, 0x40000000, "The section can be read.") \
SECTION_FLAG(SCN_MEM_WRITE, 0x80000000, "The section can be written to.")

enum {
#define SECTION_FLAG(name, value, desc) COFF_##name = value,
    DEF_SECTION_FLAGS
#undef SECTION_FLAG
};
typedef u32 COFF_Section_Flags;

const u8 pe_dos_header_stub[] = {
    0x4D, 0x5A, 0x90, 0x00, 0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 
    0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0xB8, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
    0xB0, 0x00, 0x00, 0x00, 0x0E, 0x1F, 0xBA, 0x0E, 0x00, 0xB4, 
    0x09, 0xCD, 0x21, 0xB8, 0x01, 0x4C, 0xCD, 0x21, 0x54, 0x68, 
    0x69, 0x73, 0x20, 0x70, 0x72, 0x6F, 0x67, 0x72, 0x61, 0x6D, 
    0x20, 0x63, 0x61, 0x6E, 0x6E, 0x6F, 0x74, 0x20, 0x62, 0x65, 
    0x20, 0x72, 0x75, 0x6E, 0x20, 0x69, 0x6E, 0x20, 0x44, 0x4F, 
    0x53, 0x20, 0x6D, 0x6F, 0x64, 0x65, 0x2E, 0x0D, 0x0D, 0x0A, 
    0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x45, 0x6A, 
    0x31, 0xDF, 0x01, 0x0B, 0x5F, 0x8C, 0x01, 0x0B, 0x5F, 0x8C, 
    0x01, 0x0B, 0x5F, 0x8C, 0xB9, 0x7E, 0x5A, 0x8D, 0x00, 0x0B, 
    0x5F, 0x8C, 0xB9, 0x7E, 0x5D, 0x8D, 0x00, 0x0B, 0x5F, 0x8C, 
    0x52, 0x69, 0x63, 0x68, 0x01, 0x0B, 0x5F, 0x8C, 0x00, 0x00, 
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00
};

#pragma pack(push, 1)
struct COFF_Header {
    u8 signature[4];
    PE_Machine_Type machine;
    u16 number_of_sections;
    u32 time_date_stamp;
    u32 pointer_to_symbol_table; // deprecated
    u32 number_of_symbols; // deprecated
    u16 size_of_optional_header;
    PE_Characteristics characteristics;
};
#pragma pack(pop)

struct COFF_Image_Data_Dir {
    u32 virtual_address;
    u32 size;
};

#pragma pack(push, 1)
struct COFF_PE32_Plus_Header {
    u16 magic;
    u8 major_linker_version;
    u8 minor_linker_version;
    u32 size_of_code;
    u32 size_of_initialized_data;
    u32 size_of_uninitialized_data;
    u32 address_of_entry_point;
    u32 base_of_code;
    u64 image_base;
    u32 section_alignment;
    u32 file_alignment;
    u16 major_operating_system_version;
    u16 minor_operating_system_version;
    u16 major_image_version;
    u16 minor_image_version;
    u16 major_subsystem_version;
    u16 minor_subsystem_version;
    u32 win32_version_value;
    u32 size_of_image;
    u32 size_of_headers;
    u32 checksum;
    COFF_Windows_Subsystem subsystem;
    COFF_DLL_Characteristics_Flags dll_characteristics;
    u64 size_of_stack_reserve;
    u64 size_of_stack_commit;
    u64 size_of_heap_reserve;
    u64 size_of_heap_commit;
    u32 loader_flags;
    u32 number_of_rva_and_sizes;
    union {
        COFF_Image_Data_Dir data_directories[16];
        struct {
            COFF_Image_Data_Dir export_table;
            COFF_Image_Data_Dir import_table;
            COFF_Image_Data_Dir resource_table;
            COFF_Image_Data_Dir exception_table;
            COFF_Image_Data_Dir certificate_table;
            COFF_Image_Data_Dir base_relocation_table;
            COFF_Image_Data_Dir debug;
            COFF_Image_Data_Dir architecture;
            COFF_Image_Data_Dir global_ptr;
            COFF_Image_Data_Dir tls_table;
            COFF_Image_Data_Dir load_config_table;
            COFF_Image_Data_Dir bound_import_table;
            COFF_Image_Data_Dir iat;
            COFF_Image_Data_Dir delay_import_descriptor;
            COFF_Image_Data_Dir clr_runtime_header;
            COFF_Image_Data_Dir reserved;
        };
    };
};
#pragma pack(pop)

// TODO(Alexander): add COFF_Section_Flags enum

struct COFF_Section_Header {
    u8 name[8];
    u32 virtual_size;
    u32 virtual_address;
    u32 size_of_raw_data;
    u32 pointer_to_raw_data;
    u32 pointer_to_relocations;
    u32 pointer_to_line_numbers; // deprecated
    u16 number_of_relocations;
    u16 number_of_line_numbers; // deprecated
    COFF_Section_Flags characteristics;
};

inline u32
read_u32_bytes(string s) {
    assert(s.count >= 4);
    // TODO(Alexander): this is only little endian
    return (s.data[0] |
            s.data[1] << 8 |
            s.data[2] << 16 |
            s.data[3] << 24);
}
