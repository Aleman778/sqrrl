
void
pe_dump_executable(string contents) {
    string signature_location_str = string_view(contents.data + 0x3c, contents.data + 0x40);
    
    u32 coff_header_location = read_u32_bytes(signature_location_str);
    u8* curr = contents.data + coff_header_location;
    
    COFF_Header* coff = (COFF_Header*) curr;
    curr += sizeof(COFF_Header);
    
    // Check PE signature
    if (!(coff->signature[0] == 'P'  && coff->signature[1] == 'E' &&
          coff->signature[2] == '\0' && coff->signature[3] == '\0')) {
        pln("Corrupt PE file, missing signature found: %",
            f_string(create_string(4, coff->signature)));
        return;
    }
    
    // Print out COFF header
    pln("COFF header:");
    
    int machine_index = -1;
    for (int i = 0; i < fixed_array_count(pe_machine_values); i++) {
        if (pe_machine_values[i] == coff->machine) {
            machine_index = i;
        }
    }
    if (machine_index >= 0) {
        pln("  Machine: % (%)", f_u64_HEX(coff->machine), f_cstring(pe_machine_descriptions[machine_index]));
    } else {
        pln("  Machine: % (unknown)", f_u64_HEX(coff->machine));
    }
    pln("  Number of sections: %", f_uint(coff->number_of_sections));
    
    time_t timestamp = coff->time_date_stamp;
    struct tm* timeinfo = localtime(&timestamp);
    print("  Timestamp: %", f_cstring(asctime(timeinfo)));
    pln("  Pointer to symbol table: %", f_u64_HEX(coff->pointer_to_symbol_table));
    pln("  Number of symbols: %", f_u64_HEX(coff->number_of_symbols));
    pln("  Size of optional header: %", f_uint(coff->size_of_optional_header));
    pln("  Characteristics: %", f_u64_HEX(coff->characteristics));
    for (int i = 0; i < fixed_array_count(pe_characteristics_values); i++) {
        if (pe_characteristics_values[i] & coff->characteristics) {
            pln("    - %", f_cstring(pe_characteristics_descriptions[i]));
        }
    }
    
    // TODO(Alexander): support PE32, this also just assumes it's PE32+!!
    // TODO(Alexander): check for COFF header Size of optional header
    COFF_PE32_Plus_Header* opt_header = (COFF_PE32_Plus_Header*) curr;
    curr += sizeof(COFF_PE32_Plus_Header);
    pln("\nCOFF/PE32+ Header:");
    pln("  Magic: %", f_u64_HEX(opt_header->magic));
    pln("  Major Linker Version: %", f_u64(opt_header->major_linker_version));
    pln("  Minor Linker Version: %", f_u64(opt_header->minor_linker_version));
    pln("  Size of Code: %", f_u64_HEX(opt_header->size_of_code));
    pln("  Size of Initialized Data: %", f_u64_HEX(opt_header->size_of_initialized_data));
    pln("  Size of Uninitialized Data: %", f_u64_HEX(opt_header->size_of_uninitialized_data));
    pln("  Address of Entry Point: %", f_u64_HEX(opt_header->address_of_entry_point));
    pln("  Base of Code: %", f_u64_HEX(opt_header->base_of_code));
    pln("  Image Base: %", f_u64_HEX(opt_header->image_base));
    pln("  Section Alignment: %", f_u64_HEX(opt_header->section_alignment));
    pln("  File Alignment: %", f_u64_HEX(opt_header->file_alignment));
    pln("  Major Operating System Version: %", f_u64(opt_header->major_operating_system_version));
    pln("  Minor Operating System Version: %", f_u64(opt_header->minor_operating_system_version));
    pln("  Major Image Version: %", f_u64(opt_header->major_image_version));
    pln("  Minor Image Version: %", f_u64(opt_header->minor_image_version));
    pln("  Major Subsystem Version: %", f_u64(opt_header->major_subsystem_version));
    pln("  Minor Subsystem Version: %", f_u64(opt_header->minor_subsystem_version));
    pln("  Win32 Version Value: %", f_u64(opt_header->win32_version_value));
    pln("  Size of Image: %", f_u64_HEX(opt_header->size_of_image));
    pln("  Size of Headers: %", f_u64_HEX(opt_header->size_of_headers));
    pln("  Checksum: %", f_u64_HEX(opt_header->checksum));
    
    int subsystem_index = -1;
    for (int i = 0; i < fixed_array_count(coff_subsystem_values); i++) {
        if (coff_subsystem_values[i] == opt_header->subsystem) {
            subsystem_index = i;
        }
    }
    if (subsystem_index >= 0) {
        pln("  Subsystem: % (%)", f_u64_HEX(opt_header->subsystem), 
            f_cstring(coff_subsystem_descriptions[subsystem_index]));
    } else {
        pln("  Subsystem: %", f_u64_HEX(opt_header->subsystem));
    }
    
    pln("  DLL Characteristics: %", f_u64_HEX(opt_header->dll_characteristics));
    for (int i = 0; i < fixed_array_count(coff_dll_characteristics_values); i++) {
        if (coff_dll_characteristics_values[i] & opt_header->dll_characteristics) {
            pln("    - %", f_cstring(coff_dll_characteristics_descriptions[i]));
        }
    }
    
    pln("  Size of Stack Reserve: %", f_u64_HEX(opt_header->size_of_stack_reserve));
    pln("  Size of Stack Commit: %", f_u64_HEX(opt_header->size_of_stack_commit));
    pln("  Size of Heap Reserve: %", f_u64_HEX(opt_header->size_of_heap_reserve));
    pln("  Size of Heap Commit: %", f_u64_HEX(opt_header->size_of_heap_commit));
    pln("  Loader Flags: %", f_u64_HEX(opt_header->loader_flags));
    pln("  Number of RVA and Sizes: %", f_u64_HEX(opt_header->number_of_rva_and_sizes));
    
    pln("\nData directories:");
    for (int i = 0; i < fixed_array_count(opt_header->data_directories); i++) {
        COFF_Image_Data_Dir* dir = &opt_header->data_directories[i];
        if (dir->size != 0) {
            pln("  Entry % % %", f_int(i), f_u64_HEX(dir->virtual_address), f_u64_HEX(dir->size));
        }
    }
    
    
    Memory_Arena machine_code = {};
    
    pln("\nSection Table:");
    for (int i = 0; i < coff->number_of_sections; i++) {
        COFF_Section_Header* section = (COFF_Section_Header*) curr;
        curr += sizeof(COFF_Section_Header);
        
        string section_name = create_string(8, section->name);
        pln("  %:", f_string(section_name));
        pln("    Virtual Address: %", f_u64_HEX(section->virtual_address));
        pln("    Size: %", f_u64_HEX(section->virtual_size));
        pln("    Pointer to Raw Data: %", f_u64_HEX(section->pointer_to_raw_data));
        pln("    Size of Raw Data: %", f_u64_HEX(section->size_of_raw_data));
        pln("    Pointer to relocations: %", f_u64_HEX(section->pointer_to_relocations));
        pln("    Number of relocations: %", f_u64_HEX(section->number_of_relocations));
        pln("    File Offset: %", f_u64_HEX(section->pointer_to_raw_data));
        
        //__debugbreak();
        if (string_equals(section_name, create_string(8, (u8*) ".text\0\0\0"))) {
            void* dest = arena_push_size(&machine_code, section->size_of_raw_data, 1);
            memcpy(dest, contents.data + section->pointer_to_raw_data, section->size_of_raw_data);
        }
    }
    
    
    pln("\nX64 Machine Code (% bytes):", f_umm(machine_code.curr_used));
    for (int byte_index = 0; byte_index < machine_code.curr_used; byte_index++) {
        u8 byte = machine_code.base[byte_index];
        if (byte > 0xF) {
            printf("%hhX ", byte);
        } else {
            printf("0%hhX ", byte);
        }
        
        if (byte_index % 20 == 19) {
            printf("\n");
        }
    }
    printf("\n\n");
}

void
push_dos_header_stub(Memory_Arena* arena) {
    int size = fixed_array_count(pe_dos_header_stub);
    void* dest = arena_push_size(arena, size, 1);
    memcpy(dest, pe_dos_header_stub, size);
}

void
convert_x64_machine_code_to_pe_executable(Memory_Arena* arena,
                                          u8* x64_machine_code, u32 x64_machine_code_size,
                                          u8* entry_point) {
    time_t timestamp = time(0);
    
    push_dos_header_stub(arena);
    
    COFF_Header* header = arena_push_struct(arena, COFF_Header);
    header->signature[0] = 'P'; header->signature[1] = 'E';
    header->machine = COFF_MACHINE_AMD64;
    header->number_of_sections = 1; // .text section for now
    header->time_date_stamp = (u32) timestamp;
    header->size_of_optional_header = sizeof(COFF_PE32_Plus_Header);
    header->characteristics = COFF_EXECUTABLE_IMAGE | COFF_LARGE_ADDRESS_AWARE;
    
    COFF_PE32_Plus_Header* opt_header = arena_push_struct(arena, COFF_PE32_Plus_Header);
    opt_header->magic = 0x20b;
    opt_header->major_linker_version = 0;
    opt_header->minor_linker_version = 1;
    
    opt_header->dll_characteristics = 0x8160;
    
    // TODO(Alexander): not sure about these values, check these later
    opt_header->size_of_image = 0x2000;
    opt_header->size_of_initialized_data = 0;
    opt_header->base_of_code = 0x1000;
    opt_header->address_of_entry_point = (u32) (opt_header->base_of_code + 
                                                (entry_point - x64_machine_code));
    opt_header->image_base = 0x140000000;
    opt_header->section_alignment = 0x1000;
    opt_header->file_alignment = 0x200;
    
    //opt_header->size_of_image = // TODO: 
    opt_header->size_of_headers = (u32) align_forward(sizeof(pe_dos_header_stub) + 
                                                      sizeof(COFF_Header) +
                                                      sizeof(COFF_PE32_Plus_Header) + 
                                                      sizeof(COFF_Section_Header) *
                                                      header->number_of_sections,
                                                      opt_header->file_alignment);
    
    opt_header->subsystem = COFF_SUBSYSTEM_WINDOWS_CUI;
    opt_header->major_operating_system_version = 6;
    opt_header->major_subsystem_version = 6;
    
    opt_header->size_of_stack_reserve = 0x100000;
    opt_header->size_of_stack_commit = 0x1000;
    opt_header->size_of_heap_reserve = 0x100000;
    opt_header->size_of_heap_commit = 0x1000;
    
    COFF_Section_Header* text_section = arena_push_struct(arena, COFF_Section_Header);
    memcpy(text_section->name, ".text\0\0\0", 8);
    text_section->size_of_raw_data = (u32) align_forward(x64_machine_code_size, 
                                                         opt_header->file_alignment);
    u8* dest = (u8*) arena_push_size(arena,
                                     text_section->size_of_raw_data,
                                     opt_header->file_alignment,
                                     ArenaPushFlag_Align_From_Zero);
    text_section->virtual_address = opt_header->base_of_code;//(u32) (dest - arena->base);
    text_section->virtual_size = x64_machine_code_size;
    text_section->pointer_to_raw_data = (u32) (dest - arena->base);
    memcpy(dest, x64_machine_code, x64_machine_code_size);
    text_section->characteristics = COFF_SCN_MEM_READ | COFF_SCN_MEM_EXECUTE | COFF_SCN_CNT_CODE;
    
    
    pe_dump_executable(create_string(arena->curr_used, arena->base));
}
