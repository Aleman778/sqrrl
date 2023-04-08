
internal u32
ptr_to_rva(u32 virtual_base, Memory_Arena* arena, void* ptr) {
    return virtual_base + (u32) arena_relative_pointer(arena, ptr);
}


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
            pln("  Entry % % %", f_int(i), f_u64_HEX(dir->rva), f_u64_HEX(dir->size));
        }
    }
    
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
            pln("\nX64 Machine Code (% bytes):", f_umm(section->size_of_raw_data));
            u8* machine_code = contents.data + section->pointer_to_raw_data;
            u32 size = section->size_of_raw_data;
            for (u32 byte_index = 0; byte_index < size; byte_index++) {
                u8 byte = machine_code[byte_index];
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
    }
    
    
}

void
push_dos_header_stub(Memory_Arena* arena) {
    int size = fixed_array_count(pe_dos_header_stub);
    void* dest = arena_push_size(arena, size, 1);
    memcpy(dest, pe_dos_header_stub, size);
}

COFF_Section_Header*
push_coff_section(Memory_Arena* arena, COFF_PE32_Plus_Header* opt_header, cstring name, 
                  u32 size, u32 characteristics) {
    
    COFF_Section_Header* section = arena_push_struct(arena, COFF_Section_Header);
    memcpy(section->name, name, 8);
    section->size_of_raw_data = (u32) align_forward(size, opt_header->file_alignment);
    section->characteristics = characteristics;
    section->virtual_address =  opt_header->size_of_image;
    opt_header->size_of_image = (u32) align_forward(opt_header->size_of_image + size,
                                                    opt_header->section_alignment);
    section->virtual_size = size;
    return section;
}

void
push_coff_import_directory_table(Memory_Arena* rdata_arena, 
                                 COFF_PE32_Plus_Header* opt_header,
                                 Library_Import_Table* import_table, 
                                 u32 virtual_base) {
    
    u32 idt_base = 0;
    
    COFF_Import_Directory_Table* idt = 0;
    for_map(import_table->libs, it) {
        string_id library_id = it->key;
        Library_Imports* imports = &it->value;
        
        COFF_Import_Directory_Table* entry = arena_push_struct(rdata_arena, COFF_Import_Directory_Table,
                                                               ArenaPushFlag_Align_From_Zero);
        
        if (idt == 0) {
            idt = entry;
            idt_base = (u32) arena_relative_pointer(rdata_arena, idt);
            if (map_count(import_table->libs) > 0) {
                opt_header->import_table.rva = virtual_base + idt_base;
                opt_header->import_address_table.rva = virtual_base; // NOTE(Alexander): always start at 0
                opt_header->import_address_table.size += 8; // NOTE(Alexander): null entry
            }
        }
    }
    if (idt) {
        // Push null entry
        arena_push_struct(rdata_arena, COFF_Import_Directory_Table, ArenaPushFlag_Align_From_Zero);
        opt_header->import_table.size = (u32) arena_total_used(rdata_arena) - idt_base;
        pln("SIZE: %", f_u32(opt_header->import_table.size));
        
        COFF_Import_Directory_Table* entry = idt;
        
        for_map(import_table->libs, it) {
            string library_name = vars_load_string(it->key);
            array(Library_Function)* import_names = it->value.functions;
            
            pln("SETUP IMPORT -> %", f_u64_HEX(entry));
            
            // Lookup table
            u64* lookup_table = 0;
            u64* address_table = 0;
            
            entry->address_table_rva = virtual_base;
            if (array_count(import_names) > 0) {
                // TODO(Alexander): HACK to get the address table entry
                Compilation_Unit* cu = import_names[0].type->Function.unit;
                address_table = (u64*) cu->external_address;
                entry->address_table_rva += (u32) cu->external_address;
                
                Memory_Block* block = rdata_arena->current_block;
                while (block) {
                    address_table = (u64*) (cu->external_address + block->base);
                    block = block->prev_block;
                }
                
                pln("address_table = %, external_address = %", f_u64_HEX(address_table),
                    f_u64_HEX(cu->external_address));
            }
            
            for_array(import_names, ident, _) {
                u64* lookup_entry = arena_push_struct(rdata_arena, u64);
                if (!lookup_table) {
                    lookup_table = lookup_entry;
                }
            }
            arena_push_struct(rdata_arena, u64); // null entry
            
            entry->lookup_table_rva = ptr_to_rva(virtual_base, rdata_arena, lookup_table);
            
            // Hint
            u64* lookup_entry = lookup_table;
            u64* address_entry = address_table;
            for_array_v(import_names, function, _1) {
                string function_name = vars_load_string(function.name);
                smm size = function_name.count + 3; // 3: hint (2 bytes) + null-term
                u8* hint = (u8*) arena_push_size(rdata_arena, size, 8);
                memcpy(hint + 2, function_name.data, function_name.count);
                
                u64 hint_rva = ptr_to_rva(virtual_base, rdata_arena, hint);
                *lookup_entry++ = hint_rva;
                *address_entry++ = hint_rva;
                opt_header->import_address_table.size += 8;
                
                // TODO(Alexander): to be more correct we should mask out the last bit
            }
            
            // Library name
            void* library_name_dest = arena_push_size(rdata_arena, library_name.count + 1, 8);
            memcpy(library_name_dest, library_name.data, library_name.count);
            entry->name_rva = ptr_to_rva(virtual_base, rdata_arena, library_name_dest);
            pln("LIB: %", f_string(library_name));
            
            entry++;
        }
    }
}

struct PE_Executable {
    COFF_Header* main_header;
    COFF_PE32_Plus_Header* opt_header;
    
    COFF_Section_Header* text_section;
    COFF_Section_Header* rdata_section;
    COFF_Section_Header* data_section; // TODO: might break Type_Info data into separate one
    COFF_Section_Header* reloc_section;
    
    u8* text_data;
    u32 text_data_size;
    
    Memory_Arena header_arena;
    Memory_Arena rdata_arena;
    Memory_Arena data_arena;
    Memory_Arena reloc_arena;
};

PE_Executable
convert_to_pe_executable(Memory_Arena* arena,
                         u8* machine_code, u32 machine_code_size,
                         Library_Import_Table* import_table,
                         Type_Info_Packer* type_info_packer,
                         Memory_Arena* rdata_arena,
                         u8* entry_point) {
    
    PE_Executable result = {};
    
    time_t timestamp = time(0);
    
    push_dos_header_stub(arena);
    
    // PE\0\0 signature and mandatory COFF header
    COFF_Header* header = arena_push_struct(arena, COFF_Header);
    result.main_header = header;
    header->signature[0] = 'P'; header->signature[1] = 'E';
    header->machine = COFF_MACHINE_AMD64;
    header->time_date_stamp = (u32) timestamp;
    header->size_of_optional_header = sizeof(COFF_PE32_Plus_Header);
    header->characteristics = COFF_EXECUTABLE_IMAGE | COFF_LARGE_ADDRESS_AWARE;
    
    // Optional headers (mandatory PE32+ for executable)
    COFF_PE32_Plus_Header* opt_header = arena_push_struct(arena, COFF_PE32_Plus_Header);
    result.opt_header = opt_header;
    opt_header->magic = 0x20b;
    opt_header->major_linker_version = 0;
    opt_header->minor_linker_version = 1;
    opt_header->number_of_rva_and_sizes = fixed_array_count(opt_header->data_directories);
    
    opt_header->dll_characteristics = 0x8160;
    
    // TODO(Alexander): not sure about these values, check these later
    opt_header->image_base = 0x140000000;
    opt_header->section_alignment = 0x1000;
    opt_header->file_alignment = 0x200;
    opt_header->size_of_initialized_data = 0;
    opt_header->base_of_code = 0x1000;
    opt_header->size_of_image = opt_header->base_of_code;
    opt_header->address_of_entry_point = (u32) (opt_header->base_of_code + 
                                                (entry_point - machine_code));
    
    opt_header->subsystem = COFF_SUBSYSTEM_WINDOWS_CUI;
    opt_header->major_operating_system_version = 6;
    opt_header->major_subsystem_version = 6;
    
    opt_header->size_of_stack_reserve = 0x100000;
    opt_header->size_of_stack_commit = 0x1000;
    opt_header->size_of_heap_reserve = 0x100000;
    opt_header->size_of_heap_commit = 0x1000;
    
    // Section headers
    COFF_Section_Header* text_section = 0;
    COFF_Section_Header* rdata_section = 0;
    COFF_Section_Header* data_section = 0;
    COFF_Section_Header* reloc_section = 0;
    
    // .text section
    text_section = push_coff_section(arena, opt_header, COFF_TEXT_SECTION, machine_code_size, 
                                     COFF_SCN_CNT_CODE | COFF_SCN_MEM_EXECUTE | COFF_SCN_MEM_READ);
    result.text_section = text_section;
    opt_header->size_of_code += text_section->size_of_raw_data;
    header->number_of_sections++; // added .text
    result.header_arena = *arena;
    
    // .rdata section
    push_coff_import_directory_table(rdata_arena, opt_header,
                                     import_table, opt_header->size_of_image);
    u32 rdata_size = (u32) arena_total_used(rdata_arena);
    pln("%", f_u32(rdata_size));
    if (rdata_size > 0) {
        rdata_section = push_coff_section(arena, opt_header, COFF_RDATA_SECTION, rdata_size,
                                          COFF_SCN_CNT_INITIALIZED_DATA | COFF_SCN_MEM_READ);
        result.rdata_section = rdata_section;
        opt_header->size_of_initialized_data += rdata_section->size_of_raw_data;
        header->number_of_sections++; // added .rdata
    }
    
    // .data section
    u32 data_size = (u32) arena_total_used(&type_info_packer->arena);
    if (data_size > 0) {
        data_section = push_coff_section(arena, opt_header, COFF_DATA_SECTION, data_size,
                                         (u32) COFF_SCN_CNT_INITIALIZED_DATA | COFF_SCN_MEM_READ | COFF_SCN_MEM_WRITE);
        result.data_section = data_section;
        opt_header->size_of_initialized_data += data_section->size_of_raw_data;
        header->number_of_sections++; // added .data
    }
    
    // Create base relocation table (for Type_Info_Packer)
    Memory_Arena reloc_arena = {};
    COFF_Base_Relocation_Table* reloc_table = arena_push_struct(&reloc_arena, COFF_Base_Relocation_Table);
    reloc_table->page_rva = data_section->virtual_address;
    for_array(type_info_packer->relocations, reloc, reloc_index) {
        u32* reloc_entry = arena_push_struct(&reloc_arena, u32);
        *reloc_entry = 0xA000 + reloc->from;
        
        *((u64*) reloc->from_ptr) = (u64) (opt_header->image_base + 
                                           data_section->virtual_address + 
                                           reloc->to);
    }
    reloc_table->block_size = (u32) arena_total_used(&reloc_arena);
    
    // .reloc section
    u32 reloc_size = (u32) arena_total_used(&reloc_arena);
    if (reloc_size > 0) {
        reloc_section = push_coff_section(arena, opt_header, COFF_RELOC_SECTION, reloc_size,
                                          COFF_SCN_CNT_INITIALIZED_DATA | COFF_SCN_MEM_READ | COFF_SCN_MEM_DISCARDABLE);
        result.reloc_section = reloc_section;
        opt_header->size_of_initialized_data += reloc_section->size_of_raw_data;
        header->number_of_sections++; // added .data
    }
    
    umm actual_header_size = arena_total_used(arena);
    opt_header->size_of_headers = (u32) align_forward(actual_header_size,
                                                      opt_header->file_alignment);
    
    
    // Layout sections
    u32 output_size = opt_header->size_of_headers;
    if (text_section) {
        text_section->pointer_to_raw_data = output_size;
        output_size += text_section->size_of_raw_data;
        
        result.text_data = machine_code;
        result.text_data_size = machine_code_size;
        
    }
    
    if (rdata_section) {
        rdata_section->pointer_to_raw_data = output_size;
        output_size += rdata_section->size_of_raw_data;
        
        result.rdata_arena = *rdata_arena;
    }
    
    if (data_section) {
        data_section->pointer_to_raw_data = output_size;
        output_size += data_section->size_of_raw_data;
        
        result.data_arena = type_info_packer->arena;
    }
    
    if (reloc_section) {
        reloc_section->pointer_to_raw_data = output_size;
        output_size += reloc_section->size_of_raw_data;
        
        result.reloc_arena = reloc_arena;
    }
    
    return result;
}

void
write_pe_executable_to_file(File_Handle output_file, PE_Executable* pe) {
    
    // NOTE(Alexander): all headers have been set after this point
    umm actual_header_size = arena_total_used(&pe->header_arena);
    write_memory_block_to_file(output_file, pe->header_arena.current_block);
    write_padding_to_file(output_file, actual_header_size, pe->opt_header->size_of_headers);
    
    if (pe->text_section) {
        DEBUG_write(output_file, pe->text_data, pe->text_data_size);
        write_padding_to_file(output_file, pe->text_data_size, pe->text_section->size_of_raw_data);
        
    }
    
    if (pe->rdata_section) {
        umm rdata_size = arena_total_used(&pe->rdata_arena);
        write_memory_block_to_file(output_file, pe->rdata_arena.current_block);
        write_padding_to_file(output_file, rdata_size, pe->rdata_section->size_of_raw_data);
    }
    
    if (pe->data_section) {
        umm data_size = arena_total_used(&pe->data_arena);
        write_memory_block_to_file(output_file, pe->data_arena.current_block);
        write_padding_to_file(output_file, data_size, pe->data_section->size_of_raw_data);
    }
    
    if (pe->reloc_section) {
        umm reloc_size = arena_total_used(&pe->reloc_arena);
        write_memory_block_to_file(output_file, pe->reloc_arena.current_block);
        write_padding_to_file(output_file, reloc_size, pe->reloc_section->size_of_raw_data);
    }
}
