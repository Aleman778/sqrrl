
/***************************************************************************
 * Services that the platform layer provides to the compiler
 ***************************************************************************/

void DEBUG_log_backtrace();

struct Canonicalized_Path {
    cstring fullpath;
    cstring file_part;
    b32 success;
};

Canonicalized_Path
DEBUG_get_canonicalized_path(cstring filename, cstring working_directory, cstring curr_file_dir = 0);

Canonicalized_Path
DEBUG_get_system_canonicalized_path(cstring filepath);

void
DEBUG_free_canonicalized_path(Canonicalized_Path canonicalized_path);

struct Read_File_Result {
    void* contents;
    u32 contents_size;
};

typedef void* File_Handle;

Read_File_Result DEBUG_read_entire_file(cstring filepath);

Read_File_Result DEBUG_read_entire_system_header(cstring filepath);
bool DEBUG_write_entire_file(cstring filename, void* data, u32 size);

File_Handle DEBUG_open_file_for_writing(cstring filename);
bool DEBUG_write(File_Handle file_handle, void* data, u32 size);
bool DEBUG_close_file(File_Handle file);

void DEBUG_free_file_memory(void* memory);

void* DEBUG_get_external_procedure_address(cstring library, cstring procedure_name);

void DEBUG_set_current_directory(cstring path);

void
write_memory_block_to_file(Memory_Block* block, File_Handle file_handle) {
    if (!block) return;
    write_memory_block_to_file(block->prev_block, file_handle);
    DEBUG_write(file_handle, block->base, (u32) block->used);
}


// NOTE(Alexander): forward declare
struct Ast_File;

void DEBUG_add_debug_symbols(Ast_File* ast_file, u8* base_address_ptr);


// TODO:
// * We want to be able to handle virtual memory allocations here.
// * Also we want to be able to make a virtual memory block executable for jitting code.
// ...


/***************************************************************************
 * Services that the compiler provides to the platform layer
 ***************************************************************************/

void
platform_error(string message) {
    pln("error: %", f_string(message));
    
    DEBUG_log_backtrace();
}

// TBD.