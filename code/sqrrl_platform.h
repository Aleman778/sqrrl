
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
write_memory_block_to_file(File_Handle file_handle, Memory_Block* block) {
    if (!block) return;
    
    //pln("base = %, size = %, used = %", f_u64_HEX(block->base), f_u64_HEX(block->size), f_u64_HEX(block->used));
    write_memory_block_to_file(file_handle, block->prev_block);
    DEBUG_write(file_handle, block->base, (u32) block->used);
}

void
write_padding_to_file(File_Handle file_handle, umm size, umm aligned_size) {
    umm padding_size = aligned_size - size;
    //pln("pad: %", f_u64_HEX(padding_size));
    if (padding_size) {
        void* padding = allocate_zeros(padding_size);
        DEBUG_write(file_handle, padding, (u32) padding_size);
        free(padding);
    }
}

// NOTE(Alexander): forward declare
struct Ast_File;

void DEBUG_add_debug_symbols(Ast_File* ast_file, u8* base_address_ptr);

// NOTE(Alexander): Similar to setjmp/ longjmp
void* DEBUG_capture_context();
void DEBUG_restore_context(void* exec_context);

void DEBUG_sleep(u32 time_ms);

// TODO(Alexander): this is probably not how we want to do multithreading!!!
void* DEBUG_create_thread(int (*proc)(void*), void* data);
bool DEBUG_join_thread(void* thread_handle, u32 timeout_ms);

void set_custom_exception_handler(int (*handler)(void));


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