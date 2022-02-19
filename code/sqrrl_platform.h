
/***************************************************************************
 * Services that the platform layer provides to the compiler
 ***************************************************************************/

void DEBUG_log_backtrace();


struct Read_File_Result {
    void* contents;
    u32 contents_size;
};

Read_File_Result DEBUG_read_entire_file(cstring filename);

void DEBUG_free_file_memory(void* memory);

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
}

// TBD.