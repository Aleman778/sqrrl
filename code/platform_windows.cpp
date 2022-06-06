
#include "sqrrl.cpp"

#include <windows.h>
#include "dbghelp.h"

#ifndef PATH_MAX
#define PATH_MAX 200 // TODO(alexander): this shouldn't be used, just for debug code!!!
#endif

#if BUILD_DEBUG
void 
DEBUG_log_backtrace() {
    DWORD machine = IMAGE_FILE_MACHINE_AMD64;
    HANDLE process = GetCurrentProcess();
    HANDLE thread = GetCurrentThread();
    CONTEXT context = {};
    context.ContextFlags = CONTEXT_FULL;
    RtlCaptureContext(&context);
    
    SymInitialize(process, NULL, TRUE);
    SymSetOptions(SYMOPT_LOAD_LINES);
    
    STACKFRAME64 frame = {};
    frame.AddrPC.Offset    = context.Rip;
    frame.AddrFrame.Offset = context.Rbp;
    frame.AddrStack.Offset = context.Rsp;
    frame.AddrPC.Mode      = AddrModeFlat;
    frame.AddrFrame.Mode   = AddrModeFlat;
    frame.AddrStack.Mode   = AddrModeFlat;
    
    pln("Callstack:");
    
    StackWalk64(machine,
                process,
                thread,
                &frame,
                &context,
                NULL,
                SymFunctionTableAccess64,
                SymGetModuleBase64,
                NULL);
    
    while (StackWalk64(machine,
                       process,
                       thread,
                       &frame,
                       &context,
                       NULL,
                       SymFunctionTableAccess64,
                       SymGetModuleBase64,
                       NULL)) {
        
        DWORD64 function_address = 0;
        PCHAR module_name = 0;
        PCHAR function_name = 0;
        PCHAR file_name = 0;
        u32 line_number = 0;
        
        function_address = frame.AddrPC.Offset;
        DWORD64 module_base = SymGetModuleBase64(process, frame.AddrPC.Offset);
        char module_buffer[PATH_MAX];
        if (module_base && GetModuleFileNameA((HINSTANCE) module_base, module_buffer, PATH_MAX)) {
            module_name = module_buffer;
        }
        
        char symbol_buffer[sizeof(PIMAGEHLP_SYMBOL64) + 255];
        PIMAGEHLP_SYMBOL64 symbol = (PIMAGEHLP_SYMBOL64) symbol_buffer;
        symbol->SizeOfStruct = sizeof(IMAGEHLP_SYMBOL64) + 255;
        symbol->MaxNameLength = 254;
        if (SymGetSymFromAddr64(process, frame.AddrPC.Offset, NULL, symbol)) {
            function_name = symbol->Name;
        }
        
        DWORD offset = 0;
        IMAGEHLP_LINE64 line;
        line.SizeOfStruct = sizeof(IMAGEHLP_LINE64);
        if (SymGetLineFromAddr64(process, frame.AddrPC.Offset, &offset, &line)) {
            file_name   = line.FileName;
            line_number = line.LineNumber;
        }
        //strcmp(module_name, "sqrrl") == 0 &&  // what is this????
        if (strcmp(function_name, "__scrt_common_main_seh") != 0) {
            printf("%s:%u: in function %s\n", file_name, line_number, function_name);
        }
    }
    printf("\n");
    
    SymCleanup(process);
}
#else
void 
DEBUG_log_backtrace() {
    
}
#endif

Read_File_Result
DEBUG_read_entire_file(cstring filename) {
    Read_File_Result result = {};
    HANDLE file_handle = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    
    if (file_handle != INVALID_HANDLE_VALUE) {
        LARGE_INTEGER file_size;
        if (GetFileSizeEx(file_handle, &file_size)) {
            if (file_size.QuadPart > U32_MAX) {
                platform_error(string_format("file `%` exeeds maximum file size of 4GB", f_cstring(filename)));
                return result;
            }
            
            result.contents_size = (u32) file_size.QuadPart;
            result.contents = VirtualAlloc(0, result.contents_size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
            
            if (result.contents) {
                if (!ReadFile(file_handle, result.contents, (u32) result.contents_size, 0, 0)) {
                    platform_error(string_format("failed to read file `%`", f_cstring(filename)));
                }
                
            } else {
                platform_error(string_format("out of memory when allocating space for `%`", f_cstring(filename)));
            }
            
        } else {
            platform_error(string_format("failed to read file `%`", f_cstring(filename)));
        }
        
        CloseHandle(file_handle);
    } else {
        platform_error(string_format("file `%` was not found", f_cstring(filename)));
    }
    
    return result;
}

bool
DEBUG_write_entire_file(cstring filename, void* data, u32 size) {
    Read_File_Result result = {};
    HANDLE file_handle = CreateFileA(filename, GENERIC_WRITE, FILE_SHARE_WRITE, 0, OPEN_ALWAYS, 0, 0);
    
    if (file_handle != INVALID_HANDLE_VALUE) {
        
        DWORD out_size = 0;
        bool success = WriteFile(file_handle, data, size, &out_size, 0);
        
        CloseHandle(file_handle);
        
        return out_size == size && success;
    } else {
        platform_error(string_format("file `%` was not found", f_cstring(filename)));
    }
    
    return false;
}

void
DEBUG_free_file_memory(void* memory) {
    VirtualFree(memory, 0, MEM_RELEASE);
}

void
asm_buffer_prepare_for_execute(void* data, umm size) {
    DWORD prev_protect = 0;
    VirtualProtect(data, size, PAGE_EXECUTE_READ, &prev_protect);
}


int main(int argc, char* argv[]) {
    umm asm_buffer_size = 1024;
    void* asm_buffer = VirtualAlloc(0, asm_buffer_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    
    // NOTE(alexander): here goes any platform specific initialization
    return compiler_main_entry(argc, argv, asm_buffer, asm_buffer_size, &asm_buffer_prepare_for_execute);
}