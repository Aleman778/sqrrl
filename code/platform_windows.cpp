#include "sqrrl.cpp"

#include <windows.h>
#include "dbghelp.h"

#ifndef PATH_MAX
#define PATH_MAX 260 // TODO(alexander): this shouldn't be used, just for debug code!!!
#endif

global cstring windows_system_header_shared = 0;
global array(cstring)* windows_system_header_dirs = 0;
//global cstring working_directory = "";

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

internal inline Canonicalized_Path
canonicalize_path(cstring filepath) {
    Canonicalized_Path result = {};
    
    DWORD buffer_count = PATH_MAX; // TODO(Alexander): ugh PATH_MAX is not very large.
    result.fullpath = (cstring) malloc(buffer_count);
    
    // TODO(Alexander): maybe need to switch to UNICODE mode for longer filepaths
    // TODO(Alexander): more error check we can get the actual count and retry
    // NOTE(Alexander): converts file paths with /../ etc. to its absolute version
    DWORD actual_count = GetFullPathNameA(filepath, buffer_count, 
                                          (LPSTR) result.fullpath, 
                                          (LPSTR*) &result.file_part);
    
    result.success = actual_count > 0;
    
    return result;
}

Canonicalized_Path
DEBUG_get_canonicalized_path(cstring filename, cstring working_dir, cstring curr_file_dir) {
    // searches first in curr file directory (optionally), else in current working directory.
    cstring filepath = 0;
    
    // NOTE(Alexander): we aren't case sentitive in windows
    cstring relative_filepath = 0;
    if (curr_file_dir) {
        
        relative_filepath = cstring_concat(curr_file_dir, filename);
        //pln("relative path: %\n", f_cstring(relative_filepath));
        HANDLE file_handle = CreateFileA(relative_filepath, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
        
        if (file_handle != INVALID_HANDLE_VALUE) {
            filepath = relative_filepath;
            CloseHandle(file_handle);
        }
    }
    
    cstring absolute_filepath = 0;
    if (!filepath) {
        absolute_filepath = cstring_concat(working_dir, filename);
        cstring_to_lower_ascii_nocopy(absolute_filepath);
        filepath = absolute_filepath;
    }
    
    Canonicalized_Path result = canonicalize_path(filepath);
    
    HANDLE file_handle = CreateFileA(result.fullpath, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    if (file_handle != INVALID_HANDLE_VALUE) {
        CloseHandle(file_handle);
    } else {
        // TODO(Alexander): this is a hack to work around windows headers uses 
        // shared as working dir, we need to support multiple contexts for this.
        //bool poppack = string_equals(string_lit(filename), string_lit("poppack.h"));
        //if (poppack) {
        for_array_v(windows_system_header_dirs, dir, _) {
            if (string_begins_with(string_lit(curr_file_dir), string_lit(dir))) {
                cstring tmp_path = cstring_concat(windows_system_header_shared, filename);
                result = canonicalize_path(tmp_path);
                cstring_free(tmp_path);
                break;
            }
        }
    }
    
    
    if (!result.success) {
        platform_error(string_lit("Failed to find "));
    }
    
    
    //pln("path: `%`\nname: `%`", f_cstring(result.fullpath), f_cstring(result.file_part));
    if (relative_filepath) {
        cstring_free(relative_filepath);
    }
    
    if (absolute_filepath) {
        cstring_free(absolute_filepath);
    }
    
    return result;
}

Canonicalized_Path
DEBUG_get_system_canonicalized_path(cstring filename) {
    Canonicalized_Path result = {};
    
    //pln("Searching for `%` in:", f_cstring(filename));
    
    cstring filepath = 0;
    
    for_array_v (windows_system_header_dirs, dir, _) {
        filepath = cstring_concat(dir, filename);
        
        //pln("- %", f_cstring(dir));
        
        HANDLE file_handle = CreateFileA(filepath, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
        
        if (file_handle != INVALID_HANDLE_VALUE) {
            filepath = filepath;
            
            CloseHandle(file_handle);
            result = canonicalize_path(filepath);
            
            if (result.success) {
                cstring_free(filepath);
                break;
            }
        }
        
        cstring_free(filepath);
        filepath = 0;
    }
    
    if (!result.success) {
        platform_error(string_format("system header file `%` is not found", f_cstring(filename)));
    }
    
    //pln("SYS! path: `%`\nname: `%`", f_cstring(result.fullpath), f_cstring(result.file_part));
    
    return result;
}

void
DEBUG_free_canonicalized_path(Canonicalized_Path path) {
    cstring_free(path.fullpath);
}



Read_File_Result
read_entire_file_handle(HANDLE file_handle, cstring filename) {
    Read_File_Result result = {};
    
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
                platform_error(string_format("failed to read file `%`, win32 error code: `0x%` see\nhttps://docs.microsoft.com/en-us/windows/win32/debug/system-error-codes for more info", 
                                             f_cstring(filename), f_u64_HEX(GetLastError())));
                
                DEBUG_free_file_memory(result.contents);
                result.contents_size = 0;
                result.contents = 0;
            }
            
        } else {
            platform_error(string_format("out of memory when allocating space for `%`", f_cstring(filename)));
        }
        
    } else {
        platform_error(string_format("failed to read file `%`", f_cstring(filename)));
    }
    
    return result;
}

Read_File_Result
DEBUG_read_entire_file(cstring filename) {
    Read_File_Result result = {};
    
    HANDLE file_handle = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
    
    if (file_handle != INVALID_HANDLE_VALUE) {
        result = read_entire_file_handle(file_handle, filename);
        CloseHandle(file_handle);
    } else {
        platform_error(string_format("file `%` was not found", f_cstring(filename)));
    }
    
    return result;
}


bool
DEBUG_write_entire_file(cstring filename, void* data, u32 size) {
    Read_File_Result result = {};
    DeleteFileA(filename);
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

void*
DEBUG_get_external_procedure_address(cstring library, cstring procedure_name) {
    // TODO(Alexander): reuse prev LoadLibraryA
    HMODULE mod = LoadLibraryA(library);
    return GetProcAddress(mod, procedure_name);
}

void
asm_buffer_prepare_for_execute(void* data, umm size) {
    DWORD prev_protect = 0;
    VirtualProtect(data, size, PAGE_EXECUTE_READ, &prev_protect);
}


cstring
read_string_from_system_registry(HKEY key, cstring value_name) {
    // Uses malloc to try and read the value, retries until sucessfull
    
    cstring result = 0;
    
    DWORD required_count;
    if (RegQueryValueExA(key, value_name, 0, 0, 0, &required_count) == ERROR_SUCCESS) {
        
        for (;;) {
            DWORD count = required_count + 1; // null terminated
            result = (cstring) malloc(count + 1); // for extra for safety
            if (!result) break;
            
            
            DWORD type;
            auto status = RegQueryValueExA(key, value_name, 0, &type, (LPBYTE) result, &count);
            if (status == ERROR_MORE_DATA) {
                cstring_free(result);
                required_count = count;
                continue;
            }
            
            // Check if query is valid and that the type is string
            if ((status != ERROR_SUCCESS) || (type != REG_SZ)) {
                cstring_free(result);
                result = {};
            }
            break;
        }
    }
    
    return result;
}

cstring
find_windows_kits_include_dir() {
    cstring result = 0;
    
    HKEY main_key;
    auto status = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows Kits\\Installed Roots",
                                0, KEY_QUERY_VALUE | KEY_WOW64_32KEY | KEY_ENUMERATE_SUB_KEYS, &main_key);
    if (status == ERROR_SUCCESS) {
        cstring windows10_root = read_string_from_system_registry(main_key, "KitsRoot10");
        if (windows10_root != 0) {
            
            // TODO(Alexander): more Microsoft craziness, the version number needs to also be found
            result = cstring_concat(windows10_root, "Include\\10.0.22000.0\\");
            cstring_free(windows10_root);
        }
        
        RegCloseKey(main_key);
    }
    
    return result;
}

int 
main(int argc, char* argv[]) {
    // Enable UTF-8 encoding
    SetConsoleOutputCP(65001);
    
    umm asm_buffer_size = megabytes(1);
    void* asm_buffer = VirtualAlloc(0, asm_buffer_size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
    
    {
        // Setup compiler module path
        Canonicalized_Path path = canonicalize_path("../modules/");
        array_push(windows_system_header_dirs, path.fullpath);
    }
    
    cstring windows_system_headers_root_dir = find_windows_kits_include_dir();
    if (windows_system_headers_root_dir != 0) {
        windows_system_header_shared = cstring_concat(windows_system_headers_root_dir, "shared\\");
        array_push(windows_system_header_dirs, cstring_concat(windows_system_headers_root_dir, "um\\"));
        array_push(windows_system_header_dirs, cstring_concat(windows_system_headers_root_dir, "ucrt\\"));
        array_push(windows_system_header_dirs, windows_system_header_shared);
        array_push(windows_system_header_dirs, cstring_concat(windows_system_headers_root_dir, "winrt\\"));
        array_push(windows_system_header_dirs, cstring_concat(windows_system_headers_root_dir, "cppwinrt\\"));
        cstring_free(windows_system_headers_root_dir);
    }
    
    // TODO(Alexander): we need more microsoft craziness to get all VS paths
    // for now I will just hard code these
    array_push(windows_system_header_dirs, 
               "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.29.30133\\include\\");
    
    bool is_debugger_present = IsDebuggerPresent();
    
    return compiler_main_entry(argc, argv, 
                               asm_buffer, asm_buffer_size, &asm_buffer_prepare_for_execute, is_debugger_present);
}
