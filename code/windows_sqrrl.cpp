
void DEBUG_log_backtrace();

#include "sqrrl.h"
#include "sqrrl.cpp"

#include <windows.h>
#include "dbghelp.h"

#ifndef PATH_MAX
#define PATH_MAX 200 // TODO(alexander): this shouldn't be used, just for debug code!!!
#endif

// NOTE(alexander): THIS IS ONLY FOR DEBUGGING
void DEBUG_log_backtrace() {
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


int main(int argc, char* argv[]) {
    // NOTE(alexander): here goes any platform specific initialization
    return compiler_main_entry(argc, argv);
}