#define PRINT_BYTECODE 1

typedef void* HANDLE;
#ifdef __cplusplus
typedef unsigned int DWORD;
#else
typedef u32 DWORD;
#endif
typedef DWORD* LPDWORD;
typedef int BOOL;
typedef void VOID;
typedef VOID* LPVOID;

#ifdef __cplusplus
#define STD_OUTPUT_HANDLE ((DWORD) -11)
#else
#define STD_OUTPUT_HANDLE (cast(DWORD) -11)
#endif


#ifdef __cplusplus
extern "C"
#else
@link("kernel32.dll")
extern
#endif
{
    HANDLE GetStdHandle(DWORD nStdHandle);
    
    BOOL WriteConsoleA(HANDLE  hConsoleOutput,
                       const VOID    *lpBuffer,
                       DWORD   nNumberOfCharsToWrite,
                       LPDWORD lpNumberOfCharsWritten,
                       LPVOID  lpReserved);
}


int
main() {
#ifdef __cplusplus
    const char* message = "Hello Fadi!";
#else
    cstring message = "Hello Fadi!";
#endif
    DWORD message_count = 11;
    DWORD message_count_written = 0;
    HANDLE stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteConsoleA(stdout, (LPVOID) message, message_count, &message_count_written, 0);
    
    return 0;
}













#ifdef __cplusplus
int
mainCRTStartup() {
    return main();
}
#endif