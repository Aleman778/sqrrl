// NOTE: slightly modified version of windows.h library to work with the Sqrrl language
// This source code file is not owned by the author(s) of Sqrrl, majority of this file
// is copyrighted by Microsoft Corporation as stated below:

// Copyright (c) Microsoft Corporation. All rights reserved.

typedef cstring LPCSTR;
typedef cstring PSTR;

typedef long LONG;
typedef LONG *PLONG;
typedef unsigned long ULONG;
typedef ULONG *PULONG;
typedef unsigned short USHORT;
typedef USHORT *PUSHORT;
typedef unsigned char UCHAR;
typedef UCHAR* PUCHAR;
typedef char* PSZ;
typedef u32 DWORD;
typedef u64 DWORD64;
typedef u16 WCHAR;    // wc,   16-bit UNICODE character
typedef int BOOL;
typedef u8 BYTE;
typedef u16 WORD;
typedef f32 FLOAT;
typedef FLOAT* PFLOAT;
typedef BOOL* PBOOL;
typedef BOOL* LPBOOL;
typedef BYTE* PBYTE;
typedef BYTE* LPBYTE;
typedef int* PINT;
typedef int* LPINT;
typedef WORD* PWORD;
typedef WORD* LPWORD;
typedef long* LPLONG;
typedef DWORD* PDWORD;
typedef DWORD* LPDWORD;
typedef void* PVOID;
typedef void* LPVOID;
typedef const void* LPCVOID;

typedef int                 INT;
typedef unsigned int        UINT;
typedef char CHAR;
typedef short SHORT;
typedef unsigned int        *PUINT;
typedef unsigned long POINTER_64_INT;
typedef signed char INT8;
typedef INT8* PINT8;
typedef signed short INT16;
typedef INT16* PINT16;
typedef signed int INT32;
typedef INT32* PINT32;
typedef signed __int64 INT64;
typedef INT64* PINT64;
typedef unsigned char UINT8;
typedef UINT8* PUINT8;
typedef unsigned short UINT16;
typedef UINT16* PUINT16;
typedef unsigned int UINT32;
typedef UINT32* PUINT32;
typedef unsigned __int64 UINT64;
typedef UINT64* PUINT64;
typedef signed int LONG32;
typedef LONG32* PLONG32;
typedef unsigned int ULONG32;
typedef ULONG32* PULONG32;
typedef unsigned int DWORD32;
typedef DWORD32* PDWORD32;
typedef s64 INT_PTR;
typedef INT_PTR* PINT_PTR;
typedef s64 UINT_PTR;
typedef UINT_PTR* PUINT_PTR;
typedef s64 LONG_PTR;
typedef LONG_PTR* PLONG_PTR;
typedef s64 ULONG_PTR;
typedef ULONG_PTR* PULONG_PTR;
typedef s64 LONGLONG;
typedef u64 ULONGLONG;
typedef ULONG_PTR SIZE_T;
typedef SIZE_T* PSIZE_T
typedef void* __ptr64 HANDLE64;
typedef HANDLE64* PHANDLE64;
typedef void* HANDLE;
typedef HANDLE* PHANDLE;

typedef int FARPROC();

typedef long HRESULT;

// HRESULT codes
#define S_OK 0

#define SUCCEEDED(hr) (((HRESULT)(hr)) >= 0)

typedef unsigned short UHALF_PTR;
typedef UHALF_PTR* PUHALF_PTR;
typedef short HALF_PTR;
typedef HALF_PTR* PHALF_PTR;
typedef  long SHANDLE_PTR;
typedef  unsigned long HANDLE_PTR;

typedef UINT_PTR            WPARAM;
typedef LONG_PTR            LPARAM;
typedef LONG_PTR            LRESULT;
typedef HANDLE          *SPHANDLE;
typedef HANDLE           *LPHANDLE;
typedef HANDLE              HGLOBAL;
typedef HANDLE              HLOCAL;
typedef HANDLE              GLOBALHANDLE;
typedef HANDLE              LOCALHANDLE;
typedef WORD                ATOM;   
struct HKEY__{int unused;}; typedef struct HKEY__ *HKEY;
typedef HKEY *PHKEY;
struct HMETAFILE__{int unused;}; typedef struct HMETAFILE__ *HMETAFILE;
struct HINSTANCE__{int unused;}; typedef struct HINSTANCE__ *HINSTANCE;
typedef HINSTANCE HMODULE;      
struct HRGN__{int unused;}; typedef struct HRGN__ *HRGN;
struct HRSRC__{int unused;}; typedef struct HRSRC__ *HRSRC;
struct HSPRITE__{int unused;}; typedef struct HSPRITE__ *HSPRITE;
struct HLSURF__{int unused;}; typedef struct HLSURF__ *HLSURF;
struct HSTR__{int unused;}; typedef struct HSTR__ *HSTR;
struct HTASK__{int unused;}; typedef struct HTASK__ *HTASK;
struct HWINSTA__{int unused;}; typedef struct HWINSTA__ *HWINSTA;
struct HKL__{int unused;}; typedef struct HKL__ *HKL;
typedef int HFILE;

// Winerrors
#define ERROR_SUCCESS                    0L
#define ERROR_DEVICE_NOT_CONNECTED       1167L


#ifndef FALSE
#define FALSE               0
#endif

#ifndef TRUE
#define TRUE                1
#endif

#define CALLBACK    __stdcall
#define WINAPI      __stdcall
#define WINAPIV     __cdecl
#define APIENTRY    WINAPI
#define APIPRIVATE  __stdcall
#define PASCAL      __stdcall
#define STDMETHODCALLTYPE __cdecl

// TODO(Alexander): right now we don't have a keyword to denote this
#define NEAR
#define FAR


#define FILE_READ_DATA            ( 0x0001 )    // file & pipe
#define FILE_LIST_DIRECTORY       ( 0x0001 )    // directory

#define FILE_WRITE_DATA           ( 0x0002 )    // file & pipe
#define FILE_ADD_FILE             ( 0x0002 )    // directory

#define FILE_APPEND_DATA          ( 0x0004 )    // file
#define FILE_ADD_SUBDIRECTORY     ( 0x0004 )    // directory
#define FILE_CREATE_PIPE_INSTANCE ( 0x0004 )    // named pipe


#define FILE_READ_EA              ( 0x0008 )    // file & directory

#define FILE_WRITE_EA             ( 0x0010 )    // file & directory

#define FILE_EXECUTE              ( 0x0020 )    // file
#define FILE_TRAVERSE             ( 0x0020 )    // directory

#define FILE_DELETE_CHILD         ( 0x0040 )    // directory

#define FILE_READ_ATTRIBUTES      ( 0x0080 )    // all

#define FILE_WRITE_ATTRIBUTES     ( 0x0100 )    // all

#define FILE_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED | SYNCHRONIZE | 0x1FF)

#define FILE_GENERIC_READ         (STANDARD_RIGHTS_READ     |\
FILE_READ_DATA           |\
FILE_READ_ATTRIBUTES     |\
FILE_READ_EA             |\
SYNCHRONIZE)


#define FILE_GENERIC_WRITE        (STANDARD_RIGHTS_WRITE    |\
FILE_WRITE_DATA          |\
FILE_WRITE_ATTRIBUTES    |\
FILE_WRITE_EA            |\
FILE_APPEND_DATA         |\
SYNCHRONIZE)


#define FILE_GENERIC_EXECUTE      (STANDARD_RIGHTS_EXECUTE  |\
FILE_READ_ATTRIBUTES     |\
FILE_EXECUTE             |\
SYNCHRONIZE)

#define FILE_SHARE_READ                 0x00000001  
#define FILE_SHARE_WRITE                0x00000002  
#define FILE_SHARE_DELETE               0x00000004  
#define FILE_ATTRIBUTE_READONLY             0x00000001  
#define FILE_ATTRIBUTE_HIDDEN               0x00000002  
#define FILE_ATTRIBUTE_SYSTEM               0x00000004  
#define FILE_ATTRIBUTE_DIRECTORY            0x00000010  
#define FILE_ATTRIBUTE_ARCHIVE              0x00000020  
#define FILE_ATTRIBUTE_DEVICE               0x00000040  
#define FILE_ATTRIBUTE_NORMAL               0x00000080  
#define FILE_ATTRIBUTE_TEMPORARY            0x00000100  
#define FILE_ATTRIBUTE_SPARSE_FILE          0x00000200  
#define FILE_ATTRIBUTE_REPARSE_POINT        0x00000400  
#define FILE_ATTRIBUTE_COMPRESSED           0x00000800  
#define FILE_ATTRIBUTE_OFFLINE              0x00001000  
#define FILE_ATTRIBUTE_NOT_CONTENT_INDEXED  0x00002000  
#define FILE_ATTRIBUTE_ENCRYPTED            0x00004000  
#define FILE_ATTRIBUTE_INTEGRITY_STREAM     0x00008000  
#define FILE_ATTRIBUTE_VIRTUAL              0x00010000  
#define FILE_ATTRIBUTE_NO_SCRUB_DATA        0x00020000  
#define FILE_ATTRIBUTE_EA                   0x00040000  
#define FILE_ATTRIBUTE_PINNED               0x00080000  
#define FILE_ATTRIBUTE_UNPINNED             0x00100000  
#define FILE_ATTRIBUTE_RECALL_ON_OPEN       0x00040000  
#define FILE_ATTRIBUTE_RECALL_ON_DATA_ACCESS 0x00400000 
#define TREE_CONNECT_ATTRIBUTE_PRIVACY      0x00004000  
#define TREE_CONNECT_ATTRIBUTE_INTEGRITY    0x00008000  
#define TREE_CONNECT_ATTRIBUTE_GLOBAL       0x00000004  
#define TREE_CONNECT_ATTRIBUTE_PINNED       0x00000002  
#define FILE_ATTRIBUTE_STRICTLY_SEQUENTIAL  0x20000000  
#define FILE_NOTIFY_CHANGE_FILE_NAME    0x00000001   
#define FILE_NOTIFY_CHANGE_DIR_NAME     0x00000002   
#define FILE_NOTIFY_CHANGE_ATTRIBUTES   0x00000004   
#define FILE_NOTIFY_CHANGE_SIZE         0x00000008   
#define FILE_NOTIFY_CHANGE_LAST_WRITE   0x00000010   
#define FILE_NOTIFY_CHANGE_LAST_ACCESS  0x00000020   
#define FILE_NOTIFY_CHANGE_CREATION     0x00000040   
#define FILE_NOTIFY_CHANGE_SECURITY     0x00000100   
#define FILE_ACTION_ADDED                   0x00000001   
#define FILE_ACTION_REMOVED                 0x00000002   
#define FILE_ACTION_MODIFIED                0x00000003   
#define FILE_ACTION_RENAMED_OLD_NAME        0x00000004   
#define FILE_ACTION_RENAMED_NEW_NAME        0x00000005   
#define MAILSLOT_NO_MESSAGE             ((DWORD)-1) 
#define MAILSLOT_WAIT_FOREVER           ((DWORD)-1) 
#define FILE_CASE_SENSITIVE_SEARCH          0x00000001  
#define FILE_CASE_PRESERVED_NAMES           0x00000002  
#define FILE_UNICODE_ON_DISK                0x00000004  
#define FILE_PERSISTENT_ACLS                0x00000008  
#define FILE_FILE_COMPRESSION               0x00000010  
#define FILE_VOLUME_QUOTAS                  0x00000020  
#define FILE_SUPPORTS_SPARSE_FILES          0x00000040  
#define FILE_SUPPORTS_REPARSE_POINTS        0x00000080  
#define FILE_SUPPORTS_REMOTE_STORAGE        0x00000100  
#define FILE_RETURNS_CLEANUP_RESULT_INFO    0x00000200  
#define FILE_SUPPORTS_POSIX_UNLINK_RENAME   0x00000400  
#define FILE_SUPPORTS_BYPASS_IO             0x00000800  
#define FILE_SUPPORTS_STREAM_SNAPSHOTS      0x00001000  
#define FILE_SUPPORTS_CASE_SENSITIVE_DIRS   0x00002000  

#define FILE_VOLUME_IS_COMPRESSED           0x00008000  
#define FILE_SUPPORTS_OBJECT_IDS            0x00010000  
#define FILE_SUPPORTS_ENCRYPTION            0x00020000  
#define FILE_NAMED_STREAMS                  0x00040000  
#define FILE_READ_ONLY_VOLUME               0x00080000  
#define FILE_SEQUENTIAL_WRITE_ONCE          0x00100000  
#define FILE_SUPPORTS_TRANSACTIONS          0x00200000  
#define FILE_SUPPORTS_HARD_LINKS            0x00400000  
#define FILE_SUPPORTS_EXTENDED_ATTRIBUTES   0x00800000  
#define FILE_SUPPORTS_OPEN_BY_FILE_ID       0x01000000  
#define FILE_SUPPORTS_USN_JOURNAL           0x02000000  
#define FILE_SUPPORTS_INTEGRITY_STREAMS     0x04000000  
#define FILE_SUPPORTS_BLOCK_REFCOUNTING     0x08000000  
#define FILE_SUPPORTS_SPARSE_VDL            0x10000000  
#define FILE_DAX_VOLUME                     0x20000000  
#define FILE_SUPPORTS_GHOSTING              0x40000000  

#define FILE_INVALID_FILE_ID               ((LONGLONG)-1LL) 

struct FILE_ID_128 {                               
    BYTE  Identifier[16];                                   
} 
typedef FILE_ID_128 *PFILE_ID_128;

//
//  These are the generic rights.
//

#define GENERIC_READ                     (0x80000000L)
#define GENERIC_WRITE                    (0x40000000L)
#define GENERIC_EXECUTE                  (0x20000000L)
#define GENERIC_ALL                      (0x10000000L)

#define MAX_PATH          260

typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME;
//, *PFILETIME, *LPFILETIME;

typedef union _LARGE_INTEGER {
    struct {
        DWORD LowPart;
        LONG HighPart;
    } DUMMYSTRUCTNAME;
    struct {
        DWORD LowPart;
        LONG HighPart;
    } u;
    LONGLONG QuadPart;
} LARGE_INTEGER;
typedef LARGE_INTEGER* PLARGE_INTEGER;
typedef LARGE_INTEGER* LPLARGE_INTEGER;

typedef struct _SECURITY_ATTRIBUTES {
    DWORD nLength;
    LPVOID lpSecurityDescriptor;
    BOOL bInheritHandle;
} SECURITY_ATTRIBUTES;
typedef SECURITY_ATTRIBUTES* PSECURITY_ATTRIBUTES;
typedef SECURITY_ATTRIBUTES* LPSECURITY_ATTRIBUTES;

struct OVERLAPPED {
    ULONG_PTR Internal;
    ULONG_PTR InternalHigh;
    union {
        struct {
            DWORD Offset;
            DWORD OffsetHigh;
        } DUMMYSTRUCTNAME;
        PVOID Pointer;
    } DUMMYUNIONNAME;
    
    HANDLE  hEvent;
};
typedef OVERLAPPED *LPOVERLAPPED;


typedef struct _WIN32_FIND_DATAA {
    DWORD dwFileAttributes;
    FILETIME ftCreationTime;
    FILETIME ftLastAccessTime;
    FILETIME ftLastWriteTime;
    DWORD nFileSizeHigh;
    DWORD nFileSizeLow;
    DWORD dwReserved0;
    DWORD dwReserved1;
    CHAR   cFileName[ MAX_PATH ];
    CHAR   cAlternateFileName[ 14 ];
#ifdef _MAC
    DWORD dwFileType;
    DWORD dwCreatorType;
    WORD  wFinderFlags;
#endif
} WIN32_FIND_DATAA;
typedef WIN32_FIND_DATAA* PWIN32_FIND_DATAA;
typedef WIN32_FIND_DATAA* LPWIN32_FIND_DATAA;

typedef WIN32_FIND_DATAA WIN32_FIND_DATA;
typedef PWIN32_FIND_DATAA PWIN32_FIND_DATA;
typedef LPWIN32_FIND_DATAA LPWIN32_FIND_DATA;

@link("kernel32.dll")
extern {
    BOOL WINAPI CopyFileA(LPCSTR lpExistingFileName,
                          LPCSTR lpNewFileName,
                          BOOL bFailIfExists);
}

/*
* Handle API
*/

//
// Constants
//
#define INVALID_HANDLE_VALUE (cast(HANDLE)cast(LONG_PTR)-1)


@link("kernel32.dll")
extern {
    BOOL WINAPI CloseHandle(HANDLE hObject);
}

/*
* Libloader API
*/

@link("kernel32.dll")
extern {
    HMODULE WINAPI LoadLibraryA(LPCSTR lpLibFileName);
    
    BOOL WINAPI FreeLibrary(HMODULE hLibModule);
    
    FARPROC WINAPI GetProcAddress(HMODULE hModule,
                                  LPCSTR lpProcName);
}


/*
* User
*/
struct HWND__{int unused;}; typedef struct HWND__ *HWND;
struct HHOOK__{int unused;}; typedef struct HHOOK__ *HHOOK;
typedef void * HGDIOBJ;
struct HACCEL__{int unused;}; typedef struct HACCEL__ *HACCEL;
struct HBITMAP__{int unused;}; typedef struct HBITMAP__ *HBITMAP;
struct HBRUSH__{int unused;}; typedef struct HBRUSH__ *HBRUSH;
struct HCOLORSPACE__{int unused;}; typedef struct HCOLORSPACE__ *HCOLORSPACE;
struct HDC__{int unused;}; typedef struct HDC__ *HDC;
struct HGLRC__{int unused;}; typedef struct HGLRC__ *HGLRC;          
struct HDESK__{int unused;}; typedef struct HDESK__ *HDESK;
struct HENHMETAFILE__{int unused;}; typedef struct HENHMETAFILE__ *HENHMETAFILE;
struct HFONT__{int unused;}; typedef struct HFONT__ *HFONT;
struct HICON__{int unused;}; typedef struct HICON__ *HICON;
struct HMENU__{int unused;}; typedef struct HMENU__ *HMENU;
struct HPALETTE__{int unused;}; typedef struct HPALETTE__ *HPALETTE;
struct HPEN__{int unused;}; typedef struct HPEN__ *HPEN;
struct HWINEVENTHOOK__{int unused;}; typedef struct HWINEVENTHOOK__ *HWINEVENTHOOK;
struct HMONITOR__{int unused;}; typedef struct HMONITOR__ *HMONITOR;
struct HUMPD__{int unused;}; typedef struct HUMPD__ *HUMPD;
typedef HICON HCURSOR;      
typedef DWORD   COLORREF;
typedef DWORD   *LPCOLORREF;
typedef struct tagRECT
{
    LONG    left;
    LONG    top;
    LONG    right;
    LONG    bottom;
} RECT;// *PRECT,  *NPRECT;
typedef RECT* LPRECT;
typedef const RECT * LPCRECT;
typedef struct _RECTL       
{
    LONG    left;
    LONG    top;
    LONG    right;
    LONG    bottom;
} RECTL;// *PRECTL, *LPRECTL;
typedef const RECTL * LPCRECTL;
typedef struct tagPOINT
{
    LONG  x;
    LONG  y;
} POINT;// *PPOINT,  *NPPOINT,  
typedef POINT *LPPOINT;
typedef struct _POINTL      
{
    LONG  x;
    LONG  y;
} POINTL;// *PPOINTL;
typedef struct tagSIZE
{
    LONG        cx;
    LONG        cy;
} SIZE;// *PSIZE, *LPSIZE;
typedef SIZE SIZEL;
typedef SIZE* PSIZEL;
typedef SIZE* LPSIZEL;
typedef struct tagPOINTS
{
    SHORT   x;
    SHORT   y;
} POINTS;// *PPOINTS, *LPPOINTS;

//typedef LRESULT __stdcall(HWND, UINT, WPARAM, LPARAM)* WNDPROC;
typedef LRESULT __stdcall WNDPROC(HWND, UINT, WPARAM, LPARAM);



/* DIB color table identifiers */

#define DIB_RGB_COLORS      0 /* color table in RGBs */
#define DIB_PAL_COLORS      1 /* color table in palette indices */

/*
 * Window Styles
 */
#define WS_OVERLAPPED       0x00000000L
#define WS_POPUP            0x80000000L
#define WS_CHILD            0x40000000L
#define WS_MINIMIZE         0x20000000L
#define WS_VISIBLE          0x10000000L
#define WS_DISABLED         0x08000000L
#define WS_CLIPSIBLINGS     0x04000000L
#define WS_CLIPCHILDREN     0x02000000L
#define WS_MAXIMIZE         0x01000000L
#define WS_CAPTION          0x00C00000L     /* WS_BORDER | WS_DLGFRAME  */
#define WS_BORDER           0x00800000L
#define WS_DLGFRAME         0x00400000L
#define WS_VSCROLL          0x00200000L
#define WS_HSCROLL          0x00100000L
#define WS_SYSMENU          0x00080000L
#define WS_THICKFRAME       0x00040000L
#define WS_GROUP            0x00020000L
#define WS_TABSTOP          0x00010000L

#define WS_MINIMIZEBOX      0x00020000L
#define WS_MAXIMIZEBOX      0x00010000L


#define WS_TILED            WS_OVERLAPPED
#define WS_ICONIC           WS_MINIMIZE
#define WS_SIZEBOX          WS_THICKFRAME
#define WS_TILEDWINDOW      WS_OVERLAPPEDWINDOW

/*
 * Common Window Styles
 */
#define WS_OVERLAPPEDWINDOW (WS_OVERLAPPED     | \
WS_CAPTION        | \
WS_SYSMENU        | \
WS_THICKFRAME     | \
WS_MINIMIZEBOX    | \
WS_MAXIMIZEBOX)

#define WS_POPUPWINDOW      (WS_POPUP          | \
WS_BORDER         | \
WS_SYSMENU)

#define WS_CHILDWINDOW      (WS_CHILD)


/*
 * Class styles
 */
#define CS_VREDRAW          0x0001
#define CS_HREDRAW          0x0002
#define CS_DBLCLKS          0x0008
#define CS_OWNDC            0x0020
#define CS_CLASSDC          0x0040
#define CS_PARENTDC         0x0080
#define CS_NOCLOSE          0x0200
#define CS_SAVEBITS         0x0800
#define CS_BYTEALIGNCLIENT  0x1000
#define CS_BYTEALIGNWINDOW  0x2000
#define CS_GLOBALCLASS      0x4000

#define CS_IME              0x00010000
#define CS_DROPSHADOW       0x00020000

#define CW_USEDEFAULT       ((int)0x80000000)

/*
 * ShowWindow() Commands
 */
#define SW_HIDE             0
#define SW_SHOWNORMAL       1
#define SW_NORMAL           1
#define SW_SHOWMINIMIZED    2
#define SW_SHOWMAXIMIZED    3
#define SW_MAXIMIZE         3
#define SW_SHOWNOACTIVATE   4
#define SW_SHOW             5
#define SW_MINIMIZE         6
#define SW_SHOWMINNOACTIVE  7
#define SW_SHOWNA           8
#define SW_RESTORE          9
#define SW_SHOWDEFAULT      10
#define SW_FORCEMINIMIZE    11
#define SW_MAX              11


/*
 * Window Messages
 */

#define WM_NULL                         0x0000
#define WM_CREATE                       0x0001
#define WM_DESTROY                      0x0002
#define WM_MOVE                         0x0003
#define WM_SIZE                         0x0005

#define WM_ACTIVATE                     0x0006
/*
 * WM_ACTIVATE state values
 */
#define     WA_INACTIVE     0
#define     WA_ACTIVE       1
#define     WA_CLICKACTIVE  2

#define WM_SETFOCUS                     0x0007
#define WM_KILLFOCUS                    0x0008
#define WM_ENABLE                       0x000A
#define WM_SETREDRAW                    0x000B
#define WM_SETTEXT                      0x000C
#define WM_GETTEXT                      0x000D
#define WM_GETTEXTLENGTH                0x000E
#define WM_PAINT                        0x000F
#define WM_CLOSE                        0x0010
#ifndef _WIN32_WCE
#define WM_QUERYENDSESSION              0x0011
#define WM_QUERYOPEN                    0x0013
#define WM_ENDSESSION                   0x0016
#endif
#define WM_QUIT                         0x0012
#define WM_ERASEBKGND                   0x0014
#define WM_SYSCOLORCHANGE               0x0015
#define WM_SHOWWINDOW                   0x0018
#define WM_WININICHANGE                 0x001A
#if(WINVER >= 0x0400)
#define WM_SETTINGCHANGE                WM_WININICHANGE
#endif /* WINVER >= 0x0400 */

/* Ternary raster operations */
#define SRCCOPY             (DWORD)0x00CC0020 /* dest = source                   */
#define SRCPAINT            (DWORD)0x00EE0086 /* dest = source OR dest           */
#define SRCAND              (DWORD)0x008800C6 /* dest = source AND dest          */
#define SRCINVERT           (DWORD)0x00660046 /* dest = source XOR dest          */
#define SRCERASE            (DWORD)0x00440328 /* dest = source AND (NOT dest )   */
#define NOTSRCCOPY          (DWORD)0x00330008 /* dest = (NOT source)             */
#define NOTSRCERASE         (DWORD)0x001100A6 /* dest = (NOT src) AND (NOT dest) */
#define MERGECOPY           (DWORD)0x00C000CA /* dest = (source AND pattern)     */
#define MERGEPAINT          (DWORD)0x00BB0226 /* dest = (NOT source) OR dest     */
#define PATCOPY             (DWORD)0x00F00021 /* dest = pattern                  */
#define PATPAINT            (DWORD)0x00FB0A09 /* dest = DPSnoo                   */
#define PATINVERT           (DWORD)0x005A0049 /* dest = pattern XOR dest         */
#define DSTINVERT           (DWORD)0x00550009 /* dest = (NOT dest)               */
#define BLACKNESS           (DWORD)0x00000042 /* dest = BLACK                    */
#define WHITENESS           (DWORD)0x00FF0062 /* dest = WHITE                    */

#define WM_DEVMODECHANGE                0x001B
#define WM_ACTIVATEAPP                  0x001C
#define WM_FONTCHANGE                   0x001D
#define WM_TIMECHANGE                   0x001E
#define WM_CANCELMODE                   0x001F
#define WM_SETCURSOR                    0x0020
#define WM_MOUSEACTIVATE                0x0021
#define WM_CHILDACTIVATE                0x0022
#define WM_QUEUESYNC                    0x0023

#define WM_GETMINMAXINFO                0x0024

#define WM_KEYFIRST                     0x0100
#define WM_KEYDOWN                      0x0100
#define WM_KEYUP                        0x0101
#define WM_CHAR                         0x0102
#define WM_DEADCHAR                     0x0103
#define WM_SYSKEYDOWN                   0x0104
#define WM_SYSKEYUP                     0x0105
#define WM_SYSCHAR                      0x0106
#define WM_SYSDEADCHAR                  0x0107

#define WM_MOUSEFIRST                   0x0200
#define WM_MOUSEMOVE                    0x0200
#define WM_LBUTTONDOWN                  0x0201
#define WM_LBUTTONUP                    0x0202
#define WM_LBUTTONDBLCLK                0x0203
#define WM_RBUTTONDOWN                  0x0204
#define WM_RBUTTONUP                    0x0205
#define WM_RBUTTONDBLCLK                0x0206
#define WM_MBUTTONDOWN                  0x0207
#define WM_MBUTTONUP                    0x0208
#define WM_MBUTTONDBLCLK                0x0209
#define WM_MOUSEWHEEL                   0x020A
#define WM_XBUTTONDOWN                  0x020B
#define WM_XBUTTONUP                    0x020C
#define WM_XBUTTONDBLCLK                0x020D
#define WM_MOUSEHWHEEL                  0x020E
#define WM_MOUSELAST                    0x020E

/* Value for rolling one detent */
#define WHEEL_DELTA                     120
#define GET_WHEEL_DELTA_WPARAM(wParam)  ((short)HIWORD(wParam))

/* Setting to scroll one page for SPI_GET/SETWHEELSCROLLLINES */
#define WHEEL_PAGESCROLL                (UINT_MAX)

#define GET_KEYSTATE_WPARAM(wParam)     (LOWORD(wParam))
#define GET_NCHITTEST_WPARAM(wParam)    ((short)LOWORD(wParam))
#define GET_XBUTTON_WPARAM(wParam)      (HIWORD(wParam))

/* XButton values are WORD flags */
#define XBUTTON1      0x0001
#define XBUTTON2      0x0002

#define WM_PARENTNOTIFY                 0x0210
#define WM_ENTERMENULOOP                0x0211
#define WM_EXITMENULOOP                 0x0212

#define WM_NEXTMENU                     0x0213
#define WM_SIZING                       0x0214
#define WM_CAPTURECHANGED               0x0215
#define WM_MOVING                       0x0216

#define PAGE_NOACCESS           0x01    
#define PAGE_READONLY           0x02    
#define PAGE_READWRITE          0x04    
#define PAGE_WRITECOPY          0x08    
#define PAGE_EXECUTE            0x10    
#define PAGE_EXECUTE_READ       0x20    
#define PAGE_EXECUTE_READWRITE  0x40    
#define PAGE_EXECUTE_WRITECOPY  0x80    
#define PAGE_GUARD             0x100    
#define PAGE_NOCACHE           0x200    
#define PAGE_WRITECOMBINE      0x400    
#define PAGE_GRAPHICS_NOACCESS           0x0800    
#define PAGE_GRAPHICS_READONLY           0x1000    
#define PAGE_GRAPHICS_READWRITE          0x2000    
#define PAGE_GRAPHICS_EXECUTE            0x4000    
#define PAGE_GRAPHICS_EXECUTE_READ       0x8000    
#define PAGE_GRAPHICS_EXECUTE_READWRITE 0x10000    
#define PAGE_GRAPHICS_COHERENT          0x20000    
#define PAGE_GRAPHICS_NOCACHE           0x40000    
#define PAGE_ENCLAVE_THREAD_CONTROL 0x80000000  
#define PAGE_REVERT_TO_FILE_MAP     0x80000000  
#define PAGE_TARGETS_NO_UPDATE      0x40000000  
#define PAGE_TARGETS_INVALID        0x40000000  
#define PAGE_ENCLAVE_UNVALIDATED    0x20000000  
#define PAGE_ENCLAVE_MASK           0x10000000  
#define PAGE_ENCLAVE_DECOMMIT       (PAGE_ENCLAVE_MASK | 0) 
#define PAGE_ENCLAVE_SS_FIRST       (PAGE_ENCLAVE_MASK | 1) 
#define PAGE_ENCLAVE_SS_REST        (PAGE_ENCLAVE_MASK | 2) 
#define MEM_COMMIT                      0x00001000  
#define MEM_RESERVE                     0x00002000  
#define MEM_REPLACE_PLACEHOLDER         0x00004000  
#define MEM_RESERVE_PLACEHOLDER         0x00040000  
#define MEM_RESET                       0x00080000  
#define MEM_TOP_DOWN                    0x00100000  
#define MEM_WRITE_WATCH                 0x00200000  
#define MEM_PHYSICAL                    0x00400000  
#define MEM_ROTATE                      0x00800000  
#define MEM_DIFFERENT_IMAGE_BASE_OK     0x00800000  
#define MEM_RESET_UNDO                  0x01000000  
#define MEM_LARGE_PAGES                 0x20000000  
#define MEM_4MB_PAGES                   0x80000000  
#define MEM_64K_PAGES                   (MEM_LARGE_PAGES | MEM_PHYSICAL)  
#define MEM_UNMAP_WITH_TRANSIENT_BOOST  0x00000001  
#define MEM_COALESCE_PLACEHOLDERS       0x00000001  
#define MEM_PRESERVE_PLACEHOLDER        0x00000002  
#define MEM_DECOMMIT                    0x00004000  
#define MEM_RELEASE                     0x00008000  
#define MEM_FREE                        0x00010000  

/* constants for the biCompression field */
#define BI_RGB        0L
#define BI_RLE8       1L
#define BI_RLE4       2L
#define BI_BITFIELDS  3L
#define BI_JPEG       4L
#define BI_PNG        5L

/*
 * PeekMessage() Options
 */
#define PM_NOREMOVE         0x0000
#define PM_REMOVE           0x0001
#define PM_NOYIELD          0x0002

#define CALLBACK    __cdecl

/*
 * Virtual Keys, Standard Set
 */
#define VK_LBUTTON        0x01
#define VK_RBUTTON        0x02
#define VK_CANCEL         0x03
#define VK_MBUTTON        0x04    /* NOT contiguous with L & RBUTTON */

#define VK_XBUTTON1       0x05    /* NOT contiguous with L & RBUTTON */
#define VK_XBUTTON2       0x06    /* NOT contiguous with L & RBUTTON */

/*
 * 0x07 : reserved
 */


#define VK_BACK           0x08
#define VK_TAB            0x09

/*
 * 0x0A - 0x0B : reserved
 */

#define VK_CLEAR          0x0C
#define VK_RETURN         0x0D

/*
 * 0x0E - 0x0F : unassigned
 */

#define VK_SHIFT          0x10
#define VK_CONTROL        0x11
#define VK_MENU           0x12
#define VK_PAUSE          0x13
#define VK_CAPITAL        0x14

#define VK_KANA           0x15
#define VK_HANGEUL        0x15  /* old name - should be here for compatibility */
#define VK_HANGUL         0x15
#define VK_IME_ON         0x16
#define VK_JUNJA          0x17
#define VK_FINAL          0x18
#define VK_HANJA          0x19
#define VK_KANJI          0x19
#define VK_IME_OFF        0x1A

#define VK_ESCAPE         0x1B

#define VK_CONVERT        0x1C
#define VK_NONCONVERT     0x1D
#define VK_ACCEPT         0x1E
#define VK_MODECHANGE     0x1F

#define VK_SPACE          0x20
#define VK_PRIOR          0x21
#define VK_NEXT           0x22
#define VK_END            0x23
#define VK_HOME           0x24
#define VK_LEFT           0x25
#define VK_UP             0x26
#define VK_RIGHT          0x27
#define VK_DOWN           0x28
#define VK_SELECT         0x29
#define VK_PRINT          0x2A
#define VK_EXECUTE        0x2B
#define VK_SNAPSHOT       0x2C
#define VK_INSERT         0x2D
#define VK_DELETE         0x2E
#define VK_HELP           0x2F

/*
 * VK_0 - VK_9 are the same as ASCII '0' - '9' (0x30 - 0x39)
 * 0x3A - 0x40 : unassigned
 * VK_A - VK_Z are the same as ASCII 'A' - 'Z' (0x41 - 0x5A)
 */

#define VK_LWIN           0x5B
#define VK_RWIN           0x5C
#define VK_APPS           0x5D

/*
 * 0x5E : reserved
 */

#define VK_SLEEP          0x5F

#define VK_NUMPAD0        0x60
#define VK_NUMPAD1        0x61
#define VK_NUMPAD2        0x62
#define VK_NUMPAD3        0x63
#define VK_NUMPAD4        0x64
#define VK_NUMPAD5        0x65
#define VK_NUMPAD6        0x66
#define VK_NUMPAD7        0x67
#define VK_NUMPAD8        0x68
#define VK_NUMPAD9        0x69
#define VK_MULTIPLY       0x6A
#define VK_ADD            0x6B
#define VK_SEPARATOR      0x6C
#define VK_SUBTRACT       0x6D
#define VK_DECIMAL        0x6E
#define VK_DIVIDE         0x6F
#define VK_F1             0x70
#define VK_F2             0x71
#define VK_F3             0x72
#define VK_F4             0x73
#define VK_F5             0x74
#define VK_F6             0x75
#define VK_F7             0x76
#define VK_F8             0x77
#define VK_F9             0x78
#define VK_F10            0x79
#define VK_F11            0x7A
#define VK_F12            0x7B
#define VK_F13            0x7C
#define VK_F14            0x7D
#define VK_F15            0x7E
#define VK_F16            0x7F
#define VK_F17            0x80
#define VK_F18            0x81
#define VK_F19            0x82
#define VK_F20            0x83
#define VK_F21            0x84
#define VK_F22            0x85
#define VK_F23            0x86
#define VK_F24            0x87

#if(_WIN32_WINNT >= 0x0604)

/*
 * 0x88 - 0x8F : UI navigation
 */

#define VK_NAVIGATION_VIEW     0x88 // reserved
#define VK_NAVIGATION_MENU     0x89 // reserved
#define VK_NAVIGATION_UP       0x8A // reserved
#define VK_NAVIGATION_DOWN     0x8B // reserved
#define VK_NAVIGATION_LEFT     0x8C // reserved
#define VK_NAVIGATION_RIGHT    0x8D // reserved
#define VK_NAVIGATION_ACCEPT   0x8E // reserved
#define VK_NAVIGATION_CANCEL   0x8F // reserved

#endif /* _WIN32_WINNT >= 0x0604 */

#define VK_NUMLOCK        0x90
#define VK_SCROLL         0x91

/*
 * NEC PC-9800 kbd definitions
 */
#define VK_OEM_NEC_EQUAL  0x92   // '=' key on numpad

/*
 * Fujitsu/OASYS kbd definitions
 */
#define VK_OEM_FJ_JISHO   0x92   // 'Dictionary' key
#define VK_OEM_FJ_MASSHOU 0x93   // 'Unregister word' key
#define VK_OEM_FJ_TOUROKU 0x94   // 'Register word' key
#define VK_OEM_FJ_LOYA    0x95   // 'Left OYAYUBI' key
#define VK_OEM_FJ_ROYA    0x96   // 'Right OYAYUBI' key

/*
 * 0x97 - 0x9F : unassigned
 */

/*
 * VK_L* & VK_R* - left and right Alt, Ctrl and Shift virtual keys.
 * Used only as parameters to GetAsyncKeyState() and GetKeyState().
 * No other API or message will distinguish left and right keys in this way.
 */
#define VK_LSHIFT         0xA0
#define VK_RSHIFT         0xA1
#define VK_LCONTROL       0xA2
#define VK_RCONTROL       0xA3
#define VK_LMENU          0xA4
#define VK_RMENU          0xA5

#if(_WIN32_WINNT >= 0x0500)
#define VK_BROWSER_BACK        0xA6
#define VK_BROWSER_FORWARD     0xA7
#define VK_BROWSER_REFRESH     0xA8
#define VK_BROWSER_STOP        0xA9
#define VK_BROWSER_SEARCH      0xAA
#define VK_BROWSER_FAVORITES   0xAB
#define VK_BROWSER_HOME        0xAC

#define VK_VOLUME_MUTE         0xAD
#define VK_VOLUME_DOWN         0xAE
#define VK_VOLUME_UP           0xAF
#define VK_MEDIA_NEXT_TRACK    0xB0
#define VK_MEDIA_PREV_TRACK    0xB1
#define VK_MEDIA_STOP          0xB2
#define VK_MEDIA_PLAY_PAUSE    0xB3
#define VK_LAUNCH_MAIL         0xB4
#define VK_LAUNCH_MEDIA_SELECT 0xB5
#define VK_LAUNCH_APP1         0xB6
#define VK_LAUNCH_APP2         0xB7

#endif /* _WIN32_WINNT >= 0x0500 */

/*
 * 0xB8 - 0xB9 : reserved
 */

#define VK_OEM_1          0xBA   // ';:' for US
#define VK_OEM_PLUS       0xBB   // '+' any country
#define VK_OEM_COMMA      0xBC   // ',' any country
#define VK_OEM_MINUS      0xBD   // '-' any country
#define VK_OEM_PERIOD     0xBE   // '.' any country
#define VK_OEM_2          0xBF   // '/?' for US
#define VK_OEM_3          0xC0   // '`~' for US

/*
 * 0xC1 - 0xC2 : reserved
 */


/*
 * 0xC3 - 0xDA : Gamepad input
 */

#define VK_GAMEPAD_A                         0xC3 // reserved
#define VK_GAMEPAD_B                         0xC4 // reserved
#define VK_GAMEPAD_X                         0xC5 // reserved
#define VK_GAMEPAD_Y                         0xC6 // reserved
#define VK_GAMEPAD_RIGHT_SHOULDER            0xC7 // reserved
#define VK_GAMEPAD_LEFT_SHOULDER             0xC8 // reserved
#define VK_GAMEPAD_LEFT_TRIGGER              0xC9 // reserved
#define VK_GAMEPAD_RIGHT_TRIGGER             0xCA // reserved
#define VK_GAMEPAD_DPAD_UP                   0xCB // reserved
#define VK_GAMEPAD_DPAD_DOWN                 0xCC // reserved
#define VK_GAMEPAD_DPAD_LEFT                 0xCD // reserved
#define VK_GAMEPAD_DPAD_RIGHT                0xCE // reserved
#define VK_GAMEPAD_MENU                      0xCF // reserved
#define VK_GAMEPAD_VIEW                      0xD0 // reserved
#define VK_GAMEPAD_LEFT_THUMBSTICK_BUTTON    0xD1 // reserved
#define VK_GAMEPAD_RIGHT_THUMBSTICK_BUTTON   0xD2 // reserved
#define VK_GAMEPAD_LEFT_THUMBSTICK_UP        0xD3 // reserved
#define VK_GAMEPAD_LEFT_THUMBSTICK_DOWN      0xD4 // reserved
#define VK_GAMEPAD_LEFT_THUMBSTICK_RIGHT     0xD5 // reserved
#define VK_GAMEPAD_LEFT_THUMBSTICK_LEFT      0xD6 // reserved
#define VK_GAMEPAD_RIGHT_THUMBSTICK_UP       0xD7 // reserved
#define VK_GAMEPAD_RIGHT_THUMBSTICK_DOWN     0xD8 // reserved
#define VK_GAMEPAD_RIGHT_THUMBSTICK_RIGHT    0xD9 // reserved
#define VK_GAMEPAD_RIGHT_THUMBSTICK_LEFT     0xDA // reserved


typedef struct tagWNDCLASSA {
    UINT        style;
    WNDPROC     lpfnWndProc;
    int         cbClsExtra;
    int         cbWndExtra;
    HINSTANCE   hInstance;
    HICON       hIcon;
    HCURSOR     hCursor;
    HBRUSH      hbrBackground;
    LPCSTR      lpszMenuName;
    LPCSTR      lpszClassName;
} WNDCLASSA; //, *PWNDCLASSA,  *NPWNDCLASSA,  *LPWNDCLASSA;

typedef struct tagRGBTRIPLE {
    BYTE    rgbtBlue;
    BYTE    rgbtGreen;
    BYTE    rgbtRed;
} RGBTRIPLE;// *PRGBTRIPLE,  *NPRGBTRIPLE,  *LPRGBTRIPLE;

typedef struct tagRGBQUAD {
    BYTE    rgbBlue;
    BYTE    rgbGreen;
    BYTE    rgbRed;
    BYTE    rgbReserved;
} RGBQUAD;

typedef RGBQUAD* LPRGBQUAD;
typedef LONG   LCSCSTYPE;
typedef LONG    LCSGAMUTMATCH;
typedef long            FXPT16DOT16;//,  *LPFXPT16DOT16;
typedef long            FXPT2DOT30;//,  *LPFXPT2DOT30;

typedef struct tagCIEXYZ
{
    FXPT2DOT30 ciexyzX;
    FXPT2DOT30 ciexyzY;
    FXPT2DOT30 ciexyzZ;
} CIEXYZ;
typedef CIEXYZ* LPCIEXYZ;

typedef struct tagICEXYZTRIPLE
{
    CIEXYZ  ciexyzRed;
    CIEXYZ  ciexyzGreen;
    CIEXYZ  ciexyzBlue;
} CIEXYZTRIPLE;
typedef CIEXYZTRIPLE* LPCIEXYZTRIPLE;

typedef struct tagBITMAPINFOHEADER{
    DWORD      biSize;
    LONG       biWidth;
    LONG       biHeight;
    WORD       biPlanes;
    WORD       biBitCount;
    DWORD      biCompression;
    DWORD      biSizeImage;
    LONG       biXPelsPerMeter;
    LONG       biYPelsPerMeter;
    DWORD      biClrUsed;
    DWORD      biClrImportant;
} BITMAPINFOHEADER;//,  *LPBITMAPINFOHEADER, *PBITMAPINFOHEADER;

typedef struct tagBITMAPINFO {
    BITMAPINFOHEADER    bmiHeader;
    RGBQUAD             bmiColors[1];
} BITMAPINFO;//,  *LPBITMAPINFO, *PBITMAPINFO;

typedef struct tagMSG {
    HWND        hwnd;
    UINT        message;
    WPARAM      wParam;
    LPARAM      lParam;
    DWORD       time;
    POINT       pt;
} MSG//, *PMSG, *NPMSG;
typedef MSG* LPMSG;


// Time API
typedef UINT MMRESULT;
/* timer error return values */
#define TIMERR_NOERROR        (0)                  /* no error */
#define TIMERR_NOCANDO        (TIMERR_BASE+1)      /* request not completed */
#define TIMERR_STRUCT         (TIMERR_BASE+33)     /* time struct size */

@link("winmm.dll")
extern {
    MMRESULT timeBeginPeriod(UINT uPeriod);
}

#define ReadTimeStampCounter() rdtsc()

@link("kernel32.dll")
extern {
    BOOL __stdcall FreeConsole();
    
    HMODULE __stdcall GetModuleHandleA(LPCSTR lpModuleName);
    
    LPVOID __stdcall VirtualAlloc(LPVOID lpAddress,
                                  SIZE_T dwSize,
                                  DWORD flAllocationType,
                                  DWORD flProtect);
    
    BOOL __stdcall VirtualFree(LPVOID lpAddress,
                               SIZE_T dwSize,
                               DWORD dwFreeType);
    
    void __stdcall Sleep(DWORD dwMilliseconds);
    
    DWORD __stdcall GetLastError();
    
    BOOL WINAPI QueryPerformanceCounter(LARGE_INTEGER* lpPerformanceCount);
    
    BOOL WINAPI QueryPerformanceFrequency(LARGE_INTEGER *lpFrequency);
    
}

@link("user32.dll")
extern {
    int __stdcall MessageBoxA(HWND hWnd,
                              LPCSTR lpText,
                              LPCSTR lpCaption,
                              UINT uType);
    
    BOOL __stdcall PeekMessageA(LPMSG lpMsg,
                                HWND hWnd,
                                UINT wMsgFilterMin,
                                UINT wMsgFilterMax,
                                UINT wRemoveMsg);
    
    BOOL __stdcall TranslateMessage(const MSG *lpMsg);
    
    LRESULT __stdcall DispatchMessageA(const MSG *lpMsg);
    
    LRESULT __stdcall DefWindowProcA(HWND hWnd,
                                     UINT Msg,
                                     WPARAM wParam,
                                     LPARAM lParam);
    
    
    ATOM __stdcall RegisterClassA(const WNDCLASSA *lpWndClass);
    
    HWND __stdcall CreateWindowExA(DWORD dwExStyle,
                                   LPCSTR lpClassName,
                                   LPCSTR lpWindowName,
                                   DWORD dwStyle,
                                   int X,
                                   int Y,
                                   int nWidth,
                                   int nHeight,
                                   HWND hWndParent,
                                   HMENU hMenu,
                                   HINSTANCE hInstance,
                                   LPVOID lpParam);
    
    BOOL WINAPI GetCursorPos(LPPOINT lpPoint);
    
    BOOL WINAPI ScreenToClient(HWND hWnd, LPPOINT lpPoint);
    
    BOOL __stdcall GetWindowRect(HWND hWnd, LPRECT lpRect);
    
    BOOL __stdcall GetClientRect(HWND hWnd, LPRECT lpRect);
    
    SHORT WINAPI GetKeyState(int nVirtKey);
    
    BOOL ShowWindow(HWND hWnd, int nCmdShow);
    
    HDC __stdcall GetDC(HWND hWnd);
    
}


/* Device Parameters for GetDeviceCaps() */
#define DRIVERVERSION 0     /* Device driver version                    */
#define TECHNOLOGY    2     /* Device classification                    */
#define HORZSIZE      4     /* Horizontal size in millimeters           */
#define VERTSIZE      6     /* Vertical size in millimeters             */
#define HORZRES       8     /* Horizontal width in pixels               */
#define VERTRES       10    /* Vertical height in pixels                */
#define BITSPIXEL     12    /* Number of bits per pixel                 */
#define PLANES        14    /* Number of planes                         */
#define NUMBRUSHES    16    /* Number of brushes the device has         */
#define NUMPENS       18    /* Number of pens the device has            */
#define NUMMARKERS    20    /* Number of markers the device has         */
#define NUMFONTS      22    /* Number of fonts the device has           */
#define NUMCOLORS     24    /* Number of colors the device supports     */
#define PDEVICESIZE   26    /* Size required for device descriptor      */
#define CURVECAPS     28    /* Curve capabilities                       */
#define LINECAPS      30    /* Line capabilities                        */
#define POLYGONALCAPS 32    /* Polygonal capabilities                   */
#define TEXTCAPS      34    /* Text capabilities                        */
#define CLIPCAPS      36    /* Clipping capabilities                    */
#define RASTERCAPS    38    /* Bitblt capabilities                      */
#define ASPECTX       40    /* Length of the X leg                      */
#define ASPECTY       42    /* Length of the Y leg                      */
#define ASPECTXY      44    /* Length of the hypotenuse                 */

#define LOGPIXELSX    88    /* Logical pixels/inch in X                 */
#define LOGPIXELSY    90    /* Logical pixels/inch in Y                 */

#define SIZEPALETTE  104    /* Number of entries in physical palette    */
#define NUMRESERVED  106    /* Number of reserved entries in palette    */
#define COLORRES     108    /* Actual color resolution                  */

// Printing related DeviceCaps. These replace the appropriate Escapes

#define PHYSICALWIDTH   110 /* Physical Width in device units           */
#define PHYSICALHEIGHT  111 /* Physical Height in device units          */
#define PHYSICALOFFSETX 112 /* Physical Printable Area x margin         */
#define PHYSICALOFFSETY 113 /* Physical Printable Area y margin         */
#define SCALINGFACTORX  114 /* Scaling factor x                         */
#define SCALINGFACTORY  115 /* Scaling factor y                         */
// Display driver specific

#define VREFRESH        116  /* Current vertical refresh rate of the    */
/* display device (for displays only) in Hz*/
#define DESKTOPVERTRES  117  /* Horizontal width of entire desktop in   */
/* pixels                                  */
#define DESKTOPHORZRES  118  /* Vertical height of entire desktop in    */
/* pixels                                  */
#define BLTALIGNMENT    119  /* Preferred blt alignment                 */


@link("gdi32.dll")
extern {
    int   WINAPI GetDeviceCaps(HDC hdc, int index);
    
    BOOL  __stdcall BitBlt(HDC hdc,
                           int x, int y, int cx, int cy,
                           HDC hdcSrc, 
                           int x1, int y1, 
                           DWORD rop);
    
    int __stdcall StretchDIBits(HDC hdc,
                                int xDest,
                                int yDest,
                                int DestWidth,
                                int DestHeight,
                                int xSrc,
                                int ySrc,
                                int SrcWidth,
                                int SrcHeight,
                                const void * lpBits,
                                const BITMAPINFO * lpbmi,
                                UINT iUsage,
                                DWORD rop);
}

/*
* File API
*/

//
// Constants
//
#define CREATE_NEW          1
#define CREATE_ALWAYS       2
#define OPEN_EXISTING       3
#define OPEN_ALWAYS         4
#define TRUNCATE_EXISTING   5

#define INVALID_FILE_SIZE ((DWORD)0xFFFFFFFF)
#define INVALID_SET_FILE_POINTER ((DWORD)-1)
#define INVALID_FILE_ATTRIBUTES ((DWORD)-1)

@link("kernel32.dll")
extern {
    
    LONG WINAPI CompareFileTime(const FILETIME* lpFileTime1, 
                                const FILETIME* lpFileTime2);
    
    BOOL WINAPI CreateDirectoryA(LPCSTR lpPathName,
                                 LPSECURITY_ATTRIBUTES lpSecurityAttributes);
    
    HANDLE WINAPI CreateFileA(LPCSTR lpFileName,
                              DWORD dwDesiredAccess,
                              DWORD dwShareMode,
                              LPSECURITY_ATTRIBUTES lpSecurityAttributes,
                              DWORD dwCreationDisposition,
                              DWORD dwFlagsAndAttributes,
                              HANDLE hTemplateFile);
    
    
    HANDLE WINAPI FindFirstFileA(LPCSTR lpFileName,
                                 LPWIN32_FIND_DATAA lpFindFileData);
    
    BOOL WINAPI FindClose(HANDLE hFindFile);
    
    DWORD WINAPI GetFileSize(HANDLE hFile,
                             LPDWORD lpFileSizeHigh);
    
    BOOL WINAPI GetFileSizeEx(HANDLE hFile,
                              PLARGE_INTEGER lpFileSize);
    
    DWORD WINAPI GetFileType(HANDLE hFile);
    
    BOOL WINAPI ReadFile(HANDLE hFile,
                         LPVOID lpBuffer,
                         DWORD nNumberOfBytesToRead,
                         LPDWORD lpNumberOfBytesRead,
                         LPOVERLAPPED lpOverlapped);
    BOOL WINAPI WriteFile(HANDLE hFile,
                          LPCVOID lpBuffer,
                          DWORD nNumberOfBytesToWrite,
                          LPDWORD lpNumberOfBytesWritten,
                          LPOVERLAPPED lpOverlapped);
}

/*
* Lib loader API
*/

/*
 * GUID
 */

typedef struct _GUID {
    unsigned long  Data1;
    unsigned short Data2;
    unsigned short Data3;
    unsigned char  Data4[ 8 ];
} GUID;
typedef GUID *LPGUID;
typedef const GUID *LPCGUID;

#define DEFINE_GUID(name, l, w1, w2, b1, b2, b3, b4, b5, b6, b7, b8) \
const GUID name = { l, w1, w2, { b1, b2,  b3,  b4,  b5,  b6,  b7,  b8 } }

typedef GUID IID;
typedef IID *LPIID;

#define REFIID const IID*

/*
 * Direct Sound API
 */

// DirectSound Component GUID {47D4D946-62E8-11CF-93BC-444553540000}
DEFINE_GUID(CLSID_DirectSound, 0x47d4d946, 0x62e8, 0x11cf, 0x93, 0xbc, 0x44, 0x45, 0x53, 0x54, 0x0, 0x0);

// DirectSound 8.0 Component GUID {3901CC3F-84B5-4FA4-BA35-AA8172B8A09B}
DEFINE_GUID(CLSID_DirectSound8, 0x3901cc3f, 0x84b5, 0x4fa4, 0xba, 0x35, 0xaa, 0x81, 0x72, 0xb8, 0xa0, 0x9b);

// DirectSound Capture Component GUID {B0210780-89CD-11D0-AF08-00A0C925CD16}
DEFINE_GUID(CLSID_DirectSoundCapture, 0xb0210780, 0x89cd, 0x11d0, 0xaf, 0x8, 0x0, 0xa0, 0xc9, 0x25, 0xcd, 0x16);

// DirectSound 8.0 Capture Component GUID {E4BCAC13-7F99-4908-9A8E-74E3BF24B6E1}
DEFINE_GUID(CLSID_DirectSoundCapture8, 0xe4bcac13, 0x7f99, 0x4908, 0x9a, 0x8e, 0x74, 0xe3, 0xbf, 0x24, 0xb6, 0xe1);

// DirectSound Full Duplex Component GUID {FEA4300C-7959-4147-B26A-2377B9E7A91D}
DEFINE_GUID(CLSID_DirectSoundFullDuplex, 0xfea4300c, 0x7959, 0x4147, 0xb2, 0x6a, 0x23, 0x77, 0xb9, 0xe7, 0xa9, 0x1d);


// DirectSound default playback device GUID {DEF00000-9C6D-47ED-AAF1-4DDA8F2B5C03}
DEFINE_GUID(DSDEVID_DefaultPlayback, 0xdef00000, 0x9c6d, 0x47ed, 0xaa, 0xf1, 0x4d, 0xda, 0x8f, 0x2b, 0x5c, 0x03);

// DirectSound default capture device GUID {DEF00001-9C6D-47ED-AAF1-4DDA8F2B5C03}
DEFINE_GUID(DSDEVID_DefaultCapture, 0xdef00001, 0x9c6d, 0x47ed, 0xaa, 0xf1, 0x4d, 0xda, 0x8f, 0x2b, 0x5c, 0x03);

// DirectSound default device for voice playback {DEF00002-9C6D-47ED-AAF1-4DDA8F2B5C03}
DEFINE_GUID(DSDEVID_DefaultVoicePlayback, 0xdef00002, 0x9c6d, 0x47ed, 0xaa, 0xf1, 0x4d, 0xda, 0x8f, 0x2b, 0x5c, 0x03);

// DirectSound default device for voice capture {DEF00003-9C6D-47ED-AAF1-4DDA8F2B5C03}
DEFINE_GUID(DSDEVID_DefaultVoiceCapture, 0xdef00003, 0x9c6d, 0x47ed, 0xaa, 0xf1, 0x4d, 0xda, 0x8f, 0x2b, 0x5c, 0x03);


//
// Return Codes
//

// The function completed successfully
#define DS_OK                           S_OK

// The call succeeded, but we had to substitute the 3D algorithm
#define DS_NO_VIRTUALIZATION            MAKE_HRESULT(0, _FACDS, 10)

// The call failed because resources (such as a priority level)
// were already being used by another caller
#define DSERR_ALLOCATED                 MAKE_DSHRESULT(10)

// The control (vol, pan, etc.) requested by the caller is not available
#define DSERR_CONTROLUNAVAIL            MAKE_DSHRESULT(30)

// An invalid parameter was passed to the returning function
#define DSERR_INVALIDPARAM              E_INVALIDARG

// This call is not valid for the current state of this object
#define DSERR_INVALIDCALL               MAKE_DSHRESULT(50)

// An undetermined error occurred inside the DirectSound subsystem
#define DSERR_GENERIC                   E_FAIL

// The caller does not have the priority level required for the function to
// succeed
#define DSERR_PRIOLEVELNEEDED           MAKE_DSHRESULT(70)

// Not enough free memory is available to complete the operation
#define DSERR_OUTOFMEMORY               E_OUTOFMEMORY

// The specified WAVE format is not supported
#define DSERR_BADFORMAT                 MAKE_DSHRESULT(100)

// The function called is not supported at this time
#define DSERR_UNSUPPORTED               E_NOTIMPL

// No sound driver is available for use
#define DSERR_NODRIVER                  MAKE_DSHRESULT(120)

// This object is already initialized
#define DSERR_ALREADYINITIALIZED        MAKE_DSHRESULT(130)

// This object does not support aggregation
#define DSERR_NOAGGREGATION             CLASS_E_NOAGGREGATION

// The buffer memory has been lost, and must be restored
#define DSERR_BUFFERLOST                MAKE_DSHRESULT(150)

// Another app has a higher priority level, preventing this call from
// succeeding
#define DSERR_OTHERAPPHASPRIO           MAKE_DSHRESULT(160)

// This object has not been initialized
#define DSERR_UNINITIALIZED             MAKE_DSHRESULT(170)

// The requested COM interface is not available
#define DSERR_NOINTERFACE               E_NOINTERFACE

// Access is denied
#define DSERR_ACCESSDENIED              E_ACCESSDENIED

// Tried to create a DSBCAPS_CTRLFX buffer shorter than DSBSIZE_FX_MIN milliseconds
#define DSERR_BUFFERTOOSMALL            MAKE_DSHRESULT(180)

// Attempt to use DirectSound 8 functionality on an older DirectSound object
#define DSERR_DS8_REQUIRED              MAKE_DSHRESULT(190)

// A circular loop of send effects was detected
#define DSERR_SENDLOOP                  MAKE_DSHRESULT(200)

// The GUID specified in an audiopath file does not match a valid MIXIN buffer
#define DSERR_BADSENDBUFFERGUID         MAKE_DSHRESULT(210)

// The object requested was not found (numerically equal to DMUS_E_NOT_FOUND)
#define DSERR_OBJECTNOTFOUND            MAKE_DSHRESULT(4449)

// The effects requested could not be found on the system, or they were found
// but in the wrong order, or in the wrong hardware/software locations.
#define DSERR_FXUNAVAILABLE             MAKE_DSHRESULT(220)

//
// Flags
//

#define DSCAPS_PRIMARYMONO          0x00000001
#define DSCAPS_PRIMARYSTEREO        0x00000002
#define DSCAPS_PRIMARY8BIT          0x00000004
#define DSCAPS_PRIMARY16BIT         0x00000008
#define DSCAPS_CONTINUOUSRATE       0x00000010
#define DSCAPS_EMULDRIVER           0x00000020
#define DSCAPS_CERTIFIED            0x00000040
#define DSCAPS_SECONDARYMONO        0x00000100
#define DSCAPS_SECONDARYSTEREO      0x00000200
#define DSCAPS_SECONDARY8BIT        0x00000400
#define DSCAPS_SECONDARY16BIT       0x00000800

#define DSSCL_NORMAL                0x00000001
#define DSSCL_PRIORITY              0x00000002
#define DSSCL_EXCLUSIVE             0x00000003
#define DSSCL_WRITEPRIMARY          0x00000004

#define DSSPEAKER_DIRECTOUT         0x00000000
#define DSSPEAKER_HEADPHONE         0x00000001
#define DSSPEAKER_MONO              0x00000002
#define DSSPEAKER_QUAD              0x00000003
#define DSSPEAKER_STEREO            0x00000004
#define DSSPEAKER_SURROUND          0x00000005
#define DSSPEAKER_5POINT1           0x00000006  // obsolete 5.1 setting
#define DSSPEAKER_7POINT1           0x00000007  // obsolete 7.1 setting
#define DSSPEAKER_7POINT1_SURROUND  0x00000008  // correct 7.1 Home Theater setting
#define DSSPEAKER_5POINT1_SURROUND  0x00000009  // correct 5.1 setting
#define DSSPEAKER_7POINT1_WIDE      DSSPEAKER_7POINT1
#define DSSPEAKER_5POINT1_BACK      DSSPEAKER_5POINT1

#define DSSPEAKER_GEOMETRY_MIN      0x00000005  //   5 degrees
#define DSSPEAKER_GEOMETRY_NARROW   0x0000000A  //  10 degrees
#define DSSPEAKER_GEOMETRY_WIDE     0x00000014  //  20 degrees
#define DSSPEAKER_GEOMETRY_MAX      0x000000B4  // 180 degrees

#define DSSPEAKER_COMBINED(c, g)    ((DWORD)(((BYTE)(c)) | ((DWORD)((BYTE)(g))) << 16))
#define DSSPEAKER_CONFIG(a)         ((BYTE)(a))
#define DSSPEAKER_GEOMETRY(a)       ((BYTE)(((DWORD)(a) >> 16) & 0x00FF))

#define DSBCAPS_PRIMARYBUFFER       0x00000001
#define DSBCAPS_STATIC              0x00000002
#define DSBCAPS_LOCHARDWARE         0x00000004
#define DSBCAPS_LOCSOFTWARE         0x00000008
#define DSBCAPS_CTRL3D              0x00000010
#define DSBCAPS_CTRLFREQUENCY       0x00000020
#define DSBCAPS_CTRLPAN             0x00000040
#define DSBCAPS_CTRLVOLUME          0x00000080
#define DSBCAPS_CTRLPOSITIONNOTIFY  0x00000100
#define DSBCAPS_CTRLFX              0x00000200
#define DSBCAPS_STICKYFOCUS         0x00004000
#define DSBCAPS_GLOBALFOCUS         0x00008000
#define DSBCAPS_GETCURRENTPOSITION2 0x00010000
#define DSBCAPS_MUTE3DATMAXDISTANCE 0x00020000
#define DSBCAPS_LOCDEFER            0x00040000
#define DSBCAPS_TRUEPLAYPOSITION    0x00080000

#define DSBPLAY_LOOPING             0x00000001
#define DSBPLAY_LOCHARDWARE         0x00000002
#define DSBPLAY_LOCSOFTWARE         0x00000004
#define DSBPLAY_TERMINATEBY_TIME    0x00000008
#define DSBPLAY_TERMINATEBY_DISTANCE    0x000000010
#define DSBPLAY_TERMINATEBY_PRIORITY    0x000000020

#define DSBSTATUS_PLAYING           0x00000001
#define DSBSTATUS_BUFFERLOST        0x00000002
#define DSBSTATUS_LOOPING           0x00000004
#define DSBSTATUS_LOCHARDWARE       0x00000008
#define DSBSTATUS_LOCSOFTWARE       0x00000010
#define DSBSTATUS_TERMINATED        0x00000020

#define DSBLOCK_FROMWRITECURSOR     0x00000001
#define DSBLOCK_ENTIREBUFFER        0x00000002

#define DSBFREQUENCY_ORIGINAL       0
#define DSBFREQUENCY_MIN            100
#if DIRECTSOUND_VERSION >= 0x0900
#define DSBFREQUENCY_MAX            200000
#else
#define DSBFREQUENCY_MAX            100000
#endif

#define DSBPAN_LEFT                 -10000
#define DSBPAN_CENTER               0
#define DSBPAN_RIGHT                10000

#define DSBVOLUME_MIN               -10000
#define DSBVOLUME_MAX               0

#define DSBSIZE_MIN                 4
#define DSBSIZE_MAX                 0x0FFFFFFF
#define DSBSIZE_FX_MIN              150  // NOTE: Milliseconds, not bytes

#define DSBNOTIFICATIONS_MAX        100000UL

#define DS3DMODE_NORMAL             0x00000000
#define DS3DMODE_HEADRELATIVE       0x00000001
#define DS3DMODE_DISABLE            0x00000002

#define DS3D_IMMEDIATE              0x00000000
#define DS3D_DEFERRED               0x00000001

#define DS3D_MINDISTANCEFACTOR      FLT_MIN
#define DS3D_MAXDISTANCEFACTOR      FLT_MAX
#define DS3D_DEFAULTDISTANCEFACTOR  1.0f

#define DS3D_MINROLLOFFFACTOR       0.0f
#define DS3D_MAXROLLOFFFACTOR       10.0f
#define DS3D_DEFAULTROLLOFFFACTOR   1.0f

#define DS3D_MINDOPPLERFACTOR       0.0f
#define DS3D_MAXDOPPLERFACTOR       10.0f
#define DS3D_DEFAULTDOPPLERFACTOR   1.0f

#define DS3D_DEFAULTMINDISTANCE     1.0f
#define DS3D_DEFAULTMAXDISTANCE     1000000000.0f

#define DS3D_MINCONEANGLE           0
#define DS3D_MAXCONEANGLE           360
#define DS3D_DEFAULTCONEANGLE       360

#define DS3D_DEFAULTCONEOUTSIDEVOLUME DSBVOLUME_MAX

// IDirectSoundCapture attributes

#define DSCCAPS_EMULDRIVER          DSCAPS_EMULDRIVER
#define DSCCAPS_CERTIFIED           DSCAPS_CERTIFIED
#define DSCCAPS_MULTIPLECAPTURE     0x00000001

// IDirectSoundCaptureBuffer attributes

#define DSCBCAPS_WAVEMAPPED         0x80000000
#if DIRECTSOUND_VERSION >= 0x0800
#define DSCBCAPS_CTRLFX             0x00000200
#endif

#define DSCBLOCK_ENTIREBUFFER       0x00000001

#define DSCBSTATUS_CAPTURING        0x00000001
#define DSCBSTATUS_LOOPING          0x00000002

#define DSCBSTART_LOOPING           0x00000001

#define DSBPN_OFFSETSTOP            0xFFFFFFFF

#define DS_CERTIFIED                0x00000000
#define DS_UNCERTIFIED              0x00000001

struct DSBUFFERDESC
{
    DWORD           dwSize;
    DWORD           dwFlags;
    DWORD           dwBufferBytes;
    DWORD           dwReserved;
    LPWAVEFORMATEX  lpwfxFormat;
    GUID            guid3DAlgorithm;
}
typedef DSBUFFERDESC *LPDSBUFFERDESC;

typedef const DSBUFFERDESC *LPCDSBUFFERDESC;

typedef struct _DSCAPS
{
    DWORD           dwSize;
    DWORD           dwFlags;
    DWORD           dwMinSecondarySampleRate;
    DWORD           dwMaxSecondarySampleRate;
    DWORD           dwPrimaryBuffers;
    DWORD           dwMaxHwMixingAllBuffers;
    DWORD           dwMaxHwMixingStaticBuffers;
    DWORD           dwMaxHwMixingStreamingBuffers;
    DWORD           dwFreeHwMixingAllBuffers;
    DWORD           dwFreeHwMixingStaticBuffers;
    DWORD           dwFreeHwMixingStreamingBuffers;
    DWORD           dwMaxHw3DAllBuffers;
    DWORD           dwMaxHw3DStaticBuffers;
    DWORD           dwMaxHw3DStreamingBuffers;
    DWORD           dwFreeHw3DAllBuffers;
    DWORD           dwFreeHw3DStaticBuffers;
    DWORD           dwFreeHw3DStreamingBuffers;
    DWORD           dwTotalHwMemBytes;
    DWORD           dwFreeHwMemBytes;
    DWORD           dwMaxContigFreeHwMemBytes;
    DWORD           dwUnlockTransferRateHwBuffers;
    DWORD           dwPlayCpuOverheadSwBuffers;
    DWORD           dwReserved1;
    DWORD           dwReserved2;
} DSCAPS;
typedef DSCAPS *LPDSCAPS;
typedef const DSCAPS *LPCDSCAPS;

typedef struct _DSBCAPS
{
    DWORD           dwSize;
    DWORD           dwFlags;
    DWORD           dwBufferBytes;
    DWORD           dwUnlockTransferRate;
    DWORD           dwPlayCpuOverhead;
} DSBCAPS;
typedef DSBCAPS *LPDSBCAPS;
typedef const DSBCAPS *LPCDSBCAPS;


// Part of COM API
struct IUnknownVtbl
{
    HRESULT* STDMETHODCALLTYPE QueryInterface(IUnknown* This,
                                              /* [in] */ REFIID riid,
                                              /* [annotation][iid_is][out] */ 
                                              void **ppvObject);
    
    ULONG* STDMETHODCALLTYPE AddRef(IUnknown* This);
    
    ULONG* STDMETHODCALLTYPE Release(IUnknown* This);
}

struct IUnknown
{
    IUnknownVtbl* lpVtbl;
}

typedef /* [unique] */ IUnknown *LPUNKNOWN;

#define STDMETHOD(method)       HRESULT method
#define STDMETHOD_(type,method) type method
#define THIS_                   void* This,
#define THIS                    void* This

#ifdef CONST_VTABLE
#undef CONST_VTBL
#define CONST_VTBL const
#define DECLARE_INTERFACE(iface) struct iface { \
const iface##Vtbl FAR* lpVtbl; \
} \
const iface##Vtbl
#else
#undef CONST_VTBL
#define CONST_VTBL
#define DECLARE_INTERFACE(iface) struct iface { \
iface##Vtbl FAR* lpVtbl; \
} \
struct iface##Vtbl
#endif
#define DECLARE_INTERFACE_(iface, baseiface)    DECLARE_INTERFACE(iface)
#define DECLARE_INTERFACE_IID(iface, iid)               DECLARE_INTERFACE(iface)
#define DECLARE_INTERFACE_IID_(iface, baseiface, iid)   DECLARE_INTERFACE_(iface, baseiface)


DEFINE_GUID(IID_IDirectSound, 0x279AFA83, 0x4981, 0x11CE, 0xA5, 0x21, 0x00, 0x20, 0xAF, 0x0B, 0xE5, 0x60);

#undef INTERFACE
#define INTERFACE IDirectSound

DECLARE_INTERFACE_(IDirectSound, IUnknown)
{
    // IUnknown methods
    STDMETHOD(QueryInterface)       (THIS_  REFIID,  LPVOID*);
    STDMETHOD_(ULONG,AddRef)        (THIS);
    STDMETHOD_(ULONG,Release)       (THIS);
    
    // IDirectSound methods
    STDMETHOD(CreateSoundBuffer)    (THIS_  LPCDSBUFFERDESC pcDSBufferDesc,  LPDIRECTSOUNDBUFFER *ppDSBuffer, LPUNKNOWN pUnkOuter);
    STDMETHOD(GetCaps)              (THIS_  LPDSCAPS pDSCaps);
    STDMETHOD(DuplicateSoundBuffer) (THIS_  LPDIRECTSOUNDBUFFER pDSBufferOriginal,  LPDIRECTSOUNDBUFFER *ppDSBufferDuplicate);
    STDMETHOD(SetCooperativeLevel)  (THIS_ HWND hwnd, DWORD dwLevel);
    STDMETHOD(Compact)              (THIS);
    STDMETHOD(GetSpeakerConfig)     (THIS_ LPDWORD pdwSpeakerConfig);
    STDMETHOD(SetSpeakerConfig)     (THIS_ DWORD dwSpeakerConfig);
    STDMETHOD(Initialize)           (THIS_ LPCGUID pcGuidDevice);
};


#define IDirectSound_QueryInterface(p,a,b)       IUnknown_QueryInterface((void*) p,a,b)
#define IDirectSound_AddRef(p)                   IUnknown_AddRef((void*) p)
#define IDirectSound_Release(p)                  IUnknown_Release((void*) p)
#define IDirectSound_CreateSoundBuffer(p,a,b,c)  (p)->lpVtbl->CreateSoundBuffer((void*) p,a,b,c)
#define IDirectSound_GetCaps(p,a)                (p)->lpVtbl->GetCaps((void*) p,a)
#define IDirectSound_DuplicateSoundBuffer(p,a,b) (p)->lpVtbl->DuplicateSoundBuffer((void*) p,a,b)
#define IDirectSound_SetCooperativeLevel(p,a,b)  (p)->lpVtbl->SetCooperativeLevel((void*) p,a,b)
#define IDirectSound_Compact(p)                  (p)->lpVtbl->Compact((void*) p)
#define IDirectSound_GetSpeakerConfig(p,a)       (p)->lpVtbl->GetSpeakerConfig((void*) p,a)
#define IDirectSound_SetSpeakerConfig(p,b)       (p)->lpVtbl->SetSpeakerConfig((void*) p,b)
#define IDirectSound_Initialize(p,a)             (p)->lpVtbl->Initialize((void*) p,a)

DEFINE_GUID(IID_IDirectSoundBuffer, 0x279AFA85, 0x4981, 0x11CE, 0xA5, 0x21, 0x00, 0x20, 0xAF, 0x0B, 0xE5, 0x60);

#undef INTERFACE
#define INTERFACE IDirectSoundBuffer

DECLARE_INTERFACE_(IDirectSoundBuffer, IUnknown)
{
    // IUnknown methods
    STDMETHOD(QueryInterface)       (THIS_ REFIID, LPVOID*);
    STDMETHOD_(ULONG,AddRef)        (THIS);
    STDMETHOD_(ULONG,Release)       (THIS);
    
    // IDirectSoundBuffer methods
    STDMETHOD(GetCaps)              (THIS_ LPDSBCAPS pDSBufferCaps);
    STDMETHOD(GetCurrentPosition)   (THIS_ LPDWORD pdwCurrentPlayCursor, LPDWORD pdwCurrentWriteCursor);
    STDMETHOD(GetFormat)            (THIS_ LPWAVEFORMATEX pwfxFormat, DWORD dwSizeAllocated, LPDWORD pdwSizeWritten);
    STDMETHOD(GetVolume)            (THIS_  LPLONG plVolume);
    STDMETHOD(GetPan)               (THIS_  LPLONG plPan);
    STDMETHOD(GetFrequency)         (THIS_  LPDWORD pdwFrequency);
    STDMETHOD(GetStatus)            (THIS_  LPDWORD pdwStatus);
    STDMETHOD(Initialize)           (THIS_  LPDIRECTSOUND pDirectSound, LPCDSBUFFERDESC pcDSBufferDesc);
    STDMETHOD(Lock)                 (THIS_ DWORD dwOffset, DWORD dwBytes,
                                     LPVOID *ppvAudioPtr1, LPDWORD pdwAudioBytes1,
                                     LPVOID *ppvAudioPtr2, LPDWORD pdwAudioBytes2, DWORD dwFlags);
    STDMETHOD(Play)                 (THIS_ DWORD dwReserved1, DWORD dwPriority, DWORD dwFlags);
    STDMETHOD(SetCurrentPosition)   (THIS_ DWORD dwNewPosition);
    STDMETHOD(SetFormat)            (THIS_ LPCWAVEFORMATEX pcfxFormat);
    STDMETHOD(SetVolume)            (THIS_ LONG lVolume);
    STDMETHOD(SetPan)               (THIS_ LONG lPan);
    STDMETHOD(SetFrequency)         (THIS_ DWORD dwFrequency);
    STDMETHOD(Stop)                 (THIS);
    STDMETHOD(Unlock)               (THIS_  LPVOID pvAudioPtr1, DWORD dwAudioBytes1, LPVOID pvAudioPtr2, DWORD dwAudioBytes2);
    STDMETHOD(Restore)              (THIS);
};

#define IDirectSoundBuffer_QueryInterface(p,a,b)        IUnknown_QueryInterface((void*) p,a,b)
#define IDirectSoundBuffer_AddRef(p)                    IUnknown_AddRef((void*) p)
#define IDirectSoundBuffer_Release(p)                   IUnknown_Release((void*) p)
#define IDirectSoundBuffer_GetCaps(p,a)                 (p)->lpVtbl->GetCaps((void*) p,a)
#define IDirectSoundBuffer_GetCurrentPosition(p,a,b)    (p)->lpVtbl->GetCurrentPosition((void*) p,a,b)
#define IDirectSoundBuffer_GetFormat(p,a,b,c)           (p)->lpVtbl->GetFormat((void*) p,a,b,c)
#define IDirectSoundBuffer_GetVolume(p,a)               (p)->lpVtbl->GetVolume((void*) p,a)
#define IDirectSoundBuffer_GetPan(p,a)                  (p)->lpVtbl->GetPan((void*) p,a)
#define IDirectSoundBuffer_GetFrequency(p,a)            (p)->lpVtbl->GetFrequency((void*) p,a)
#define IDirectSoundBuffer_GetStatus(p,a)               (p)->lpVtbl->GetStatus((void*) p,a)
#define IDirectSoundBuffer_Initialize(p,a,b)            (p)->lpVtbl->Initialize((void*) p,a,b)
#define IDirectSoundBuffer_Lock(p,a,b,c,d,e,f,g)        (p)->lpVtbl->Lock((void*) p,a,b,c,d,e,f,g)
#define IDirectSoundBuffer_Play(p,a,b,c)                (p)->lpVtbl->Play((void*) p,a,b,c)
#define IDirectSoundBuffer_SetCurrentPosition(p,a)      (p)->lpVtbl->SetCurrentPosition((void*) p,a)
#define IDirectSoundBuffer_SetFormat(p,a)               (p)->lpVtbl->SetFormat((void*) p,a)
#define IDirectSoundBuffer_SetVolume(p,a)               (p)->lpVtbl->SetVolume((void*) p,a)
#define IDirectSoundBuffer_SetPan(p,a)                  (p)->lpVtbl->SetPan((void*) p,a)
#define IDirectSoundBuffer_SetFrequency(p,a)            (p)->lpVtbl->SetFrequency((void*) p,a)
#define IDirectSoundBuffer_Stop(p)                      (p)->lpVtbl->Stop((void*) p)
#define IDirectSoundBuffer_Unlock(p,a,b,c,d)            (p)->lpVtbl->Unlock((void*) p,a,b,c,d)
#define IDirectSoundBuffer_Restore(p)                   (p)->lpVtbl->Restore((void*) p)

#undef INTERFACE

typedef IDirectSound                 *LPDIRECTSOUND;
typedef IDirectSoundBuffer           *LPDIRECTSOUNDBUFFER;

/* OLD general waveform format structure (information common to all formats) */
typedef struct waveformat_tag {
    WORD    wFormatTag;        /* format type */
    WORD    nChannels;         /* number of channels (i.e. mono, stereo, etc.) */
    DWORD   nSamplesPerSec;    /* sample rate */
    DWORD   nAvgBytesPerSec;   /* for buffer estimation */
    WORD    nBlockAlign;       /* block size of data */
} WAVEFORMAT;
//typedef WAVEFORMAT *PWAVEFORMAT, NEAR *NPWAVEFORMAT, FAR *LPWAVEFORMAT;

/* flags for wFormatTag field of WAVEFORMAT */
#define WAVE_FORMAT_PCM     1

typedef struct tWAVEFORMATEX
{
    WORD        wFormatTag;         /* format type */
    WORD        nChannels;          /* number of channels (i.e. mono, stereo...) */
    DWORD       nSamplesPerSec;     /* sample rate */
    DWORD       nAvgBytesPerSec;    /* for buffer estimation */
    WORD        nBlockAlign;        /* block size of data */
    WORD        wBitsPerSample;     /* number of bits per sample of mono data */
    WORD        cbSize;             /* the count in bytes of the size of */
    /* extra information (after cbSize) */
} WAVEFORMATEX;
typedef WAVEFORMATEX *PWAVEFORMATEX;
typedef WAVEFORMATEX *NPWAVEFORMATEX;
typedef WAVEFORMATEX *LPWAVEFORMATEX;
typedef const WAVEFORMATEX *LPCWAVEFORMATEX;


/*
* XInput
*/

#define XINPUT_DEVTYPE_GAMEPAD    0x01

#define XINPUT_DEVSUBTYPE_UNKNOWN           0x00
#define XINPUT_DEVSUBTYPE_GAMEPAD           0x01
#define XINPUT_DEVSUBTYPE_WHEEL             0x02
#define XINPUT_DEVSUBTYPE_ARCADE_STICK      0x03
#define XINPUT_DEVSUBTYPE_FLIGHT_STICK      0x04
#define XINPUT_DEVSUBTYPE_DANCE_PAD         0x05
#define XINPUT_DEVSUBTYPE_GUITAR            0x06
#define XINPUT_DEVSUBTYPE_GUITAR_ALTERNATE  0x07
#define XINPUT_DEVSUBTYPE_DRUM_KIT          0x08
#define XINPUT_DEVSUBTYPE_GUITAR_BASS       0x0B
#define XINPUT_DEVSUBTYPE_ARCADE_PAD        0x13

#define XINPUT_CAPS_VOICE_SUPPORTED     0x0004
#define XINPUT_CAPS_FFB_SUPPORTED       0x0001
#define XINPUT_CAPS_WIRELESS            0x0002
#define XINPUT_CAPS_PMD_SUPPORTED       0x0008
#define XINPUT_CAPS_NO_NAVIGATION       0x0010

#define XINPUT_GAMEPAD_DPAD_UP          0x0001
#define XINPUT_GAMEPAD_DPAD_DOWN        0x0002
#define XINPUT_GAMEPAD_DPAD_LEFT        0x0004
#define XINPUT_GAMEPAD_DPAD_RIGHT       0x0008
#define XINPUT_GAMEPAD_START            0x0010
#define XINPUT_GAMEPAD_BACK             0x0020
#define XINPUT_GAMEPAD_LEFT_THUMB       0x0040
#define XINPUT_GAMEPAD_RIGHT_THUMB      0x0080
#define XINPUT_GAMEPAD_LEFT_SHOULDER    0x0100
#define XINPUT_GAMEPAD_RIGHT_SHOULDER   0x0200
#define XINPUT_GAMEPAD_A                0x1000
#define XINPUT_GAMEPAD_B                0x2000
#define XINPUT_GAMEPAD_X                0x4000
#define XINPUT_GAMEPAD_Y                0x8000

#define XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE  7849
#define XINPUT_GAMEPAD_RIGHT_THUMB_DEADZONE 8689
#define XINPUT_GAMEPAD_TRIGGER_THRESHOLD    30

#define XINPUT_FLAG_GAMEPAD             0x00000001

#define BATTERY_DEVTYPE_GAMEPAD         0x00
#define BATTERY_DEVTYPE_HEADSET         0x01

#define BATTERY_TYPE_DISCONNECTED       0x00    // This device is not connected
#define BATTERY_TYPE_WIRED              0x01    // Wired device, no battery
#define BATTERY_TYPE_ALKALINE           0x02    // Alkaline battery source
#define BATTERY_TYPE_NIMH               0x03    // Nickel Metal Hydride battery source
#define BATTERY_TYPE_UNKNOWN            0xFF    // Cannot determine the battery type

#define BATTERY_LEVEL_EMPTY             0x00
#define BATTERY_LEVEL_LOW               0x01
#define BATTERY_LEVEL_MEDIUM            0x02
#define BATTERY_LEVEL_FULL              0x03

#define XUSER_MAX_COUNT                 4

#define XUSER_INDEX_ANY                 0x000000FF

#define VK_PAD_A                        0x5800
#define VK_PAD_B                        0x5801
#define VK_PAD_X                        0x5802
#define VK_PAD_Y                        0x5803
#define VK_PAD_RSHOULDER                0x5804
#define VK_PAD_LSHOULDER                0x5805
#define VK_PAD_LTRIGGER                 0x5806
#define VK_PAD_RTRIGGER                 0x5807

#define VK_PAD_DPAD_UP                  0x5810
#define VK_PAD_DPAD_DOWN                0x5811
#define VK_PAD_DPAD_LEFT                0x5812
#define VK_PAD_DPAD_RIGHT               0x5813
#define VK_PAD_START                    0x5814
#define VK_PAD_BACK                     0x5815
#define VK_PAD_LTHUMB_PRESS             0x5816
#define VK_PAD_RTHUMB_PRESS             0x5817

#define VK_PAD_LTHUMB_UP                0x5820
#define VK_PAD_LTHUMB_DOWN              0x5821
#define VK_PAD_LTHUMB_RIGHT             0x5822
#define VK_PAD_LTHUMB_LEFT              0x5823
#define VK_PAD_LTHUMB_UPLEFT            0x5824
#define VK_PAD_LTHUMB_UPRIGHT           0x5825
#define VK_PAD_LTHUMB_DOWNRIGHT         0x5826
#define VK_PAD_LTHUMB_DOWNLEFT          0x5827

#define VK_PAD_RTHUMB_UP                0x5830
#define VK_PAD_RTHUMB_DOWN              0x5831
#define VK_PAD_RTHUMB_RIGHT             0x5832
#define VK_PAD_RTHUMB_LEFT              0x5833
#define VK_PAD_RTHUMB_UPLEFT            0x5834
#define VK_PAD_RTHUMB_UPRIGHT           0x5835
#define VK_PAD_RTHUMB_DOWNRIGHT         0x5836
#define VK_PAD_RTHUMB_DOWNLEFT          0x5837

#define XINPUT_KEYSTROKE_KEYDOWN        0x0001
#define XINPUT_KEYSTROKE_KEYUP          0x0002
#define XINPUT_KEYSTROKE_REPEAT         0x0004

typedef struct _XINPUT_GAMEPAD
{
    WORD                                wButtons;
    BYTE                                bLeftTrigger;
    BYTE                                bRightTrigger;
    SHORT                               sThumbLX;
    SHORT                               sThumbLY;
    SHORT                               sThumbRX;
    SHORT                               sThumbRY;
} XINPUT_GAMEPAD// *PXINPUT_GAMEPAD;

typedef struct _XINPUT_STATE
{
    DWORD                               dwPacketNumber;
    XINPUT_GAMEPAD                      Gamepad;
} XINPUT_STATE;// *PXINPUT_STATE;

typedef struct _XINPUT_VIBRATION
{
    WORD                                wLeftMotorSpeed;
    WORD                                wRightMotorSpeed;
} XINPUT_VIBRATION;//, *PXINPUT_VIBRATION;

typedef struct _XINPUT_CAPABILITIES
{
    BYTE                                Type;
    BYTE                                SubType;
    WORD                                Flags;
    XINPUT_GAMEPAD                      Gamepad;
    XINPUT_VIBRATION                    Vibration;
} XINPUT_CAPABILITIES;//, *PXINPUT_CAPABILITIES;

typedef struct _XINPUT_BATTERY_INFORMATION
{
    BYTE BatteryType;
    BYTE BatteryLevel;
} XINPUT_BATTERY_INFORMATION;// *PXINPUT_BATTERY_INFORMATION;

typedef struct _XINPUT_KEYSTROKE
{
    WORD    VirtualKey;
    WCHAR   Unicode;
    WORD    Flags;
    BYTE    UserIndex;
    BYTE    HidCode;
} XINPUT_KEYSTROKE;//, *PXINPUT_KEYSTROKE;