// Tiny version of windows.h because windows.h is a pain to parse!!!

typedef cstring LPCSTR;

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
typedef void* LPVOID;
typedef const void* LPCVOID;

typedef int                 INT;
typedef unsigned int        UINT;
typedef char CHAR;
typedef short SHORT;
typedef unsigned int        *PUINT;
typedef unsigned long POINTER_64_INT;
typedef signed char         INT8, *PINT8;
typedef signed short        INT16, *PINT16;
typedef signed int          INT32, *PINT32;
typedef signed __int64      INT64, *PINT64;
typedef unsigned char       UINT8, *PUINT8;
typedef unsigned short      UINT16, *PUINT16;
typedef unsigned int        UINT32, *PUINT32;
typedef unsigned __int64    UINT64, *PUINT64;
typedef signed int LONG32, *PLONG32;
typedef unsigned int ULONG32, *PULONG32;
typedef unsigned int DWORD32, *PDWORD32;
typedef s64 INT_PTR, *PINT_PTR;
typedef s64 UINT_PTR, *PUINT_PTR;
typedef s64 LONG_PTR, *PLONG_PTR;
typedef s64 ULONG_PTR, *PULONG_PTR;
typedef ULONG_PTR SIZE_T, *PSIZE_T;
typedef void* __ptr64 HANDLE64;
typedef HANDLE64 *PHANDLE64;
typedef void *HANDLE;
typedef HANDLE *PHANDLE;

typedef unsigned short UHALF_PTR, *PUHALF_PTR;
typedef short HALF_PTR, *PHALF_PTR;
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
typedef struct _FILETIME {
    DWORD dwLowDateTime;
    DWORD dwHighDateTime;
} FILETIME, *PFILETIME, *LPFILETIME;

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
} RECT, *PRECT,  *NPRECT;
typedef RECT* LPRECT;
typedef const RECT * LPCRECT;
typedef struct _RECTL       
{
    LONG    left;
    LONG    top;
    LONG    right;
    LONG    bottom;
} RECTL, *PRECTL, *LPRECTL;
typedef const RECTL * LPCRECTL;
typedef struct tagPOINT
{
    LONG  x;
    LONG  y;
} POINT, *PPOINT,  *NPPOINT,  *LPPOINT;
typedef struct _POINTL      
{
    LONG  x;
    LONG  y;
} POINTL, *PPOINTL;
typedef struct tagSIZE
{
    LONG        cx;
    LONG        cy;
} SIZE, *PSIZE, *LPSIZE;
typedef SIZE               SIZEL;
typedef SIZE               *PSIZEL, *LPSIZEL;
typedef struct tagPOINTS
{
    SHORT   x;
    SHORT   y;
} POINTS, *PPOINTS, *LPPOINTS;


//typedef LRESULT __stdcall(HWND, UINT, WPARAM, LPARAM)* WNDPROC;
typedef LRESULT __stdcall (HWND, UINT, WPARAM, LPARAM) WNDPROC;



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
} WNDCLASSA, *PWNDCLASSA,  *NPWNDCLASSA,  *LPWNDCLASSA;

typedef struct tagRGBTRIPLE {
    BYTE    rgbtBlue;
    BYTE    rgbtGreen;
    BYTE    rgbtRed;
} RGBTRIPLE, *PRGBTRIPLE,  *NPRGBTRIPLE,  *LPRGBTRIPLE;

typedef struct tagRGBQUAD {
    BYTE    rgbBlue;
    BYTE    rgbGreen;
    BYTE    rgbRed;
    BYTE    rgbReserved;
} RGBQUAD;

typedef RGBQUAD * LPRGBQUAD;
typedef LONG   LCSCSTYPE;
typedef LONG    LCSGAMUTMATCH;
typedef long            FXPT16DOT16,  *LPFXPT16DOT16;
typedef long            FXPT2DOT30,  *LPFXPT2DOT30;

typedef struct tagCIEXYZ
{
    FXPT2DOT30 ciexyzX;
    FXPT2DOT30 ciexyzY;
    FXPT2DOT30 ciexyzZ;
} CIEXYZ;
typedef CIEXYZ   *LPCIEXYZ;

typedef struct tagICEXYZTRIPLE
{
    CIEXYZ  ciexyzRed;
    CIEXYZ  ciexyzGreen;
    CIEXYZ  ciexyzBlue;
} CIEXYZTRIPLE;
typedef CIEXYZTRIPLE     *LPCIEXYZTRIPLE;

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
} BITMAPINFOHEADER,  *LPBITMAPINFOHEADER, *PBITMAPINFOHEADER;

typedef struct tagBITMAPINFO {
    BITMAPINFOHEADER    bmiHeader;
    RGBQUAD             bmiColors[1];
} BITMAPINFO,  *LPBITMAPINFO, *PBITMAPINFO;

typedef struct tagMSG {
    HWND        hwnd;
    UINT        message;
    WPARAM      wParam;
    LPARAM      lParam;
    DWORD       time;
    POINT       pt;
} MSG, *PMSG, *NPMSG;
typedef MSG* LPMSG;

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
    
    BOOL __stdcall GetWindowRect(HWND hWnd, LPRECT lpRect);
    
    BOOL __stdcall GetClientRect(HWND hWnd, LPRECT lpRect);
    
    BOOL ShowWindow(HWND hWnd, int nCmdShow);
    
    HDC __stdcall GetDC(HWND hWnd);
    
}

@link("Gdi32.dll")
extern {
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