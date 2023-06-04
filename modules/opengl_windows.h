
@link("opengl32.dll")
extern {
    HGLRC __stdcall wglCreateContext(HDC hdc);
    
    BOOL __stdcall wglDeleteContext(HGLRC hglrc);
    
    BOOL __stdcall wglMakeCurrent(HDC hdc, HGLRC hglrc);
    
    void* __stdcall wglGetProcAddress(LPCSTR function_name);
}


// Accepted as an attribute name in <*attribList>:
#define WGL_CONTEXT_MAJOR_VERSION_ARB           0x2091
#define WGL_CONTEXT_MINOR_VERSION_ARB           0x2092
#define WGL_CONTEXT_LAYER_PLANE_ARB             0x2093
#define WGL_CONTEXT_FLAGS_ARB                   0x2094
#define WGL_CONTEXT_PROFILE_MASK_ARB            0x9126

// WGL_CONTEXT_FLAGS in <*attribList>:
#define WGL_CONTEXT_DEBUG_BIT_ARB               0x0001
#define WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB  0x0002

// WGL_CONTEXT_PROFILE_MASK_ARB in <*attribList>:
#define WGL_CONTEXT_CORE_PROFILE_BIT_ARB        0x00000001
#define WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB 0x00000002

// Error cods for wglCreateContextAttribsARB
#define ERROR_INVALID_VERSION_ARB               0x2095
#define ERROR_INVALID_PROFILE_ARB               0x2096

typedef HGLRC wgl_create_context_attribs_arb(HDC hDC, 
                                             HGLRC hshareContext,
                                             const int *attribList);
global wgl_create_context_attribs_arb* wglCreateContextAttribsARB;

global Dynamic_Library opengl_library;
// TODO: COMPILER BUG: for some reason a whitespace is required at the end of line below, wat
#define GLAPI @link_dynamic(opengl_library)
