
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

// TODO: COMPILER BUG: out of order processing of these APIs don't work without these types specified first!
typedef void GLvoid;
typedef u32 GLenum;
typedef f32 GLfloat;
typedef s32 GLint;
typedef int GLsizei;
typedef u32 GLbitfield;
typedef f64 GLdouble;
typedef s32 GLuint;
typedef u8 GLboolean;
typedef u8 GLubyte;
typedef f32 GLclampf;
typedef f64 GLclampd;
typedef smm GLsizeiptr;
typedef smm GLintptr;
typedef s8 GLchar;
typedef s16 GLshort;
typedef s8 GLbyte;
typedef u16 GLushort;
typedef u16 GLhalf;
//typedef struct __GLsync* GLsync;
typedef void* GLsync;
typedef u64 GLuint64;
typedef s64 GLint64;

@link("opengl32.dll")
extern {
    HGLRC __stdcall wglCreateContext(HDC hdc);
    
    BOOL __stdcall wglDeleteContext(HGLRC hglrc);
    
    BOOL __stdcall wglMakeCurrent(HDC hdc, HGLRC hglrc);
    
    void* __stdcall wglGetProcAddress(LPCSTR function_name);
    
    // OpenGL 1.0 and 1.1 are linked at compile time
#define GLAPI 
    // TODO: remove later (fixed function pipeline)
    GLAPI void APIENTRY glBegin (GLenum mode);
    GLAPI void APIENTRY glVertex3f (GLfloat x, GLfloat y, GLfloat z);
    GLAPI void APIENTRY glColor3f (GLfloat red, GLfloat green, GLfloat blue);
    GLAPI void APIENTRY glEnd (void);
    
#ifndef GL_VERSION_1_0
#define GL_VERSION_1_0 1
    GLAPI void APIENTRY glCullFace (GLenum mode);
    GLAPI void APIENTRY glFrontFace (GLenum mode);
    GLAPI void APIENTRY glHint (GLenum target, GLenum mode);
    GLAPI void APIENTRY glLineWidth (GLfloat width);
    GLAPI void APIENTRY glPointSize (GLfloat size);
    GLAPI void APIENTRY glPolygonMode (GLenum face, GLenum mode);
    GLAPI void APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
    GLAPI void APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
    GLAPI void APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
    GLAPI void APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
    GLAPI void APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
    GLAPI void APIENTRY glTexImage1D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const void *pixels);
    GLAPI void APIENTRY glTexImage2D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels);
    GLAPI void APIENTRY glDrawBuffer (GLenum buf);
    GLAPI void APIENTRY glClear (GLbitfield mask);
    GLAPI void APIENTRY glClearColor (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
    GLAPI void APIENTRY glClearStencil (GLint s);
    GLAPI void APIENTRY glClearDepth (GLdouble depth);
    GLAPI void APIENTRY glStencilMask (GLuint mask);
    GLAPI void APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
    GLAPI void APIENTRY glDepthMask (GLboolean flag);
    GLAPI void APIENTRY glDisable (GLenum cap);
    GLAPI void APIENTRY glEnable (GLenum cap);
    GLAPI void APIENTRY glFinish (void);
    GLAPI void APIENTRY glFlush (void);
    GLAPI void APIENTRY glBlendFunc (GLenum sfactor, GLenum dfactor);
    GLAPI void APIENTRY glLogicOp (GLenum opcode);
    GLAPI void APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
    GLAPI void APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
    GLAPI void APIENTRY glDepthFunc (GLenum func);
    GLAPI void APIENTRY glPixelStoref (GLenum pname, GLfloat param);
    GLAPI void APIENTRY glPixelStorei (GLenum pname, GLint param);
    GLAPI void APIENTRY glReadBuffer (GLenum src);
    GLAPI void APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels);
    GLAPI void APIENTRY glGetBooleanv (GLenum pname, GLboolean *data);
    GLAPI void APIENTRY glGetDoublev (GLenum pname, GLdouble *data);
    GLAPI GLenum APIENTRY glGetError (void);
    GLAPI void APIENTRY glGetFloatv (GLenum pname, GLfloat *data);
    GLAPI void APIENTRY glGetIntegerv (GLenum pname, GLint *data);
    GLAPI const GLubyte *APIENTRY glGetString (GLenum name);
    GLAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, void *pixels);
    GLAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
    GLAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
    GLAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
    GLAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
    GLAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
    GLAPI void APIENTRY glDepthRange (GLdouble n, GLdouble f);
    GLAPI void APIENTRY glViewport (GLint x, GLint y, GLsizei width, GLsizei height);
#endif
    
#ifndef GL_VERSION_1_1
#define GL_VERSION_1_1 1
    // OpenGL 1.1
    GLAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
    GLAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const void *indices);
    GLAPI void APIENTRY glGetPointerv (GLenum pname, void **params);
    GLAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
    GLAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLint border);
    GLAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
    GLAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
    GLAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
    GLAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const void *pixels);
    GLAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
    GLAPI void APIENTRY glBindTexture (GLenum target, GLuint texture);
    GLAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
    GLAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
    GLAPI GLboolean APIENTRY glIsTexture (GLuint texture);
#endif
}

// OpenGL 1.2+ needs to be linked at runtime
global Dynamic_Library opengl_library;
// TODO: COMPILER BUG: for some reason a whitespace is required at the end of line below, wat
#undef GLAPI
#define GLAPI @link_dynamic(opengl_library)
