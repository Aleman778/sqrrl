#ifdef __cplusplus

#include "windows.h"

#include <cstdio>
#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <cstdarg>
#include <cmath>
#include <cassert>

typedef unsigned int uint;
typedef int8_t       s8;
typedef uint8_t      u8;
typedef int16_t      s16;
typedef uint16_t     u16;
typedef int32_t      s32;
typedef uint32_t     u32;
typedef int64_t      s64;
typedef uint64_t     u64;
typedef uintptr_t    umm;
typedef intptr_t     smm;
typedef float        f32;
typedef double       f64;
typedef int32_t      b32;
typedef const char*  cstring;

f32
random_f32() {
    return (f32) rand() / (RAND_MAX + 1.0f);
}

#define round_f32(x) roundf(x)
#define pln(...)
#define print_format(...)
#define debug_break() __debugbreak()

#else

#include <basic.sq>

#endif

#include "renderer.cpp"

//#define PRINT_AST 1
//#define PRINT_BYTECODE 1

bool is_running = true;

LRESULT CALLBACK
win32_main_callback(HWND window, UINT message, WPARAM w_param, LPARAM l_param) {
    LRESULT result = 0;
    
    if (message == WM_CLOSE) {
        is_running = false;
    } else if (message == WM_DESTROY) {
        is_running = false;
    } else {
        result = DefWindowProcA(window, message, w_param, l_param);
    }
    
    return result;
}


struct Win32_Offscreen_Buffer {
    BITMAPINFO info;
    void* memory;
    int width;
    int height;
    int bytes_per_row;
    int bytes_per_pixel;
    int scale;
};

void
win32_resize_offscreen_buffer(Win32_Offscreen_Buffer* buffer, 
                              HDC window_device_context, 
                              int width, int height) {
    // TODO(alexander): maybe don't free first, free after, then free first if that fails.
    if (buffer->memory) {
        VirtualFree(buffer->memory, 0, MEM_RELEASE);
    }
    
    buffer->width = width;
    buffer->height = height;
    buffer->bytes_per_pixel = 4;
    
    buffer->info.bmiHeader.biSize = (u32) sizeof(buffer->info.bmiHeader);
    buffer->info.bmiHeader.biWidth = buffer->width;
    buffer->info.bmiHeader.biHeight = -buffer->height;
    buffer->info.bmiHeader.biPlanes = 1;
    buffer->info.bmiHeader.biBitCount = 32;
    buffer->info.bmiHeader.biCompression = BI_RGB;
    
    // TODO(Alexander): probably not where we should do this!
    int buffer_memory_size = (buffer->width*buffer->height)*buffer->bytes_per_pixel;
    buffer->memory = VirtualAlloc(0, buffer_memory_size, MEM_COMMIT, PAGE_READWRITE);
    
    buffer->bytes_per_row = buffer->width*buffer->bytes_per_pixel;
}

void
win32_clear_color(Win32_Offscreen_Buffer* buffer, v4 color) {
    u32 pixel_value = rgba_pack_u32(color);
    //pln("%", (u32*) pixel_value);
    s64 pitch = (s64) buffer->width*(s64) buffer->bytes_per_pixel;
    u8* row = (u8*) buffer->memory;
    for (s32 y = 0; y < buffer->height; y += 1) {
        u32* pixel = (u32*) row;
        for (s32 x = 0; x < buffer->width; x += 1) {
            *pixel = pixel_value;
            pixel += 1;
        }
        row += pitch;
    }
}

void
win32_render_buffer(Win32_Offscreen_Buffer* dest_buffer, HDR_Software_Texture* src_buffer, HDC window_device_context, int width, int height, int samples_per_pixel) {
    int size = dest_buffer->width*dest_buffer->height;
    
    f32* src = src_buffer->data;
    u32* dest = (u32*) (dest_buffer->memory);
    for (int i = 0; i < size; i += 1) {
        
        f32 scale = 1.0f / (f32) samples_per_pixel;
        
        // Divide the color by the number of samples and gamma-correct for gamma=2.0.
        f32 r = sqrt(*src * scale); src += 1;
        f32 g = sqrt(*src * scale); src += 1;
        f32 b = sqrt(*src * scale); src += 2;
        
        u32 ir = (u32) (256.0f*clamp_f32(r, 0.0f, 0.999f));
        u32 ig = (u32) (256.0f*clamp_f32(g, 0.0f, 0.999f));
        u32 ib = (u32) (256.0f*clamp_f32(b, 0.0f, 0.999f));
        
        *dest = ir << 16 | ig << 8 | ib; dest += 1;
    }
    
    StretchDIBits(window_device_context,
                  0, 0, width, height,
                  0, 0, dest_buffer->width, dest_buffer->height,
                  dest_buffer->memory,
                  &dest_buffer->info,
                  DIB_RGB_COLORS, 
                  SRCCOPY | BLACKNESS);
}

struct Game_Button_State {
    s32 num_half_transitions;
    bool ended_down;
};

inline bool
was_pressed(Game_Button_State state) {
    return state.num_half_transitions > 1 ||
    (state.num_half_transitions == 1 && state.ended_down);
}

inline bool
was_released(Game_Button_State state) {
    return state.num_half_transitions > 1 ||
    (state.num_half_transitions == 1 && !state.ended_down);
}

inline bool
is_down(Game_Button_State state) {
    return state.ended_down;
}

struct Game_Controller {
    Game_Button_State move_up;
    Game_Button_State move_right;
    Game_Button_State move_down;
    Game_Button_State move_left;
};

void
win32_process_keyboard_message(Game_Button_State* new_state, bool is_down) {
    if (new_state->ended_down != is_down) {
        new_state->num_half_transitions += 1;
        new_state->ended_down = is_down;
    }
}

int
main() {
    HINSTANCE h_instance = GetModuleHandleA(0);
    
    WNDCLASSA wndclass = {};
    wndclass.style = CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc = win32_main_callback;
    wndclass.hInstance = h_instance;
    wndclass.lpszClassName = "RaytracerWindowClass";
    
    f32 aspect_ratio = 16.0f / 9.0f;
    int width = 400;
    int height = (int) ((f32) width / aspect_ratio);
    
    if (RegisterClassA(&wndclass)) {
        //debug_break();
        HWND window = CreateWindowExA(0,
                                      "RaytracerWindowClass",
                                      "Sqrrl Raytracer",
                                      WS_OVERLAPPEDWINDOW | WS_VISIBLE,
                                      CW_USEDEFAULT,
                                      CW_USEDEFAULT,
                                      width, height,
                                      0, 0,
                                      wndclass.hInstance,
                                      0);
        
        HDC device_context = GetDC(window);
        
        Win32_Offscreen_Buffer offscreen_buffer;
        win32_resize_offscreen_buffer(&offscreen_buffer, device_context, width, height);
        
        v4 sky_color;//v4 { r: 0.0f, g: 0.5f, b: 1.0f, a: 1.0f };
        sky_color.r = 0.0f;
        sky_color.g = 0.5f;
        sky_color.b = 1.0f;
        sky_color.a = 1.0f;
        //win32_clear_color(&offscreen_buffer, sky_color);
        
        Material mat0 = {};
        mat0.type = MT_Dielectric;
        mat0.refraction_index = 1.5f;
        mat0.albedo = vec3_rgb(149, 52, 173);
        //mat0.roughness = 0.0f;
        
        Material mat1 = {};
        mat1.type = MT_Dielectric;
        mat1.refraction_index = 1.5f;
        mat1.albedo = vec3_rgb(232, 112, 79);
        //mat1.roughness = 0.3f;
        
        Material mat2 = {};
        mat2.type = MT_Metallic;
        mat2.albedo = vec3_rgb(74, 127, 212);
        mat2.roughness = 1.0f;
        
        Material mat3 = {};
        mat3.albedo = vec3_rgb(128, 171, 120);
        
        Game_State state = {};
        state.spheres[0].p = vec3(0.0f, 0.0f, -1.0f);
        state.spheres[0].radius = 0.5f;
        state.spheres[0].material = &mat0;
        
        state.spheres[1].p = vec3(-1.0f, 0.0f, -1.0f);
        state.spheres[1].radius = 0.5f;
        state.spheres[1].material = &mat1;
        
        state.spheres[2].p = vec3(1.0f, 0.0f, -1.0f);
        state.spheres[2].radius = 0.5f;
        state.spheres[2].material = &mat2;
        
        state.spheres[3].p = vec3(0.0f, -100.5f, -1.0f);
        state.spheres[3].radius = 100.0f;
        state.spheres[3].material = &mat3;
        
        Game_Controller controller = {};
        
        HDR_Software_Texture texture = {};
        texture.width = width;
        texture.height = height;
        texture.pitch = texture.width*4;
        int buffer_memory_size = (texture.width*texture.height)*4*4;
        texture.data = (f32*) VirtualAlloc(0, buffer_memory_size, MEM_COMMIT, PAGE_READWRITE);
        
        int i = 0;
        while (is_running) {
            
            {
                Game_Controller new_controller = {};
                new_controller.move_up.ended_down = controller.move_up.ended_down;
                new_controller.move_left.ended_down = controller.move_left.ended_down;
                new_controller.move_down.ended_down = controller.move_down.ended_down;
                new_controller.move_right.ended_down = controller.move_right.ended_down;
                controller = new_controller;
            }
            
            RECT dimensions;
            if (GetClientRect(window, &dimensions)) {
                width = dimensions.right;
                height = dimensions.bottom;
            }
            
            render(&texture, &state);
            win32_render_buffer(&offscreen_buffer, &texture, device_context, width, height,
                                state.samples_per_pixel);
            
            //win32_clear_color(&offscreen_buffer, sky_color);
            
            //pln("Frame %", i + 1);
            
            MSG message;
            while (PeekMessageA(&message, 0, 0, 0, PM_REMOVE)) {
                if (message.message == WM_QUIT) {
                    is_running = false;
                    
                } else if (message.message == WM_SYSKEYDOWN ||
                           message.message == WM_SYSKEYUP ||
                           message.message == WM_KEYDOWN ||
                           message.message == WM_KEYUP) {
                    
                    u32 vkcode = (u32) message.wParam;
                    bool was_down = (message.lParam & (1 << 30)) != 0;
                    bool is_down = (message.lParam & (1 << 31)) == 0;
                    
                    if (was_down != is_down) {
                        if (vkcode == 'W') {
                            win32_process_keyboard_message(&controller.move_up, is_down);
                        } else if (vkcode == 'A') {
                            win32_process_keyboard_message(&controller.move_left, is_down);
                        } else if (vkcode == 'S') {
                            win32_process_keyboard_message(&controller.move_down, is_down);
                        } else if (vkcode == 'D') {
                            win32_process_keyboard_message(&controller.move_right, is_down);
                        }
                    }
                    
                    bool alt_key_was_down = (message.lParam & (1 << 29)) != 0;
                    if (alt_key_was_down && vkcode == VK_F4) {
                        is_running = false;
                    }
                } else {
                    TranslateMessage(&message);
                    DispatchMessageA(&message);
                }
            }
            
            bool discard_buffer = false;
            if (is_down(controller.move_up)) {
                state.camera_p.z -= 0.1f;
                discard_buffer = true;
            }
            
            if (is_down(controller.move_down)) {
                state.camera_p.z += 0.1f;
                discard_buffer = true;
            }
            
            if (is_down(controller.move_left)) {
                state.camera_p.x -= 0.1f;
                discard_buffer = true;
            }
            
            if (is_down(controller.move_right)) {
                state.camera_p.x += 0.1f;
                discard_buffer = true;
            }
            
            if (discard_buffer) {
                state.samples_per_pixel = 0;
                int size = texture.width*texture.height;
                f32* src = texture.data;
                for (int j = 0; j < size; j += 1) {
                    *src = 0.0f; src += 1;
                    *src = 0.0f; src += 1;
                    *src = 0.0f; src += 2;
                }
            }
            
            Sleep(1);
            i += 1;
        }
        
    } else {
        pln("Failed to create a window")
    }
    
}
