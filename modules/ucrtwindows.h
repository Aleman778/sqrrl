
// TODO(Alexander): this is temporary dependency on C-runtime,
//                  later on we will implement these functions ourselves.

#define RAND_MAX 32767

typedef u8* va_list;

@link("ucrtbased.dll")
extern {
    
    @extern_name("cosf")   f32 _f32_cos(f32 num);
    @extern_name("acosf")  f32 _f32_acos(f32 num);
    @extern_name("sinf")   f32 _f32_sin(f32 num);
    @extern_name("asinf")  f32 _f32_asin(f32 num);
    @extern_name("tanf")   f32 _f32_tan(f32 num);
    @extern_name("atanf")  f32 _f32_atan(f32 num);
    @extern_name("sqrtf")  f32 _f32_sqrt(f32 num);
    @extern_name("roundf") f32 _f32_round(f32 num);
    @extern_name("floorf") f32 _f32_floor(f32 num);
    @extern_name("ceilf")  f32 _f32_ceil(f32 num);
    @extern_name("absf")   f32 _f32_abs(f32 num);
    
    @extern_name("rand")  f32 _s32_random(f32 num);
    @extern_name("srand") f32 _s32_seed_random(f32 num);
    
    f32 va_start(va_list list, va_arg prev_arg);
}
