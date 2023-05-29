
// TODO(Alexander): this is temporary dependency on C-runtime,
//                  later on we will implement these functions ourselves.

#define RAND_MAX 32767

@link("ucrtbased.dll")
extern {
    @extern_name("cosf")   f32 _cos_f32(f32 num);
    @extern_name("acosf")  f32 _acos_f32(f32 num);
    @extern_name("sinf")   f32 _sin_f32(f32 num);
    @extern_name("asinf")  f32 _asin_f32(f32 num);
    @extern_name("tanf")   f32 _tan_f32(f32 num);
    @extern_name("atanf")  f32 _atan_f32(f32 num);
    @extern_name("atan2f")  f32 _atan2_f32(f32 y, f32 x);
    @extern_name("sqrtf")  f32 _sqrt_f32(f32 num);
    @extern_name("roundf") f32 _round_f32(f32 num);
    @extern_name("floorf") f32 _floor_f32(f32 num);
    @extern_name("ceilf")  f32 _ceil_f32(f32 num);
    @extern_name("powf")  f32 _pow_f32(f32 num, f32 power);
    
    @extern_name("cos")   f64 _cos_f64(f64 num);
    @extern_name("acos")  f64 _acos_f64(f64 num);
    @extern_name("sin")   f64 _sin_f64(f64 num);
    @extern_name("asin")  f64 _asin_f64(f64 num);
    @extern_name("tan")   f64 _tan_f64(f64 num);
    @extern_name("atan")  f64 _atan_f64(f64 num);
    @extern_name("atan2")  f64 _atan2_f64(f64 y, f64 x);
    @extern_name("sqrt")  f64 _sqrt_f64(f64 num);
    @extern_name("round") f64 _round_f64(f64 num);
    @extern_name("floor") f64 _floor_f64(f64 num);
    @extern_name("ceil")  f64 _ceil_f64(f64 num);
    @extern_name("pow")  f64 _pow_f64(f64 num, f64 power);
    
    void* malloc(umm size);
    void* calloc(umm num, umm size);
    void* realloc(void* memory, umm new_size);
    void memcpy(void* dest, void* src, umm size);
    void memset(void* dest, int byte, umm size);
    void free(void* memory);
}

@link("ucrtbase.dll")
extern {
    @extern_name("rand")  s32  _random_s32();
    @extern_name("srand") void _seed_random_s32(u32 seed);
}
