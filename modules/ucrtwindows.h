
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
    @extern_name("sqrtf")  f32 _sqrt_f32(f32 num);
    @extern_name("roundf") f32 _round_f32(f32 num);
    @extern_name("floorf") f32 _floor_f32(f32 num);
    @extern_name("ceilf")  f32 _ceil_f32(f32 num);
    @extern_name("absf")   f32 _abs_f32(f32 num);
}

@link("ucrtbase.dll")
extern {
    @extern_name("rand")  s32  _random_s32();
    @extern_name("srand") void _seed_random_s32(u32 seed);
    
    
    int fflush(void* ostream);
}
