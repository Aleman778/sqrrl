
f32
degrees_to_radians(f32 degrees) {
    return degrees * PI_F32 / 180.0f;
}

f32
random_f32_between(f32 minimum, f32 maximum) {
    assert(maximum >= minimum);
    return minimum + (maximum-minimum)*random_f32();
}

f32
clamp_f32(f32 v, f32 min, f32 max) {
    if (v < min) {
        v = min;
    } else if (v > max) {
        v = max;
    }
    return v;
}

struct v3 {
    f32 x;
    f32 y;
    f32 z;
};

struct v4 {
    f32 r;
    f32 g;
    f32 b;
    f32 a;
};

struct ray {
    v3 origin;
    v3 direction;
};

v3 vec3_scale(v3 v, f32 s);
v3 vec3_add(v3 a, v3 b);

v3
ray_at(ray r, f32 t) {
    v3 v = vec3_scale(r.direction, t);
    return vec3_add(r.origin, v);
}

inline v3
vec3(f32 x, f32 y, f32 z) {
    v3 result;
    result.x = x;
    result.y = y;
    result.z = z;
    return result;
}

inline u32
round_f32_to_u32(f32 value) {
    return (u32) round_f32(value);
}

inline u32
rgba_pack_u32(v4 c) {
    return (round_f32_to_u32(c.a * 255.0f) << 24 |
            round_f32_to_u32(c.r * 255.0f) << 16 |
            round_f32_to_u32(c.g * 255.0f) << 8 |
            round_f32_to_u32(c.b * 255.0f));
}

inline v3
vec3_rgb(int r, int g, int b) {
    f32 fr = (f32) r/255.0f;
    f32 fg = (f32) g/255.0f;
    f32 fb = (f32) b/255.0f;
    return vec3(fr, fg, fb);
}

inline f32
min_f32(f32 x, f32 y) {
    return x < y ? x : y;
}

inline f32
abs_f32(f32 x) {
    if (x < 0.0f) {
        return -x;
    }
    return x;
}

inline bool
vec3_near_zero(v3 v) {
    f32 e = 1e-8f;
    return (abs_f32(v.x) < e && 
            abs_f32(v.y) < e &&
            abs_f32(v.z) < e);
}

inline v3
vec3_add(v3 a, v3 b) {
    v3 result;
    result.x = a.x + b.x;
    result.y = a.y + b.y;
    result.z = a.z + b.z;
    return result;
}

inline v3
vec3_sub(v3 a, v3 b) {
    v3 result;
    result.x = a.x - b.x;
    result.y = a.y - b.y;
    result.z = a.z - b.z;
    return result;
}

inline v3
vec3_mul(v3 a, v3 b) {
    v3 result;
    result.x = a.x * b.x;
    result.y = a.y * b.y;
    result.z = a.z * b.z;
    return result;
}

inline v3
vec3_scale(v3 v, f32 s) {
    v3 result;
    result.x = v.x * s;
    result.y = v.y * s;
    result.z = v.z * s;
    return result;
}

inline f32
vec3_dot(v3 a, v3 b) {
    return a.x*b.x + a.y*b.y + a.z*b.z;
}

inline v3
vec3_div(v3 a, v3 b) {
    v3 result;
    result.x = a.x / b.x;
    result.y = a.y / b.y;
    result.z = a.z / b.z;
    return result;
}

inline v3
vec3_cross(v3 a, v3 b) {
    v3 result;
    result.x = a.y * b.z - a.z * b.y;
    result.x = a.z * b.x - a.x * b.z;
    result.x = a.x * b.y - a.y * b.x;
    return result;
}

inline v3
vec3_reflect(v3 v, v3 normal) {
    f32 s = vec3_dot(v, normal)*2;
    return vec3_sub(v, vec3_scale(normal, s));
}

inline f32
vec3_length_squared(v3 v) {
    return v.x*v.x + v.y*v.y + v.z*v.z;
}

inline f32
vec3_length(v3 v) {
    return sqrt(v.x*v.x + v.y*v.y + v.z*v.z);
}

inline v3
vec3_normalize(v3 v) {
    f32 length = vec3_length(v);
    f32 inv_length = 1.0f / length;
    v3 result = vec3_scale(v, inv_length);
    return result;
}

inline void
print_vec3(v3 v) {
#ifdef __cplusplus
    printf("vec3(%f, %f, %f)\n",v.x,v.y,v.z);
#else
    print_format("vec3(%, %, %)\n",v.x,v.y,v.z);
#endif
}
