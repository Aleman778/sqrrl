// Based on https://raytracing.github.io/books/RayTracingInOneWeekend.html

#define PI_F32 3.1415926535897932385f
#define INF_F32 100000000000000000000000000000000000.0f

#include "math.cpp"

v3
random_vec3_in_unit_sphere() {
    while (true) {
        v3 v;
        v.x = random_f32();
        v.y = random_f32();
        v.z = random_f32();
        if (vec3_length_squared(v) >= 1.0f) {
            continue;
        }
        return v;
    }
}

v3
random_unit_vec3() {
    return vec3_normalize(random_vec3_in_unit_sphere());
}

v3
random_vec3_in_hemisphere(v3 normal) {
    v3 rnd = random_vec3_in_unit_sphere();
    if (vec3_dot(rnd, normal) > 0.0f) {
        return rnd;
    } else {
        return vec3_scale(rnd, -1.0f);
    }
}

v3
ray_sky_color(ray r) {
    v3 dir = r.direction;
    dir = vec3_normalize(dir);
    
    f32 t = 0.5f*(dir.y + 1.0f);
    f32 t_inv = 1.0f-t;
    
    v3 sky_color_0 = vec3(0.98f, 0.61f, 0.51f);
    v3 sky_color_1 = vec3_rgb(52, 94, 173);
    
    //v3 sky_color_0 = vec3(1.0f, 1.0f, 1.0f);
    //v3 sky_color_1 = vec3(0.5f, 0.7f, 1.0f);
    //
    v3 color_0 = vec3_scale(sky_color_0, t_inv);
    v3 color_1 = vec3_scale(sky_color_1, t);
    v3 color = vec3_add(color_0, color_1);
    return color;
}

struct Material;

struct Hit_Result {
    v3 p;
    v3 normal;
    f32 t;
    Material* material;
    bool front_face;
};

enum Material_Type {
    Lambertian,
    Metallic,
    Dielectric
};

#ifdef __cplusplus
#define MT_Lambertian Lambertian
#define MT_Metallic Metallic
#define MT_Dielectric Dielectric

#else

#define MT_Lambertian Material_Type.Lambertian
#define MT_Metallic Material_Type.Metallic
#define MT_Dielectric Material_Type.Dielectric
#endif

struct Material {
    Material_Type type;
    v3 albedo;
    f32 roughness; // only for Metallic
    f32 refraction_index; // only for Dieletric
};

struct Sphere {
    v3 p;
    f32 radius;
    Material* material;
};

Hit_Result
sphere_hit(Sphere sphere, ray r, f32 t_min, f32 t_max) {
    Hit_Result result;
    result.t = -1.0f;
    result.material = sphere.material;
    
    v3 oc = vec3_sub(r.origin, sphere.p);
    
    f32 a = vec3_length_squared(r.direction);
    f32 half_b = vec3_dot(oc, r.direction);
    f32 c = vec3_length_squared(oc) - sphere.radius*sphere.radius;
    f32 discriminant = half_b*half_b - a*c;
    
    if (discriminant < 0.0f) {
        return result;
    }
    
    // TODO(Alexander): something wrong with this calculation
    // we shouldn't need to reverse order of root +/-
    f32 sqrt_discriminant = sqrt(discriminant);
    
    //debug_break();
    f32 root = (-half_b - sqrt_discriminant) / a;
    if (root < t_min || root >= t_max) {
        root = (-half_b + sqrt_discriminant) / a;
        if (root < t_min || root >= t_max) {
            return result;
        }
    }
    
    result.t = root;
    result.p = ray_at(r, root);
    
    v3 outward_normal = vec3_sub(result.p, sphere.p);
    outward_normal = vec3_scale(outward_normal, 1.0f/sphere.radius);
    if (vec3_dot(r.direction, outward_normal) < 0.0f) {
        result.front_face = true;
        result.normal = outward_normal;
    } else {
        result.front_face = false;
        result.normal = vec3_scale(outward_normal, -1.0f);
    }
    
    return result;
}

struct Scatter_Result {
    v3 attenuation;
    ray scattered;
    bool scatter;
};

Scatter_Result
lambertian_scatter(Hit_Result hit) {
    Scatter_Result result;
    
    //v3 target = vec3_add(hit.p, vec3_add(hit.normal, random_unit_vec3(hit.normal)));
    v3 scatter_direction = vec3_add(hit.normal, random_unit_vec3());
    
    if (vec3_near_zero(scatter_direction)) {
        scatter_direction = hit.normal;
    }
    
    result.scattered.origin = hit.p;
    result.scattered.direction = scatter_direction;
    result.attenuation = hit.material->albedo;
    result.scatter = true;
    
    return result;
}

Scatter_Result
metallic_scatter(ray r, Hit_Result hit) {
    Scatter_Result result;
    
    v3 reflected = vec3_reflect(vec3_normalize(r.direction), hit.normal);
    v3 fuzz = random_unit_vec3();
    fuzz = vec3_scale(fuzz, hit.material->roughness);
    
    result.scattered.origin = hit.p;
    result.scattered.direction = vec3_add(reflected, fuzz);
    result.attenuation = hit.material->albedo;
    result.scatter = vec3_dot(reflected, hit.normal) > 0;
    
    return result;
}

v3
vec3_refract(v3 uv, v3 normal, f32 etai_over_etat) {
    //debug_break();
    f32 cos_theta = min_f32(vec3_dot(vec3_scale(uv, -1.0f), normal), 1.0f);
    
    v3 perp = vec3_scale(normal, cos_theta);
    perp = vec3_add(uv, perp);
    perp = vec3_scale(perp, etai_over_etat);
    
    v3 parallel = vec3_scale(normal, -sqrt(abs_f32(1.0f - vec3_length_squared(perp))));
    
    return vec3_add(perp, parallel);
}

f32
reflectance(f32 cos_theta, f32 refraction_ratio) {
    // Schlick's approximation
    f32 r0 = (1-refraction_ratio) / (1+refraction_ratio);
    r0 = r0 * r0;
    return r0 + (1 - r0) * pow((1 - cos_theta), 5);
}


int scatter_count = 0;

Scatter_Result
dieletric_scatter(ray r, Hit_Result hit) {
    Scatter_Result result;
    result.attenuation = vec3(1.0f, 1.0f, 1.0f);
    
    //hit.normal = vec3(-0.027596f, 0.827434f, 0.560884f);
    //r.direction = vec3(-1.408918f, 0.574960f, -1.000000f);
    //print_vec3(r.direction);
    //hit.normal = vec3_normalize(vec3(1.0f, 1.0f, 0.0f));
    
    f32 refraction_ratio = hit.material->refraction_index;
    // TODO(Alexander): front_face seems wrong
    if (hit.front_face) {
        refraction_ratio = 1.0f/refraction_ratio;
    }
    
    v3 unit_direction = vec3_normalize(r.direction);
    //print_vec3(unit_direction);
    v3 v = vec3_scale(unit_direction, -1.0f);
    f32 cos_theta = min_f32(vec3_dot(v, hit.normal), 1.0f);
    //printf("%f\n", cos_theta);
    //pln("%", cos_theta);
    f32 sin_theta = sqrt(1.0f - cos_theta*cos_theta);
    //pln("%", sin_theta);
    //printf("%f\n", sin_theta);
    //printf("%f\n", refraction_ratio);
    
    bool cannot_refract = (refraction_ratio * sin_theta) > 1.0f;
    if (cannot_refract || reflectance(cos_theta, refraction_ratio) > random_f32()) {
        result.scattered.direction = vec3_reflect(unit_direction, hit.normal);
    } else {
        result.scattered.direction = vec3_refract(unit_direction, hit.normal, refraction_ratio);
    }
    
    result.scattered.origin = hit.p;
    result.scatter = true;
    
    return result;
}

v3
ray_color(ray r, Sphere* spheres, int sphere_count, int depth) {
    if (depth <= 0) {
        return vec3(0.0f, 0.0f, 0.0f);
    }
    
    f32 t_min = 0.001f;
    f32 t_max = INF_F32;
    
    Hit_Result found_hit = {};
    found_hit.t = -1.0f;
    
    for (int i = 0; i < sphere_count; i += 1) {
        Sphere s = spheres[i];
        //pln("sphere% - pos: %, %, % - radius: %", i, s.p.x, s.p.y, s.p.z, s.radius);
        Hit_Result hit = sphere_hit(s, r, t_min, t_max);
        if (hit.t > 0.0f) {
            t_max = hit.t;
            found_hit = hit;
        }
    }
    
    v3 color;
    if (found_hit.t > 0.0f) {
        
        
        Scatter_Result s = {};
        if (found_hit.material->type == MT_Lambertian) {
            s = lambertian_scatter(found_hit);
        } else if (found_hit.material->type == MT_Metallic) {
            s = metallic_scatter(r, found_hit);
        } else if (found_hit.material->type == MT_Dielectric) {
            s = dieletric_scatter(r, found_hit);
        }
        
        depth -= 1;
        if (s.scatter) {
            v3 c = ray_color(s.scattered, spheres, sphere_count, depth);
            color = vec3_mul(c, s.attenuation);
        } else {
            color = vec3(0.0f, 0.0f, 0.0f);
        }
    } else {
        color = ray_sky_color(r);
    }
    
    return color;
}

struct Game_State {
    v3 camera_look_at;
    v3 camera_p;
    
    Sphere spheres[5];
    
    int samples_per_pixel;
};

struct HDR_Software_Texture {
    f32* data;
    s32 width;
    s32 height; 
    s32 pitch;
};

int
render(HDR_Software_Texture* texture, Game_State* state) {
    //f32 aspect_ratio = 16.0f / 9.0f;
    f32 aspect_ratio = (f32) texture->width / (f32) texture->height;
    int max_ray_depth = 10;
    
    //pln("resolution: % x %", texture->width, texture->height);
    v3 vec3_up = vec3(0.0f, 1.0f, 0.0f);
    v3 look_at = vec3(0.0f, 0.0f, -1.0f);
    v3 forward = vec3_normalize(vec3_sub(state->camera_p, look_at));
    v3 right = vec3_normalize(vec3_cross(vec3_up, forward));
    v3 up = vec3_cross(forward, right);
    
    
    //pln("%, %, %", forward, right, up);
    f32 viewport_height = 2.0f;
    f32 viewport_width = aspect_ratio * viewport_height;
    
    v3 origin = state->camera_p;
    //v3 origin = vec3(0.0f, 2.0f, 0.0f);
    v3 horizontal = vec3_scale(right, viewport_width);
    v3 vertical = vec3_scale(up, viewport_height);
    
    
    // Compute the lower left screen coordinate
    v3 temp = vec3_scale(horizontal, 0.5f);
    v3 lower_left_corner = vec3_sub(origin, temp);
    temp = vec3_scale(vertical, 0.5f);
    lower_left_corner = vec3_sub(lower_left_corner, temp);
    lower_left_corner = vec3_sub(lower_left_corner, forward);
    
    //pln("P3\n% %\n255", texture->width, texture->height);
    state->samples_per_pixel += 1;
    
    f32* row = texture->data;
    for (int y = texture->height-1; y >= 0; y -= 1) {
        
        f32* texel = (f32*) row;
        for (int x = 0; x < texture->width; x += 1) {
            
            v3 color;
            color.x = *texel;
            color.y = *(texel + 1);
            color.z = *(texel + 2);
            
            //debug_break();
            f32 u = ((f32) x + random_f32()) / (f32) texture->width;
            f32 v = ((f32) y + random_f32()) / (f32) texture->height;
            
            temp = vec3_scale(horizontal, u);
            v3 dir = vec3_add(lower_left_corner, temp);
            temp = vec3_scale(vertical, v);
            dir = vec3_add(dir, temp);
            dir = vec3_sub(dir, origin);
            
            //print_vec3(dir);
            ray r;
            r.origin = origin;
            r.direction = dir;
            
#ifdef __cplusplus
            v3 c = ray_color(r, state->spheres, 
                             sizeof(state->spheres)/sizeof(state->spheres[0]),
                             max_ray_depth);
#else
            v3 c = ray_color(r, state->spheres.data, state->spheres.count, max_ray_depth);
#endif
            color = vec3_add(color, c);
            
            
            *texel = color.x;
            //debug_break();
            texel += 1;
            *texel = color.y;
            texel += 1;
            *texel = color.z;
            texel += 2;
            
            //pln("% % %", ir, ig, ib);
            //return 0;
        }
        
        s64 p = (s64) texture->pitch;
        row += p;
    }
    
    return 0;
}
