#version 330 core
out vec4 frag_color;

in vec3 p;
in vec4 vertex_color;

uniform float t;

void
main() {

    float circle1 = clamp(pow(cos(sqrt(p.x*p.x + (p.y)*(p.y))*80.0f + t), 40.0f), 0.0f, 1.0f);
    float circle2 = clamp(pow(cos(sqrt((p.x - 1.0f)*(p.x - 1.0f) + (p.y + 1.0f)*(p.y + 1.0f))*80.0f + t), 40.0f), 0.0f, 1.0f);
    float circle3 = clamp(pow(cos(sqrt((p.x + 1.0f)*(p.x + 1.0f) + (p.y + 1.0f)*(p.y + 1.0f))*80.0f + t), 40.0f), 0.0f, 1.0f);

    frag_color = vec4(circle1, circle2, circle3, 1.0f);
//vec4(vertex_color.x*(circle1 + circle2 + circle3), vertex_color.y*circle2, vertex_color.z*circle3, 1.0f);
}