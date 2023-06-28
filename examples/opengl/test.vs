#version 330 core
layout (location = 0) in vec3 in_position;
layout (location = 1) in vec2 in_texcoord;
layout (location = 2) in vec3 in_tint;

out vec2 v_texcoord;
out vec4 v_tint;

uniform mat4 u_projection;

void
main() {
   gl_Position = u_projection * vec4(in_position, 1.0f);
   v_tint = vec4(in_tint, 1.0f);
   v_texcoord = in_texcoord;
}
