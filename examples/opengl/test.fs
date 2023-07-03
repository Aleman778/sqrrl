#version 330 core

out vec4 frag_color;

in vec2 v_texcoord;
in vec4 v_tint;

uniform sampler2D u_texture;

void
main() {
    frag_color = texture(u_texture, v_texcoord) * v_tint;
}