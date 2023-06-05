#version 330 core
layout (location = 0) in vec3 position;

out vec4 vertex_color;

void
main() {
   gl_Position = vec4(position, 0.0f);
   vertex_color = vec4(position, 0.0f);
}
