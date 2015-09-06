#version 440

layout(location = 3) in vec3 normal;
layout(location = 4) in vec4 color;

out vec4 final_color;

void main()
{
    final_color = vec4(1.0f, 1.0f, 0.0f, 1.0f);
}
