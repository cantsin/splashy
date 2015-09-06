#version 440

precision highp float;

const vec3 ambient = vec3(0.1, 0.1, 0.1);
const vec3 normalized_light = normalize(vec3(0.5, 0.5, 2.0));

layout(location = 3) in vec3 normal;
layout(location = 4) in vec4 color;

out vec4 final_color;

void main()
{
    float diffuse = clamp(dot(normalized_light, normalize(normal)), 0.0, 1.0);
    final_color = vec4(ambient + diffuse * color.xyz, color.w);
}
