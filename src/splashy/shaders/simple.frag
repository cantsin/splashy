#version 440

precision highp float;

const vec3 ambient = vec3(0.1, 0.1, 0.1);
const vec3 normalized_light = normalize(vec3(0.5, 0.5, 0.5));

in VS_OUT {
    layout(location = 3) vec3 normal;
    layout(location = 4) vec4 color;
} fs_in;

out vec4 final_color;

void main()
{
    float diffuse = clamp(dot(normalized_light, fs_in.normal), 0.0, 1.0);
    final_color = vec4(ambient + diffuse * fs_in.color.xyz, fs_in.color.w);
}
