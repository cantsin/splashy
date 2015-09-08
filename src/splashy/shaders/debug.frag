#version 440

in GS_OUT {
    layout(location = 9) in vec3 normal;
    layout(location = 10) in vec4 color;
} fs_in;

out vec4 final_color;

void main()
{
    final_color = vec4(fs_in.color.rgb, 0.2f);
}
