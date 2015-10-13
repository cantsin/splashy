#version 440

precision highp float;

uniform mat4 projection_matrix;
uniform mat4 model_view_matrix;
uniform mat4 vertex_matrix;

const vec4 light_position = vec4(0.0, 500.0, 1000.0, 1.0);

layout(location = 0) in vec4 vertex_position;
layout(location = 1) in vec4 vertex_normal;
layout(location = 2) in vec4 vertex_color;

out VS_OUT {
    layout(location = 3) vec3 normal;
    layout(location = 4) vec4 color;
    layout(location = 5) vec3 light;
} vs_out;

void main()
{
    gl_Position = projection_matrix * model_view_matrix * vertex_matrix * vertex_position;
    vs_out.normal = (model_view_matrix * vertex_normal).xyz;
    vs_out.color = vertex_color;
    vs_out.light = (model_view_matrix * light_position).xyz - (model_view_matrix * vertex_position).xyz;
}
