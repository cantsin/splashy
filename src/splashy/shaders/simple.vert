#version 440

precision highp float;

uniform mat4 projection_matrix;
uniform mat4 model_view_matrix;
uniform mat4 vertex_matrix;

layout(location = 0) in vec4 vertex_position;
layout(location = 1) in vec4 vertex_normal;
layout(location = 2) in vec4 vertex_color;

layout(location = 3) out vec3 normal;
layout(location = 4) out vec4 color;

void main()
{
    gl_Position = projection_matrix * model_view_matrix * vertex_matrix * vertex_position;
    normal = (model_view_matrix * vertex_normal).xyz;
    color = vertex_color;
}
