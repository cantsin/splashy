#version 130

precision highp float;

uniform mat4 projection_matrix;
uniform mat4 model_view_matrix;
uniform mat4 vertex_mat;

in vec4 vertex_position;
in vec4 vertex_normal;
in vec4 vertex_color;

out vec3 normal;
out vec4 color;

void main()
{
    gl_Position = projection_matrix * model_view_matrix * vertex_mat * vertex_position;
    normal = (model_view_matrix * vertex_normal).xyz;
    color = vertex_color;
}
