#version 440

precision highp float;

uniform mat4 projection_matrix;
uniform mat4 model_view_matrix;
uniform mat4 vertex_matrix;
uniform vec3 cell_velocity;

layout(location = 0) in vec4 vertex_position;

out VS_OUT {
    layout(location = 3) mat4 mvp;
    layout(location = 7) vec3 velocity;
} vs_out;

void main()
{
    gl_Position = vertex_position;
    vs_out.mvp = projection_matrix * model_view_matrix * vertex_matrix;
    vs_out.velocity = cell_velocity;
}
