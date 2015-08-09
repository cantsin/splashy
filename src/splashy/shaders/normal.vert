#version 130

precision highp float;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;
uniform mat4 vertex_mat;
in vec4 vertex_position;
in vec4 vertex_normal;
in vec4 vertex_color;

out vec3 normal;
out vec4 color;

void main()
{
    gl_Position = projectionMatrix * modelViewMatrix * vertex_mat * vertex_position;
    normal = (modelViewMatrix * vertex_normal).xyz;
    color = vertex_color;
}
