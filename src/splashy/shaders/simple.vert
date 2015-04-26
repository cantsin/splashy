#version 330

precision highp float;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;
in vec4 vertex_position;
in vec4 vertex_normal;

out vec3 normal;

void main(void)
{
    gl_Position = projectionMatrix * modelViewMatrix * vertex_position;
    normal = (modelViewMatrix * vertex_normal).xyz;
}
