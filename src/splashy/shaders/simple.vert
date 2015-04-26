#version 330

precision highp float;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;
in vec3 vertex_position;
in vec3 vertex_normal;

out vec3 normal;

void main(void)
{
    gl_Position = projectionMatrix * modelViewMatrix * vec4(vertex_position, 1.0);
    normal = (modelViewMatrix * vec4(vertex_normal, 0)).xyz;
}
