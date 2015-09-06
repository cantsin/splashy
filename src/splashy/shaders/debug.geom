#version 330 core

layout (triangles) in;
layout (line_strip, max_vertices = 2) out;

in VertexData {
    vec3 normal;
    vec3 color;
} VertexIn[];

out VertexData {
    vec3 normal;
    vec3 color;
} VertexOut;

const float MAGNITUDE = 0.4f;

void main()
{
    vec4 tri_centroid = (gl_in[0].gl_Position +
                         gl_in[1].gl_Position +
                         gl_in[2].gl_Position) / 3.0;
    gl_Position = tri_centroid;
    VertexOut.normal = VertexIn[0].normal;
    VertexOut.color = VertexIn[0].color;
    EmitVertex();
    gl_Position = tri_centroid + vec4(VertexIn[0].normal, 0.0f) * MAGNITUDE;
    VertexOut.normal = VertexIn[0].normal;
    VertexOut.color = VertexIn[0].color;
    EmitVertex();
    EndPrimitive();
}
