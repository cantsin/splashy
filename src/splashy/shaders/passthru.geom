#version 330 core

layout (triangles) in;
layout (triangle_strip, max_vertices=3) out;

in VertexData {
    vec3 normal;
    vec3 color;
} VertexIn[3];

out VertexData {
    vec3 normal;
    vec3 color;
} VertexOut;

void main()
{
    for(int i = 0; i < gl_in.length(); i++)
    {
        // copy attributes
        gl_Position = gl_in[i].gl_Position;
        VertexOut.normal = VertexIn[i].normal;
        VertexOut.color = VertexIn[i].color;

        // done with the vertex
        EmitVertex();
    }
}
