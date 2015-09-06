#version 440

layout (triangles) in;
layout (line_strip, max_vertices = 2) out;

in VS_OUT {
    vec3 normal;
    vec3 color;
} gs_in[];

out GS_OUT {
    vec3 normal;
    vec3 color;
} gs_out;

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
