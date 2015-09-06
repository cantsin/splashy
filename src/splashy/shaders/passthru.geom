#version 440

layout (triangles) in;
layout (triangle_strip, max_vertices=3) out;

in VS_OUT {
    vec3 normal;
    vec3 color;
} gs_in[3];

out GS_OUT {
    vec3 normal;
    vec3 color;
} gs_out;

void main()
{
    for(int i = 0; i < gl_in.length(); i++)
    {
        // copy attributes
        gl_Position = gl_in[i].gl_Position;
        gs_out.normal = gs_in[i].normal;
        gs_out.color = gs_in[i].color;

        // done with the vertex
        EmitVertex();
    }
}
