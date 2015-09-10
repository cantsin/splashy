#version 440

layout (triangles) in;
layout (triangle_strip, max_vertices=3) out;

in VS_OUT {
    layout(location = 3) vec3 normal;
    layout(location = 4) vec4 color;
} gs_in[3];

out GS_OUT {
    layout(location = 5) vec3 normal;
    layout(location = 6) vec4 color;
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
