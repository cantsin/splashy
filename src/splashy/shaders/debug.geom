#version 440

layout (triangles) in;
layout (line_strip, max_vertices = 2) out;

in VS_OUT {
    layout(location = 3) vec3 normal;
    layout(location = 4) vec4 color;
} gs_in[3];

out GS_OUT {
    layout(location = 5) vec3 normal;
    layout(location = 6) vec4 color;
} gs_out;

const float MAGNITUDE = 10.0f;

void main()
{
    vec4 tri_centroid = (gl_in[0].gl_Position +
                         gl_in[1].gl_Position +
                         gl_in[2].gl_Position) / 3.0;
    gl_Position = tri_centroid;
    gs_out.normal = gs_in[0].normal;
    gs_out.color = gs_in[0].color;
    EmitVertex();
    gl_Position = tri_centroid + vec4(gs_in[0].normal, 0.0f) * MAGNITUDE;
    gs_out.normal = gs_in[0].normal;
    gs_out.color = gs_in[0].color;
    EmitVertex();
    EndPrimitive();
}
