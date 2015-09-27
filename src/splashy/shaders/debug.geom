#version 440

layout (triangles) in;
layout (line_strip, max_vertices = 2) out;

in VS_OUT {
    layout(location = 6) mat4 mvp;
} gs_in[3];

const float MAGNITUDE = 10.0f;

void main()
{
    vec3 ab = gl_in[1].gl_Position.xyz - gl_in[0].gl_Position.xyz;
    vec3 ac = gl_in[2].gl_Position.xyz - gl_in[0].gl_Position.xyz;
    vec3 face_normal = normalize(cross(ab, ac));
    vec4 tri_centroid = (gl_in[0].gl_Position +
                         gl_in[1].gl_Position +
                         gl_in[2].gl_Position) / 3.0;
    gl_Position = gs_in[0].mvp * tri_centroid;
    EmitVertex();
    gl_Position = gs_in[0].mvp * (tri_centroid - vec4(face_normal, 0.0f) * MAGNITUDE);
    EmitVertex();
    EndPrimitive();
}
