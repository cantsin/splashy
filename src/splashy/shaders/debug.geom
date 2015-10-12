#version 440

layout (triangles) in;
layout (line_strip, max_vertices = 2) out;

in VS_OUT {
    layout(location = 3) mat4 mvp;
    layout(location = 7) vec3 velocity;
} gs_in[3];

const float MAGNITUDE = 1.0f;

void main()
{
    vec3 ab = gl_in[1].gl_Position.xyz - gl_in[0].gl_Position.xyz;
    vec3 ac = gl_in[2].gl_Position.xyz - gl_in[0].gl_Position.xyz;
    vec3 velocity = gs_in[0].velocity;
    vec3 face_normal = normalize(cross(ab, ac));
    vec3 normal = (dot(velocity, face_normal)/dot(face_normal, face_normal)) * face_normal;
    vec4 tri_centroid = vec4(gl_in[0].gl_Position.xyz + ac * 0.5, 1.0);
    gl_Position = gs_in[0].mvp * tri_centroid;
    EmitVertex();
    gl_Position = gs_in[0].mvp * (tri_centroid + vec4(normal, 0.0f) * MAGNITUDE);
    EmitVertex();
    EndPrimitive();
}
