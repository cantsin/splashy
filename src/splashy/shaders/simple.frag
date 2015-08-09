#version 130

const vec3 ambient = vec3(0.1, 0.1, 0.1);
const vec3 lightVecNormalized = normalize(vec3(0.5, 0.5, 2.0));

in vec3 normal;
in vec4 color;

out vec4 outputColor;

void main()
{
    float diffuse = clamp(dot(lightVecNormalized, normalize(normal)), 0.0, 1.0);
    outputColor = vec4(ambient + diffuse * color.xyz, color.w);
}
