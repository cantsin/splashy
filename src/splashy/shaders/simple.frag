#version 330
const vec3 ambient = vec3(0.1, 0.1, 0.1);
const vec3 lightVecNormalized = normalize(vec3(0.5, 0.5, 2.0));
const vec3 lightColor = vec3(0.3, 0.8, 0.2);

in vec3 normal;

out vec4 outputColor;

void main(void)
{
    float diffuse = clamp(dot(lightVecNormalized, normalize(normal)), 0.0, 1.0);
    outputColor = vec4(ambient + diffuse * lightColor, 1.0);
}
