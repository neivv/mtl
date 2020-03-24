
struct LightInfo
{
    vec3 color;
    vec3 pos;
};

struct FragmentInfo
{
    vec3 pos;
    vec3 normal;
    vec4 specular;
    vec4 color;
    float ao;
};

vec3 Lighting(LightInfo light, FragmentInfo fragment, float falloff, vec3 eye)
{
    vec3 lightDirection = normalize(light.pos - fragment.pos);
    float lambertian = clamp(dot(lightDirection, fragment.normal), 0, 1);

    float specValue = 0;
    if(lambertian > 0)
    {
        vec3 viewDir = normalize(eye - fragment.pos);
        vec3 halfDir = normalize(lightDirection + viewDir);

        // This was -> max(dot(halfDir, normal), 0.0)
        // but there were artifacts at the edges of units when specAngle
        // went above one
        float specAngle = clamp(dot(halfDir, fragment.normal), 0.0, 1.0);

        float specFactor = 16;
        specValue = pow(specAngle, specFactor);
    }
    vec3 result = lambertian * fragment.color.rgb * light.color;
    result += specValue * fragment.specular.rgb;
    result *= fragment.ao;
    result *= falloff;

    return result;
}

vec3 DirectionalLighting(FragmentInfo fragment, vec3 eye)
{
    LightInfo directional;
    directional.pos = vec3(200, 50, 500);
    directional.color = vec3(1.0);
    return Lighting(directional, fragment, 1.0, eye);
}

