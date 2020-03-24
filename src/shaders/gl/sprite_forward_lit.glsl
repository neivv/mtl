
#include <util_lighting.glsl>
#include <util_scale.glsl>
#include <util_hallucinate.glsl>
#include <util_team_color.glsl>

in vec2 texCoord;

layout(location = 0) out vec4 frag_color;

uniform sampler2D spriteTex;
uniform sampler2D teamcolorTex;
uniform sampler2D emissiveTex; // This is the GameMap's normal texture
uniform sampler2D normalTex;
uniform sampler2D specularTex;
uniform sampler2D ao_d_lTex;

uniform vec4 multiplyColor;
uniform vec4 teamColor;
uniform vec2 invResolution;
uniform vec4 data;

#define MAX_LIGHTS 8
#define ALL_LIGHTS 1

#if ALL_LIGHTS
uniform vec3 lightPositions[MAX_LIGHTS];
uniform vec3 lightColors[MAX_LIGHTS];
uniform vec3 lightData[MAX_LIGHTS];
#endif

void main()
{
    float hallucinate = data.x;
    float flipped = data.y;
    float lightingScale = data.w;

    FragmentInfo fragment;
    fragment.specular = texture(specularTex, texCoord);
    fragment.normal = normal_full(texture(normalTex, texCoord), flipped);

    vec2 maskCoord = gl_FragCoord.xy * invResolution;
    vec4 emissive    = texture(emissiveTex, maskCoord);

    if(emissive.b < 0.1)
        discard;

    float mask_value = texture(teamcolorTex, texCoord).r;

    // Diffuse color == sprite * team * multiply
    fragment.color    = texture(spriteTex, texCoord);
    fragment.color.rgb = TeamColor(fragment.color.rgb, teamColor.rgb, mask_value);
    fragment.color.rgb = HallucinationColor(fragment.color.rgb, fragment.color.rgb, hallucinate);
    fragment.color *= multiplyColor;
    frag_color.a = fragment.color.a;

    vec4 ao_d_l = texture(ao_d_lTex, texCoord);

    float z = ao_d_l.g * (75 * lightingScale);
    fragment.ao = ao_d_l.r;

    vec2 resolution = (1 / invResolution);
    vec3 eye = vec3(vec2(resolution) * vec2(0.5, 0.25), 500 * lightingScale);

    fragment.pos = vec3(0, 0, 0);
    frag_color.rgb = DirectionalLighting(fragment, eye, vec3(1.0));

#if ALL_LIGHTS
    fragment.pos = vec3(gl_FragCoord.xy, z);
    for(int i = 0; i < MAX_LIGHTS; ++i)
    {
        LightInfo light;
        light.color = lightColors[i];
        light.pos = lightPositions[i];
        float lightRadius = lightData[i].r;
        float lightIntensity = lightData[i].g;

        // gl_FragCoord starts in the lower left, lights start in the upper left
        light.pos.y = resolution.y - light.pos.y;
        light.pos.z = 125 * lightingScale;

        float fragmentDistance = distance(fragment.pos.xy, light.pos.xy);
        float falloff = clamp(lightRadius - fragmentDistance, 0.0, lightRadius);
        falloff /= max(lightRadius, 0.00001);
        float power = mix(0, lightIntensity, falloff);

        frag_color.rgb += Lighting(light, fragment, power, eye);
    }
#endif
    //frag_color.rgb = fragment.normal;
}

