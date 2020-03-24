
#include <util_lighting.glsl>
#include <util_scale.glsl>
#include <util_effect.glsl>

in vec2 texCoord;
flat in ivec2 lightCoord; // x == offset, y == count;

layout(location = 0) out vec4 frag_color;
layout(location = 1) out vec4 frag_effect;

#deferred_samplers
uniform sampler2D spriteTex; // Contains teamColors baked in
uniform sampler2D effectTex;
uniform sampler2D emissiveTex;
uniform sampler2D specularTex;
uniform sampler2D normalTex;
uniform sampler2D ao_d_lTex;
uniform samplerBuffer lightBuffer;

uniform float spriteWidth;
uniform vec2 invResolution;
uniform vec4 data;

void main()
{
    float lightingScale = data.w;

    FragmentInfo fragment;
    fragment.specular = texture(specularTex, texCoord);
    fragment.normal   = texture(normalTex, texCoord).rgb; // b has cloaked?
    vec3 ao_d_l       = texture(ao_d_lTex, texCoord).rgb;
    vec3 emissive     = texture(emissiveTex, texCoord).rgb;

    // Read and apply cloak/warp effects
    vec4 effect;
    ApplyEffect_Cloak(spriteTex, effectTex, texCoord, fragment.color, effect, spriteWidth);

    fragment.normal = normal_final(fragment.normal);

    float z = ao_d_l.g * (75 * lightingScale);
    fragment.ao = ao_d_l.r;

    vec2 resolution = (1 / invResolution);
    vec3 eye = vec3(vec2(resolution) * vec2(0.5, 0.25), 500 * lightingScale);

    // Directional Light
    fragment.pos = vec3(0, 0, 0);
    vec3 base_color = fragment.color.rgb;
    vec3 lit_color = DirectionalLighting(fragment, eye);

    // Draw the shadows
    lit_color = ApplyEffect_Shadow(lit_color, effect.rgb);

    int offset = lightCoord.x;
    int count = lightCoord.y;

    fragment.pos = vec3(gl_FragCoord.xy, z);
    for(int i = 0; i < count; ++i)
    {
        LightInfo light;
        vec4 posTexel = texelFetch(lightBuffer, offset + 0);
        vec4 colTexel = texelFetch(lightBuffer, offset + 1);
        light.pos = posTexel.rgb;
        light.color = colTexel.rgb;
        float lightRadius = posTexel.a;
        float lightIntensity = colTexel.a;
        offset += 2;

#if 1
        light.color *= 2;
        lightIntensity *= 0.6;
#endif

        // gl_FragCoord starts in the lower left, lights start in the upper left
        light.pos.y = resolution.y - light.pos.y;
        light.pos.z = 125 * lightingScale;

        float fragmentDistance = distance(fragment.pos.xy, light.pos.xy);
        float falloff = clamp(lightRadius - fragmentDistance, 0.0, lightRadius);
        falloff /= lightRadius;
        //falloff *= falloff;
        falloff *= 0.9;
        float power = mix(0, lightIntensity, falloff);

        lit_color += Lighting(light, fragment, power, eye);

#if 0 // Show power
        lit_color += vec3(power);
#endif

#if 0 // show light radius
        float absd = abs(fragmentDistance - lightRadius);
        float epsilon = 0.5;
        if(absd <= epsilon)
        {
            lit_color = light.color;
        }
#endif

#if 0 // Show affected pixels
        if(fragmentDistance < lightRadius)
        {
            lit_color += vec3(0.15);
        }
#endif


#if 0 // Show light position
        if(fragmentDistance < lightRadius / 25)
        {
            lit_color += vec3(0.85);
        }
#endif

#if 0 // Show affected tiles
        lit_color += vec3(0.05);
#endif
    }
    lit_color += emissive.rgb;

    frag_color = vec4(mix(base_color, lit_color, ao_d_l.b), 1.0);
    frag_effect = vec4(emissive, 1.0);

    float b = dot(lit_color, vec3(0.30196078, 0.59215686, 0.10980392));
    //float b = dot(frag_color.rgb, vec3(0.30196078, 0.59215686, 0.10980392));
    //b = (lit_color.r + lit_color.g + lit_color.b) / 3.0;
    if(b > 1.8)
        frag_effect.rgb += lit_color;


//DEBUG VIEW
    // Disable bloom:
    //frag_effect = vec4(vec3(0), 1);

    //frag_color.rgb = vec3(texture(normalTex, texCoord).b); // b has cloaked
    //frag_color.rgb = vec3(ao_d_l.g); // depth
    //frag_color.rgb = vec3(ao_d_l.b); // lighting
    //frag_color.rgb = fragment.normal;
    //frag_color.rgb = emissive;
    //frag_color.rgb = fragment.color.rgb;
    //frag_color.rgb = fragment.specular.rgb;
    //frag_color.rgb = vec3(fragment.ao);
    //frag_color = texture(effectTex, texCoord);
}

