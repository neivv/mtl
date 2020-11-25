#include <util_scale.glsl>

in vec2 texCoord;
in vec2 mapCoord;

layout(location = 0) out vec4 frag_color;

uniform vec2 invResolution;
uniform vec4 data;
uniform vec4 multiplyColor;

uniform sampler2D spriteTex;
uniform sampler2D sampleTex; // Noise texture

#ifdef DEFERRED
layout(location = 1) out vec4 frag_emissive;
layout(location = 2) out vec4 frag_specular;
layout(location = 3) out vec4 frag_normal;
#endif

#define PI (3.14)

void main()
{
    float time = data.z;

    vec2 noiseUV = vec2(mapCoord.x, mapCoord.y + (time / 10.0));
    noiseUV.x = noiseUV.x + (cos(time/10.0) * 0.3);
    vec4 noise = texture(sampleTex, noiseUV);
    vec4 baseColor = texture(spriteTex, texCoord);

    vec2 sOffset;
    sOffset.x = cos(noise.r * PI + time * 0.5) * 1.9;
    sOffset.y = sin(noise.g * PI + time * 0.5) * 0.5;
    sOffset *= invResolution;

    float redBleed = 1.2;
    vec2 uv = texCoord + sOffset;
    vec4 offsetColor = texture(spriteTex, uv);
    float intensity = min((offsetColor.r * redBleed) * 2.0, 1);

    // Below 0.2 in the red channel gets no distortion
    intensity *= mix(offsetColor.a, 0, step(offsetColor.r, 0.2)); // Mask is in the alpha channel

    vec4 color;
    color.rgb = mix(baseColor.rgb, offsetColor.rgb, intensity);
    color.a = 1.0;
    float emissive = max(offsetColor.r - 0.5, 0) * 4.0;
    vec4 emissive_vec = vec4(emissive, 0, 0, 1);

#ifdef DEFERRED
    frag_color = color;
    frag_emissive = emissive_vec;
    frag_specular = frag_color;
    frag_normal = vec4(normal_default(), 1.0);
#else
    frag_color = color * multiplyColor + emissive_vec;
#endif

}

