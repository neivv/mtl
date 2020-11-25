#include <util_scale.hlsl>

struct VS_OUTPUT
{
    float4 pos : SV_POSITION;
    float2 texCoord : TEXCOORD0;
    float2 mapCoord : TEXCOORD1;
};

cbuffer constants {
    float4 multiplyColor;
    float4 solidColor;
    float4 playerColor;
    float spriteWidth;
    float spriteHeight;
    float2 invResolution;
    float4 data;
};

SamplerState spriteTex : register(s0);
SamplerState noiseSampler : register(s1);

Texture2D <float4> sprite: register(t0);
Texture2D <float4> noiseTex : register(t1);

struct PS_OUTPUT
{
    float4 frag_color : SV_Target0;
#ifdef DEFERRED
    float4 frag_emissive : SV_Target1;
    float4 frag_specular : SV_Target2;
    float4 frag_normal : SV_Target3;
#endif
};

#define PI (3.14)

PS_OUTPUT main(VS_OUTPUT v)
{
    PS_OUTPUT o;

    float time = data.z;
    float2 noiseUV = float2(v.mapCoord.x, v.mapCoord.y - (time / 10.0));
    noiseUV.x = noiseUV.x + (cos(time/10.0) * 0.3);
    float4 noise = noiseTex.Sample(noiseSampler, noiseUV);
    float4 baseColor = sprite.Sample(spriteTex, v.texCoord);

    float2 sOffset;
    sOffset.x = cos(noise.r * PI + time * 0.5) * 1.9;
    sOffset.y = sin(noise.g * PI + time * 0.5) * 0.5;
    sOffset *= invResolution;

    float redBleed = 1.2;
    float2 uv = v.texCoord + sOffset;
    float4 offsetColor = sprite.Sample(spriteTex, uv);
    float intensity = min((offsetColor.r * redBleed) * 2.0, 1);
    float emissive = max(offsetColor.r - 0.5, 0) * 4.0;
    float3 emissive_vec = float3(emissive, 0, 0);

    // Below 0.2 in the red channel gets no distortion
    intensity *= lerp(offsetColor.a, 0, step(offsetColor.r, 0.2)); // Mask is in the alpha channel

    float3 color = lerp(baseColor.rgb, offsetColor.rgb, intensity);

#ifdef DEFERRED
    o.frag_color = float4(color.rgb, 1.0);
    o.frag_emissive = float4(emissive_vec.rgb, 1.0);
    o.frag_specular = o.frag_color;
    o.frag_normal = float4(normal_default(), 1.0);
#else
    o.frag_color = float4(color.rgb * multiplyColor.rgb + emissive_vec.rgb, 1.0);
#endif

    return o;
}

