#include <util_scale.hlsl>

struct VS_OUTPUT
{
    float4 pos : SV_POSITION;
    float2 texCoord : TEXCOORD0;
};

SamplerState spriteTexSampler : register(s0);

cbuffer constants {
    float4 multiplyColor;
    float4 solidColor;
    float4 playerColor;
    float spriteWidth;
    float spriteHeight;
    float2 invResolution;
    float4 data;
};

Texture2D <float4> sprite_tex: register(t0);

struct PS_OUTPUT
{
    float4 frag_color : SV_Target0;
    float4 frag_effect : SV_Target1;
};

PS_OUTPUT main(VS_OUTPUT v)
{
    float4 tex = sprite_tex.Sample(spriteTexSampler, v.texCoord);
    PS_OUTPUT o;
    o.frag_color = tex * multiplyColor;

#if DRAW_EFFECT
    o.frag_effect = tex;
#ifdef ALPHA_DRAW
    if (o.frag_effect.a < 1)
        o.frag_effect.a = 1;
    else
        o.frag_effect.a = 0;
#elif !defined(COLOR_DRAW)
    if (o.frag_effect.a < 1)
        o.frag_effect.a = 0;
#endif

#else 
    // not DRAW_EFFECT

    // This shader is used for health bars on the terrain, and we want
    // to clear the effects from under those
    o.frag_effect = float4(0.0, 0.0, 0.0, 1.0);
#endif

    return o;
}

