#include <util_scale.hlsl>

struct VS_OUTPUT
{
    float4 pos : SV_POSITION;
    float2 texCoord : TEXCOORD0;
    float2 mapCoord : TEXCOORD1;  // large tiles
    float2 mapCoord2 : TEXCOORD2; // small tiles
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

SamplerState sampleTex : register(s1);  // normal 1 frame 1
SamplerState sampleTex2 : register(s2); // normal 1 frame 2 (for interpolation)
SamplerState sampleTex3 : register(s3); // normal detail frame 1
SamplerState sampleTex4 : register(s4); // normal detail frame 2 (for interpolation)

Texture2D <float4> sprite_tex: register(t0);
Texture2D <float4> tex1 : register(t1);
Texture2D <float4> tex2 : register(t2);
Texture2D <float4> tex3 : register(t3);
Texture2D <float4> tex4 : register(t4);

float3 WaterSample(float2 mapCoord, float2 mapCoord2, float time, float textureFade)
{
    float t = time / 20.0;

    float2 mappedUV1 = mapCoord;
    mappedUV1.x -= (t / 2.0);
    mappedUV1.y += (t / 1.0);

    float2 mappedUV2 = mapCoord2;
    mappedUV2.x -= t * 2.0;
    mappedUV2.y += t * 1.8;

    float3 normalFrameOne = tex1.Sample(sampleTex, mappedUV1).rgb;
    float3 normalFrameTwo = tex2.Sample(sampleTex2, mappedUV1).rgb;
    float3 normal = lerp(normalFrameOne, normalFrameTwo, textureFade);

    float3 normal2FrameOne = tex3.Sample(sampleTex3, mappedUV2).rgb;
    float3 normal2FrameTwo = tex4.Sample(sampleTex4, mappedUV2).rgb;
    float3 normalDetail = lerp(normal2FrameOne, normal2FrameTwo, textureFade);

    float detailScale = 0.6;
    normal = scale(normal);
    normalDetail = scale(normalDetail) * detailScale;

    float3 combined = normalize(float3(float2(normal.xy + normalDetail.xy) * detailScale, normal.z));
    return combined;
}

float WaterSpecPower(float3 normal, float2 uv, float gameX)
{
// scale uv to (-1, 1) so the center of the screen is { 0, 0 }
    float2 specUV = scale(uv);
    // Show specular towards the middle of the screen
    float lightX = lerp(-0.1, 0.1, 1.0 - clamp(gameX, 0.0, 1.0));
    float xDistance = clamp(1.0 - distance(specUV.x, lightX), 0, 1);
    float yDistance = clamp(1.0 - distance(specUV.y, 0.2), 0, 1);
    float spower = yDistance * pow(xDistance, 3);
    spower *= 0.6;
    return spower;
}

struct PS_OUTPUT
{
    float4 frag_color : SV_Target0;
#ifdef DEFERRED
    float4 frag_effect : SV_Target1;
    float4 frag_normal : SV_Target2;
#endif
};

PS_OUTPUT main(VS_OUTPUT v)
{
    PS_OUTPUT o;

    float gameX       = data.x;
    float textureFade = data.y;
    float time        = data.z;

    float4 tex = sprite_tex.Sample(spriteTexSampler, v.texCoord);
    float3 groundColor = tex.rgb;

    float3 normal = WaterSample(v.mapCoord, v.mapCoord2, time, textureFade);

    float2 uv = v.texCoord;
    uv.x += normal.x * 0.019;
    uv.y -= normal.y * 0.029;

    float4 texColor = sprite_tex.Sample(spriteTexSampler, uv);
    float destMask = min(tex.a, texColor.a);
    float sourceMask = tex.a;

#if 0
    // stepping at 0.5 looks nice around doodads, but causes bleeding around shorelines
    const float stepPower = 0.5;
    vec3 result = lerp(texColor.rgb, groundColor, step(destMask, stepPower));
    // View:
    /////frag_color.rgb += float3(step(destMask, stepPower)) * 0.5;
#else
    float3 result = lerp(texColor.rgb, groundColor, 1.0 - destMask);
#endif

    float spower = WaterSpecPower(normal, v.texCoord, gameX);
    float specValue = ((normal.x*1.2 + normal.y + normal.z) / 3.0) * spower;
    float3 waterColor = result + specValue;

    // Put specular on the ground, even where distortion can't be applied
    groundColor = lerp(groundColor, groundColor + specValue, sourceMask);

#ifdef DEFERRED
    o.frag_specular = vec4(float3(lerp(groundColor, vec3(specValue), destMask)), 1.0);
    o.frag_normal = vec4(unscale(normal), destMask);
    o.frag_color = float4(float3(lerp(groundColor, waterColor, destMask)), 1.0);
#else 
    o.frag_color = float4(float3(lerp(groundColor, waterColor, destMask)), 1.0) * multiplyColor;
#endif
    return o;
}

