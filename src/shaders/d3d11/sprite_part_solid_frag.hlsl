struct VS_OUTPUT
{
    float4 pos : SV_POSITION;
    float2 texCoord : TEXCOORD0;
};

SamplerState sprite_sampler : register(s0);
SamplerState team_color_sampler : register(s2);

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
Texture2D <float4> team_color_tex: register(t2);

struct PS_OUTPUT
{
    float4 frag_color : SV_Target0;
    float4 frag_effect : SV_Target1;
};

PS_OUTPUT main(VS_OUTPUT v)
{
    PS_OUTPUT o;
    float4 sprite_color = sprite_tex.Sample(sprite_sampler, v.texCoord);
    float3 brightness_weights = float3(0.30196078, 0.59215686, 0.10980392);
    // Lol blizzard bug, orig shader uses sprite_color.rrr
    float brightness = dot(brightness_weights, sprite_color.rgb);
    if (brightness < multiplyColor.a) {
        discard;
    }

    float team_color_mask = team_color_tex.Sample(team_color_sampler, v.texCoord).x;

    o.frag_color.rgb = lerp(sprite_color.rgb, sprite_color.rgb * playerColor.rgb, team_color_mask);
    o.frag_color.rgb *= multiplyColor.rgb;
    o.frag_color.a = sprite_color.a;
    o.frag_effect = float4(0.0, 0.0, 0.0, sprite_color.a);

    return o;
}

