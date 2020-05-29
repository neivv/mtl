struct VS_OUTPUT
{
    float4 pos : SV_POSITION;
    float2 tex_coord : TEXCOORD0;
    uint2 light_params : TEXCOORD1;
};

SamplerState tex0_1_sampler : register(s0);
SamplerState tex2_sampler : register(s2);
SamplerState tex3_sampler : register(s3);
SamplerState tex4_sampler : register(s4);
SamplerState tex5_sampler : register(s5);

cbuffer constants {
    float4 multiplyColor;
    float4 solidColor;
    float4 playerColor;
    float spriteWidth;
    float spriteHeight;
    float2 invResolution;
    float4 data;
};

Texture2D <float4> tex0: register(t0);
Texture2D <float4> tex1: register(t1);
Texture2D <float4> tex2: register(t2);
Texture2D <float4> specular_tex: register(t3);
Texture2D <float4> tex4: register(t4);
Texture2D <float4> tex5: register(t5);

struct Light {
    float2 pos;
    float unk8;
    float radius;
    float3 unk16;
    float unk28;
};

StructuredBuffer<Light> lights : register(t7);

struct PS_OUTPUT
{
    float4 frag_color : SV_Target0;
    float4 frag_effect : SV_Target1;
};

// For cloak effects
// offset_strength selects extra offset between 0 and 8px (0.0 .. 1.0)
float calculate_sprite_offset(float sample1, float offset_strength, float2 tex_coord) {
    // Between 5px and 13 px depending on sample1
    float sample2_x_off = (sample1 + 0.625) * spriteWidth * 8.0;
    float2 sample2_coord = float2(tex_coord.x + sample2_x_off, tex_coord.y);
    float sample2 = tex1.Sample(tex0_1_sampler, sample2_coord).z;

    float extra_offset = offset_strength * spriteWidth * 8.0;
    // if (sample1 != 0 && sample2 != 0)
    if (sample2 * sample1 > 0.0) {
        // So this could be up to 21 pixel offset? Seems really high for the cloak effect
        // Maybe tex1.z represents stacked cloaked unit count?
        return tex_coord.x + extra_offset + sample2_x_off;
    } else {
        return tex_coord.x + extra_offset;
    }
}

PS_OUTPUT main(VS_OUTPUT v)
{
    PS_OUTPUT o;

    float3 tex1_sample = tex1.Sample(tex0_1_sampler, v.tex_coord).xyz;
    float3 emissive_sample = tex2.Sample(tex2_sampler, v.tex_coord).xyz;
    float3 specular_sample = specular_tex.Sample(tex3_sampler, v.tex_coord).xyz;
    float2 normal_sample = tex4.Sample(tex4_sampler, v.tex_coord).xy * 2.0 - 1.0;
    float3 ao_sample = tex5.Sample(tex5_sampler, v.tex_coord).xyz;

    float sprite_tex_x = calculate_sprite_offset(tex1_sample.z, tex1_sample.x, v.tex_coord);
    float2 sprite_tex_coord = float2(sprite_tex_x, v.tex_coord.y);
    float3 sprite_color = tex0.Sample(tex0_1_sampler, sprite_tex_coord).xyz;

    float2 normal_sq = normal_sample * normal_sample;
    float normal_z = sqrt(max(0.1, 1.0 - normal_sq.x - normal_sq.y));
    float3 normal = normalize(float3(normal_sample.xy, normal_z));

    float2 resolution = float2(1.0, 1.0) / invResolution;
    float3 r6 = float3(resolution.xy * float2(0.5, 0.25), data.w * 500.0);

    float3 const_vec = float3(0.365148, 0.182574, 0.912871);
    float base_color_strength = max(0.0, dot(const_vec, normal));
    float3 r7 = normalize(const_vec + normalize(r6));
    float specular_result = pow(saturate(dot(r7, normal)), 16.0);
    float specular = base_color_strength > 0.0 ? specular_result : 0.0;

    float3 base_light = sprite_color * base_color_strength + specular_sample * specular;
    base_light *= ao_sample.x;
    // This reduces both specular strength and applies color to sprite_color
    base_light *= multiplyColor.rgb;

    float3 light_sum = base_light * (1.0 - 0.5 * tex1_sample.y);

    float avg_light = dot(multiplyColor.rgb, 1.0 / 3.0);

    float pos_z = data.w * ao_sample.y * 75.0;
    float light_z = data.w * 125.0;
    float3 pos = float3(v.pos.xy, pos_z);
    float3 normalized_r6 = normalize(r6 - pos);

    uint light_start_idx = v.light_params.x >> 1;
    uint light_count = v.light_params.y;
    o.frag_color.rgb = float3(0.0, 0.0, 0.0);
    for (uint n = 0; n < light_count; n += 1) {
        uint i = light_start_idx + n;
        float2 light_pos_raw = lights[i].pos;
        float3 light_color = lights[i].unk16;
        float radius = lights[i].radius;
        float intensity_raw = lights[i].unk28;
        float intensity = lerp(1.0, 0.0, avg_light - min(intensity_raw, avg_light));
        //light_color *= 2;
        float3 light_pos = float3(light_pos_raw.x, resolution.y - light_pos_raw.y, light_z);

        float light_distance = length(v.pos.xy - light_pos.xy);
        float distance_mul = clamp(radius - light_distance, 0.0, radius) / radius;
        float light_mul = intensity * 0.9 * distance_mul;

        float3 r8_xyz = normalize(light_pos - pos);
        float base_color_strength = max(0.0, dot(r8_xyz, normal));
        float3 r8 = normalize(r8_xyz + normalized_r6);
        float specular_result = pow(saturate(dot(r8, normal)), 16.0);
        float specular = base_color_strength > 0 ? specular_result : 0;

        float3 val = sprite_color * base_color_strength * light_color;
        val += specular_sample * specular;
        val *= ao_sample.x;
        light_sum += light_mul * val;

        // Debug: Display lights with radius
        //o.frag_color.rgb += distance_mul.xxx;
    }

    float3 lit_color = emissive_sample + light_sum;
    o.frag_color.rgb = lerp(sprite_color, lit_color, ao_sample.z);
    o.frag_color.a = 1.0;

    //o.frag_color.rgb = light_count / 2.0;
    //o.frag_color.rgb = normal;
    //o.frag_color.rgb = emissive_sample;
    //o.frag_color.rgb = ao_sample;
    //o.frag_color.rgb = ao_sample.x;


    float3 lit_weights = float3(0.301961, 0.592157, 0.109804);
    if (dot(lit_color, lit_weights) > 1.8) {
        // Why
        // Equal to `emissive_sample * 2 + light_sum`
        // I guess that this is supposed to mean that if there's really many lights
        // they should be taken into account as well. But chances are that this
        // is also just going to saturate at 1.0 at that point?
        o.frag_effect.rgb = emissive_sample + lit_color;
    } else {
        o.frag_effect.rgb = emissive_sample;
    }
    o.frag_effect.a = 1.0;
    return o;
}

