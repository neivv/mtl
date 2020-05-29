
float3 scale(float3 v)
{
    return (v * 2) - 1.f;
}

float2 scale(float2 v)
{
    return (v * 2) - 1.f;
}

float unscale(float f)
{
    return (f * 0.5) + 0.5;
}

float3 unscale(float3 v)
{
    return (v + 1) / 2.f;
}

float3 normal_fixup(float4 v, float flipped)
{
    float3 normal = float3(v.r, v.a, 0);
    normal = scale(normal.rgb);
    normal.z = sqrt(1 - normal.x * normal.x - normal.y * normal.y);
    normal.r *= flipped;
    normal = unscale(normal.rgb);
    return normal;
}

float3 normal_default()
{
    float4 baseNormal = float4(0.5, 0, 0, 0.5);
    return normal_fixup(baseNormal, 1.0);
}

