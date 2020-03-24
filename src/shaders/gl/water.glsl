in vec2 texCoord;

#include <util_scale.glsl>

layout(location = 0) out vec4 frag_color;
uniform sampler2D spriteTex;

#ifdef DEFERRED
layout(location = 1) out vec4 frag_specular;
layout(location = 2) out vec4 frag_normal;
#endif

in vec2 mapCoord;  // large tiles
in vec2 mapCoord2; // small tiles

uniform vec4 data;
uniform vec4 multiplyColor;

uniform sampler2D sampleTex;  // normal 1 frame 1
uniform sampler2D sampleTex2; // normal 1 frame 2 (for interpolation)
uniform sampler2D sampleTex3; // normal detail frame 1
uniform sampler2D sampleTex4; // normal detail frame 2 (for interpolation)

vec3 WaterSample(float time, float animTime)
{
    float t = time / 20.0;

    vec2 mappedUV1 = mapCoord;
    mappedUV1.x -= (t / 2.0);
    mappedUV1.y -= (t / 1.0);

    vec2 mappedUV2 = mapCoord2;
    mappedUV2.x -= t * 2.0;
    mappedUV2.y -= t * 1.8;

    vec3 normalFrameOne = texture(sampleTex, mappedUV1).rgb;
    vec3 normalFrameTwo = texture(sampleTex2, mappedUV1).rgb;
    vec3 normal = mix(normalFrameOne, normalFrameTwo, animTime);

    vec3 normal2FrameOne = texture(sampleTex3, mappedUV2).rgb;
    vec3 normal2FrameTwo = texture(sampleTex4, mappedUV2).rgb;
    vec3 normalDetail = mix(normal2FrameOne, normal2FrameTwo, animTime);

    float detailScale = 0.6;
    normal = scale(normal);
    normalDetail = scale(normalDetail) * detailScale;

    vec3 combined = normalize(vec3(vec2(normal.xy + normalDetail.xy) * detailScale, normal.z));

    return combined;
}

float WaterSpecPower(vec3 normal, vec2 uv, float gameX)
{
// scale uv to (-1, 1) so the center of the screen is { 0, 0 }
    vec2 specUV = scale(uv);
    // Show specular towards the middle of the screen
    float lightX = mix(-0.1, 0.1, 1.0 - clamp(gameX, 0.0, 1.0));
    float xDistance = clamp(1.0 - distance(specUV.x, lightX), 0, 1);
    float yDistance = clamp(1.0 - distance(specUV.y, -0.2), 0, 1);
    float spower = yDistance * pow(xDistance, 3);
    spower *= 0.6;
    return spower;
}

void main()
{
    float gameX       = data.x;
    float textureFade = data.y;
    float time        = data.z;

    vec4 tex = texture(spriteTex, texCoord);
    vec3 groundColor = tex.rgb;

    vec3 normal = WaterSample(time, textureFade);
    vec2 uv = texCoord;
    uv.x += normal.x * 0.019;
    uv.y += normal.y * 0.029;

    vec4 texColor = texture(spriteTex, uv);
    float destMask = min(tex.a, texColor.a);
    float sourceMask = tex.a;

#if 0
    // stepping at 0.5 looks nice around doodads, but causes bleeding around shorelines
    const float stepPower = 0.5;
    vec3 result = mix(texColor.rgb, groundColor, step(destMask, stepPower));
    // View:
    /////frag_color.rgb += vec3(step(destMask, stepPower)) * 0.5;
#else
    vec3 result = mix(texColor.rgb, groundColor, 1.0 - destMask);
#endif

    float spower = WaterSpecPower(normal, texCoord, gameX);
    float specValue = ((normal.x*1.2 + normal.y + normal.z) / 3.0) * spower;// length(normal);
    vec3 waterColor = result + specValue;

    // Put specular on the ground, even where distortion can't be applied
    groundColor = mix(groundColor, groundColor + specValue, sourceMask);

#ifdef DEFERRED
    // We draw black into tilemap specular now...  //vec4(mix(groundColor, vec3(specValue), destMask), 1.0);
    frag_specular = vec4(vec3(0), 1);
    frag_normal = vec4(unscale(normal), destMask);
#endif
    frag_color = vec4(vec3(mix(groundColor, waterColor, destMask)), 1.0) * multiplyColor;
}

