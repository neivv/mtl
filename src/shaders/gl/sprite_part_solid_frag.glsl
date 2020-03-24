#version 330 core

#include <util_team_color.glsl>

in vec2 texCoord;

layout(location = 0) out vec4 frag_color;
layout(location = 1) out vec4 frag_effect;

uniform sampler2D spriteTex;
uniform sampler2D teamcolorTex;

uniform float spriteWidth;
uniform vec4 multiplyColor; // Only used for the alpha channel for fading out in this shader
uniform vec4 teamColor;

// The deferred version of this shader uses deferred_sprite
void main()
{
    vec4 spriteColor = texture(spriteTex, texCoord);

    vec3 dp = vec3(0.30196078, 0.59215686, 0.10980392);
    float brightness = dot(dp, vec3(spriteColor));

    if(brightness < multiplyColor.a)
        discard;

    // overwrite shadow and cloak information
    frag_effect = vec4(0, 0, 0, spriteColor.a);

    float mask_value = texture(teamcolorTex, texCoord).r;
    frag_color.rgb = TeamColor(spriteColor.rgb, teamColor.rgb, mask_value) * multiplyColor.rgb;
    frag_color.a = spriteColor.a;
}

