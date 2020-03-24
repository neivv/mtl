
in vec2 texCoord;
layout(location = 0) out vec4 frag_color;
layout(location = 1) out vec4 frag_effect;
uniform sampler2D spriteTex;

void main()
{
    vec4 tex = texture(spriteTex, texCoord);
    frag_color = tex;

#if DRAW_EFFECT
    frag_effect = tex;
#ifdef ALPHA_DRAW
    if(frag_effect.a < 1)
        frag_effect.a = 1;
    else
        frag_effect.a = 0;
#elif !defined(COLOR_DRAW)
    if(frag_effect.a < 1)
        frag_effect.a = 0;
#endif

#else
    // This shader is used for health bars on the terrain, and we want
    // to clear the effects from under those
    frag_effect = vec4(vec3(0), 1);
#endif
}

