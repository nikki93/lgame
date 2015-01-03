#version 150

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

in mat3 wmat[];

out vec2 texcoord;

uniform vec2 atlas_size;

void main()
{
    gl_Position = vec4(wmat[0] * vec3(-0.5,  0.5, 1.0), 1.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    texcoord = vec2(0.0, 64.0) / atlas_size;
    EmitVertex();

    gl_Position = vec4(wmat[0] * vec3(-0.5, -0.5, 1.0), 1.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    texcoord = vec2(0.0, 96.0) / atlas_size;
    EmitVertex();

    gl_Position = vec4(wmat[0] * vec3( 0.5,  0.5, 1.0), 1.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    texcoord = vec2(32.0, 64.0) / atlas_size;
    EmitVertex();

    gl_Position = vec4(wmat[0] * vec3( 0.5, -0.5, 1.0), 1.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    texcoord = vec2(32.0, 96.0) / atlas_size;
    EmitVertex();

    EndPrimitive();
}

