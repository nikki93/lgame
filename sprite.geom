#version 150

layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

void main()
{
    gl_Position = gl_in[0].gl_Position + vec4(-0.5,  0.5, 0.0, 0.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + vec4(-0.5, -0.5, 0.0, 0.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + vec4( 0.5,  0.5, 0.0, 0.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + vec4( 0.5, -0.5, 0.0, 0.0);
    gl_Position *= vec4(0.08, 0.1066666667, 1.0, 1.0);
    EmitVertex();

    EndPrimitive();
}

