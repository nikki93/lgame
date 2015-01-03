#version 150

in vec3 wmat0;                  // columns of world transform matrix
in vec3 wmat1;
in vec3 wmat2;
in vec2 texcell;
in vec2 texsize;

out mat3 wmat;
out vec2 texcell_;
out vec2 texsize_;

void main()
{
    wmat = mat3(wmat0, wmat1, wmat2);
    texcell_ = texcell;
    texsize_ = texsize;
}

