#version 150

in vec3 wmat0;
in vec3 wmat1;
in vec3 wmat2;

out mat3 wmat;

void main()
{
    wmat = mat3(wmat0, wmat1, wmat2);
}

