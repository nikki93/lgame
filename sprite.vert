#version 150

in vec2 pos;
in vec2 cell;
in vec2 size;

out vec2 cell_;
out vec2 size_;

void main()
{
    gl_Position = vec4(pos, 0.0, 1.0);
    cell_ = cell;
    size_ = size;
}

