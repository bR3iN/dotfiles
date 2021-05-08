function update
    sudo apt update
    sudo apt upgrade -y
    sudo apt autoremove
    flatpak update
end

alias cat batcat

set fish_greeting
set fish_prompt_pwd_dir_length 0

set -x EDITOR nvim
set -x VISUAL nvim
set -x CPATH . ~
