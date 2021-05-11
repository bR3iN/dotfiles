#!/usr/bin/fish

function update
    sudo apt update
    sudo apt upgrade -y
    sudo apt autoremove
    flatpak update
end

if which bat > /dev/null
    set batcmd 'bat'
else if which batcat > /dev/null
    set batcmd 'batcat'
end

alias cat $batcmd

alias igrep 'grep -i'
alias less 'less --mouse'
alias ll 'ls -AhlF'
alias gs 'git status'
alias ga 'git add'
alias gc 'git commit'
alias gl 'git log'
alias gd 'git diff'

set fish_greeting
set fish_prompt_pwd_dir_length 0
set -x  GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
set -x  EDITOR nvim
set -x  VISUAL nvim
set -x  CDPATH . ~
set -x  MANPAGER 'nvim +Man!'
set -x  PAGER "$batcmd"
set -x  BAT_PAGER "less"
set -xa PATH "$HOME/.node_modules/bin"

bind \e, history-token-search-forward

if test -f "$HOME/.fish_private"
    source "$HOME/.fish_private"
end
