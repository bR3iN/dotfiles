#!/usr/bin/fish

if [ -x /usr/bin/bat ]
    alias cat bat
    set -x PAGER bat
    set -x BAT_PAGER less
else if [ -x /usr/bin/batcat ]
    alias cat batcat
    set -x PAGER batcat
    set -x BAT_PAGER less
else
    set -x PAGER less
end

alias ll 'ls -AhlF'
alias z zellij

alias usage 'du -hd 1'
alias igrep 'grep -i'
alias less 'less --mouse'
alias gs 'git status'
alias ga 'git add'
alias gc 'git commit'
alias gl 'git log'
alias gd 'git diff'
alias se sudoedit
alias te 'TERM=xterm toolbox enter'
alias tc 'toolbox create --image toolbox'
alias gdb 'gdb -tui'

set fish_greeting
#set fish_prompt_pwd_dir_length 0
set -x GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
set -x CDPATH . ~

bind \el true
bind \eh true
bind \eo __fish_list_current_token
bind \em __fish_man_page
bind \e, history-token-search-forward
bind \co open_config

# Enable colors in git prompt
set __fish_git_prompt_showcolorhints 1

if [ $TERM = xterm-kitty ] && [ -x /usr/bin/kitty ]
    alias ssh 'kitty +kitten ssh'
end

if [ -f /usr/share/fzf/shell/key-bindings.fish ] && status is-interactive
    source /usr/share/fzf/shell/key-bindings.fish
    fzf_key_bindings
end
