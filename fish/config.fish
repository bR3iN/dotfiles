#!/usr/bin/fish

if which bat &> /dev/null
    alias cat 'bat'
    set -x PAGER 'bat'
    set -x BAT_PAGER 'less'
else if which batcat &> /dev/null
    alias cat 'batcat'
    set -x PAGER 'batcat' 
    set -x BAT_PAGER 'less'
else
    set -x PAGER 'less'
end

if [ -x /usr/bin/fish ]
    alias tmux='SHELL=/usr/bin/fish command tmux'
end

alias igrep 'grep -i'
alias less 'less --mouse'
alias ll 'ls -AhlF'
alias gs 'git status'
alias ga 'git add'
alias gc 'git commit'
alias gl 'git log'
alias gd 'git diff'
alias se 'sudoedit'

set fish_greeting
#set fish_prompt_pwd_dir_length 0
set -x  GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'
#set -x  EDITOR nvim
#set -x  VISUAL nvim
set -x  CDPATH . ~
set -x  MANPAGER 'nvim +Man!'
#set -xa PATH "$HOME/.node_modules/bin"
#set -xa PATH "$HOME/.local/bin"

bind \e, history-token-search-forward

if test -f "$HOME/.config/fish/fish.local"
    source "$HOME/.config/fish/fish.local"
end

if [ $TERM = xterm-kitty ]
    alias ssh 'kitty +kitten ssh'
end

if [ -f /usr/share/fzf/shell/key-bindings.fish ]
    source /usr/share/fzf/shell/key-bindings.fish
end

function run_and_close
    $argv &
    disown
    exit
end
