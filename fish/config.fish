#!/usr/bin/fish

function update
    sudo apt update
    sudo apt upgrade -y
    sudo apt autoremove
    flatpak update
end

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
    # Output of `kitty + complete setup fish`
    function __kitty_completions
        # Send all words up to the one before the cursor
        commandline -cop | kitty +complete fish
    end
    complete -f -c kitty -a "(__kitty_completions)"

    alias ssh 'kitty +kitten ssh'
end

# vim: filetype=sh
