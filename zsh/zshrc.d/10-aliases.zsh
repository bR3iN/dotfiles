alias ga='git add'
alias gc='git commit'
alias gd='git diff'
alias gl='git log'
alias grep='grep --color'
alias gs='git status'
alias less='less --mouse'
alias ll='ls --color -FlhA'
alias ls='ls --color -F'
alias se='sudoedit'

if [ "$TERM" = xterm-kitty ] && [ -x /usr/bin/kitty ]; then
    alias ss='kitty +kitten ssh'
fi

if [ -x /usr/bin/bat ]; then
    alias cat=bat
    export PAGER=bat
    export BAT_PAGER=less
elif [ -x /usr/bin/batcat ]; then
    alias cat=batcat
    export PAGER=batcat
    export BAT_PAGER=less
else
    export PAGER=less
fi
