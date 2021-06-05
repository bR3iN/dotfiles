setxkbmap -option altwin:prtsc_rwin
PATH="$PATH:$HOME/.node_modules/bin:$HOME/.local/bin"
export QT_SCALE_FACTOR=1.25
export EDITOR=nvim
export VISUAL=nvim

# Load machine specific configuration
[ -f ~/.local_profile ] && . ~/.local_profile

# vim: filetype=sh
