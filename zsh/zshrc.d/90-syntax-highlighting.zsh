# Needs to be sourced at the end of `.zshrc`
source "$ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=magenta'
ZSH_HIGHLIGHT_STYLES[comment]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=yellow'
ZSH_HIGHLIGHT_STYLES[path]='fg=cyan'
ZSH_HIGHLIGHT_STYLES[path_approx]='fg=orange'
ZSH_HIGHLIGHT_STYLES[default]='fg=15,dim'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=yellow'
