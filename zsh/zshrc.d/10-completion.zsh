# On an ambiguous completion, insert first match immediately.
# Note: `no_list_ambiguous` only inserts the common substring
# but doesn't cirumvents the prompt if there are many options.
#setopt menu_complete

# Complete unambiguous substring _and_ show menu
setopt no_list_ambiguous

# Highlight unambiguous part
zstyle -e ':completion:*' list-colors 'reply=("${PREFIX:+=(#bi)($PREFIX:t)(?)*==34}:${(s.:.)LS_COLORS}")'

# Also match case-insensitive and complete from a substring
# Legend:
#   <empty>             match prefix
#   m:{a-zA-Z}={A-Za-z} match case insensitive
#   r:|[._-]=* r:|=*    match substring but only after one of "._-"
#   r:|=* l:|=*         match substring
zstyle ':completion:*' matcher-list \
    '' \
    'm:{a-zA-Z}={A-Za-z}' \
    'r:|=* l:|=*' \
    'm:{a-zA-Z}={A-Za-z} r:|=* l:|=*'

# Don't store cache in config directory
# TODO: check its used, delete old cache
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompcache"

# Initialize completion
autoload -Uz compinit; compinit
zstyle ':completion:*' completer _extensions _complete _approximate
zstyle ':completion:*' menu select
alias a=zsh

# Group completion items under their description
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:messages' format ' %F{purple} -- %d --%f'
zstyle ':completion:*:warnings' format ' %F{red}!! no matches found !!%f'

# Highlight the description of options in yellow
# FIXME: doesn't work for `ls -s<Tab>`
# zstyle ':completion:*:options' list-colors '=-- *=2;33'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
