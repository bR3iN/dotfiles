# Evaluate prompt
setopt prompt_subst

autoload -Uz vcs_info
zstyle ':vcs_info:*' formats ' (%b)'

# Executed before each prompt
precmd() { vcs_info }

PROMPT='%F{green}%n%f@%m:%B%F{14}%~%f%b$vcs_info_msg_0_
➤ ' # Last character is a non-breaking space to prevent glyph enlargement
