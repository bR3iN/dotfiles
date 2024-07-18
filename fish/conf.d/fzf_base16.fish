# Base16 Tokyonight Moon
# Author: bR3iN

set -l color00 '#1E2030'
set -l color01 '#222436'
set -l color02 '#2F334D'
set -l color03 '#444A73'
set -l color04 '#828BB8'
set -l color05 '#C8D3F5'
set -l color06 '#89DDFF'
set -l color07 '#B4F9F8'
set -l color08 '#FF757F'
set -l color09 '#FF966C'
set -l color0A '#FFC777'
set -l color0B '#C3E88D'
set -l color0C '#86E1FC'
set -l color0D '#82AAFF'
set -l color0E '#C099FF'
set -l color0F '#4FD6BE'
set -l bg      '#1B1C2B'

set -l FZF_NON_COLOR_OPTS

for arg in (echo $FZF_DEFAULT_OPTS | tr " " "\n")
    if not string match -q -- "--color*" $arg
        set -a FZF_NON_COLOR_OPTS $arg
    end
end

set -Ux FZF_DEFAULT_OPTS "$FZF_NON_COLOR_OPTS"\
" --color=bg+:$color01,bg:$bg,spinner:$color0C,hl:$color0D"\
" --color=fg:$color04,header:$color0D,info:$color0A,pointer:$color0C"\
" --color=marker:$color0C,fg+:$color06,prompt:$color0A,hl+:$color0D"
