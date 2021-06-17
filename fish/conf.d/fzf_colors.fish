# Base16 Pop-OS
# Author: bR3iN

set -l color00 '#202020'
set -l color01 '#2A2A2A'
set -l color02 '#333333'
set -l color03 '#3D3D3D'
set -l color04 '#E3E3E3'
set -l color05 '#EBEBEB'
set -l color06 '#F2F2F2'
set -l color07 '#F4F4F4'
set -l color08 '#E23434'
set -l color09 '#F19822'
set -l color0A '#FFCE51'
set -l color0B '#73C48F'
set -l color0C '#48B9C7'
set -l color0D '#34E2E2'
set -l color0E '#AD7FA8'
set -l color0F '#AD9B7F'

set -l FZF_NON_COLOR_OPTS

for arg in (echo $FZF_DEFAULT_OPTS | tr " " "\n")
    if not string match -q -- "--color*" $arg
        set -a FZF_NON_COLOR_OPTS $arg
    end
end

set -U FZF_DEFAULT_OPTS "$FZF_NON_COLOR_OPTS"\
" --color=bg+:$color01,bg:$color00,spinner:$color0C,hl:$color0D"\
" --color=fg:$color04,header:$color0D,info:$color0A,pointer:$color0C"\
" --color=marker:$color0C,fg+:$color06,prompt:$color0A,hl+:$color0D"
