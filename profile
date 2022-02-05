export PATH="$PATH:$HOME/.local/bin:$HOME/.node_modules/bin:$HOME/.cargo/bin"

export EDITOR=nvim
export VISUAL=nvim
export MANPAGER='nvim +Man!'

# Colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

if [ -d ~/.profile.d ]; then
    for file in ~/.profile.d/*; do
        source "$file" 2> /dev/null
    done
fi

# vim: ft=bash
