export PATH="$PATH:$HOME/.local/bin:$HOME/.node_modules/bin:$HOME/.cargo/bin:$HOME/.ghcup/bin:$HOME/.cabal/bin:$HOME/.nix-profile/bin"

export EDITOR=nvim
export VISUAL=nvim
export MANPAGER='nvim +Man!'

export GHCUP_USE_XDG_DIRS=

if [ -x "/usr/bin/ssh-agent" ]; then
    eval `ssh-agent` > /dev/null
fi

for file in ~/.profile.d/*; do
    source "$file" 2> /dev/null
done &> /dev/null

# vim: ft=bash
