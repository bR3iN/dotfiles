if [ -d ~/.profile.d ]; then
    for file in ~/.profile.d/*; do
        source "$file" 2> /dev/null
    done
fi
