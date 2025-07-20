function open_config
    set file (fd -Lt f '.' ~/.config | fzf)
    if test -n "$file"
        "$EDITOR" "$file"
    end
end
