function sudo --wraps sudo
    if test -z "$argv"
        sudo fish
    else
        command sudo $argv
    end
end
