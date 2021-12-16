function unbak
    for file in $argv
        if string match -q --regex '\.bak/?$' $file
            set -l new_file (string replace --regex '\.bak/?$' '' $file)
            mv -i $file $new_file
        else
            echo -n (set_color red)
            echo -n 'ERROR: '
            echo -n (set_color normal)
            echo -n "File '"$file"' does not end in '.bak'"
            echo
        end
    end
end
