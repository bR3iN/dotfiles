function bak
    for file in $argv
        set file (string trim --right --chars=/ $file)
        mv -i $file $file'.bak'
    end
end
