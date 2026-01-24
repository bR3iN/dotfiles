function rgrep --wraps grep
    grep -R $argv 2>> /dev/null
end
