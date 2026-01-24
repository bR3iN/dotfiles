# Setup starship prompt
starship init fish | source

# Setup post-cmd prompt
function post_command_action --on-event fish_postexec
    STARSHIP_CONFIG=~/.config/starship-post.toml starship prompt \
        --terminal-width=$COLUMNS --status=$status --pipestatus=$pipestatus \
        --cmd-duration="$CMD_DURATION$cmd_duration"
end
