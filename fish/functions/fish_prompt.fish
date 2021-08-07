function fish_prompt --description 'Write out the prompt'
    set last_status $status
    set last_pipestatus $pipestatus
    set color_host $fish_color_host
    set lhs
    set rhs

    if set -q SSH_TTY
        set color_host $fish_color_host_remote
    else
        set color_host $fish_color_host
    end

    if [ "$USER" = "root" ]
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
    else
        set color_cwd $fish_color_cwd
        set suffix '➤'
    end

    # User
    set -a lhs (set_color $fish_color_user) $USER

    set -a lhs (set_color normal) '@'

    # Host
    set -a lhs (set_color $color_host) (prompt_hostname)
    set -a lhs (set_color normal) ':'

    # PWD
    set -a lhs (set_color $color_cwd) (prompt_pwd)

    # Git
    set -a lhs (set_color normal) (fish_vcs_prompt)

    # Exit code
    set -a lhs (__fish_print_pipestatus " [" "]" "|" (set_color $fish_color_error) (set_color --bold $fish_color_error) $last_pipestatus)

    function get_width
        echo -n (math (echo -s $argv | string replace --all -r '.*?[mGKH]?' '' | wc -c) - 1)
    end

    set    date_format
    set -a date_format (set_color bryellow) "%d %b"
    set -a date_format (set_color normal) ", "
    set -a date_format (set_color bryellow) "%H"
    set -a date_format (set_color normal) ":"
    set -a date_format (set_color bryellow) "%M"
    set -a date_format (set_color normal) ":"
    set -a date_format (set_color bryellow) "%S"

    set -a rhs (set_color normal) [
    set -a rhs (set_color red) "$CMD_DURATION ms"
    set -a rhs (set_color normal) "; "
    set -a rhs (date +(echo -sn $date_format))
    set -a rhs (set_color normal) ]

    # Print first line
    echo -n -s $lhs
    printf "%-"(math $COLUMNS - (get_width $lhs) - (get_width $rhs))"s" " "
    echo -n -s $rhs

    echo

    # Suffix
    if not test $last_status -eq 0
        set_color $fish_color_error
    else
        set_color normal
    end
    echo -n "$suffix "
    set_color normal
end
