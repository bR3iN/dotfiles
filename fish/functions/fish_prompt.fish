function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status
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
        set suffix '❯'
    end

    if set -q VIRTUAL_ENV
        set -l venv_name (basename $VIRTUAL_ENV)
        set -a lhs (set_color normal) '('
        set -a lhs (set_color blue) $venv_name
        set -a lhs (set_color normal) ') '
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


    # Exit code
    set -a rhs (__fish_print_pipestatus "" " " "|" (set_color normal) (set_color --bold $fish_color_error) $last_pipestatus)

    set -a rhs (set_color normal) [

    set -a rhs (set_color blue) "$CMD_DURATION ms"
    set -a rhs (set_color normal) "; "
    set -a rhs (date +(echo -sn $date_format))
    set -a rhs (set_color normal) ]

    # Print first line
    echo -n -s $lhs
    printf "%-"(math $COLUMNS - (get_width $lhs) - (get_width $rhs) - (get_width $VIRTUAL_ENV_PROMPT))"s" " "
    echo -n -s $rhs

    echo

    # # Suffix
    # if not test $last_status -eq 0
    #     set_color $fish_color_error
    # else
    #     set_color normal
    # end

    # Non-breaking space to prevent glyph enlargement
    # echo -n "$suffix "
    echo -n "$suffix "
    set_color normal
end
