# On Kitty terminal, disable opacity while neovim is running
if [ $TERM = xterm-kitty ]
    function __get-opacity
        grep '^background_opacity' $argv \
            | awk '{ print $2 }' 2> /dev/null
    end

    set -l KITTY_DIR ~/.config/kitty
    set current_background_opacity (__get-opacity $KITTY_DIR/local.conf)

    if [ -z $current_background_opacity ]
        set current_background_opacity (__get-opacity $KITTY_DIR/kitty.conf)
    end

    if [ -z $current_background_opacity ]
        set current_background_opacity 1
    end

    function nvim --wraps nvim --description 'Wrap neovim on Kitty terminal'
        kitty @ set-background-opacity 1
        command nvim $argv
        kitty @ set-background-opacity $current_background_opacity
    end
end
