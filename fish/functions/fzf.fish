function fzf --wraps fzf
    if [ -z "$TMUX" ]
        command fzf $argv
    else
        fzf-tmux $argv
    end
end
    
