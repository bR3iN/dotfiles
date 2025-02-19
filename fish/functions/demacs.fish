function demacs --wraps='emacs --init-directory ~/.doom.d' --wraps='emacs --init-directory ~/.config/doom' --description 'alias demacs emacs --init-directory ~/.config/doom'
  emacs --init-directory ~/.config/doom $argv
        
end
