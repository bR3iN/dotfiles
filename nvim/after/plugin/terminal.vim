nnoremap <Plug>OpenTerminal :below split term://fish \| resize 10<CR>

augroup terminal
    au!
    au TermOpen * setlocal nonumber norelativenumber | startinsert
    au BufEnter * if &buftype == 'terminal' | :startinsert | endif
augroup END
