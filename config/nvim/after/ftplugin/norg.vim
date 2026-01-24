vnoremap <buffer><silent> g> :<C-u>'<,'>s/^\w*\zs\ze\*\+/*/g<CR>
nnoremap <buffer><silent> _ :Neorg toc inline<CR>jj

" Copy a link to the current file into the selected register
noremap <buffer><silent> <leader>nr :<C-u>call setreg(v:register, "{:".expand("%:t:r").":}")<CR>
noremap <buffer><silent> <leader>nR :<C-u>call setreg(v:register, "{:$references/".expand("%:t:r").":}")<CR>

" Insert a link to the alternative file
inoremap <buffer><silent> <C-r>3 {:<C-r>=expand("#:t:r")<Cr>:}
inoremap <buffer><silent> <C-r># {:$references/<C-r>=expand("#:t:r")<Cr>:}

" Find back-references to current file in current directory
noremap <buffer><silent> <leader>nb :<C-u>grep! -RF -e '{:<C-r>=expand("%:t:r")<CR>:}' -e '{:$/<C-r>=expand("%:t:r")<CR>:}' '<C-r>=expand('%:h')<CR>'<CR>:<C-u>copen<CR>

abbreviate <buffer> #c #contexts
abbreviate <buffer> -( - ( )
abbreviate <buffer> -p - P.
abbreviate <buffer> -c - Ch.
abbreviate <buffer> REF * References-
