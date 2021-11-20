let g:netrw_banner = 0
let g:netrw_winsize = 30

nnoremap <silent> <leader>tf :Lexplore<CR>
nnoremap <silent> <leader>tc :Lexplore %:p:h<CR>

augroup netrw_config
    autocmd!
    autocmd filetype netrw call NetrwCreateKeybindings()
augroup END

function! NetrwCreateKeybindings()
    nmap <buffer> l <CR>
    nmap <buffer> h -

    nmap <buffer> L <CR>:Lexplore<CR>
    nmap <buffer> P <C-w>z
endfunction
