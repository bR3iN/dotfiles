" Options {{{
setlocal textwidth=80
" }}}

" Misc Mappings{{{
nnoremap <buffer> <leader>f gqq
vnoremap <buffer> <leader>f gq
nnoremap <buffer> <leader>u ~h

" Use a literal <cr> in recursive mappings instead of
" the one overwritten by nvim-compe
inoremap <buffer> <plug>cr <cr>

nmap <buffer> <leader>pad f$%i<plug>cr<Esc>%a<plug>cr<Esc>k%ge
nmap <buffer> <leader>Pad <leader>padds$ySSi
nmap <buffer> <leader>PPad cs$$<cr><leader>Pad

nnoremap <buffer> <leader>td <Esc>o%TODO:<space>

" overwrites mapping from init.vim
nnoremap <buffer> <leader>qo :<c-u>copen<cr>/error<cr>
" }}}

" Cmd Surround {{{
vnoremap <buffer> <leader>em c\emph{<c-r>"}<Esc>
vnoremap <buffer> <leader>op c\mathop{<c-r>"}<Esc>
vnoremap <buffer> <leader>it c\textit{<c-r>"}<Esc>
" }}}

" vim-surround {{{
let b:surround_66 = "\\{\r\\}" " \{ \} on B
let b:surround_105 = "\\[\r\\]" " \[ \] on i
" }}}
