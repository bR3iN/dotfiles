" Misc Mappings{{{
nnoremap <buffer> <leader>f gqq
vnoremap <buffer> <leader>f gq
nnoremap <buffer> <leader>u ~h

nmap <buffer> <leader>pad f$%i<Enter><Esc>%a<Enter><Esc>k%ge
nmap <buffer> <leader>Pad \padds$ySSi

nnoremap <buffer> <leader>tt :VimtexTocToggle<CR>
nnoremap <buffer> <leader>td <Esc>o%TODO: 
" }}}

" Cmd Surround {{{
vnoremap <buffer> <leader>em c\emph{<c-r>"}<Esc>
vnoremap <buffer> <leader>op c\mathop{<c-r>"}<Esc>
vnoremap <buffer> <leader>it c\textit{<c-r>"}<Esc>
"vmap <leader>op S}i\mathop
"vmap <leader>it S}i\textit
" }}}

" vim-surround {{{
let b:surround_66 = "\\{\r\\}" " \{ \} on B
let b:surround_105 = "\\[\r\\]" " \[ \] on i
" }}}

augroup ftplugin_tex " {{{
    autocmd BufRead <buffer> main.tex VimtexCompile  
augroup END
"}}}

