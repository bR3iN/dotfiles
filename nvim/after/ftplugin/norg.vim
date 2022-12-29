setlocal conceallevel=2
setlocal nowrap
vnoremap g> :<C-u>'<,'>s/^\w*\zs\ze\*\+/*/g<CR>
abbreviate <buffer> #c #contexts
abbreviate <buffer> -( - ( )
