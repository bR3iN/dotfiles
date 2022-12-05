setlocal conceallevel=2
vnoremap g> :<C-u>'<,'>s/^\w*\zs\ze\*\+/*/g<CR>
abbreviate <buffer> #c #contexts
abbreviate <buffer> -[ - [ ]
