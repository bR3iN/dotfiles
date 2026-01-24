" The keybindings for g{ and g} below move the cursor to the
" beginning and the end of the current paragraph, respectively.
" Think of { and } but the cursor moves one row less and
" preserves the column. Especially useful in virtual block mode.

if exists('g:move_in_paragraph_loaded')
    finish
endif
let g:move_in_paragraph_loaded = 1

xnoremap <expr><silent>g} '<esc>'.v:count1.':<c-u>call MoveInParagraph(1, 0)<cr>'
xnoremap <expr><silent>g{ '<esc>'.v:count1.':<c-u>call MoveInParagraph(1, 1)<cr>'
nnoremap       <silent>g}                   :<c-u>call MoveInParagraph(0, 0)<cr>
nnoremap       <silent>g{                   :<c-u>call MoveInParagraph(0, 1)<cr>
onoremap       <silent>g}                   :<c-u>call MoveInParagraph(0, 0)<cr>
onoremap       <silent>g{                   :<c-u>call MoveInParagraph(0, 1)<cr>


function! MoveInParagraph(visual, backwards)
    let to_column = (col('.') == 1) ? '' : col('.') - 1.'l'
    exec 'normal! '.(a:visual ? 'gv' : '').v:count1.(a:backwards == 0 ? '}' : '{')
    if a:backwards
        exec 'normal! '.(line('.') == 1         ? '^' : 'j').to_column
    else
        exec 'normal! '.(line('.') == line('$') ? '^' : 'k').to_column
    endif
endfunction
