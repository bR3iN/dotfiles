" Enviroment Variables {{{
let $VIMCONFDIR = '~/.vim'
let $NVIMCONFDIR = '~/.config/nvim'
let $PLUGINS = '~/.config/nvim/plugins.vim'
if executable('nvr') | let $VISUAL="nvr -cc split --remote-wait +'set bufhidden=wipe'" | endif
" }}}

" General Options {{{
set cmdheight=2
set confirm
set ignorecase
set hidden 
set mouse=a
set number relativenumber
set ruler
set scrolloff=1
set showcmd
set smartcase " requires ignorecase
set wildmode=longest:full,full
set undofile
set undodir=~/.config/nvim/undo
set splitright
set splitbelow

" Wrap behaviour
set breakindent
set wrap

" Default tab behaviour
set shiftwidth=4
set tabstop=4
" }}}

" Navigation {{{
tnoremap <silent> <M-h> <C-\><C-n><C-w>h
tnoremap <silent> <M-j> <C-\><C-n><C-w>j
tnoremap <silent> <M-k> <C-\><C-n><C-w>k
tnoremap <silent> <M-l> <C-\><C-n><C-w>l

tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

"nnoremap <silent> <C-H> <C-w>h
"nnoremap <silent> <C-J> <C-w>j
"nnoremap <silent> <C-K> <C-w>k
"nnoremap <silent> <C-L> <C-w>l

nnoremap <leader>ev :split $MYVIMRC<CR>
" }}}

" Load plugins {{{
let g:tmux_navigator_no_mappings = 1

if filereadable(expand("~/.config/nvim/plugins.vim"))
	source ~/.config/nvim/plugins.vim
endif
" }}}

" Configure LSP {{{
lua require('lsp') 
lua require('treesitter')
lua require('completion')
" }}}

" Configure plugins {{{

" NERDCommenter {{{
nmap <silent>  <Plug>NERDCommenterToggle
vmap <silent>  <Plug>NERDCommenterToggle
" }}}

" NERDTree {{{
"nnoremap <silent> <C-t> :NERDTreeToggle<CR>
"nnoremap <silent> <C-n> :NERDTreeFind<CR>
	" Exit Vim if NERDTree is the only window left.
"augroup plugin_NERDTree
	"autocmd!
	"autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
				"\ quit | endif
"augroup END
" }}}

" Tmux Navigator {{{
nnoremap <silent> <M-h> :TmuxNavigateLeft<cr>
nnoremap <silent> <M-j> :TmuxNavigateDown<cr>
nnoremap <silent> <M-k> :TmuxNavigateUp<cr>
nnoremap <silent> <M-l> :TmuxNavigateRight<cr>
" }}}

" nvim-compe {{{
set completeopt=menu,menuone,noselect,noinsert,preview
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" }}}

" FZF {{{
"nnoremap <silent> <C-p> :<C-u>FZF<CR>
" }}}

" Vimtex {{{
"let g:vimtex_view_method = 'zathura'
"let g:vimtex_quickfix_mode = 0
"let g:tex_flavor='latex'
" }}}

" }}}

" Autocmd {{{

" config_files {{{
augroup filetype_config
	autocmd!
	autocmd BufWritePost ~/.config/nvim/*.{vim,lua} source $MYVIMRC
	autocmd FileType vim setlocal foldmethod=marker
augroup END
"" }}}

" tex_files {{{
"augroup filetype_tex
	"autocmd FileType tex let b:surround_105 = "\\[\r\\]" " \[ \] on i
	"autocmd FileType tex nmap \pad %i<Enter><Esc>%a<Enter><Esc>k%ge
	"autocmd Filetype tex set conceallevel=2
	"autocmd BufRead main.tex VimtexCompile	
"augroup END
" }}}

" init_misc {{{
augroup init_misc
	autocmd!
	autocmd BufWritePre /tmp/* setlocal noundofile
augroup END
" }}}

" terminal {{{
augroup terminal
	autocmd!
	autocmd TermOpen * setlocal nonumber norelativenumber | startinsert
	autocmd BufEnter * if &buftype == 'terminal' | :startinsert | endif
augroup END
" }}}

" }}}

" Commands {{{

" Terminal {{{
function! OpenTerminal()
	split term://bash 
	resize 10
endfunction
" }}}

" }}}

" Mappings {{{
nnoremap <silent> <leader>ot <cmd>call OpenTerminal()<CR>
nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
" }}}

" Appearence {{{
"colorscheme nord
"set termguicolors
hi StatusLine ctermbg=8 ctermfg=green cterm=bold
hi StatusLineNC ctermbg=8 ctermfg=green cterm=NONE
hi VertSplit cterm=None ctermfg=8 ctermbg=NONE
hi LineNr ctermfg=7
hi CursorLineNr ctermfg=7
hi Pmenu ctermbg=8 ctermfg=11
hi PmenuSel ctermbg=0 ctermfg=14
hi PmenuThump ctermbg=7
hi LspDiagnosticsDefaultError ctermfg=9
hi LspDiagnosticsDefaultWarning ctermfg=11
hi LspDiagnosticsDefaultHint ctermfg=13
hi LspDiagnosticsDefaultInformation ctermfg=10

set fillchars+=vert:\│
" }}}
