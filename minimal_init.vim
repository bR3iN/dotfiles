" Variables {{{
let maplocalleader = " "
let mapleader = " "
let $MYVIMRC = "~/.config/nvim/init.vim"
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
set expandtab
" }}}

" Navigation {{{
nnoremap <leader>b  :<c-u>ls<cr>:<c-u>b 

tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

nnoremap <F1> <nop>
" }}}

" Load Plugins {{{
" Preload Settings {{{
let g:tmux_navigator_no_mappings = 1
let g:NERDCreateDefaultMappings = 0
" }}}

call plug#begin('~/.config/nvim/plugged')

" Themes
"Plug 'morhetz/gruvbox'
"Plug 'arcticicestudio/nord-vim'
"Plug 'joshdick/onedark.vim'

Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'preservim/nerdcommenter'

Plug 'christoomey/vim-tmux-navigator'

call plug#end()
" }}}

" Configure Plugins {{{

" NERDCommenter {{{
nmap <silent>  <Plug>NERDCommenterToggle
vmap <silent>  <Plug>NERDCommenterToggle
inoremap <silent>  <Esc>:exec "normal \<Plug>NERDCommenterToggle"<cr>a
" }}}

" Tmux Navigator {{{
nnoremap <silent> <M-h> :<c-u>TmuxNavigateLeft<cr>
nnoremap <silent> <M-j> :<c-u>TmuxNavigateDown<cr>
nnoremap <silent> <M-k> :<c-u>TmuxNavigateUp<cr>
nnoremap <silent> <M-l> :<c-u>TmuxNavigateRight<cr>

tnoremap <silent> <M-h> <C-\><C-n>:TmuxNavigateLeft<cr>
tnoremap <silent> <M-j> <C-\><C-n>:TmuxNavigateDown<cr>
tnoremap <silent> <M-k> <C-\><C-n>:TmuxNavigateUp<cr>
tnoremap <silent> <M-l> <C-\><C-n>:TmuxNavigateRight<cr>
" }}}

" }}}

" Autocmd {{{

augroup config_files " {{{
	autocmd!
	autocmd BufWritePost ~/.config/nvim/*.{vim,lua} source $MYVIMRC
	autocmd BufWritePost ~/.dotfiles/nvim/*.{vim,lua} source $MYVIMRC
	autocmd FileType vim setlocal foldmethod=marker
	autocmd FileType tmux setlocal foldmethod=marker
augroup END
"" }}}

augroup init_misc " {{{
	autocmd!
	autocmd BufWritePre /tmp/* setlocal noundofile
augroup END
" }}}

augroup terminal " {{{
	autocmd!
	autocmd TermOpen * setlocal nonumber norelativenumber | startinsert
	autocmd BufEnter * if &buftype == 'terminal' | :startinsert | endif
augroup END
" }}}

" }}}

" Misc Mappings {{{
nnoremap <silent> <C-L> :<c-u>nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
nnoremap <leader>w :<c-u>w<cr>
nnoremap <leader>qq :<c-u>q<cr>
inoremap <c-u> <esc>viwUea
nnoremap Y y$
" }}}

" Appearence {{{
"colorscheme nord
"set termguicolors
hi StatusLine ctermbg=8 ctermfg=green cterm=bold
hi StatusLineNC ctermbg=8 ctermfg=green cterm=NONE
hi VertSplit cterm=None ctermfg=8 ctermbg=NONE
hi LineNr ctermfg=7
"hi CursorLineNr ctermfg=7
hi Pmenu ctermbg=8 ctermfg=11
hi PmenuSel ctermbg=0 ctermfg=14
hi PmenuThump ctermbg=7
hi LspDiagnosticsDefaultError ctermfg=9
hi LspDiagnosticsDefaultWarning ctermfg=11
hi LspDiagnosticsDefaultHint ctermfg=13
hi LspDiagnosticsDefaultInformation ctermfg=10
hi CursorLine ctermfg=14

set fillchars+=vert:\│
" }}}

