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
nnoremap <leader>sb	:<c-u>ls<cr>:<c-u>sb 
nnoremap <leader>vb	:<c-u>ls<cr>:<c-u>vert sb 


tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

nnoremap <F1> <nop>

nnoremap <leader>ev :<c-u>edit   ~/.config/nvim/init.vim<CR>
nnoremap <leader>sv :<c-u>split  ~/.config/nvim/init.vim<CR>
nnoremap <leader>vv :<c-u>vsplit ~/.config/nvim/init.vim<CR>

nnoremap <leader>co :<c-u>copen<cr>
nnoremap <leader>cc :<c-u>cclose<cr>
" }}}

" Load Plugins {{{
" Preload Settings {{{
let g:tmux_navigator_no_mappings = 1
let g:NERDCreateDefaultMappings = 0
" }}}

call plug#begin('~/.config/nvim/plugged')

"Plug 'SirVer/ultisnips'
"Plug 'honza/vim-snippets' " snippet collection for ultisnips

" Themes
"Plug 'morhetz/gruvbox'
"Plug 'arcticicestudio/nord-vim'
"Plug 'joshdick/onedark.vim'

"Plug 'vim-airline/vim-airline' 
Plug 'lervag/vimtex'
"Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
"Plug 'junegunn/fzf.vim'
Plug 'takac/vim-hardtime'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'preservim/nerdcommenter'
Plug 'preservim/nerdtree' |
             "\ Plug 'Xuyuanp/nerdtree-git-plugin' |
             \ Plug 'ryanoasis/vim-devicons' |
			" \ Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

Plug 'nvim-treesitter/nvim-treesitter'
Plug 'christoomey/vim-tmux-navigator'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'

"Plug 'jackguo380/vim-lsp-cxx-highlight'

call plug#end()
" }}}

" Configure LSP {{{
lua require('lsp') 
lua require('treesitter')
lua require('completion')
" }}}

" Configure Plugins {{{

" NERDCommenter {{{
nmap <silent>  <Plug>NERDCommenterToggle
vmap <silent>  <Plug>NERDCommenterToggle
inoremap <silent>  <Esc>:exec "normal \<Plug>NERDCommenterToggle"<cr>a
" }}}

" NERDTree {{{
nnoremap <silent> <leader>nt :<c-u>NERDTreeToggle<CR>
nnoremap <silent> <leader>nf  :<c-u>NERDTreeFind<CR>
	" Exit Vim if NERDTree is the only window left.
augroup plugin_NERDTree
    autocmd!
    "autocmd BufEnter * NERDTreeFind | wincmd p
    autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
                \ quit | endif
augroup END
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

" nvim-compe {{{
set completeopt=menu,menuone,noselect,noinsert,preview
inoremap <silent><expr> <C-Space> compe#complete()
inoremap <silent><expr> <CR>      compe#confirm('<CR>')
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" }}}

" FZF {{{
"nnoremap <silent> <leader>p :<C-u>FZF<CR>
" }}}

" Vimtex {{{
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode = 0
let g:tex_flavor='latex'
let g:vimtex_format_enabled = 1
augroup vimtex_config
    autocmd BufRead main.tex VimtexCompile  
augroup END
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
    "autocmd FileType help autocmd BufWinEnter <buffer> wincmd L
augroup END
" }}}

augroup terminal " {{{
	autocmd!
	autocmd TermOpen * setlocal nonumber norelativenumber | startinsert
	autocmd BufEnter * if &buftype == 'terminal' | :startinsert | endif
augroup END
" }}}

" }}}

" Commands {{{

" Terminal {{{
function OpenTerminal()
	:below split term://bash 
	resize 10
endfunction
" }}}

" }}}

" Misc Mappings {{{
nnoremap <silent> <leader>ot <cmd>call OpenTerminal()<CR>
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

