" Variables {{{
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
" }}}

" Navigation {{{
tnoremap <silent> <M-h> <C-\><C-n><C-w>h
tnoremap <silent> <M-j> <C-\><C-n><C-w>j
tnoremap <silent> <M-k> <C-\><C-n><C-w>k
tnoremap <silent> <M-l> <C-\><C-n><C-w>l

tnoremap <Esc> <C-\><C-n>
tnoremap <C-v><Esc> <Esc>

nnoremap <F1> <nop>
"nnoremap <silent> <C-H> <C-w>h
"nnoremap <silent> <C-J> <C-w>j
"nnoremap <silent> <C-K> <C-w>k
"nnoremap <silent> <C-L> <C-w>l

nnoremap <leader>ev :split ~/.config/nvim/init.vim<CR>
" }}}

" Load plugins {{{
" Preload settings {{{
let g:tmux_navigator_no_mappings = 1
" }}}

call plug#begin('~/.config/nvim/plugged')

"Plug 'SirVer/ultisnips'
"Plug 'honza/vim-snippets' " snippet collection for ultisnips
"Plug 'ludovicchabant/vim-gutentags'

"Plug 'sheerun/vim-polyglot'
" Themes
"Plug 'morhetz/gruvbox'
"Plug 'arcticicestudio/nord-vim'
"Plug 'joshdick/onedark.vim'

"Plug 'vim-airline/vim-airline' 
"Plug 'lervag/vimtex'
"Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
"Plug 'junegunn/fzf.vim'
Plug 'takac/vim-hardtime'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'preservim/nerdcommenter'
Plug 'preservim/nerdtree' |
			\ Plug 'Xuyuanp/nerdtree-git-plugin' 
			" \ Plug 'ryanoasis/vim-devicons' |
			" \ Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

Plug 'nvim-treesitter/nvim-treesitter'
Plug 'christoomey/vim-tmux-navigator'
Plug 'neovim/nvim-lspconfig'
Plug 'hrsh7th/nvim-compe'
"Plug 'nvim-lua/completion-nvim'
"Plug 'pierreglaser/folding-nvim'

"Plug 'jackguo380/vim-lsp-cxx-highlight'

call plug#end()
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
imap <silent>  <Esc>a
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
augroup config_files
	autocmd!
	autocmd BufWritePost ~/.config/nvim/*.{vim,lua} source $MYVIMRC
	autocmd FileType vim setlocal foldmethod=marker
	autocmd FileType tmux setlocal foldmethod=marker
	autocmd BufRead,BufNewFile dotfiles.conf set filetype=dosini
augroup END
"" }}}

" tex_files {{{
"augroup filetype_tex
	"atocmd!
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
