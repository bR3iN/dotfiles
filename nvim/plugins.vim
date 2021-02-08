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
