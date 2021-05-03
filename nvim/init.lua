local cmd             = vim.cmd
local augroup         = require'utils'.augroup
local map             = require'utils'.map
local unrequire       = require'utils'.unrequire
local prequire        = require'utils'.prequire -- pcall require
--local prequire        = require -- for debugging

-- Load shared vimrc
cmd 'runtime vimrc'

-- Load and configure plugins {{{

-- Initialize paq-nvim {{{
cmd 'packadd paq-nvim'
unrequire 'paq-nvim'
local paq = require'paq-nvim'.paq
paq { 'savq/paq-nvim', opt=true }
-- }}}

paq 'tpope/vim-surround'
paq 'tpope/vim-repeat'
paq 'preservim/nerdcommenter'
paq 'christoomey/vim-tmux-navigator'

--paq 'preservim/nerdtree' do 
    --paq 'Xuyuanp/nerdtree-git-plugin'
    --paq 'ryanoasis/vim-devicons'
    --paq 'tiagofumo/vim-nerdtree-syntax-highlight'
--end

paq { 'junegunn/fzf', run = vim.fn['fzf#install'] } do
    --paq 'junegunn/fzf.vim'
end

paq 'hrsh7th/vim-vsnip'
paq 'rafamadriz/friendly-snippets'
paq 'hrsh7th/vim-vsnip-integ'

-- LSP {{{
paq 'neovim/nvim-lspconfig'
local lsps = {
    'ccls',
    'sumneko_lua',
    'bashls',
    'vimls',
    'tsserver',
}
for _, lsp in ipairs(lsps) do
    prequire('lsp.'..lsp)
end
-- }}}

paq 'lervag/vimtex'
prequire('vimtex')

paq 'hrsh7th/nvim-compe'
prequire('nvim-compe')

paq 'nvim-treesitter/nvim-treesitter' do
    paq 'nvim-treesitter/nvim-treesitter-textobjects'
end
prequire('treesitter')
-- }}}

prequire('terminal')

map('n', '<leader>ed', ':FZF<CR>')

augroup 'init.lua' [[
    au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=150}
]]

augroup 'filetype_lua' [[
    autocmd FileType lua nnoremap <buffer> <leader>r :lua dofile(vim.fn.expand('%'))<cr>
    autocmd FileType lua let b:surround_66 = "{\r}\1\1"
]]
