local cmd             = vim.cmd
local augroup         = require'utils'.augroup
local map             = require'utils'.map
local unrequire       = require'utils'.unrequire
local prequire        = require'utils'.prequire -- pcall require
--local prequire        = require -- for debugging

-- Load shared vimrc
cmd 'runtime vimrc'

-- Initialize paq-nvim
cmd 'packadd paq-nvim'
unrequire 'paq-nvim'
local paq = require'paq-nvim'.paq
paq { 'savq/paq-nvim', opt=true }

-- Load and configure plugins {{{

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

-- LSP {{{
paq 'neovim/nvim-lspconfig'
prequire('lsp.ccls')
if prequire('lsp') then
    local with_defaults = require'lsp'.with_defaults
    with_defaults 'bashls'
    with_defaults 'vimls'
    with_defaults 'tsserver'
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

map('i', '<Tab>',   'pumvisible() ? "\\<C-n>" : "\\<Tab>"',   {expr = true})
map('i', '<S-Tab>', 'pumvisible() ? "\\<C-p>" : "\\<S-Tab>"', {expr = true})
map('n', '<leader>ed', ':FZF<CR>')

augroup 'filetype_lua' [[
    autocmd FileType lua nnoremap <buffer> <leader>r :lua dofile(vim.fn.expand('%'))<cr>
    autocmd FileType lua let b:surround_66 = "{\r}\1\1"
]]
