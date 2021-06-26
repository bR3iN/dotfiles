local cmd             = vim.cmd
local augroup         = require'utils'.augroup
local map             = require'utils'.map
local unrequire       = require'utils'.unrequire -- unload module
local prequire        = require'utils'.prequire  -- pcall require
--local prequire        = require                -- for debugging

-- Load shared vimrc
cmd 'runtime vimrc'

-- Load and configure plugins
cmd 'packadd paq-nvim'
unrequire 'paq-nvim'
paq = require'paq-nvim'.paq

paq 'tpope/vim-surround'
paq 'tpope/vim-repeat'
paq 'preservim/nerdcommenter'
paq 'christoomey/vim-tmux-navigator'
paq 'georgewitteman/vim-fish'

paq { 'savq/paq-nvim', opt = true } do
    map ('n', '<leader>pi', ':PaqInstall<CR>')
    map ('n', '<leader>pu', ':PaqUpdate<CR>')
    map ('n', '<leader>pc', ':PaqClean<CR>')
end

paq 'neomake/neomake' do
    map ('n', '<leader>nm', ':<C-u>Neomake<CR>')
    map ('n', '<leader>nc', ':<C-u>NeomakeClean<CR>')
end

--paq 'preservim/nerdtree' do
    --paq 'Xuyuanp/nerdtree-git-plugin'
    --paq 'ryanoasis/vim-devicons'
    --paq 'tiagofumo/vim-nerdtree-syntax-highlight'
--end

paq { 'junegunn/fzf', run = vim.fn['fzf#install'] } do
    --paq 'junegunn/fzf.vim'
    map ('n', '<leader>ed', ':FZF<CR>')
end

paq 'neovim/nvim-lspconfig' do
    prequire'lsp.ccls'
    prequire'lsp.sumneko_lua'
    prequire'lsp.texlab'
    if prequire'lsp' then
        require'lsp'.with_defaults('bashls')
        require'lsp'.with_defaults('vimls')
        require'lsp'.with_defaults('tsserver')
    end
end

paq 'lervag/vimtex' do
    vim.g.vimtex_format_enabled = 1
    vim.g.vimtex_quickfix_mode  = 0
    vim.g.vimtex_view_method    = 'zathura'
    vim.g.tex_flavor            = 'latex'
end

paq 'hrsh7th/nvim-compe' do
    if prequire'nvim-compe' then
        require'nvim-compe'.tab_role('<Tab>')
        require'nvim-compe'.stab_role('<S-Tab>')
        require'nvim-compe'.confirm('<CR>')
        require'nvim-compe'.cancel('<C-e>')
    end
end

paq 'hrsh7th/vim-vsnip' do
    paq 'rafamadriz/friendly-snippets'
    paq 'hrsh7th/vim-vsnip-integ'
    vim.g.vsnip_snippet_dir = vim.fn.stdpath('config') .. '/vsnip'
end

paq 'nvim-treesitter/nvim-treesitter' do
    paq 'nvim-treesitter/nvim-treesitter-textobjects'
    prequire('treesitter')
end

--map('n', '<leader>ot', ':below split term://fish | resize 10<CR>')
augroup 'terminal' [[
    autocmd TermOpen * setlocal nonumber norelativenumber | startinsert
    autocmd BufEnter * if &buftype == 'terminal' | :startinsert | endif
]]

augroup 'init.lua' [[
    au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=150}
]]
