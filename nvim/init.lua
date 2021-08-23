local cmd             = vim.cmd
local utils           = require'utils'
local augroup         = utils.augroup
local map             = utils.map

-- Load shared vimrc
cmd 'runtime vimrc'

-- Load and configure plugins
map ('n', '<leader>pu', ':lua require"pkg".update()<CR>')
map ('n', '<leader>pc', ':lua require"pkg".clean()<CR>')
map ('n', '<leader>pl', ':lua require"pkg".list()<CR>')

local add = require'pkg'.init()

add 'tpope/vim-surround'
add 'tpope/vim-repeat'
add 'tpope/vim-commentary'
add 'christoomey/vim-tmux-navigator'
add 'georgewitteman/vim-fish'
add 'rust-lang/rust.vim'
add 'lervag/vimtex'

add ('neomake/neomake', function()
    map ('n', '<leader>nm', ':<C-u>Neomake<CR>')
    map ('n', '<leader>nc', ':<C-u>NeomakeClean<CR>')
end)

add ('junegunn/fzf', function()
    map ('n', '<leader>ed', ':FZF<CR>')
end)

add ('neovim/nvim-lspconfig', function()
    -- require 'lsp.ccls'
    -- require 'lsp.sumneko_lua'
    require 'lsp.rls'
    -- require 'lsp.texlab'
    require 'lsp.bashls'
    require 'lsp.vimls'
    -- require 'lsp.tsserver'
end)

add ('hrsh7th/nvim-compe', function()
    local compe = require'plugins.nvim-compe'
    compe.setup()
    compe.bind.tab_role('<Tab>')
    compe.bind.stab_role('<S-Tab>')
    compe.bind.confirm('<CR>')
    compe.bind.cancel('<C-e>')
end)

add ('hrsh7th/vim-vsnip', function()
    add 'rafamadriz/friendly-snippets'
    add 'hrsh7th/vim-vsnip-integ'
    vim.g.vsnip_snippet_dir = vim.fn.stdpath('config') .. '/vsnip'
end)

add ('nvim-treesitter/nvim-treesitter', function()
    add 'nvim-treesitter/nvim-treesitter-textobjects'
    require 'plugins.treesitter'
    vim.api.nvim_exec('TSUpdate', true)
end)

-- Configure terminal
map('n', '<leader>ot', ':below split term://fish | resize 10<CR>')
augroup 'terminal' [[
    autocmd TermOpen * setlocal nonumber norelativenumber | startinsert
    autocmd BufEnter * if &buftype == 'terminal' | :startinsert | endif
]]

-- Misc
require'operators'.setup {
    OpenInBrowser = { n = 'gb', v = 'gb', line = 'gbb' }
}

augroup 'init.lua' [[
    au TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=150}
]]

if vim.env['KITTY_LISTEN_ON']  then
    require'kitty'
end
