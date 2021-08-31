-- Imports {{{

local utils       = require'utils'
local augroup     = utils.augroup
local command     = utils.command
local map         = utils.map
local add         = require'pkg'.init()

-- }}}

-- Set variables {{{

vim.g.browser = 'qutebrowser'

-- Set leader keys
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

if vim.fn.executable('nvr') == 1 then
    vim.env.VISUAL='nvr -cc split --remote-wait +"set bufhidden=wipe"'
end

-- }}}

-- Options {{{

utils.set_options {
    cmdheight = 2,
    confirm = true,
    hidden  = true,
    ignorecase = true,
    mouse = 'a',
    number = true,
    relativenumber = true,
    ruler = true,
    scrolloff = 5,
    shell = '/usr/bin/bash',
    showcmd = true,
    smartcase = true, --requires ignorecase
    splitbelow = true,
    splitright = true,
    wildignorecase = true,
    wildmode =  'longest:full,full',

    -- Undo behaviour
    undofile = true,
    undodir  = vim.env['HOME'] .. '/.local/share/nvim/undo',

    -- Wrap behaviour
    breakindent = true,
    wrap = true,

    -- Default tab behaviour
    shiftwidth = 4,
    tabstop = 4,
    expandtab = true,
}

-- let :find search subdirectories
vim.opt.path:append('**')

--}}}

-- Mappings {{{

map ('n', 'Y', 'y$')
map ('n', '<leader>d', '"_d')
map ('n', '<leader>V', 'vg_') -- Does not mark newline
map ('n', 'gQ', '<nop>')
map ('n', '<C-c>', '<C-^>')
map ('n', '<C-h>', ':ToggleComments<CR>')
map ('n', '<C-L>', ':<c-u>nohlsearch<CR><C-L>')
map ('n', '<leader>mk', ':make<CR>')

map ('t', '<Esc>', '<C-\\><C-n>')
map ('t', '<C-v><Esc>', '<Esc>')

-- Open text in browser
map ('n', 'gb',   '<Plug>OpenInBrowser', { noremap = false })
map ('v', 'gb',   '<Plug>OpenInBrowser', { noremap = false })
map ('n', 'gbb', 'V<Plug>OpenInBrowser', { noremap = false })

-- Open terminal
map('n', '<leader>ot', '<Plug>OpenTerminal', { noremap = false })

-- Open init.lua
map ('n', '<leader>ev', ':<c-u>edit   $MYVIMRC<CR>')
map ('n', '<leader>sv', ':<c-u>split  $MYVIMRC<CR>')
map ('n', '<leader>vv', ':<c-u>vsplit $MYVIMRC<CR>')

-- Open files
map ('n', '<leader>f' , ':find<space>'      , { silent = false })
map ('n', '<leader>sf', ':sfind<space>'     , { silent = false })
map ('n', '<leader>vf', ':vert sfind<space>', { silent = false })
-- Buffer navigation
map ('n', '<leader>b' , ':<C-u>ls<cr>:<c-u>b<space>'      , { silent = false })
map ('n', '<leader>sb', ':<C-u>ls<cr>:<c-u>sb<space>'     , { silent = false })
map ('n', '<leader>vb', ':<C-u>ls<cr>:<c-u>vert sb<space>', { silent = false })
map ('n', '<leader>db', ':<C-u>ls<cr>:<c-u>bd<space>'     , { silent = false })

-- Navigate quickfix and location lists
map ('n', ']q', ':<C-u>cnext<CR>')
map ('n', '[q', ':<C-u>cprev<CR>')
map ('n', ']Q', ':<C-u>clast<CR>')
map ('n', '[Q', ':<C-u>cfirst<CR>')
map ('n', '<leader>oq', ':<C-u>copen<CR>')
map ('n', '<leader>cq', ':<C-u>cclose<CR>')

map ('n', ']l', ':<C-u>lnext<CR>')
map ('n', '[l', ':<C-u>lprev<CR>')
map ('n', ']L', ':<C-u>llast<CR>')
map ('n', '[L', ':<C-u>lfirst<CR>')
map ('n', '<leader>ol', ':<C-u>lopen<CR>')
map ('n', '<leader>cl', ':<C-u>lclose<CR>')

-- Write and quit
map ('n', '<leader>w' , ':<c-u>w<cr>')
map ('n', '<leader>sw', ':<c-u>w !pkexec tee % >/dev/null<CR>')
map ('n', '<leader>qq', ':<c-u>q<cr>')

-- Manage plugins
map ('n', '<leader>pu', ':lua require"pkg".update()<CR>')
map ('n', '<leader>pc', ':lua require"pkg".clean()<CR>')
map ('n', '<leader>pl', ':lua require"pkg".list()<CR>')

-- Navigate history containing substring
map ('c', '<M-p>', '<up>')
map ('c', '<M-n>', '<down>')

-- Capitalize word in front of cursor
map ('i', '<c-u>', '<esc>viwUea')

-- Resize windows
map ('n', '<M-,>', '<C-w>+')
map ('n', '<M-.>', '<C-w>-')
map ('n', '<M-<>', '<C-w><')
map ('n', '<M->>', '<C-w>>')

-- Naviate panes
add ('christoomey/vim-tmux-navigator', function()
    vim.g.tmux_navigator_no_mappings = 1
    map ('n', '<M-h>'    , ':<C-u>TmuxNavigateLeft<CR>')
    map ('n', '<M-j>'    , ':<C-u>TmuxNavigateDown<CR>')
    map ('n', '<M-k>'    , ':<C-u>TmuxNavigateUp<CR>')
    map ('n', '<M-l>'    , ':<C-u>TmuxNavigateRight<CR>')
    map ('n', '<M-left>' , ':<C-u>TmuxNavigateLeft<CR>')
    map ('n', '<M-down>' , ':<C-u>TmuxNavigateDown<CR>')
    map ('n', '<M-up>'   , ':<C-u>TmuxNavigateUp<CR>')
    map ('n', '<M-right>', ':<C-u>TmuxNavigateRight<CR>')
    map ('t', '<M-h>'    , '<C-\\><C-n>:TmuxNavigateLeft<CR>')
    map ('t', '<M-j>'    , '<C-\\><C-n>:TmuxNavigateDown<CR>')
    map ('t', '<M-k>'    , '<C-\\><C-n>:TmuxNavigateUp<CR>')
    map ('t', '<M-l>'    , '<C-\\><C-n>:TmuxNavigateRight<CR>')
    map ('t', '<M-left>' , '<C-\\><C-n>:TmuxNavigateLeft<CR>')
    map ('t', '<M-down>' , '<C-\\><C-n>:TmuxNavigateDown<CR>')
    map ('t', '<M-up>'   , '<C-\\><C-n>:TmuxNavigateUp<CR>')
    map ('t', '<M-right>', '<C-\\><C-n>:TmuxNavigateRight<CR>')
end)

-- }}}

-- Load and configure plugins {{{

add 'tpope/vim-repeat'
add 'tpope/vim-commentary'
-- add 'georgewitteman/vim-fish'

add ('tpope/vim-surround', function()
    vim.g.surround_66 = '{\r}\1\1'  -- 'B'
end)

add ('lervag/vimtex', function()
    vim.g.vimtex_format_enabled = 1
    vim.g.vimtex_quickfix_mode = 0
    vim.g.vimtex_view_method  = 'zathura'
    vim.g.tex_flavor = 'latex'
end)

add ('rust-lang/rust.vim', function()
    vim.g.rust_conceal = 1
    vim.g.rust_fold = 2
end)

add ('neomake/neomake', function()
    map ('n', '<leader>nm', ':<C-u>Neomake<CR>')
    map ('n', '<leader>nc', ':<C-u>NeomakeClean<CR>')
end)

add ('neovim/nvim-lspconfig', function()
    -- require 'lsp.ccls'
    require 'lsp.sumneko_lua'
    require 'lsp.rls'
    require 'lsp.bashls'
    require 'lsp.vimls'
    -- require 'lsp.texlab'
    -- require 'lsp.tsserver'
end)

add ('hrsh7th/nvim-cmp', function()
    add 'hrsh7th/cmp-nvim-lsp'
    add 'hrsh7th/cmp-nvim-lua'
    add 'hrsh7th/cmp-path'
    add 'hrsh7th/cmp-vsnip'
    add 'hrsh7th/cmp-calc'
    require'plugins.nvim-cmp'.setup {
        tab_role = '<Tab>',
        stab_role = '<S-Tab>',
        confirm = '<CR>',
        cancel = '<C-e>',
    }
end)

add ('hrsh7th/vim-vsnip', function()
    add 'rafamadriz/friendly-snippets'
    add 'hrsh7th/vim-vsnip-integ'
    vim.g.vsnip_snippet_dir = vim.fn.stdpath('config') .. '/vsnip'
end)

add ('nvim-treesitter/nvim-treesitter', function()
    add 'nvim-treesitter/nvim-treesitter-textobjects'
    require 'plugins.treesitter'
    vim.schedule(function()
        pcall(vim.api.nvim_exec, 'TSUpdate', true)
    end)
end)

-- }}}

-- Commands {{{

-- Creates current directory
command ('Mkdir', 'call mkdir(expand("%:h"), "p")')

-- Create tags asynchrounously
command ('MakeTags', function()
    require'utils.async'.spawn{'ctags', '-R', '.'}
end)

-- }}}

-- Autocmds {{{

augroup 'init.lua' [[
    " Autoload config files on save
    autocmd BufWritePost ~/.{dotfiles,config}/nvim/*.{vim,lua} lua dofile(vim.env.MYVIMRC)

    " Fold via marker in config files
    autocmd BufRead  ~/.{config,dotfiles}/* setlocal foldmethod=marker

    " Misc
	autocmd BufWritePre /tmp/* setlocal noundofile
    autocmd TextYankPost * silent! lua vim.highlight.on_yank {higroup='IncSearch', timeout=150}
]]

-- }}}

-- Appearence {{{

vim.opt.fillchars = { vert = '│' }
vim.cmd 'colorscheme base16'

-- }}}
