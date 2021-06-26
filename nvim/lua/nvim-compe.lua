local utils = require'utils'
local map   = utils.map

local M = {}

local function expand(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

_compe_tab = function(default)
    if vim.fn.pumvisible() == 1 then
        return expand '<C-n>'
    elseif vim.fn['vsnip#jumpable'](1) == 1 then
        return expand '<Plug>(vsnip-jump-next)'
    else
        return expand(default)
    end
end

_compe_stab = function(default)
    if vim.fn.pumvisible() == 1 then
        return expand '<C-p>'
    elseif vim.fn['vsnip#jumpable'](-1) == 1 then
        return expand '<Plug>(vsnip-jump-prev)'
    else
        return vim.fn['compe#complete']()
    end
end

M.tab_role  = function(str)
    local opt = { expr = true, noremap = false }
    map('i', str, string.format('v:lua._compe_tab("%s")', str), opt)
end

M.stab_role = function(str)
    local opt = { expr = true, noremap = false }
    map('i', str, string.format('v:lua._compe_stab("%s")', str), opt)
end

M.cancel = function(str)
    map('i', str, 'compe#close(\'<Esc>\')' , {expr = true})
end

M.confirm = function(str)
    map('i', str, 'compe#confirm(\'<CR>\')', {expr = true})
end

local function setup()
    --vim.o.completeopt = 'menu,menuone,noselect,noinsert,preview'
    vim.o.completeopt = 'menuone,noselect'

    require'compe'.setup {
        enabled              = true;
        autocomplete         = true;
        debug                = false;
        min_length           = 1;
        preselect            = 'enable';
        throttle_time        = 80;
        source_timeout       = 200;
        incomplete_delay     = 400;
        --allow_prefix_unmatch = false;
        max_abbr_width       = 100;
        max_kind_width       = 100;
        max_menu_width       = 100;
        documentation        = true;

        source = {
            buffer          = true;
            calc            = true;
            nvim_lsp        = true;
            nvim_lua        = true;
            nvim_treesitter = true;
            path            = true;
            tags            = true;
            --omni            = true
            vsnip           = true;
        };
    }
end

setup()
return M
