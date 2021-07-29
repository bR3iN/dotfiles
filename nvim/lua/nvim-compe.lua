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
        -- nvim_treesitter = true;
        path            = true;
        tags            = true;
        omni            = true;
        vsnip           = true;
    };
}

local keybindings = require'nvim-compe.keybindings'
keybindings.tab_role('<Tab>')
keybindings.stab_role('<S-Tab>')
keybindings.confirm('<CR>')
keybindings.cancel('<C-e>')
