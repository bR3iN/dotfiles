local cmp = require'cmp'

local function feed(str)
    return vim.fn.feedkeys(vim.api.nvim_replace_termcodes(str, true, true, true))
end

local role_to_func = {
    tab_role = function(fallback)
        if vim.fn.pumvisible() == 1 then
            feed '<C-n>'
        elseif vim.fn['vsnip#jumpable'](1) == 1 then
            feed '<Plug>(vsnip-jump-next)'
        else
            return fallback()
        end
    end,

    stab_role = function(fallback)
        if vim.fn.pumvisible() == 1 then
            feed '<C-p>'
        elseif vim.fn['vsnip#jumpable'](-1) == 1 then
            feed '<Plug>(vsnip-jump-prev)'
        else
            cmp.complete()
        end
    end,

    confirm = cmp.mapping.confirm({ select = true }),

    cancel = cmp.mapping.close(),
}

local function setup(tbl)
    local mapping = {}
    for role, key in pairs(tbl) do
        mapping[key] = role_to_func[role]
    end

    require'cmp'.setup {
        snippet = {
            expand = function(args)
                vim.fn['vsnip#anonymous'](args.body)
            end,
        },

        sources = {
            { name = 'nvim_lsp' },
            { name = 'nvim_lua' },
            { name = 'vsnip'    },
            { name = 'path'     },
            { name = 'calc'     },
            { name = 'omni'     },
            { name = 'buffer'   },
        },

        mapping = mapping,
        formatting = {
            format = function(entry, vim_item)
                vim_item.menu = ({
                    nvim_lsp = "[LSP]",
                    nvim_lua = "[Lua]",
                    vsnip    = "[Vsp]",
                    path     = "[Pth]",
                    calc     = "[Clc]",
                })[entry.source.name]
                return vim_item
            end,
        },
    }
end

return { setup = setup }
