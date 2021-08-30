local lspconfig = require('lspconfig')
local on_attach = require('lsp').on_attach

local sumneko_root = os.getenv('HOME')..'/Github/sumneko-lua'
local sumneko_bin  = sumneko_root..'/bin/Linux/lua-language-server'

lspconfig.sumneko_lua.setup{
    cmd = {sumneko_bin, "-E", sumneko_root .. '/main.lua'},
    settings = {
        Lua = {
            runtime = 'LuaJIT',
            path = vim.split(package.path, ';'),
            diagnostics = {
                globals = {
                    'vim',
                }
            },
            workspace = {
                library = {
                    [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                    [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
                },
            },
            telemetry = { enable = false },
        },
    },
    on_attach = on_attach('sumneko_lua'),
}
