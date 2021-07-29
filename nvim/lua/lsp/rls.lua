local lspconfig = require('lspconfig')
local on_attach = require('lsp').on_attach

lspconfig.rls.setup{
    on_attach = on_attach("rls"),
    settings = {
        rust = {
            all_features = true,
        }
    }
}
