local lspconfig = require('lspconfig')

lspconfig.rls.setup{
    on_attach = require'lsp'.custom_on_attach,
    settings = {
        rust = {
            all_features = true,
        }
    }
}
