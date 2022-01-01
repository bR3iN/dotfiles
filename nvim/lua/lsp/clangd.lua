require('lspconfig').clangd.setup {
    on_attach = function(client, bufnr)
        require('lsp').custom_on_attach(client, bufnr, {
            key_maps = {
                n = {
                    ["<C-c>"] = ":ClangdSwitchSourceHeader<CR>"
                }
            }
        })
    end
}
