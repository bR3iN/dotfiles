local lspconfig = require('lspconfig')
local on_attach = require('lsp').on_attach

lspconfig.texlab.setup{
    on_attach = on_attach("texlab"),
    settings = {
        latex = {
            build = {
                onSave = true
            },
            forwardSearch = {
                executable = "zathura",
                args = {"--synctex-forward", "%l:1:%f", "%p"},
            },
        }
    }
}
