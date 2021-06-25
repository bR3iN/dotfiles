local lspconfig = require('lspconfig')
local on_attach = require('lsp').on_attach

lspconfig.ccls.setup{
    on_attach = on_attach("ccls"),
    root_dir  = lspconfig.util.root_pattern("compile_commands.json", "compile_flags.txt", ".git", ".ccls"),
}
