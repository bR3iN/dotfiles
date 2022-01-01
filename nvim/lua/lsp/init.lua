local lspconfig = require('lspconfig')

local M = {}

M.custom_on_attach = function(client, bufnr, tbl)
    local function buffer_local_map(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buffer_local_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    local default_options = {
        noremap=true,
        silent=true,
    }

    tbl = vim.tbl_deep_extend("error", tbl or {}, {
        key_maps = {
            n ={
                ['gD']         = '<Cmd>lua vim.lsp.buf.declaration()<CR>',
                ['gd']         = '<cmd>lua vim.lsp.buf.definition()<CR>',
                ['K']          = '<cmd>lua vim.lsp.buf.hover()<CR>',
                ['gi']         = '<cmd>lua vim.lsp.buf.implementation()<CR>',
                ['<C-k>']      = '<cmd>lua vim.lsp.buf.signature_help()<CR>',
                ['<leader>D']  = '<cmd>lua vim.lsp.buf.type_definition()<CR>',
                ['<leader>rn'] = '<cmd>lua vim.lsp.buf.rename()<CR>',
                ['<leader>ca'] = '<cmd>lua vim.lsp.buf.code_action()<CR>',
                ['<leader>od'] = '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>',
                ['<leader>oD'] = '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>',
                ['gr']         = '<cmd>lua vim.lsp.buf.references()<CR>',
                ['[d']         = '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>',
                [']d']         = '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>',
            }
        }
    })

    for mode, mappings in pairs(tbl.key_maps or {}) do
        for lhs, rhs in pairs(mappings) do
            buffer_local_map(mode, lhs, rhs, default_options)
        end
    end

    local capabilities = client.resolved_capabilities

    if capabilities.document_formatting then
        buffer_local_map('n', '<leader>F', '<cmd>lua vim.lsp.buf.formatting()<CR>', default_options)
    end
    if capabilities.document_range_formatting then
        buffer_local_map('v', '<leader>F', '<cmd>lua vim.lsp.buf.range_formatting()<CR>', default_options)
    end

end

M.setup_with_defaults = function(ls_name)
    lspconfig[ls_name].setup{
        on_attach = M.custom_on_attach,
    }
end

return M
