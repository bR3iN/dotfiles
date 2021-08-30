local lspconfig = require('lspconfig')
local util = lspconfig.util
local cmd = vim.cmd

local M = {}

local function custom_on_attach(lsp, client, bufnr)

	local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
	local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

	local opts = {
		noremap=true,
        silent=true,
	}

    -- See `:help lspconfig-keybindings`
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)


    local cap = client.resolved_capabilities

    if cap.document_formatting then
        buf_set_keymap('n', '<leader>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)
    end
    if cap.document_range_formatting then
        buf_set_keymap('v', '<leader>f', '<cmd>lua vim.lsp.buf.range_formatting()<CR>', opts)
	end

	-- Autocommands: {{{
	--cmd('augroup lsp_autocmd_' .. lsp)
	--cmd 'autocmd!'
    --if cap.document_formatting then
        --cmd 'autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync(nil, 1000)'
    --end
	--if cap.document_highlight then
		--cmd 'autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()'
		--cmd 'autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()'
		--cmd 'autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()'
	--end
	--cmd 'augroup END'
    -- }}}
end

M.on_attach = function(lsp)
    return function(...)
        custom_on_attach(lsp, ...)
    end
end

M.with_defaults = (function()
    return function(lsp)
        lspconfig[lsp].setup{
            on_attach = M.on_attach(lsp),
        }
    end
end)()

return M
