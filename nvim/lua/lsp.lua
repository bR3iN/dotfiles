local lspconfig = require('lspconfig')
local util = lspconfig.util

local _custom_lsp_attach = function(lsp, client, bufnr)

	local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
	local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

	local opts = {
		noremap=true,
		--silent=true,
	}

	-- See `:help nvim_buf_set_keymap()` for more information
	buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
	buf_set_keymap('n', '<c-]>', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)

	buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
	buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
	buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
	--buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
	----buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
	----buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
	--buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
	--buf_set_keymap('n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workleader_folder()<CR>', opts)
	--buf_set_keymap('n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workleader_folder()<CR>', opts)
	--buf_set_keymap('n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workleader_folders()))<CR>', opts)
	--buf_set_keymap('n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
	buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
	--buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
	--buf_set_keymap('n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
	--buf_set_keymap('n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)


	-- Autocommands:

	vim.api.nvim_command("augroup lsp_autocmd_" .. lsp) 
	vim.api.nvim_command [[autocmd!]]

	if client.resolved_capabilities.document_formatting then
		vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync(nil, 1000)]]
	elseif client.resolved_capabilities.document_range_formatting then
		vim.api.nvim_command [[autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync(nil, 1000)]]
	end


	if client.resolved_capabilities.document_highlight then
		vim.api.nvim_command [[autocmd CursorHold  <buffer> lua vim.lsp.buf.document_highlight()]]
		vim.api.nvim_command [[autocmd CursorHoldI <buffer> lua vim.lsp.buf.document_highlight()]]
		vim.api.nvim_command [[autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()]]
		--util.nvim_multiline_command [[
		--":hi LspReferenceRead cterm=bold ctermbg=red guibg=LightYellow
		--":hi LspReferenceText cterm=bold ctermbg=red guibg=LightYellow
		--":hi LspReferenceWrite cterm=bold ctermbg=red guibg=LightYellow
		--]]
	end

	vim.api.nvim_command [[augroup END]]

	-- Use LSP as the handler for omnifunc.
	--    See `:help omnifunc` and `:help ins-completion` for more information.
	buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

	-- For plugins with an `on_attach` callback, call them here. For example:
	--require('completion').on_attach(client)

end

local function custom_lsp_attach(lsp)
	return function(...) _custom_lsp_attach(lsp, ...) end
end



lspconfig.ccls.setup{

	on_attach = custom_lsp_attach("ccls"),

	root_dir = util.root_pattern("compile_commands.json", "compile_flags.txt", ".git", ".ccls"),

	init_options = {

		--highlight = { -- needed for vim-lsp-cxx-highlight
			--lsRanges = true;
		--}

	}
}


local servers = { 
	--"pyls", 
	"bashls", 
	"texlab",
	"tsserver",
}

for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup{ 
		on_attach = custom_lsp_attach(lsp)
	}
end
