local map       = require'utils'.map

--vim.g.completeopt = 'menu,menuone,noselect,noinsert,preview'
vim.g.completeopt = 'menuone,noselect'

map('i', '<C-Space>', 'compe#complete()'       , {expr = true})
map('i', '<CR>',      'compe#confirm(\'<CR>\')', {expr = true})
map('i', '<C-e>',     'compe#close(\'<C-e>\')' , {expr = true})

require'compe'.setup {
	enabled              = true;
	autocomplete         = true;
	debug                = false;
	min_length           = 1;
	preselect            = 'enable';
	throttle_time        = 80;
	source_timeout       = 200;
	incomplete_delay     = 400;
	allow_prefix_unmatch = false;
	max_abbr_width       = 100;
	max_kind_width       = 100;
	max_menu_width       = 100;
	documentation        = true;

	source = {
		path            = true;
		buffer          = true;
		calc            = true;
		nvim_lsp        = true;
		nvim_lua        = true;
		spell           = true;
		tags            = true;
		nvim_treesitter = true;
        omni            = true;   
	};
}
