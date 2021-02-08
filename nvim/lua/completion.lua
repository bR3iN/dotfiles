require'compe'.setup {
	enabled = true;
	autocomplete = true;
	debug = false;
	min_length = 1;
	preselect = 'enable';
	throttle_time = 80;
	source_timeout = 200;
	incomplete_delay = 400;
	allow_prefix_unmatch = false;

	source = {
		path = true;
		buffer =true;
		vsnip = true;
		nvim_lsp = true;
		nvim_lua = true;
		your_awesome_source = {};
	};
}
