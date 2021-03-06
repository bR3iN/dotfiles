require'nvim-treesitter.configs'.setup {

    --ensure_installed = "maintained", -- one of "all", "maintained" (parsers with maintainers), or a list of languages

	highlight = {
		enable = true,              -- false will disable the whole extension
		-- disable = { "c", "rust" },  -- list of language that will be disabled
	},

    indent = {
        enable = true
    },
    textobjects = {
        select = {
            enable = true,
            keymaps = {
                ["if"] = "@function.inner",
                ["af"] = "@function.outer",
                ["ic"] = "@call.inner",
                ["ac"] = "@call.outer",
                ["il"] = "@loop.inner",
                ["al"] = "@loop.outer",
                ["ik"] = "@conditional.inner",
                ["ak"] = "@conditional.outer",
            },
        },
    },

}
