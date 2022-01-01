require'nvim-treesitter.configs'.setup {

	highlight = {
		enable = true,
	},
    indent = {
        enable = false,
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
