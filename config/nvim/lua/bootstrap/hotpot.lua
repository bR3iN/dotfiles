vim.pack.add({"https://github.com/rktjmp/hotpot.nvim"})

-- Setup hotpot
require('hotpot').setup{
    enable_hotpot_diagnostics = false,
    compiler = {
        modules = {
            correlate = true,
        }
    },
    macros = {
        env = "_COMPILER" -- MUST be set along with any other options
    },
}
