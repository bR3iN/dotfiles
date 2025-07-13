-- TODO: For with fix; find out why it is not a comon problem and make issue upstream
vim.pack.add({"https://github.com/bR3iN/hotpot.nvim"})

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
