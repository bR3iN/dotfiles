-- TODO: Fork with fix; find out why it is not a comon problem and make issue upstream
vim.pack.add({
    {
        src = "https://github.com/rktjmp/hotpot.nvim",
        version = vim.version.range('^2.0.0')
    }
})
require('hotpot')

-- Setup hotpot
-- require('hotpot').setup{
--     enable_hotpot_diagnostics = false,
--     compiler = {
--         modules = {
--             correlate = true,
--         }
--     },
--     macros = {
--         env = "_COMPILER" -- MUST be set along with any other options
--     },
-- }
