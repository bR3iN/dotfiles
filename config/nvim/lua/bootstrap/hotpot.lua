vim.pack.add({
    {
        src = "https://github.com/rktjmp/hotpot.nvim",
        version = vim.version.range('^2.0.0')
    }
})

-- Initializes hotpot
require('hotpot')

local api = require('hotpot.api')
local ctx = api.context(vim.fn.stdpath 'config')
_G._hotpot_ctx = ctx

-- Initial compilation of config
if not vim.uv.fs_stat(ctx.metadata().destination) then
    ctx.sync()
end

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
