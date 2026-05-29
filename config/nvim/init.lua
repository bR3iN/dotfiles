-- experimental
-- vim.loader.enable()

require('bootstrap.hotpot')
local utils = require('utils')

-- Resets internal state when reloading config
utils.init()

-- (Re)load config
utils.reload('main')

-- (Re)load machine local config (fnl/local.fnl)
pcall(utils.reload, 'local')

-- Sync declared plugins
utils.sync()

-- Load trusted `.nvim.lua` (vim.o.exrc) and `.nvim.fnl` (utils/exrc) files.
vim.o.exrc = true
utils.reload('utils/exrc')

-- Remove unused plugins from disk
utils.pack_sync()
