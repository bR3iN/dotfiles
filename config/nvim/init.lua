-- experimental
-- vim.loader.enable()

require('bootstrap.hotpot')
local utils = require('utils')

-- Resets internal state when reloading config
utils.init()

-- (Re)load config
utils.reload('main')
utils.reload('appearance')

-- (Re)load machine local config
pcall(utils.reload, 'local')

-- Sync declared plugins
utils.sync()

-- exrc with added fennel implementation
vim.o.exrc = true
utils.reload('utils/exrc')

-- Remove unused plugins from disk
utils.pack_sync()
