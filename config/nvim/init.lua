-- experimental
-- vim.loader.enable()

require('bootstrap.hotpot')
local utils = require('utils')

-- Resets internal state when reloading config
utils.init()

-- (Re)load config
utils.reload('main')
utils.reload('appearance')

-- Sync declared plugins
utils.sync()
