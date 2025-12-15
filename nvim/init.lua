require('bootstrap.hotpot')
local utils = require('utils')

-- Resets internal state when reloading config
utils.init()

-- (Re)load main config
utils.reload('core')
utils.reload('plugins')

-- Sync declared plugins
utils.sync()
