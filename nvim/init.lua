require('bootstrap.hotpot')
local utils = require('utils')

-- Resets internal state when reloading config
utils.init()

-- (Re)load main config
utils.reload('main')

-- Sync declared plugins
utils.sync()
