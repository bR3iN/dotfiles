-- Bootstrap hotpot and pkg.nvim
require('bootstrap')

-- Setup hotpot
require('hotpot')

-- Initialize plugin manager; resets internal plugin list
require('pkg').init()

-- (Re)load main config
package.loaded['main'] = nil
require('main')

-- Autoremove plugins
require('pkg').clean()
