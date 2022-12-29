-- Bootstrap fennel support
require('setup_hotpot')

-- Initialize plugin manager; resets internal plugin list
require('pkg').init()

-- (Re)Load main config
package.loaded['main'] = nil
require('main')

-- Autoremove plugins
require('pkg').clean()
