local new_operator = require'utils.operators'.new
local async        = require'utils.async'

new_operator('OpenInBrowser', function(lines)
    vim.tbl_map(function(line)
        async.spawn({vim.g.browser, line})
    end, lines)
end)

