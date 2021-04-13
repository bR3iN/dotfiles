local augroup = require'utils'.augroup
local map     = require'utils'.map

local function newTerminal()
    vim.cmd 'below split term://bash'
    vim.cmd 'resize 10'
end

map('n', '<leader>ot', ':lua require\'terminal\'.new()<cr>')

augroup 'terminal' [[
    autocmd TermOpen * setlocal nonumber norelativenumber | startinsert
    autocmd BufEnter * if &buftype == 'terminal' | :startinsert | endif
]]

return {new = newTerminal}
