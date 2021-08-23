local utils = require'utils'
local map   = utils.map

local M = {}

_compe = {}

local function expand(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

_compe.tab = function(default)
    if vim.fn.pumvisible() == 1 then
        return expand '<C-n>'
    elseif vim.fn['vsnip#jumpable'](1) == 1 then
        return expand '<Plug>(vsnip-jump-next)'
    else
        return expand(default)
    end
end

_compe.stab = function(default)
    if vim.fn.pumvisible() == 1 then
        return expand '<C-p>'
    elseif vim.fn['vsnip#jumpable'](-1) == 1 then
        return expand '<Plug>(vsnip-jump-prev)'
    else
        return vim.fn['compe#complete']()
    end
end

M.tab_role  = function(str)
    local opt = { expr = true, noremap = false }
    map('i', str, 'v:lua._compe.tab("'..str..'")', opt)
end

M.stab_role = function(str)
    local opt = { expr = true, noremap = false }
    map('i', str, 'v:lua._compe.stab("'..str..'")', opt)
end

M.cancel = function(str)
    map('i', str, 'compe#close("'..str..'")' , {expr = true})
end

M.confirm = function(str)
    map('i', str, 'compe#confirm("'..str..'")', {expr = true})
end


return M
