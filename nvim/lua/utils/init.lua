local cmd = vim.cmd

local M = {}

function M.prequire(name)
    return pcall(require, name)
end

function M.map(mode, lhs, rhs, opts)
    local default = {noremap = true, silent = true}
    --local default = {noremap = true}
    local options = vim.tbl_extend('keep', opts or {}, default)
    vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

function M.augroup(name)
    return function(cmds)
        cmd('augroup ' .. name)
        cmd 'autocmd!'
        vim.api.nvim_exec(cmds, false)
        cmd 'augroup END'
    end
end

function M.unrequire(name)
    package.loaded[name] = nil
end

function M.pprint(obj) -- pretty-print
    print(vim.inspect(obj))
end

function M.iter_list(list)
    local count = 0
    return function()
        count = count + 1
        return list[count]
    end
end

function M.iter_values(tbl)
    local count = 0
    local iter, s, k = pairs(tbl)
    return function()
        k, v = iter(s, k)
        return v
    end
end

function M.iter_keys(tbl)
    local count = 0
    local iter, s, k = pairs(tbl)
    return function()
        k, v = iter(s, k)
        return k
    end
end

return M
