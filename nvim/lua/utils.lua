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

return M
