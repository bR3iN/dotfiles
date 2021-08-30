local cmd = vim.cmd

local M = {}

function M.let(tbl)
    for option, value in pairs(tbl) do
        vim.g[option] = value
    end
end

function M.set_options(t)
    for key, value in pairs(t) do
        vim.opt[key] = value
    end
end

function M.map(mode, lhs, rhs, opts)
    local default_options = {noremap = true, silent = true}
    local options = vim.tbl_extend('keep', opts or {}, default_options)
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

function M.tbl_map(func, tbl)
    for key, value in pairs(tbl) do
        func(key, value)
    end
end

function M.lst_map(func, lst)
    for _, value in ipairs(lst) do
        func(value)
    end
end

-- function M.iter_list(list)
--     local count = 0
--     return function()
--         count = count + 1
--         return list[count]
--     end
-- end

-- function M.iter_values(tbl)
--     local count = 0
--     local iter, s, k = pairs(tbl)
--     return function()
--         k, v = iter(s, k)
--         return v
--     end
-- end

-- function M.iter_keys(tbl)
--     local count = 0
--     local iter, s, k = pairs(tbl)
--     return function()
--         k, v = iter(s, k)
--         return k
--     end
-- end

_G._user_functions = {}

function add_user_function(func)
    table.insert(_G._user_functions, func)
    return #_G._user_functions
end

function call_user_function(id, ...)
    return _G._user_functions[id](...)
end

function M.command(name, arg)
    local command
    if type(arg) == 'function' then
        local id = add_user_function(arg)
        command = 'call v:lua.call_user_function('..id..')'
    else
        command = arg
    end
    vim.cmd('command! '..name..' '..command)
end


return M
