local map    = require'utils'.map
local pprint = require'utils'.pprint
local async  = require'utils.async'

_custom_operators = {}

local function create_bindings(func_name, bindings)
    if bindings['n'] then
        map ('n', bindings['n'],
            ':set operatorfunc=v:lua._custom_operators.'..func_name..'<CR>g@')
    end
    if bindings['v'] then
        map ('v', bindings['v'],
            ':<c-u>call v:lua._custom_operators.'..func_name..'(visualmode())<CR>')
    end
    if bindings['line'] then
        map ('v', bindings['line'],
            'V:<c-u>call v:lua._custom_operators.'..func_name..'(visualmode())<CR>')
    end
end

local function get_text(mode, bufnr)
    local bufnr = bufnr or 0

    local marks = (mode == 'char' or mode == 'line') and {'[', ']'} or {'<', '>'}

    local start = vim.api.nvim_buf_get_mark(bufnr, marks[1])
    local stop  = vim.api.nvim_buf_get_mark(bufnr, marks[2])
    local lines = vim.api.nvim_buf_get_lines(bufnr, start[1] - 1, stop[1], true)

    if mode == 'v' or mode == 'char' then
        lines[#lines] = lines[#lines]:sub(0, stop[2] + 1)
        lines[     1] = lines[     1]:sub(start[2] + 1)
    elseif mode == 'V' or mode == 'line' then
        -- do nothing
    else
        error('mode '..mode..' is not supported')
    end

    return lines
end

local function setup(tbl)
    for func_name, bindings in pairs(tbl) do
        create_bindings(func_name, bindings)
    end
end

function _custom_operators.OpenInBrowser(mode)
    local lines = get_text(mode)
    vim.tbl_map(function(line)
        async.spawn({'qutebrowser', line})
    end, lines)
end

return {setup = setup}
