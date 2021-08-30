local map    = require'utils'.map

_custom_operators = {}

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

local function new(name, func)
    _G._custom_operators[name] = function(mode)
        func(get_text(mode))
    end

    map ('n', '<Plug>'..name,
    ':set operatorfunc=v:lua._custom_operators.'..name..'<CR>g@')
    map ('v', '<Plug>'..name,
    ':<c-u>call v:lua._custom_operators.'..name..'(visualmode())<CR>')
end

return {
    new = new,
    get_text = get_text,
}
