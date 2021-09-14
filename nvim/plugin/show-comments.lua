local enabled = false

require'utils'.command('ToggleComments', function()
    if enabled then
        vim.cmd 'hi Comment ctermfg=8'
    else
        vim.cmd 'hi Comment ctermfg=16'
    end
    enabled = not enabled
end)
