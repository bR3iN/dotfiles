local augroup = require'utils'.augroup

vim.g.neomake_error_sign = {
    text = '➤',
    -- texthl = 'NeomakeErrorSign'
}
vim.g.neomake_warning_sign = {
    text = '➤',
    -- texthl = 'NeomakeWarningSign'
}
vim.g.neomake_message_sign = {
    text = '➤',
    -- texthl = 'NeomakemessageSign'
}
vim.g.neomake_info_sign = {
    text = '➤',
    -- texthl = 'NeomakeInfoSign'
}

augroup 'neomake_highlighting' [[
    autocmd ColorScheme *
    \ hi link NeomakeErrorSign   DiagnosticSignError |
    \ hi link NeomakeWarningSign DiagnosticSignWarn  |
    \ hi link NeomakeMessageSign DiagnosticSignHint  |
    \ hi link NeomakeInfoSign    DiagnosticSignInfo  |
    \ hi link NeomakeVirtualtextError   DiagnosticError |
    \ hi link NeomakeVirtualtextWarning DiagnosticWarn  |
    \ hi link NeomakeVirtualtextMessage DiagnosticHint  |
    \ hi link NeomakeVirtualtextInfo    DiagnosticInfo
]]
