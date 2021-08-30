if not vim.env['KITTY_LISTEN_ON'] then
    return
end

local augroup = require'utils'.augroup
local spawn   = require'utils.async'.spawn

_kitty_set_opacity = function(opacity)
    spawn(
    {'kitty', '@', 'set-background-opacity', tostring(opacity)},
    {detached = true}
    )
end

augroup 'kitty' [[
    autocmd VimEnter   * silent! call v:lua._kitty_set_opacity(1)
    autocmd VimLeave   * silent! call v:lua._kitty_set_opacity($BACKGROUND_OPACITY)
    autocmd VimResume  * silent! call v:lua._kitty_set_opacity(1)
    autocmd VimSuspend * silent! call v:lua._kitty_set_opacity($BACKGROUND_OPACITY)
]]
