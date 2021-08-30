-- Colors otherwise set in /usr/share/nvim/runtime/syntax/syncolor.vim
vim.cmd [[
hi Comment ctermfg=8
hi Constant ctermfg=5
hi Special ctermfg=17
hi Identifier ctermfg=6
hi Statement ctermfg=3
hi PreProc ctermfg=12
hi Type ctermfg=10
hi Underlined ctermfg=12
hi Ignore ctermfg=17

hi Conceal ctermbg=18
hi CursorColumn ctermbg=18
hi CursorLineNr ctermfg=7
hi Directory ctermfg=14
hi FoldColumn ctermbg=18
hi Folded ctermbg=18
hi LineNr ctermfg=8
"hi MoreMsg ctermfg=10
hi MoreMsg ctermfg=16
hi Pmenu ctermbg=18 ctermfg=11
hi PmenuSbar ctermbg=19
hi PmenuSel ctermbg=8 ctermfg=11 cterm=bold
hi PmenuThump ctermbg=7
"hi Question ctermfg=10
hi Question ctermfg=10
hi SignColumn ctermbg=18
hi SpecialKey ctermfg=6
hi StatusLine ctermbg=8 ctermfg=2 cterm=bold
hi StatusLineNC ctermbg=19 ctermfg=2 cterm=NONE
hi TabLine ctermbg=18
hi Title ctermfg=13
hi VertSplit cterm=None ctermfg=8 ctermbg=NONE
hi Visual ctermbg=19
hi WarningMsg ctermfg=9

hi LspDiagnosticsDefaultError ctermfg=9
hi LspDiagnosticsDefaultHint ctermfg=13
hi LspDiagnosticsDefaultInformation ctermfg=10
hi LspDiagnosticsDefaultWarning ctermfg=11
]]
