hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "base16"

hi Conceal ctermbg=18
hi CursorColumn ctermbg=18
hi CursorLineNr ctermfg=7
hi Directory ctermfg=14
hi FoldColumn ctermbg=18
hi Folded ctermbg=18
hi LineNr ctermfg=8
hi MoreMsg ctermfg=16
hi Pmenu ctermbg=18 ctermfg=11
hi PmenuSbar ctermbg=19
hi PmenuSel ctermbg=8 ctermfg=11 cterm=bold
hi PmenuThumb ctermbg=8
hi Question ctermfg=10
hi SignColumn ctermbg=18
hi SpecialKey ctermfg=6
hi StatusLine ctermbg=8 ctermfg=2 cterm=bold
hi StatusLineNC ctermbg=19 ctermfg=2 cterm=NONE
hi TabLine ctermbg=18
hi Title ctermfg=13
hi VertSplit cterm=None ctermfg=8 ctermbg=NONE "Linked to by FloatBorder
hi Visual ctermbg=19
hi WarningMsg ctermfg=9

" Colors otherwise set in /usr/share/nvim/runtime/syntax/syncolor.vim
hi Comment ctermfg=8
hi Constant ctermfg=5
hi Special ctermfg=17
hi Identifier ctermfg=6
hi Statement ctermfg=3
hi PreProc ctermfg=12
hi Type ctermfg=10
hi Underlined ctermfg=12
hi Ignore ctermfg=17

" Used by Neomake
hi SpellCap ctermbg=19

hi LspDiagnosticsDefaultError ctermfg=9 ctermbg=19
hi LspDiagnosticsDefaultHint ctermfg=13 ctermbg=19
hi LspDiagnosticsDefaultInformation ctermfg=10 ctermbg=19
hi LspDiagnosticsDefaultWarning ctermfg=11 ctermbg=19
hi LspDiagnosticsFloatingError       ctermbg=18
hi LspDiagnosticsFloatingHint        ctermbg=18
hi LspDiagnosticsFloatingInformation ctermbg=18
hi LspDiagnosticsFloatingWarning     ctermbg=18
