(local {: lighten : darken : mix : get-named} (require :utils.colors))
(local {: hl!} (require :utils))

(local named (get-named))

(vim.cmd "syntax reset")
(vim.cmd "highlight clear")
(set vim.g.colors_name :base16)

; c.f. https://github.com/shaunsingh/nord.nvim/blob/master/lua/nord/theme.lua
; for highlight groups used usually in colorschemes.

;; Builtin highlight groups , c.f. `:h highlight-groups`, with unset groups
;; as comments.
(hl! :ColorColumn {:bg named.base02})
(hl! :Conceal {:bg named.base01})
; CurSearch
(hl! :Cursor {:fg named.base00 :bg named.base05})
; lCursor
; CursorIM
(hl! :CursorColumn {:bg named.base01})
(hl! :CursorLine {:bg (mix named.dark_green named.base00 0.1)})
(hl! :Directory {:fg named.base0C})
(hl! :DiffAdd {:bg (darken named.base0D 0.5)})
(hl! :DiffChange {:bg (darken named.base0E 0.5)})
(hl! :DiffDelete {:bg (darken named.base0C 0.5)})
(hl! :DiffText {:bg (darken named.base08 0.5)})
; EndOfBuffer
; TermCursor
; TermCursorNCEG links
(hl! :ErrorMsg {:fg named.base05 :bg named.base08})
(hl! :WinSeparator {:fg named.base02 :cterm nil})
(hl! :Folded {:bg named.base01})
(hl! :FoldColumn {:bg named.base01})
(hl! :SignColumn {:bg nil})
; IncSearch
; Substitute
(hl! :LineNr {:fg named.base03})
; LineNrAbove
; LineNrBefore
(hl! :CursorLineNr {:fg named.base0F :cterm nil})
; CursorLineFold
; CursorLineSign
(hl! :MatchParen {:fg named.base00 :bg named.base0C})
(hl! :ModeMsg {:bold true})
; MsgArea
; MsgSeparator
(hl! :MoreMsg {:fg named.base09})
(hl! :NonText {:fg (mix named.brown named.base03 0.3)})
(hl! :Normal {:fg named.base05})

(let [bg named.base03]
  ;; (hl! :FloatBorder {:fg (lighten named.base03 0.2) : bg})
  (hl! :FloatBorder {:fg bg : bg})
  (hl! :FloatTitle {:fg named.magenta :bold true : bg})
  (hl! :NormalFloat {:fg named.base06 : bg}))

; NormalNC
(hl! :Pmenu {:fg (darken named.yellow 0.15) :bg named.base02})
(hl! :PmenuSel {:fg named.base00 :bg (darken named.yellow 0.15)})
; PmenuKind
; PmenuKindSel
; PmenuExtra
; PmenuExtraSel
(hl! :PmenuSbar {:bg named.base02})
(hl! :PmenuThumb {:bg (lighten named.base03 0.5)})
(hl! :Question {:fg named.base0B})
; QuickFixLine
(hl! :Search {:fg named.base00 :bg named.base0A})
(hl! :SpecialKey {:fg named.base0C})
; SpellBad
(hl! :SpellCap {:bg named.base03})

; " Used by Neomak
; SpellLocal
; SpellRare

(hl! :StatusLine {:bg named.base02 :fg named.base03 :bold true})
(hl! :StatusLineNC {:bg named.base02 :fg named.base03 :bold true})

(hl! :WinBar {:bg named.base02 :fg named.fg1 :bold true})
(hl! :WinBarNC {:bg named.base01 :fg named.fg0 :bold true})

(hl! :TabLine {:bg named.base01})
; TabLineFill
; TabLineSel
(hl! :Title {:fg named.base0E})
(hl! :Visual {:bg (mix named.dark_cyan named.base02 0.1)})
(hl! :VisualNOS {:bg (mix named.dark_yellow named.base02 0.1)})
(hl! :WarningMsg {:fg named.base08})
(hl! :WildMenu {:fg named.base00 :bg named.base0A})
; WinBar
; WinBarNC
; User<n>
; Menu
; Scrollbar
; Tooltip

;; Syntax groups, c.f. `group-name`. The links should been default but don't
;; seem to be.
(hl! :Comment {:fg (lighten named.base03 0.2)})

(hl! :Constant {:fg named.base0E})
(hl! :String {:link :Constant})
(hl! :Character {:link :Constant})
(hl! :Number {:link :Constant})
(hl! :Boolean {:link :Constant})
(hl! :Float {:link :Constant})

(hl! :Identifier {:fg named.base0C :bold true})
(hl! :Function {:link :Identifier})

(hl! :Statement {:fg named.base0A})
(hl! :Conditional {:link :Statement})
(hl! :Repeat {:link :Statement})
(hl! :Label {:link :Statement})
(hl! :Operator {:link :Statement})
(hl! :Keyword {:link :Statement})
(hl! :Exception {:link :Statement})

(hl! :PreProc {:fg named.base0D})
(hl! :Include {:link :PreProc})
(hl! :Define {:link :PreProc})
(hl! :Macro {:link :PreProc})
(hl! :PreCondit {:link :PreProc})

(hl! :Type {:fg named.base0B})
(hl! :StorageClass {:link :Type})
(hl! :Structure {:link :Type})
(hl! :Typedev {:link :Type})

(hl! :Special {:fg named.base0F})
(hl! :SpecialChar {:link :Special})
(hl! :Tag {:link :Special})
(hl! :Delimiter {:link :Special})
(hl! :SpecialComment {:link :Special})
(hl! :Debug {:link :Special})

(hl! :Underlined {:fg named.base0D})

(hl! :Ignore {:fg named.base0F})

(hl! :Todo {:fg named.base00 :bg named.base0A})

(hl! :Error {:fg named.base08 :bg named.base00})

;; Diagnostic highlights, named.f. `:h diagnostic-highlights`
(let [bg nil]
  (hl! :DiagnosticError {:fg named.base08 : bg})
  (hl! :DiagnosticHint {:fg named.base0D : bg})
  (hl! :DiagnosticInfo {:fg named.base0B : bg})
  (hl! :DiagnosticWarn {:fg named.base09 : bg})
  (hl! :DiagnosticSignError {:fg named.base08 : bg})
  (hl! :DiagnosticSignHint {:fg named.base0D : bg})
  (hl! :DiagnosticSignInfo {:fg named.base0B : bg})
  (hl! :DiagnosticSignWarn {:fg named.base09 : bg}))

;; Treesitter highlight groups
; c.f. with list in https://github.com/folke/tokyonight.nvim/blob/main/lua/tokyonight/theme.lua
(hl! "@annotation" {:link "PreProc"})
(hl! "@attribute" {:link "PreProc"})
(hl! "@boolean" {:link "Boolean"})
(hl! "@character" {:link "Character"})
(hl! "@character.special" {:link "SpecialChar"})
(hl! "@comment" {:link "Comment"})
(hl! "@keyword.conditional" {:link "Conditional"})
(hl! "@constant" {:link "Constant"})
(hl! "@constant.builtin" {:link "Special"})
(hl! "@constant.macro" {:link "Define"})
(hl! "@keyword.debug" {:link "Debug"})
(hl! "@keyword.directive.define" {:link "Define"})
(hl! "@keyword.exception" {:link "Exception"})
(hl! "@number.float" {:link "Float"})
(hl! "@function" {:link "Function"})
(hl! "@function.builtin" {:link "Special"})
(hl! "@function.call" {:link "@function"})
(hl! "@function.macro" {:link "Macro"})
(hl! "@keyword.import" {:link "Include"})
(hl! "@keyword.coroutine" {:link "@keyword"})
(hl! "@keyword.operator" {:link "@operator"})
(hl! "@keyword.return" {:link "@keyword"})
(hl! "@function.method" {:link "Function"})
(hl! "@function.method.call" {:link "@function.method"})
(hl! "@namespace.builtin" {:link "@variable.builtin"})
(hl! "@none" {})
(hl! "@number" {:link "Number"})
(hl! "@keyword.directive" {:link "PreProc"})
(hl! "@keyword.repeat" {:link "Repeat"})
(hl! "@keyword.storage" {:link "StorageClass"})
(hl! "@string" {:link "String"})
(hl! "@markup.link.label" {:link "SpecialChar"})
(hl! "@markup.link.label.symbol" {:link "Identifier"})
(hl! "@tag" {:link "Label"})
(hl! "@tag.attribute" {:link "@property"})
(hl! "@tag.delimiter" {:link "Delimiter"})
(hl! "@markup" {:link "@none"})
(hl! "@markup.environment" {:link "Macro"})
(hl! "@markup.environment.name" {:link "Type"})
(hl! "@markup.raw" {:link "String"})
(hl! "@markup.math" {:link "Special"})
(hl! "@markup.strong" {:bold true})
(hl! "@markup.emphasis" {:italic true})
(hl! "@markup.strikethrough" {:strikethrough true})
(hl! "@markup.underline" {:underline true})
(hl! "@markup.heading" {:link "Title"})
(hl! "@comment.note" {:fg named.hint})
(hl! "@comment.error" {:fg named.error})
(hl! "@comment.hint" {:fg named.hint})
(hl! "@comment.info" {:fg named.info})
(hl! "@comment.warning" {:fg named.warning})
(hl! "@comment.todo" {:fg named.todo})
(hl! "@markup.link.url" {:link "Underlined"})
(hl! "@type" {:link "Type"})
(hl! "@type.definition" {:link "Typedef"})
(hl! "@type.qualifier" {:link "@keyword"})

;; Highlight groups used in Plugins
; leap.nvim
(hl! :LeapLabelPrimary {:fg named.base00 :bg named.base08 :nocombine true})
(hl! :LeapLabelSecondary {:fg named.base00
                          :bg (lighten named.base0C 0.1)
                          :nocombine true})

(hl! :LeapMatch {:fg (lighten named.base0B 0.1)
                 :bg named.base00
                 :underline true
                 :nocombine true})

;; (hl! :CmpAbbr {:fg named.base0A})

;; Misc FIXME: see if we want to keep it here or overwrite also other schemes with it
