(local {: lighten : darken : mix} (require :utils.colors))
(local {: named : colored-selection} (require :config.colors))
(local {: hl! : hls!} (require :utils))


;; NOTE: User `:Inspect` to get the hl groups under the cursor

(vim.cmd "syntax reset")
(vim.cmd "highlight clear")
(set vim.g.colors_name :base16)

;; c.f. https://github.com/shaunsingh/nord.nvim/blob/master/lua/nord/theme.lua
;; for highlight groups used usually in colorschemes.


;; Base Colorscheme

;; Builtin highlight groups , c.f. `:h highlight-groups`, with unset groups
;; as comments.
(hl! :ColorColumn {:bg named.bg2})
(hl! :Conceal {:bg named.bg1})
;; CurSearch
(hl! :Cursor {:fg named.bg0 :bg named.green})
(hl! :lCursor {:fg named.bg0 :bg named.dark_yellow})
(hl! :TermCursor {:fg named.bg0 :bg named.dark_cyan})
;; lCursor
;; CursorIM
(hl! :CursorColumn {:bg named.bg1})
(hl! :CursorLine {:bg (lighten named.bg0 0.3)})
(hl! :Directory {:fg named.cyan})
(hl! :DiffAdd {:bg (darken named.blue 0.5)})
(hl! :DiffChange {:bg (darken named.magenta 0.5)})
(hl! :DiffDelete {:bg (darken named.cyan 0.5)})
(hl! :DiffText {:bg (darken named.red 0.5)})
;; EndOfBuffer
;; TermCursor
;; TermCursorNCEG links
(hl! :ErrorMsg {:fg named.fg1 :bg named.red})
(hl! :WinSeparator {:fg named.bg2 :cterm nil})
(hl! :Folded {:bg named.bg1})
(hl! :FoldColumn {:bg named.bg1})
(hl! :SignColumn {:bg nil})
;; IncSearch
;; Substitute
(hl! :LineNr {:fg named.bg3})
;; LineNrAbove
;; LineNrBefore
(hl! :CursorLineNr {:fg named.brown :cterm nil})
;; CursorLineFold
;; CursorLineSign
(hl! :MatchParen {:fg named.yellow :bold true})
;; Defined by `vim-matchup`
(hl! :ModeMsg {:bold true})
;; MsgArea
;; MsgSeparator
(hl! :MoreMsg {:fg named.orange})
(hl! :NonText {:fg (mix named.brown named.bg3 0.3)})
(hl! :Normal {:fg named.fg1})

;; Transparent background and visible border
;; (hls! {:FloatBorder {:fg named.fg0} :NormalFloat {}})

;; No visible border but padding and a background constrast
(let [bg named.dark_bg3]
  (hls! {:FloatBorder {:fg bg : bg} :NormalFloat {: bg} :FloatTitle {: bg}}))

;; Border fg extends the float bg, e.g. with `:winborder "▗,▄,▖,▌,▘,▀,▝,▐"`
;; (let [bg named.bg3]
;;   (hls! {:FloatBorder {:fg bg} :NormalFloat {: bg}}))

;; NOTE: Separator inside LSP hover is `@spell.markdown -> @spell`, but falls back to `:NormalFloat` since transparent.
(hls! {:FloatTitle {:fg named.magenta :bold true :extend true}
       ;; Extend the border-related config above
       :NormalFloat {:fg named.fg2 :extend true}})

;; NormalNC
(let [bg named.bg2]
  (hls! {:Pmenu {:fg named.fg2 : bg}
         :PmenuSbar {:bg (mix named.bg0 bg 0.7)}
         :PmenuThumb {:bg (mix named.fg0 bg 0.3)}}))

(hl! :PmenuExtra {:fg named.dark_fg0})


;; non-standard!
(hl! :PmenuDoc {:fg named.fg2 :bg named.dark_bg3})
(hl! :PmenuDocBorder {:fg named.dark_bg3 :bg named.dark_bg3})

;; (hl! :PmenuSel {:fg named.bg0 :bg (darken named.yellow 0.15)})
(hl! :PmenuSel {:bg (mix named.dark_blue named.bg0 0.2)})
(hl! :PmenuMatch {:fg named.yellow})

;; (hl! :PmenuMatchSel {:bold true})
(hl! :PmenuKind {:fg named.dark_green})
;; PmenuKindSel
;; PmenuExtra
;; PmenuExtraSel
(hl! :Question {:fg named.green})
;; QuickFixLine
(hl! :Search {:fg named.bg0 :bg named.yellow})
(hl! :SpecialKey {:fg named.cyan})
;; SpellBad
(hl! :SpellCap {:bg named.bg3})

;; TabLineFill
(hl! :Title {:fg named.magenta})
(hl! :Visual {:bg (colored-selection named.dark_cyan)})
;; [N]ot [O]wning the [S]election
(hl! :VisualNOS {:bg (colored-selection named.dark_yellow)})
(hl! :WarningMsg {:fg named.red})
(hl! :WildMenu {:fg named.bg0 :bg named.yellow})
;; WinBar
;; WinBarNC
;; User<n>
;; Menu
;; Scrollbar
;; Tooltip

;; Syntax groups, c.f. `group-name`. The links should been default but don't
;; seem to be.
(hl! :Comment {:fg (lighten named.bg3 0.2)})

(hl! :Constant {:fg named.magenta})
(hl! :String {:link :Constant})
(hl! :Character {:link :Constant})
(hl! :Number {:link :Constant})
(hl! :Boolean {:link :Constant})
(hl! :Float {:link :Constant})

(hl! :Identifier {:fg named.cyan :bold true})
(hl! :Function {:link :Identifier})

(hl! :Statement {:fg named.yellow})
(hl! :Conditional {:link :Statement})
(hl! :Repeat {:link :Statement})
(hl! :Label {:link :Statement})
(hl! :Operator {:link :Statement})
(hl! :Keyword {:link :Statement})
(hl! :Exception {:link :Statement})

(hl! :PreProc {:fg named.blue})
(hl! :Include {:link :PreProc})
(hl! :Define {:link :PreProc})
(hl! :Macro {:link :PreProc})
(hl! :PreCondit {:link :PreProc})

(hl! :Type {:fg named.green})
(hl! :StorageClass {:link :Type})
(hl! :Structure {:link :Type})
(hl! :Typedev {:link :Type})

(hl! :Special {:fg named.brown})
(hl! :SpecialChar {:link :Special})
(hl! :Tag {:link :Special})
(hl! :Delimiter {:link :Special})
(hl! :SpecialComment {:link :Special})
(hl! :Debug {:link :Special})

(hl! :Underlined {:fg named.blue})

(hl! :Ignore {:fg named.brown})

(hl! :Todo {:fg named.bg0 :bg named.yellow})

(hl! :Error {:fg named.red :bg named.bg0})

;; Diagnostic highlights, named.f. `:h diagnostic-highlights`
(let [bg nil]
  (hl! :DiagnosticError {:fg named.red : bg})
  (hl! :DiagnosticHint {:fg named.blue : bg})
  (hl! :DiagnosticInfo {:fg named.green : bg})
  (hl! :DiagnosticWarn {:fg named.orange : bg})
  (hl! :DiagnosticSignError {:fg named.red : bg})
  (hl! :DiagnosticSignHint {:fg named.blue : bg})
  (hl! :DiagnosticSignInfo {:fg named.green : bg})
  (hl! :DiagnosticSignWarn {:fg named.orange : bg}))

;; Treesitter highlight groups
;; c.f. with list in https://github.com/folke/tokyonight.nvim/blob/main/lua/tokyonight/theme.lua
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
;; leap.nvim
(hl! :LeapLabelPrimary {:fg named.bg0 :bg named.red :nocombine true})
(hl! :LeapLabelSecondary {:fg named.bg0
                          :bg (lighten named.cyan 0.1)
                          :nocombine true})

(hl! :LeapMatch {:fg (lighten named.green 0.1)
                 :bg named.bg0
                 :underline true
                 :nocombine true})


;; Overrides
;; Keep these when changing to another colorscheme

;; TODO: Can't really get rid of boldness within heirline for some reason,
;; so unset it here.
(hl! :StatusLine {:fg named.fg1 :bg named.bg2 :bold false})
(hl! :StatusLineNC {:fg named.bg2 :bg named.bg2 :bold false})

(hl! :WinBar {:fg named.fg0 :bg named.bg3})
(hl! :WinBarNC {:fg named.fg0 :bg named.bg3})

(hl! :TabLine {:fg named.fg0 :bg named.transparent})
(hl! :TabLineSel {:fg named.green :bg named.bg3})

;; Transparent background
(hl! :Normal {:extend true :bg named.transparent})

;; Navic highlight groups
(local navic-hls {:NavicIconsArray {:fg named.yellow}
                  :NavicIconsBoolean {:fg named.cyan :bold true}
                  :NavicIconsClass {:fg named.cyan}
                  :NavicIconsConstant {:fg named.yellow}
                  :NavicIconsConstructor {:fg named.cyan}
                  :NavicIconsEnum {:fg named.cyan}
                  :NavicIconsEnumMember {:fg named.dark_cyan}
                  :NavicIconsEvent {:fg named.fg0}
                  :NavicIconsField {:fg named.dark_orange :italic true}
                  :NavicIconsFile {:fg named.green}
                  :NavicIconsFunction {:fg named.blue :italic true}
                  :NavicIconsInterface {:fg named.cyan}
                  :NavicIconsKey {:fg named.cyan}
                  :NavicIconsMethod {:fg named.blue :italic true}
                  :NavicIconsModule {:fg named.dark_green :italic true}
                  :NavicIconsNamespace {:fg named.fg0 :italic true}
                  :NavicIconsNull {:fg named.cyan}
                  :NavicIconsNumber {:fg named.magenta}
                  :NavicIconsObject {:fg named.cyan}
                  :NavicIconsOperator {:fg named.cyan}
                  :NavicIconsPackage {:fg named.fg0 :italic true}
                  :NavicIconsProperty {:fg named.dark_orange :italic true}
                  :NavicIconsString {:fg named.green :italic true}
                  :NavicIconsStruct {:fg named.cyan}
                  :NavicIconsTypeParameter {:fg named.blue}
                  :NavicIconsVariable {:fg named.dark_yellow :bold true}
                  :NavicText {:fg named.fg1}
                  :NavicSeparator {:fg (darken named.fg0 0.2)}})

(each [hl-name hl-opt (pairs navic-hls)]
  ;; (set hl-opt.bg named.bg2)
  (hl! hl-name hl-opt))
