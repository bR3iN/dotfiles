(local {: add!} (require :pkg))

(fn starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

(fn hex->rgb [hex]
  (let [parse-slice (fn [start end]
                      (let [slice (hex:sub start end)]
                        (tonumber slice 16)))]
    (let [r (parse-slice 2 3)
          g (parse-slice 4 5)
          b (parse-slice 6 7)]
      [r g b])))

(fn rgb->hex [[r g b]]
  (string.format "#%02x%02x%02x" r g b))

(fn clamp [val]
  (-> val
      (math.max 0)
      (math.min 255)))

(fn add-rgb [[r1 g1 b1] [r2 g2 b2]]
  (let [add-channel #(clamp (+ $1 $2))]
    [(add-channel r1 r2)
     (add-channel g1 g2)
     (add-channel b1 b2)]))

(fn scale-rgb [[r g b] weight]
  (let [scale-channel #(clamp (* weight $1))]
    [(scale-channel r)
     (scale-channel g)
     (scale-channel b)]))

(fn scale [color weight]
  (let [rgb (hex->rgb color)]
    (rgb->hex (scale-rgb rgb weight))))

(fn mix [color1 color2 weight]
  (let [rgb1 (hex->rgb color1)
        rgb2 (hex->rgb color2)]
    (rgb->hex
      (add-rgb
        (scale-rgb rgb1 weight)
        (scale-rgb rgb2 (- 1 weight))))))

(fn lighten [color weight]
  (scale color (+ 1 weight)))

(fn darken [color weight]
  (scale color (- 1 weight)))

(fn hi! [name opts]
  (vim.api.nvim_set_hl 0 name opts))

(fn set-highlights [c]
  (if (= nil c)
    (error "set base16 colors before setting the colorscheme"))

  (vim.cmd "syntax reset")
  (vim.cmd "highlight clear")
  (set vim.g.colors_name :base16)

  ; C.f. https://github.com/shaunsingh/nord.nvim/blob/master/lua/nord/theme.lua
  ; for highlight groups used usually in colorschemes.

  ;; Builtin highlight groups , c.f. `:h highlight-groups`, with unset groups
  ;; as comments.
  ; ColorCoulumn
  (hi! :Conceal {:bg c.base01})
  ; CurSearch
  (hi! :Cursor {:fg c.base00 :bg c.base05})
  ; lCursor
  ; CursorIM
  (hi! :CursorColumn {:bg c.base01})
  (hi! :CursorLine {:bg (darken c.base01 0.05) :cterm nil})
  (hi! :Directory {:fg c.base0C})
  (hi! :DiffAdd    {:bg (darken c.base0D 0.5)})
  (hi! :DiffChange {:bg (darken c.base0E 0.5)})
  (hi! :DiffDelete {:bg (darken c.base0C 0.5)})
  (hi! :DiffText   {:bg (darken c.base08 0.5)})
  ; EndOfBuffer
  ; TermCursor
  ; TermCursorNC
  (hi! :ErrorMsg {:fg c.base05 :bg c.base08})
  (hi! :WinSeparator {:fg c.base05 :cterm nil})
  (hi! :Folded {:bg c.base01})
  (hi! :FoldColumn {:bg c.base01})
  (hi! :SignColumn {:bg c.base02})
  ; IncSearch
  ; Substitute
  (hi! :LineNr {:fg c.base03})
  ; LineNrAbove
  ; LineNrBefore
  (hi! :CursorLineNr {:fg c.base0F :cterm nil})
  ; CursorLineFold
  ; CursorLineSign
  (hi! :MatchParen {:fg c.base00 :bg c.base0C})
  (hi! :ModeMsg {:bold true})
  ; MsgArea
  ; MsgSeparator
  (hi! :MoreMsg {:fg c.base09})
  (hi! :NonText {:fg c.base01})
  (hi! :Normal {:fg c.base05})
  (hi! :NormalFloat {:fg (darken c.base0A 0.15) :bg c.base00})
  (hi! :FloatBorder {:fg (mix c.base0A c.base03 0.4)})
  ; NormalNC
  (hi! :Pmenu {:fg (darken c.base0A 0.15) :bg c.base01})
  (hi! :PmenuSel {:fg c.base00 :bg (darken c.base0A 0.15)})
  ; PmenuKind
  ; PmenuKindSel
  ; PmenuExtra
  ; PmenuExtraSel
  (hi! :PmenuSbar {:bg c.base02})
  (hi! :PmenuThumb {:bg (lighten c.base03 0.5)})
  (hi! :Question {:fg c.base0B})
  ; QuickFixLine
  (hi! :Search {:fg c.base00 :bg c.base0A})
  (hi! :SpecialKey {:fg c.base0C})
  ; SpellBad
  (hi! :SpellCap {:bg c.base03})  ; " Used by Neomak
  ; SpellLocal
  ; SpellRare
  (hi! :StatusLine {:fg c.base03 :bg c.base0B :bold true :reverse true})
  (hi! :StatusLineNC {:fg c.base03 :bg (mix c.base00 c.base0B 0.2) :bold true :reverse true})
  (hi! :TabLine {:bg c.base01})
  ; TabLineFill
  ; TabLineSel
  (hi! :Title {:fg c.base0E})
  (hi! :Visual {:bg c.base03})
  ; VisualNOS
  (hi! :WarningMsg {:fg c.base08})
  (hi! :WildMenu {:fg c.base00 :bg c.base0A})
  ; WinBar
  ; WinBarNC
  ; User<n>
  ; Menu
  ; Scrollbar
  ; Tooltip

  ;; Syntax groups, c.f. `group-name`. The links should been default but don't
  ;; seem to be.
  (hi! :Comment {:fg (lighten c.base03 0.2)})

  (hi! :Constant {:fg c.base0E})
  (hi! :String {:link :Constant})
  (hi! :Character {:link :Constant})
  (hi! :Number {:link :Constant})
  (hi! :Boolean {:link :Constant})
  (hi! :Float {:link :Constant})

  (hi! :Identifier {:fg c.base0C :bold true})
  (hi! :Function {:link :Identifier})

  (hi! :Statement {:fg c.base0A})
  (hi! :Conditional {:link :Statement})
  (hi! :Repeat {:link :Statement})
  (hi! :Label {:link :Statement})
  (hi! :Operator {:link :Statement})
  (hi! :Keyword {:link :Statement})
  (hi! :Exception {:link :Statement})

  (hi! :PreProc {:fg c.base0D})
  (hi! :Include {:link :PreProc})
  (hi! :Define {:link :PreProc})
  (hi! :Macro {:link :PreProc})
  (hi! :PreCondit {:link :PreProc})

  (hi! :Type {:fg c.base0B})
  (hi! :StorageClass {:link :Type})
  (hi! :Structure {:link :Type})
  (hi! :Typedev {:link :Type})

  (hi! :Special {:fg c.base0F})
  (hi! :SpecialChar {:link :Special})
  (hi! :Tag {:link :Special})
  (hi! :Delimiter {:link :Special})
  (hi! :SpecialComment {:link :Special})
  (hi! :Debug {:link :Special})

  (hi! :Underlined {:fg c.base0D})

  (hi! :Ignore {:fg c.base0F})

  (hi! :Todo {:fg c.base00 :bg c.base0A})

  (hi! :Error {:fg c.base08 :bg c.base00})

  ;; Diagnostic highlights, c.f. `:h diagnostic-highlights`
  (hi! :DiagnosticError     {:fg c.base08 :bg c.base00})
  (hi! :DiagnosticHint      {:fg c.base0D :bg c.base00})
  (hi! :DiagnosticInfo      {:fg c.base0B :bg c.base00})
  (hi! :DiagnosticWarn      {:fg c.base09 :bg c.base00})
  (hi! :DiagnosticSignError {:fg c.base08 :bg c.base00})
  (hi! :DiagnosticSignHint  {:fg c.base0D :bg c.base00})
  (hi! :DiagnosticSignInfo  {:fg c.base0B :bg c.base00})
  (hi! :DiagnosticSignWarn  {:fg c.base09 :bg c.base00})

  ;; Treesitter highlight groups
  ; c.f. with list in https://github.com/folke/tokyonight.nvim/blob/main/lua/tokyonight/theme.lua
  (hi! "@annotation" {:link "PreProc"})
  (hi! "@attribute" {:link "PreProc"})
  (hi! "@boolean" {:link "Boolean"})
  (hi! "@character" {:link "Character"})
  (hi! "@character.special" {:link "SpecialChar"})
  (hi! "@comment" {:link "Comment"})
  (hi! "@keyword.conditional" {:link "Conditional"})
  (hi! "@constant" {:link "Constant"})
  (hi! "@constant.builtin" {:link "Special"})
  (hi! "@constant.macro" {:link "Define"})
  (hi! "@keyword.debug" {:link "Debug"})
  (hi! "@keyword.directive.define" {:link "Define"})
  (hi! "@keyword.exception" {:link "Exception"})
  (hi! "@number.float" {:link "Float"})
  (hi! "@function" {:link "Function"})
  (hi! "@function.builtin" {:link "Special"})
  (hi! "@function.call" {:link "@function"})
  (hi! "@function.macro" {:link "Macro"})
  (hi! "@keyword.import" {:link "Include"})
  (hi! "@keyword.coroutine" {:link "@keyword"})
  (hi! "@keyword.operator" {:link "@operator"})
  (hi! "@keyword.return" {:link "@keyword"})
  (hi! "@function.method" {:link "Function"})
  (hi! "@function.method.call" {:link "@function.method"})
  (hi! "@namespace.builtin" {:link "@variable.builtin"})
  (hi! "@none" {})
  (hi! "@number" {:link "Number"})
  (hi! "@keyword.directive" {:link "PreProc"})
  (hi! "@keyword.repeat" {:link "Repeat"})
  (hi! "@keyword.storage" {:link "StorageClass"})
  (hi! "@string" {:link "String"})
  (hi! "@markup.link.label" {:link "SpecialChar"})
  (hi! "@markup.link.label.symbol" {:link "Identifier"})
  (hi! "@tag" {:link "Label"})
  (hi! "@tag.attribute" {:link "@property"})
  (hi! "@tag.delimiter" {:link "Delimiter"})
  (hi! "@markup" {:link "@none"})
  (hi! "@markup.environment" {:link "Macro"})
  (hi! "@markup.environment.name" {:link "Type"})
  (hi! "@markup.raw" {:link "String"})
  (hi! "@markup.math" {:link "Special"})
  (hi! "@markup.strong" {:bold true})
  (hi! "@markup.emphasis" {:italic true})
  (hi! "@markup.strikethrough" {:strikethrough true})
  (hi! "@markup.underline" {:underline true})
  (hi! "@markup.heading" {:link "Title"})
  (hi! "@comment.note" {:fg c.hint})
  (hi! "@comment.error" {:fg c.error})
  (hi! "@comment.hint" {:fg c.hint})
  (hi! "@comment.info" {:fg c.info})
  (hi! "@comment.warning" {:fg c.warning})
  (hi! "@comment.todo" {:fg c.todo})
  (hi! "@markup.link.url" {:link "Underlined"})
  (hi! "@type" {:link "Type"})
  (hi! "@type.definition" {:link "Typedef"})
  (hi! "@type.qualifier" {:link "@keyword"})

  ;; Highlight groups used in Plugins
  ; leap.nvim
  (hi! :LeapLabelPrimary {:fg c.base00 :bg c.base08 :nocombine true})
  (hi! :LeapLabelSecondary {:fg c.base00 :bg (lighten c.base0C 0.1) :nocombine true})
  (hi! :LeapMatch {:fg (lighten c.base0B 0.1) :bg c.base00 :underline true :nocombine true})

  ; (vim.cmd "syntax on")
  )

{: set-highlights}
