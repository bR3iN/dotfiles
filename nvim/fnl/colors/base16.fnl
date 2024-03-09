(fn starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

(fn hexcolor? [str]
  (and (= (length str) 7)
       (starts-with str :#)))

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

(fn into-bounds [val]
  (-> val
      (math.max 0)
      (math.min 255)))

(fn add-rgb [[r1 g1 b1] [r2 g2 b2]]
  (let [add-channel #(into-bounds (+ $1 $2))]
    [(add-channel r1 r2)
     (add-channel g1 g2)
     (add-channel b1 b2)]))

(fn scale-rgb [rgb weight]
  (let [scale-channel #(into-bounds (* weight $1))]
    (vim.tbl_map scale-channel rgb)))

(fn mk-parse-color [base-colors]
  (fn [color]
    (match (type color)
      :string (if (not (hexcolor? color))
                (error (.. color " is not a valid hex color code"))
                color)
      :number (. base-colors color))))

(fn mk-utils [base-colors]
  (let [parse-color (mk-parse-color base-colors)]
    (local M {})

    (fn M.scale [color weight]
      (let [rgb (hex->rgb (parse-color color))]
        (rgb->hex (scale-rgb rgb weight))))

    (fn M.mix [color1 color2 weight]
      (let [rgb1 (hex->rgb (parse-color color1))
            rgb2 (hex->rgb (parse-color color2))]
        (rgb->hex
          (add-rgb
            (scale-rgb rgb1 weight)
            (scale-rgb rgb2 (- 1 weight))))))

    (fn M.lighten [color weight]
      (M.scale color (+ 1 weight)))

    (fn M.darken [color weight]
      (M.scale color (- 1 weight)))

    (fn M.hi! [name ?fg ?bg ?val]
      (let [fg (-?> ?fg
                    (parse-color))
            bg (-?> ?bg
                    (parse-color))]
        (let [extend (partial vim.tbl_extend :error)
              val (extend (or ?val {})
                          {: fg : bg})]
          (vim.api.nvim_set_hl 0 name val))))
    M))

(fn setup [base-colors]
  (local {: scale : mix : lighten : darken : hi!} (mk-utils base-colors))

  ; (vim.cmd "syntax reset")
  ; (vim.cmd "highlight clear")
  (set vim.g.colors_name :base16)

  (hi! :Normal 6 nil)
  (hi! :Comment (lighten 4 0.2))
  (hi! :Constant 15)
  (hi! :Special 16)
  (hi! :Identifier 13 nil {:bold true})
  (hi! :Statement 11)
  (hi! :PreProc 14)
  (hi! :Type 12)
  (hi! :Underlined 14)
  (hi! :Ignore 16)

  (hi! :DiffText   nil (darken 9  0.5))
  (hi! :DiffAdd    nil (darken 14 0.5))
  (hi! :DiffChange nil (darken 15 0.5))
  (hi! :DiffDelete nil (darken 13 0.5))

  (hi! :LeapMatch (lighten 12 0.1) 1 {:underline true :nocombine true})
  (hi! :LeapLabelPrimary 1 9 {:nocombine true})
  (hi! :LeapLabelSecondary 1 (lighten 13 0.1) {:nocombine true})

  (hi! :Conceal nil 2)
  (hi! :Cursor 1 6)
  (hi! :CursorColumn nil 2)
  (hi! :CursorLine nil (darken 2 0.05) {:cterm nil})
  (hi! :CursorLineFzfLua nil nil {:underline true})
  (hi! :CursorLineNr 16 nil {:cterm nil})
  (hi! :Directory 13)
  (hi! :FoldColumn nil 2)
  (hi! :Folded nil 2)
  (hi! :LineNr 4)
  (hi! :MatchParen 1 13)
  (hi! :MoreMsg 10)
  (hi! :NonText 2)
  (hi! :Pmenu (darken 11 0.15) 2)
  (hi! :PmenuSbar nil 3)
  (hi! :PmenuSel 1 (darken 11 0.15))
  (hi! :PmenuThumb nil (lighten 4 0.5))
  (hi! :Question 12)
  (hi! :SpecialKey 13)
  (hi! :Search 1 11)
  (hi! :Todo 1 11)
  (hi! :WarningMsg 10)
  (hi! :ErrorMsg 6 9)
  (hi! :WildMenu 1 11)
  (hi! :StatusLine 4 12 {:bold true :reverse true})
  (hi! :StatusLineNC 4 (mix 1 12 0.2) {:bold true :reverse true})
  (hi! :TabLine nil 2)
  (hi! :Title 15)
  (hi! :VertSplit 4 nil {:cterm nil})
  (hi! :Visual nil 4)
  (hi! :WarningMsg 9)
  (hi! :SignColumn nil 3)

  ; " Used by Neomake
  (hi! :SpellCap nil 4)

  (hi! :NormalFloat (darken 11 0.15) 1)
  (hi! :FloatBorder (mix 11 4 0.4))
  (hi! :DiagnosticError     9  1)
  (hi! :DiagnosticHint      14 1)
  (hi! :DiagnosticInfo      12 1)
  (hi! :DiagnosticWarn      10 1)
  (hi! :DiagnosticSignError 9  1)
  (hi! :DiagnosticSignHint  14 1)
  (hi! :DiagnosticSignInfo  12 1)
  (hi! :DiagnosticSignWarn  10 1)

  (hi! :TrailingWhitespace 10 10)
  ; (vim.cmd "syntax on")
  )

(let [(ok base-colors) (pcall require :base16-colors)]
  (if (not ok)
    (print "Couldn't find base16-colors.lua")
    (setup base-colors)))
