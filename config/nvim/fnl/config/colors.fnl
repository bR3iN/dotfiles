(local {: lighten : darken : mix} (require :utils.colors))
(local {: base16 : extra} (require :base16-colors))

;; The 16 main names.
(fn to-named [base]
  {:bg0 base.base00
   :bg1 base.base01
   :bg2 base.base02
   :bg3 base.base03
   :fg0 base.base04
   :fg1 base.base05
   :fg2 base.base06
   :fg3 base.base07
   :red base.base08
   :orange base.base09
   :yellow base.base0A
   :green base.base0B
   :cyan base.base0C
   :blue base.base0D
   :magenta base.base0E
   :brown base.base0F})

(fn add-aliases [c]
  "Add named aliases or derived colors that inherit all variants (like dark_/light_)."
  (vim.tbl_extend :error c {:error c.red
                            :warn c.orange
                            :info c.green
                            :hint c.blue}))

(fn add-vars [c {: bg : dark-step : dim-step}]
  "Adds color variants, dimming towards `bg`."
  (let [mapped #(collect [name color (pairs c)]
                  (values ($1 name) ($2 color)))]
    (vim.tbl_extend :error c
                    (mapped #(.. :light_ $1) #(lighten $1 dark-step))
                    (mapped #(.. :dark_ $1) #(darken $1 dark-step))
                    (mapped #(.. :dim_ $1) #(mix $1 bg dim-step)))))

(fn add-consts [c {: bg}]
  "Add named colors that don't have associated variants."
  (vim.tbl_extend :error c
                  {:statusline c.bg2
                   :transparent bg
                   :mid (mix c.fg0 c.bg0 0.5)
                   :border (mix c.fg0 c.bg0 0.5)}))

(local named (-> base16
                 (to-named)
                 (add-aliases)
                 (add-vars {:bg extra.terminal_bg :dark-step 0.2 :dim-step 0.5})
                 (add-consts {:bg extra.terminal_bg})))

(fn colored-selection [accent]
  (mix accent named.bg1 0.03))

{: named : colored-selection}
