(local {: add_snippets} (require :luasnip))
; (get_snip_env)

(local {:snippet s :snippet_node sn :text_node t :insert_node i
        :function_node f :dynamic_node d} (require :luasnip))
(local {: fmt : fmta} (require :luasnip.extras.fmt))
(local {: rep} (require :luasnip.extras))

(local
  zk-note
  (s
   {:dscr "Zettelkasten Note"
    :trig :zk}
   [(t ["---"
        "tags: ["]) (i 1) (t ["]" ""])
    (t ["---"
        ""
        "# "]) (i 2) (t ["" "" ""])
    (i 0)
    ]))

(local by-filetype
  {;:all []
   :markdown [zk-note]})

{; Export some named snippets
 :named {: zk-note}
 ; Register snippets
 :setup (fn []
          (each [ft snippets (pairs by-filetype)]
            (add_snippets ft snippets)))}
