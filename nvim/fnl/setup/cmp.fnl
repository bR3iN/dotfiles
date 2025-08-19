(local cmp (require :cmp))
(local ls (require :luasnip))

(fn next-or-complete [_fallback]
  (if (cmp.visible)
      (cmp.select_next_item {:behavior cmp.SelectBehavior.Insert})
      (cmp.complete)))

(fn prev [fallback]
  (if (cmp.visible)
      (cmp.select_prev_item {:behavior cmp.SelectBehavior.Insert})
      (fallback)))

(fn confirm [fallback]
  (if (cmp.visible)
      (cmp.confirm {:select true})
      (fallback)))

(fn snippet-next [fallback]
  (if (ls.locally_jumpable 1)
      (ls.jump 1)
      (fallback)))

(fn snippet-prev [fallback]
  (if (ls.locally_jumpable -1)
      (ls.jump -1)
      (fallback)))

(fn snippet-expand [fallback]
  (if (ls.expandable)
      (ls.expand)
      (fallback)))

(fn abort [fallback]
  (if (cmp.visible)
      (cmp.abort)
      (fallback)))

(fn docs-down [fallback]
  (if (cmp.visible)
      (cmp.scroll_docs 5)
      (fallback)))

(fn docs-up [fallback]
  (if (cmp.visible)
      (cmp.scroll_docs -5)
      (fallback)))

(local maps {:<C-n> next-or-complete
             :<Down> next-or-complete
             :<C-p> prev
             :<Up> prev
             :<C-l> confirm
             :<Right> confirm
             :<Tab> snippet-next
             :<S-Tab> snippet-prev
             :<C-k> snippet-expand
             :<C-e> abort
             :<C-d> docs-down
             :<C-u> docs-up})

(cmp.setup {:mapping (collect [lhs rhs (pairs maps)]
                       (values lhs (cmp.mapping rhs [:i :s])))
            :completion {:completeopt "menu,menuone"}
            :snippet {:expand (fn [{: body}]
                                (ls.lsp_expand body))}
            ;; The LSP specifies that certain items should be preselected, ignoring their position in the suggestion list. This disables this.
            :preselect cmp.PreselectMode.None
            :sources [{:name :nvim_lsp}
                      {:name :luasnip}
                      {:name :orgmode}
                      {:name :nvim_lua}
                      {:name :neorg}
                      ;; Freezes sometimes when completing root dir
                      ;; {:name :path}
                      {:name :dap}
                      {:name :latex_symbols}
                      {:name :omni}
                      {:name :buffer :option {:keyword_pattern "\\k\\+"}}]
            ;; :window {:completion (cmp.config.window.bordered)
            ;;          :documentation (cmp.config.window.bordered)}
            :formatting {:format (let [display-names {:nvim_lsp "[LSP]"
                                                      :luasnip "[Snp]"
                                                      :nvim_lua "[Lua]"
                                                      :latex_symbols "[LTX]"
                                                      :path "[Pth]"
                                                      :omni "[Omn]"
                                                      :calc "[Clc]"
                                                      :buffer "[Buf]"}]
                                   (fn [{:source {: name}} item]
                                     (->> name
                                          (. display-names)
                                          (#(or $1 "[???]"))
                                          (tset item :menu))
                                     item))}})
