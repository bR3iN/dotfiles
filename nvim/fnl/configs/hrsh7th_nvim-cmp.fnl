(local {: add!} (require :pkg))
(local cmp (require :cmp))

(add! "hrsh7th/cmp-nvim-lsp")
(add! "hrsh7th/cmp-nvim-lua")
(add! "hrsh7th/cmp-path")
(add! "hrsh7th/cmp-buffer")
(add! "hrsh7th/cmp-vsnip")

(fn role->func [role]
  (let [jumpable (. vim.fn :vsnip#jumpable)
        feed (fn [str]
               (vim.fn.feedkeys
                 (vim.api.replace_termcodes str true true true)))]
    (match role
      :tab-role  (fn [fallback]
                   (if
                     (cmp.visible) (cmp.select_next_item)
                     (= 1 (jumpable 1)) (feed "<Plug>(vsnip-jump-next)")
                     (fallback)))
      :stab-role (fn [fallback]
                   (if
                     (cmp.visible) (cmp.select_prev_item)
                     (= 1 (jumpable -1)) (feed "<Plug>(vsnip-jump-prev)")
                     (cmp.complete)))
      :confirm (cmp.mapping.confirm {:select true})
      :cancel  (cmp.mapping.close)
      other    (error (.. "Unexpected value for 'role': " other)))))

(local opt-tbl
  {:snippet {:expand (fn [args]
                       ((. vim.fn :vsnip#anonymous) args.body))}
   :sources [
             {:name :nvim_lsp}
             {:name :nvim_lua}
             {:name :neorg}
             {:name :vsnip}
             {:name :path}
             {:name :calc}
             {:name :omni}
             {:name :buffer
              :option {:keyword_pattern "\\k\\+"}}
             ; {:name :neorg}
             ]
   :formatting {:format (fn [entry vim-item]
                          (let [display-name
                                (match (. entry :source :name)
                                  :nvim_lsp "[LSP]"
                                  :nvim_lua "[Lua]"
                                  :vsnip    "[Vsp]"
                                  :path     "[Pth]"
                                  :calc     "[Clc]")]
                            (tset vim-item :menu display-name)
                            vim-item))}})

(fn setup [key-by-role]
  (let [mapping (collect [role key (pairs key-by-role)]
                  (values key (role->func role)))
        {: setup} (require :cmp)]
    (tset opt-tbl :mapping mapping)
    (setup opt-tbl)))

{: setup}
