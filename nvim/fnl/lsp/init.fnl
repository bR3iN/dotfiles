(local lspconfig (require :lspconfig))
(local extend (partial vim.tbl_deep_extend :error))

(fn mk-on_attach [keymaps]
  (fn [client bufnr]
    ; Create keymaps
    (let [opts {:noremap true :silent true :buffer bufnr}]
      (each [[mode lhs] rhs (pairs keymaps)]
        (vim.keymap.set mode lhs rhs opts)))))

(fn setup [lsp-config]
  (let [{: default-keymaps : language-server} lsp-config]
    (each [_ {: name  : config : keymaps} (ipairs language-server)]
      (tset config :on_attach (-> keymaps
                                  (extend default-keymaps)
                                  (mk-on_attach)))
      ; Setup nvim-cmp integration if applicable
      (let [(ok mod) (pcall require :cmp_nvim_lsp)]
        (when ok
          (let [{: default_capabilities} mod]
            (tset config :capabilities (default_capabilities)))))
      (let [{: setup} (. lspconfig name)]
        (setup config)))))

{: setup}
