(local lspconfig (require :lspconfig))
(local extend (partial vim.tbl_extend :force))

(var default-keymaps {})

(fn set-default-keymaps! [keymaps]
  (set default-keymaps keymaps))

(fn mk-on_attach [extra-keymaps]
  (let [keymaps (extend default-keymaps extra-keymaps)]
    (fn [client bufnr]
      (let [opts {:noremap true :silent true :buffer bufnr}]
        (each [[mode lhs] rhs (pairs keymaps)]
          (vim.keymap.set mode lhs rhs opts))))))

(fn mk-capabilities []
  (let [(ok mod) (pcall require :cmp_nvim_lsp)]
    (if ok
      (let [{: default_capabilities} mod]
        (default_capabilities))
      (vim.lsp.protocol.make_client_capabilitites))))

(fn ls-setup! [name ?config ?extra-keymaps]
  (let [config (or ?config {})
        extra-keymaps (or ?extra-keymaps {})
        default-config {:capabilities (mk-capabilities)
                        :on_attach (mk-on_attach extra-keymaps)}
        {: setup} (. lspconfig name)]
    (setup (extend default-config config))))

{: set-default-keymaps!
 : ls-setup!
 : mk-on_attach
 : mk-capabilities}
