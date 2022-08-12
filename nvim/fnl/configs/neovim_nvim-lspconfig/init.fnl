(local lspconfig (require :lspconfig))
(local {: starts-with : remove-prefix} (require :utils))
(local mod ...)

(var default-keymaps {})

(fn set-default-keymaps [new-keymaps]
  (set default-keymaps new-keymaps))

(fn rhs->func [rhs]
  (let [lsp-pf :lsp:
        diag-pf :diag:]
    (if
      (starts-with rhs lsp-pf) (let [action (remove-prefix rhs lsp-pf)]
                                 (. vim.lsp.buf action))
      (starts-with rhs diag-pf) (let [action (remove-prefix rhs diag-pf)]
                                  (. vim.diagnostic action))
      rhs)))

(fn set-keymap [mode bufnr lhs rhs]
  (vim.keymap.set
    mode lhs rhs {:noremap true :silent true :buffer burnr}))

(fn mk-on_attach [keymaps]
  (local keymaps (vim.tbl_deep_extend :error keymaps default-keymaps))
  (fn [client bufnr]
    (each [lhs [mode rhs] (pairs keymaps)]
      (set-keymap mode bufnr lhs (rhs->func rhs)))))

(fn setup-with-config [ls-name]
  (let [{:config ?config :keymaps ?keymaps} (require (.. mod :. ls-name))
        config (or ?config {})
        keymaps (or ?keymaps {})]
    (tset config :on_attach (mk-on_attach keymaps))
    ((. lspconfig ls-name :setup) config)))

(fn setup-with-defaults [ls-name]
  (let [{: setup} (. (require :lspconfig)
                     ls-name)
        on_attach (mk-on_attach {})]
    (setup {: on_attach})))

(fn setup [opt-tbl]
  (let [{: keymaps :setup {: with-config : with-defaults}} opt-tbl]
    (set-default-keymaps keymaps)
    (vim.tbl_map setup-with-config with-config)
    (vim.tbl_map setup-with-defaults with-defaults)))

{: setup}
