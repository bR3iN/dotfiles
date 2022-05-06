(local lspconfig (require :lspconfig))
(local {: starts-with : remove-prefix : rhs->str} (require :utils))
(local mod ...)

(var default-keymaps {})

(fn set-default-keymaps [new-keymaps]
  (set default-keymaps new-keymaps))

(fn action->capability [action]
  (match action
    :references :find_references
    :definition :goto_definition
    :formatting :document_formatting
    :range_formatting :document_range_formatting
    _ action))

(fn parse-rhs [rhs]
  (let [lsp-pf :lsp
        diag-pf :diag]
    (if
      (starts-with rhs lsp-pf) (let [action (remove-prefix rhs lsp-pf)]
                                 (values
                                   #(. vim.lsp.buf action)
                                   (action->capability action)))
      (starts-with rhs diag-pf) (let [action (remove-prefix rhs diag-pf)]
                                  (values
                                    #(. vim.diagnostic action)
                                    nil))
      (values rhs nil))))

(fn set-keymap [mode bufnr lhs rhs]
  (vim.api.nvim_buf_set_keymap
    bufnr mode lhs (rhs->str rhs)
    {:noremap true :silent true}))

(fn mk-on_attach [keymaps]
  (local keymaps (vim.tbl_deep_extend :error keymaps default-keymaps))
  (fn [client bufnr]
    (let [caps client.server_capabilities]
      (each [lhs [mode rhs] (pairs keymaps)]
        (let [(new-rhs ?cap) (parse-rhs rhs)]
          (if (or (= cap nil)
                  (. caps cap))
            (set-keymap mode bufnr lhs new-rhs)))))))

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
