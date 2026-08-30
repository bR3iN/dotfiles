(local {: buf-opts! : keymaps!} (require :utils))

(buf-opts! {:shiftwidth 2})

(keymaps! {:opts {:buffer true}
           ;; Here to not override `lspconfig`'s `on_attach`s which defines the commands
           :n {:<localleader> {:h {:desc "Switch between header and source"
                                   :callback #(vim.cmd.LspClangdSwitchSourceHeader)}
                               :i {:desc "Show symbol info"
                                   :callback #(vim.cmd.LspClangdShowSymbolInfo)}}}})
