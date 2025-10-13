(local {: keymaps! : reload : lsps! : autocmd!} (require :utils))

(autocmd! {:event :LspAttach
           :pattern :*
           :callback (fn [{: buf}]
                       ;; Initial displaying
                       (vim.lsp.codelens.refresh {:bufnr buf})
                       ;; Updating lenses
                       (vim.api.nvim_create_autocmd
                         [:BufEnter :CursorHold :InsertLeave]
                         {:callback #(vim.lsp.codelens.refresh {:bufnr 0})
                          :buffer buf}
                         ))})

;; (vim.lsp.config :* {:on_attach (fn [_client bufnr]
;;                                  (vim.print "creating aucmd")
;;                                  (vim.api.nvim_create_autocmd [:BufEnter :CursorHold :InsertLeave]
;;                                                               {:callback #(do
;;                                                                             (vim.print "refreshing lenses")
;;                                                                             (vim.lsp.codelens.refresh {:bufnr 0}))
;;                                                                :buffer bufnr}
;;                                                               )
;;                                  )})

;; (autocmd! {:event [:BufEnter :CursorHold :InsertLeave]
;;            :pattern :*
;;            :callback #(vim.lsp.codelens.refresh {:bufnr 0})})

(lsps! {:clangd {:on_attach (fn [_client bufnr]
                              (keymaps! {:n {:<C-c> {:desc "Switch source/header"
                                                     :callback #(vim.cmd.ClangdSwitchSourceHeader)}}}
                                        {:buffer bufnr}))}
        ;; :bashls {}
        ;; :vimls {}
        :fennel_ls {}
        :cmake {}
        ;; :rust_analyzer {}  ; Configured by rustaceans instead
        ;; :marksman {}
        ;; :racket_langserver {}
        })

(lsps! (case (pcall reload :local.lspconfig)
                       (true config) config
                       (false {}) {}))
