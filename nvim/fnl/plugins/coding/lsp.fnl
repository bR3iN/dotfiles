(local {: use! : keymaps! : lsps! : autocmd! : reload} (require :utils))

(use! [:neovim/nvim-lspconfig :zk-org/zk-nvim]
      {:config (fn []
                 (autocmd! {:event :LspAttach
                            :pattern :*
                            :callback (fn [{: buf}]
                                        ;; Initial displaying
                                        (vim.lsp.codelens.refresh {:bufnr buf})
                                        ;; Updating lenses
                                        (vim.api.nvim_create_autocmd
                                          [:BufEnter :CursorHold :InsertLeave]
                                          {:callback #(vim.lsp.codelens.refresh {:bufnr 0})
                                           :buffer buf}))})

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
                          (false {}) {})))})
(let [cap-to-handler {:textDocument/hover vim.lsp.handlers.hover
                      :textDocument/signatureHelp vim.lsp.handlers.signature_help}]
  (each [cap handler (pairs cap-to-handler)]
    (tset vim.lsp.handlers cap (vim.lsp.with handler {:border _G.border-type}))))

;; (use! "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
;;       {:setup {:lsp_lines {}}
;;        :keymaps #(let [{: toggle} (require :lsp_lines)
;;                        toggle-with-inline #(let [lines-on (toggle)]
;;                                              (vim.diagnostic.config {:virtual_text (not lines-on)}))]
;;                    {:n {:<leader>tl {:desc "Toggle LSP Lines"
;;                                      :callback toggle-with-inline}}})})

(use! :folke/trouble.nvim
      {:setup {:trouble {:icons {:indent {:last "╰╴"}}}}})

(keymaps! {:n {:<Plug>win#open-diagnostics ":<C-u>Trouble diagnostics focus<CR>"
               :<Plug>win#close-diagnostics ":<C-u>Trouble diagnostics close<CR>"}})

; (set (. vim.lsp.handlers :textDocument/publishDiagnostics) (vim.lsp.with vim.lsp.diagnostic.on_publish_diagnostics {:update_in_insert true}))

(vim.diagnostic.config {:float {:border _G.border-type}
                        :virtual_lines false
                        :virtual_text {:current_line true}})
