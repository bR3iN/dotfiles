(local {: use! : keymaps! : reload : opts!} (require :utils))

;; FIXME: find place for `curl -o $HOME/.local/share/fennel-ls/docsets/nvim.lua https://git.sr.ht/~micampe/fennel-ls-nvim-docs/blob/main/nvim.lua`

(keymaps! {:n {"<leader>m" {:k {:desc "Make" :callback ":make!<CR>"}
                            :f {:desc "Make Flash"
                                :callback ":make! flash<CR>"}
                            :c {:desc "Make Clean"
                                :callback ":make! clean<CR>"}
                            :t {:desc "Make Test" :callback ":make! test<CR>"}
                            :b {:desc "Make Build"
                                :callback ":make! build<CR>"}}}})

(use! [:nvim-treesitter/nvim-treesitter
       :nvim-treesitter/nvim-treesitter-textobjects]
      ;; "nvim-treesitter/playground"
      {:setup {:nvim-treesitter.configs {:highlight {:enable true}
                                         ;; :indent {:enable true}
                                         ;; :additional_vim_regex_highlighting [:fennel]
                                         ;; :incremental_selection {:enable true
                                         ;;                         :keymaps {:node_incremental "V"
                                         ;;                                   :scope_incremental "v"}}
                                         :textobjects {:select {:enable true
                                                                :keymaps {:if "@function.inner"
                                                                          :af "@function.outer"
                                                                          :ic "@call.inner"
                                                                          :ac "@call.outer"
                                                                          :il "@loop.inner"
                                                                          :al "@loop.outer"
                                                                          :ik "@conditional.inner"
                                                                          :ak "@conditional.outer"}}
                                                       :swap {:enable true
                                                              :swap_next {:<Plug>edit#swap-param-next "@parameter.inner"}
                                                              :swap_previous {:<Plug>edit#swap-param-prev "@parameter.inner"}}}}}
       :config (fn []
                 ;; Use treesitter-based folds
                 (opts! {:foldmethod :expr
                         :foldexpr "nvim_treesitter#foldexpr()"}))})

(vim.diagnostic.config {:signs {:text {vim.diagnostic.severity.ERROR ""
                                       ; ""
                                       vim.diagnostic.severity.WARN ""
                                       ; ""
                                       vim.diagnostic.severity.INFO ""
                                       ; ""
                                       vim.diagnostic.severity.HINT ""
                                       ; "󰌵"
                                       }}})

;; (autocmd! {:event [:BufEnter]
;;            :pattern "*"
;;            :callback (vim.schedule_wrap #(when (starts-with vim.bo.filetype
;;                                                             "dap")
;;                                            (case vim.bo.buftype
;;                                              :prompt (do
;;                                                          (vim.print "")
;;                                                          (vim.cmd.startinsert)))))})

(use! :elkowar/yuck.vim)

;; Treesitter indentation for fennel is messed up, so use this plugin for this
(use! :jaawerth/fennel.vim)

(use! :lervag/vimtex
      {:config (fn []
                 (set vim.g.vimtex_format_enabled 1)
                 (set vim.g.vimtex_quickfix_mode 0)
                 (set vim.g.vimtex_view_method :zathura)
                 (set vim.g.tex_flavor :latex))})

;; Avoids "Unrecognized option: 'write-mode'" error.
(set vim.g.rustfmt_detect_version 1)

(use! [:mrcjkb/rustaceanvim]
      {:init #(let [lsp-config {:rust-analyzer {:cargo {:buildScripts {:enable true}}
                                                :procMacro {:enable true}
                                                :imports {;; Merge auto imports on the module level
                                                          :granularity {:group :module}
                                                          ;; Prefer relative auto import paths
                                                          :prefix :self}}}]
                (set vim.g.rustaceanvim
                     {:server {:default_settings lsp-config}}))})

;; Forked as plugin doesn't have an API for custom keybindings
(use! :bR3iN/jupynium.nvim {:setup {:jupynium {}}})

;; Load submodules
(reload :plugins.coding.completion)
(reload :plugins.coding.lsp)
(reload :plugins.coding.debugging)
(reload :plugins.coding.zk)
(reload :plugins.coding.git)
