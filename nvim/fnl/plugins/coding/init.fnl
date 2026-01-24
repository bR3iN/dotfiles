(local {: use! : keymaps! : reload : opts!} (require :utils))
(local {: mix : get-named} (require :utils.colors))
(local colors (get-named))

;; Coding related stuff

;; Run current file, if applicable
;; (let [call #($1)
;;       ft-to-runner {:fennel #(vim.cmd.Fnlfile "%")
;;                     :python #(vim.api.nvim_command "w !python3")
;;                     :sh #(vim.api.nvim_command "w !bash")
;;                     :fish #(vim.api.nvim_command "w !fish")
;;                     :lua #(vim.api.nvim_command "lua dofile(vim.fn.expand('%'))")}]
;;   (nmap! :<leader>rr #(-?>> vim.o.filetype
;;                             (. ft-to-runner)
;;                             (call))))

(keymaps! {:n {"<leader>m" {:k {:desc "Make" :callback ":make!<CR>"}
                            :f {:desc "Make Flash"
                                :callback ":make! flash<CR>"}
                            :c {:desc "Make Clean"
                                :callback ":make! clean<CR>"}
                            :t {:desc "Make Test" :callback ":make! test<CR>"}
                            :b {:desc "Make Build"
                                :callback ":make! build<CR>"}}}})

;; (use! :neomake/neomake
;;       {:init (do
;;                ;; Appearance
;;                (let! neomake_error_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakeErrorSign"
;;                       })
;;                (let! neomake_warning_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakeWarningSign"
;;                       })
;;                (let! neomake_message_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakemessageSign"
;;                       })
;;                (let! neomake_info_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakeInfoSign"
;;                       }))
;;        :keymaps #{:n {:<leader>n {:m {:desc "Neomake Run"
;;                                   :callback #(vim.cmd.Neomake)}
;;                               :c {:desc "Neomake Clean"
;;                                   :callback #(vim.cmd.NeomakeClean)}}}}
;;        :hl {:NeomakeErrorSign {:link :DiagnosticSignError}
;;             :NeomakeWarningSign {:link :DiagnosticSignWarn}
;;             :NeomakeMessageSign {:link :DiagnosticSignHint}
;;             :NeomakeInfoSign {:link :DiagnosticSignInfo}
;;             :NeomakeVirtualtextError {:link :DiagnosticError}
;;             :NeomakeVirtualtextWarning {:link :DiagnosticWarn}
;;             :NeomakeVirtualtextMessage {:link :DiagnosticHint}
;;             :NeomakeVirtualtextInfo {:link :DiagnosticInfo}}})

(use! [:nvim-treesitter/nvim-treesitter
       :nvim-treesitter/nvim-treesitter-textobjects]
      ;; "nvim-treesitter/playground"
      {:setup {:nvim-treesitter.configs {:highlight {:enable true}
                                         ;; :indent {:enable true}
                                         ;; :additional_vim_regex_highlighting [:fennel]
                                         ;; Use with indent=true
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

(use! :lewis6991/gitsigns.nvim
      (let [signs {:add {:text "┃"}
                   :change {:text "┃"}
                   :delete {:text "┃"}
                   :untracked {:text "┆"}}
            signs_staged {:add {:text "+"}
                          :change {:text "~"}
                          :delete {:text "-"}
                          :untracked {:text "┆"}}]
        {:setup {:gitsigns {:signcolumn false
                            ;; :numhl true
                            ;; : signs
                            : signs_staged
                            :linehl false
                            :word_diff false
                            :sign_priority 0}}
         :hl #(let [for-bg #(mix colors.bg0 $1 0.9)
                    for-inline #(mix colors.bg0 $1 0.7)]
                {;; Used in signcolumn
                 :Added {:fg colors.green}
                 :Changed {:fg colors.cyan}
                 :Removed {:fg colors.red}
                 ;; Used for linehl
                 :DiffAdd {:bg (for-bg colors.green)}
                 :DiffChange {:bg (for-bg colors.cyan)}
                 :DiffDelete {:bg (for-bg colors.red)}
                 ;; Used for word_diff
                 :GitSignsDeleteInline {:bg (for-inline colors.red)}
                 :GitSignsAddInline {:bg (for-inline colors.green)}
                 :GitSignsChangeInline {:bg (for-inline colors.cyan)}})
         :keymaps {[:n :v] {:<Plug>nav#next-hunk #(vim.cmd.Gitsigns :nav_hunk :next)
                            :<Plug>nav#prev-hunk #(vim.cmd.Gitsigns :nav_hunk :prev)}
                   :n {:<Plug>git#stage-hunk #(vim.cmd.Gitsigns :stage_hunk)
                       :<Plug>git#reset-hunk #(vim.cmd.Gitsigns :reset_hunk)
                       :<Plug>ui#show-hunk-preview #(vim.cmd.Gitsigns :preview_hunk_inline)
                       :<Plug>ui#toggle-git-diff #(vim.cmd.Gitsigns :toggle_word_diff)
                       :<Plug>win#open-git-blame #(vim.cmd.Gitsigns :blame)}}}))

;; (autocmd! {:event [:BufEnter]
;;            :pattern "*"
;;            :callback (vim.schedule_wrap #(when (starts-with vim.bo.filetype
;;                                                             "dap")
;;                                            (case vim.bo.buftype
;;                                              :prompt (do
;;                                                          (vim.print "")
;;                                                          (vim.cmd.startinsert)))))})

;; Load submodules
(reload :plugins/coding/completion)
(reload :plugins/coding/lsp)
(reload :plugins/coding/debugging)
(reload :plugins/coding/languages)
