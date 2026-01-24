(local {: use!} (require :utils))
(local {: mix : get-named} (require :utils.colors))
(local colors (get-named))

(use! :lewis6991/gitsigns.nvim
      (let [_signs {:add {:text "┃"}
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

