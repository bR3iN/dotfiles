(local {: use!} (require :utils))

(use! :tpope/vim-repeat)
;; (use! :tpope/vim-commentary)

(use! :windwp/nvim-autopairs
      {:setup {:nvim-autopairs {:enable_check_bracket_line false
                                :map_c_h true
                                :map_c_w true}}})

(use! :kylechui/nvim-surround
      {:setup {:nvim-surround {:keymaps {:insert :<C-s>
                                         :insert_line :<C-s><C-s>}}}})

;; (vim.keymap.del :i "<C-s>")
;; (add! "tpope/vim-surround"
;;       (fn []
;;         ; Find numbers with `:echo char2nr("B")`
;;         ; "B"
;;         (let! surround_66  "{\r}\1\1")))
