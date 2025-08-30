(local {: hl! : local-keymaps! : keymaps! : autocmd! : use! : ft-autocmd!} (require :utils))

(fn pick-term []
  (let [bufnrs (icollect [_ buf (ipairs (vim.api.nvim_list_bufs))]
                 (when (and (vim.api.nvim_buf_is_valid buf)
                            (= (. vim.bo buf :buftype) :terminal))
                   buf))
        {: buffers} (require :telescope.builtin)]
    (vim.print bufnrs)
    (buffers {:prompt_title "Terminal Buffers" : bufnrs})))

;; Sane `<Esc>` behaviour in terminal mode
(keymaps! {:t {:<Esc> {:desc "Exit Terminal Mode" :callback "<C-\\><C-n>"}
               ;; :<C-v><Esc> {:desc "Send Escape to Terminal" :callback :<Esc>}
               }
           :n {"<leader>T" pick-term
               "_" {:desc "Open Terminal" :callback (partial vim.cmd.terminal {:args [:fish]})}}})

(use! "waiting-for-dev/ergoterm.nvim" {:setup {:ergoterm {}}})

(use! "willothy/flatten.nvim" {:setup {:flatten {}}})

(autocmd! {:event :TermOpen
           :callback (fn []
                       ;; (set vim.wo.number false)
                       ;; (set vim.wo.relativenumber false)
                       ;; FIXME: use when having separate picker
                       ;; (set vim.bo.buflisted false)
                       (vim.cmd.startinsert)
                       (local-keymaps! {:n {;; "Forward" some keys directoy in terminal mode
                                      "" (let [keys ["<CR>" "<C-c>" "<C-n>" "<C-p>" "q"]]
                                             (collect [_ key (ipairs keys)]
                                                 (values key (.. "i" key))))
                                      }}
                                 ))})

;; (use! "akinsho/toggleterm.nvim"
;;       {:setup {:toggleterm {:highlights {:StatusLine {:guibg colors.statusline}}
;;                             :direction :float
;;                             :shade_terminals false
;;                             :open_mapping "<c-j>"}}})
