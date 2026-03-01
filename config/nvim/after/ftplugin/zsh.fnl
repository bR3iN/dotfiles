(local {: buf-opts! : keymaps!} (require :utils))

(buf-opts! {:shiftwidth 4})

(keymaps! {:opts {:buffer true}
           :n {"<localleader>" {"r" {:desc "Run"
                                     :callback #(vim.cmd "!zsh %")}}}})
