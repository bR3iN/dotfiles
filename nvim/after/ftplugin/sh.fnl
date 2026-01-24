(local {: buf-opts! : buf-keymaps!} (require :utils))

(buf-opts! {:shiftwidth 4})

(buf-keymaps! {:n {"<localleader>" {"r" {:desc "Run"
                                         :callback #(vim.cmd "!bash %")}}}})
