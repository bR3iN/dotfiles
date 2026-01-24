(local {: buf-keymaps!} (require :utils))

(buf-keymaps! {:n {:q {:desc "Close quickfix"
                       :callback :<Cmd>cclose<CR>}}})
