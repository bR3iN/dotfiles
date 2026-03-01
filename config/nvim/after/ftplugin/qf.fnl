(local {: keymaps!} (require :utils))

(keymaps! {:opts {:buffer true}
           :n {:q {:desc "Close quickfix"
                   :callback :<Cmd>quit<CR>}}})
