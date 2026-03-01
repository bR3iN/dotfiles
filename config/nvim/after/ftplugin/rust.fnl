(local {: keymaps!} (require :utils))

(keymaps! {:opts {:buffer true}
           :n {"<localleader>" {"b" {:desc "Build"
                                     :callback #(vim.cmd.Cargo :build)}
                                "t" {:desc "Test"
                                     :callback #(vim.cmd.Cargo :test)}
                                "c" {:desc "Check/Clippy"
                                     :callback #(vim.cmd.Cargo :clippy)}}}})
