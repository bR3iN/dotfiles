(local {: buf-keymaps!} (require :utils))

(buf-keymaps! {:n {"<localleader>" {"b" {:desc "Build"
                                         :callback #(vim.cmd.Cargo :build)}
                                    "t" {:desc "Test"
                                         :callback #(vim.cmd.Cargo :test)}
                                    "c" {:desc "Check/Clippy"
                                         :callback #(vim.cmd.Cargo :clippy)}}}})
