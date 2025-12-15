(local {: buf-keymaps!} (require :utils))

;; FIXME: looks weird
(let [cmd (fn [action]
            #(vim.cmd.RustLsp action))]
  (buf-keymaps! {:n {:<Plug> {:lsp#code-action (cmd :codeAction)
                               :debug#start (cmd :debuggables)
                               :debug#at-cursor (cmd :debug)
                               :edit#format-file vim.cmd.RustFmt}
                      "<localleader>" {"r" {:desc "Run"
                                            :callback (cmd :run)}
                                       "b" {:desc "Build"
                                            :callback #(vim.cmd.Cargo :build)}
                                       "t" {:desc "Test"
                                            :callback #(vim.cmd.Cargo :test)}
                                       "c" {:desc "Check/Clippy"
                                            :callback #(vim.cmd.Cargo :clippy)}}}}))
