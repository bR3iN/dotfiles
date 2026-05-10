{:lenses {:name "Code Lenses"
         :get #(vim.lsp.codelens.is_enabled)
         :set #(vim.lsp.codelens.enable $1)}
 :diag-lines {:name "LSP Lines"
             :get #(-> (vim.diagnostic.config)
                       (. :virtual_lines))
             :set #(vim.diagnostic.config {:virtual_lines (if $1
                                                              {:current_line true}
                                                              false)
                                           :virtual_text (if $1
                                                             false
                                                             {:current_line true})})}
 :diag-curr {:name "Diag Cursorline Only"
            :get #(case (vim.diagnostic.config)
                    {:virtual_text {:current_line true}} true
                    {:virtual_lines {:current_line true}} true
                    _ false)
            :set #(vim.diagnostic.config (case (vim.diagnostic.config)
                                           {:virtual_lines false} {:virtual_text (if $1
                                                                                     {:current_line true}
                                                                                     true)}
                                           {:virtual_text false} {:virtual_lines (if $1
                                                                                     {:current_line true}
                                                                                     true)}
                                           _ (error "virtual lines and text are both enabled")))}}
