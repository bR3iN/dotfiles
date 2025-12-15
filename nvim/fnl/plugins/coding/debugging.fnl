(local {: use!
        : ft!
        : keymaps!} (require :utils))

(ft! {:dap-repl #(vim.cmd "abbreviate <buffer> e -exec")
      :dap-float #(keymaps!
                     {:n {[:q :<ESC>] vim.cmd.quit}}
                     {:buffer true})})

(use! [:mason-org/mason.nvim
       :jay-babu/mason-nvim-dap.nvim ]
      {:setup {:mason {}
               :mason-nvim-dap {}}})

(use! [:mfussenegger/nvim-dap
       :nvim-neotest/nvim-nio
       ; needed by nvim-dap-ui
       :igorlfs/nvim-dap-view
       :rcarriga/nvim-dap-ui
       :theHamsta/nvim-dap-virtual-text
       ;; :mfussenegger/nvim-dap-python
       ]
      {:keymaps #(let [dap (require :dap)
                   widgets (require :dap.ui.widgets)
                   dapui (require :dapui)]
               {:n {:<Plug>debug#start vim.cmd.DapNew
                    :<leader> {"t" {;; "d" {:desc "Toggle Debug REPL"
                                    ;;      :callback dap.repl.toggle}
                                    "d" {:desc "Toggle Debug REPL"
                                         :callback vim.cmd.DapViewToggle}
                                    "D" {:desc "Toggle Debug UI"
                                         :callback dapui.toggle}}
                               "<CR>" {:desc "Run to Cursor"
                                       :callback dap.run_to_cursor}
                               "<C-CR>" {:desc "Run at Cursor"
                                       :callback :<Plug>debug#at-cursor}
                               "<TAB>" {:desc "Toggle Breakpoint"
                                        :callback dap.toggle_breakpoint}
                               "g" {"b" {:desc "Toggle Breakpoint"
                                        :callback dap.toggle_breakpoint}
                                    "l" {:desc "List Breakpoints"
                                        :callback dap.list_breakpoints}
                                    "e" {:desc "Eval Expression"
                                        :callback #(dapui.eval nil
                                                               {:enter true})}
                                    "s" {:desc "Step Into"
                                        :callback dap.step_into}
                                    "n" {:desc "Step Over"
                                        :callback dap.step_over}
                                    "o" {:desc "Step Out"
                                        :callback dap.step_out}
                                    "N" {:desc "DapNew"
                                        :callback :<Plug>debug#start}
                                    "R" {:desc "Run Last"
                                        :callback dap.run_last}
                                    "T" {:desc "Terminate"
                                        :callback dap.terminate}
                                    "c" {:desc "Continue"
                                        :callback dap.continue}
                                    "r" {:desc "Restart" :callback dap.restart}
                                    "h" {:desc "Hover"
                                        :callback widgets.hover}
                                    "p" {:desc "Preview"
                                        :callback widgets.preview}
                                    "d" {:desc "Clear Breakpoints"
                                        :callback dap.clear_breakpoints}}}}})
       :setup {:nvim-dap-virtual-text {:virt_text_pos :inline}
               :dap-view {}
               :dapui {}
               ;; :dap-python :python
               }
       :config (let [adapter-configs {:gdb {:type :executable
                                            :command :gdb
                                            :args [:--interpreter=dap
                                                   :--eval-command
                                                   "set print pretty on"]}
                                      :codelldb {:type :executable
                                                 :id :codelldb
                                                 :command :codelldb}
                                      ;; :lldb {:type :executable
                                      ;;        :id :lldb
                                      ;;        :command :/usr/bin/lldb-dap}
                                      }
                     ask-exe #(vim.fn.input "Path to executable: "
                                            (.. (vim.fn.getcwd) "/") :file)
                     filetype-configs {:cpp [{:name "Launch lldb"
                                              :type :lldb
                                              :request :launch
                                              :program ask-exe
                                              :stopOnEntry false
                                              :runInTerminal false
                                              :cwd "${workspaceFolder}"}
                                             {:name "Launch gdb"
                                              :type :gdb
                                              :request :launch
                                              :program ask-exe
                                              :stopOnEntry false
                                              :runInTerminal false
                                              :cwd "${workspaceFolder}"}]}]
                 #(do
                    (let [{: adapters : configurations} (require :dap)]
                      (each [name config (pairs adapter-configs)]
                        (set (. adapters name) config))
                      (each [name config (pairs filetype-configs)]
                        (set (. configurations name) config)))
                    ))})
