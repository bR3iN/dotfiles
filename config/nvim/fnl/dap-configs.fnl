(local {: pick_process} (require :dap.utils))
(local dap (require :dap))

(set dap.adapters
     {:gdb {:type :executable
            :command :gdb
            :args [:--interpreter=dap :--eval-command "set print pretty on"]}
      :rust-gdb {:type :executable
                 :command :gdb
                 :args [:--interpreter=dap
                        :--eval-command
                        "set print pretty on"]}
      :codelldb {:type :executable
                 :command (.. vim.env.HOME
                              :/.local/share/codelldb/adapter/codelldb)}
      :lldb {:type :executable :command :/usr/bin/lldb-dap}
      :rust-lldb {:type :executable :command :rust-lldb}})

(fn ask-exe []
  (vim.print (vim.fn.input {:prompt "Path to executable: "
                 :default (.. (vim.fn.getcwd) "/")
                 :completion :file})))

(fn pick-pid []
  (let [name (vim.fn.input {:prompt "Executable name (filter): "})]
    (pick_process {:filter name})))

(fn ask-server []
  (vim.fn.input {:prompt "gdb server: " :default "localhost:"}))

(local lldb {:name "Launch lldb"
             :type :lldb
             :request :launch
             :program ask-exe
             :stopOnEntry false
             :runInTerminal false
             :cwd "${workspaceFolder}"})

(local codelldb {:name "Launch codelldb"
                 :type :codelldb
                 :request :launch
                 :program ask-exe
                 :stopOnEntry false
                 :runInTerminal false
                 :cwd "${workspaceFolder}"})

(local gdb-exe {:name "Launch gdb"
                :type :gdb
                :request :launch
                :program ask-exe
                :cwd "${workspaceFolder}"})

(local gdb-attach {:name "Attach gdb (process)"
                   :type :gdb
                   :request :attach
                   :program ask-exe
                   :pid pick-pid
                   :cwd "${workspaceFolder}"})

(local gdb-server {:name "Attach gdb (server)"
                   :type :gdb
                   :request :attach
                   :target ask-server
                   :program ask-exe
                   :cwd "${workspaceFolder}"})

(fn rustic [config]
  ;; Also prevents modifing `config` in place
  (collect [key val (pairs config)]
    (values key (if (= key :type) (.. :rust- val) val))))

(set dap.configurations {:c [lldb gdb-exe gdb-attach gdb-server]
                         :cpp [lldb gdb-exe gdb-attach gdb-server]
                         :rust [(rustic lldb)
                                (rustic gdb-exe)
                                (rustic gdb-attach)
                                (rustic gdb-server)
                                codelldb]})
