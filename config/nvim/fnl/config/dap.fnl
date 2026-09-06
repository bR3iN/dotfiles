(import-macros {: input!} :utils.macros)

(local dap (require :dap))
(local widgets (require :dap.ui.widgets))

(local M {})

(local configs [])

(fn M.track-last []
  (set dap.listeners.after.event_initialized.store_config
       (fn [{: config}]
         (table.insert configs 1 config)
         nil)))

(fn M.run_last []
  (let [config (or (. configs 1) (error "No last DAP run"))]
    (dap.run config)))

(fn M.restart_or_run_last []
  (if (dap.session)
      (dap.restart)
      (do
        (vim.print "Rerunning last DAP session")
        (M.run_last))))

(fn M.interactive-breakpoint []
  (input! [cond
           {:prompt "Condition: "}
           count
           {:prompt "Count: "}
           log
           {:prompt "Log Message: "}]
          (dap.toggle_breakpoint cond count log)))

(set M.eval {})

(fn M.eval.visual []
  (-> (vim.fn.getregion (vim.fn.getpos ".") (vim.fn.getpos "v"))
      (widgets.hover {})))

(fn M.eval.interactive []
  (-> (vim.fn.getregion (vim.fn.getpos ".") (vim.fn.getpos "v"))
      (widgets.hover {})))

(fn M.eval.word []
  (-> (vim.fn.expand :<cexpr>)
      (widgets.hover {})))

(set M.bindings
     {:continue {:desc "Continue" :callback dap.continue :repeatable true}
      :step-into {:desc "Step Into" :callback dap.step_into :repeatable true}
      :step-back {:desc "Step Back" :callback dap.step_back :repeatable true}
      :reverse-continue {:desc "Reverse Continue"
                         :callback dap.reverse_continue
                         :repeatable true}
      :step-over {:desc "Step Over" :callback dap.step_over :repeatable true}
      :step-out {:desc "Step Out" :callback dap.step_out :repeatable true}
      :toggle-breakpoint {:desc "Toggle Breakpoint"
                          :callback dap.toggle_breakpoint}
      :toggler-breakpoint-interactive {:desc "Toggle Breakpoint (Interactive)"
                                       :callback M.interactive-breakpoint}
      
      })

M
