(local dap (require :dap))
(local M {})

(local configs [])
(set dap.listeners.after.event_initialized.store_config (fn [{: config}]
                                                            (table.insert configs 1 config)
                                                            nil))

(fn M.run_last []
    (let [config (or (. configs 1) (error "No last DAP run"))]
        (dap.run config)))

M
