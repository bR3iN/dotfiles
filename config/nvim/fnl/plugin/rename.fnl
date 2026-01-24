(fn rename-current-file []
  (let [curr-name (vim.api.nvim_buf_get_name 0)
        cb (fn [?new-name]
             (if
               (= (type ?new-name) :string)
               (vim.lsp.util.rename curr-name ?new-name)))]
    (vim.ui.input
      {:prompt "New name: "
       :default curr-name}
      cb)))
