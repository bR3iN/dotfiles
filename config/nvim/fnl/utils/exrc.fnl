(local {: autocmd!} (require :utils))

;; Basically excr's implementation on .fnl files
(let [{: eval-string} (require :hotpot.api.eval)
      files (vim.fs.find ".nvim.fnl"
                         {:type :file
                          :upward true
                          :limit math.huge
                          :path (vim.uv.cwd)})]
  (each [_ file (ipairs files) &until (not vim.o.exrc)]
    (case (vim.secure.read file)
      nil nil
      content (eval-string content))))

(autocmd! {:event :BufWritePost
           :pattern ".nvim.{lua,fnl}"
           :callback (fn [{: buf : file}]
                       (let [action (case (vim.fn.confirm (string.format "Add '%s' to trust database? "
                                                                         file)
                                                          "&Yes\n&No\n&Remove" 2
                                                          :Q)
                                      0 nil
                                      1 :allow
                                      2 :deny
                                      3 :remove)]
                         (when action
                           (vim.secure.trust {: action :bufnr buf}))))})
