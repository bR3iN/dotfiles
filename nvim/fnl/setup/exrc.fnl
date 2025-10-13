(local {: eval-string} (require :hotpot.api.eval))

(let [files (vim.fs.find ".nvim.fnl"
                         {:type :file
                          :upward true
                          :limit math.huge
                          :path (vim.uv.cwd)})]
  (each [_ file (ipairs files) &until (not vim.o.exrc)]
    (case (vim.secure.read file)
      nil nil
      content (eval-string content))))
