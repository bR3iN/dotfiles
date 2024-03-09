(let [{: find_files} (require :telescope.builtin)
      {: cache-prefix} (require :hotpot.api.cache)]
  ; Search in cache
  (vim.keymap.set :n "<leader>fc"
                  #(find_files
                     {:cwd (cache-prefix)
                      :hidden true})
                  {:buffer true
                   :silent true})
  ; Evaluate file
  (vim.keymap.set :n "<Plug>RunFile"
                  #(vim.cmd.Fnlfile "%")
                  {:buffer true
                   :silent true}))

