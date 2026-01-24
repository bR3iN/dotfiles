(local {: buf-opts! : buf-keymaps!} (require :utils))

(buf-opts! {:commentstring ";; %s" :formatprg "fnlfmt -"})

(let [{: find_files} (require :telescope.builtin)
      {: cache-prefix} (require :hotpot.api.cache)
      {: eval-file} (require :hotpot.api.eval)]
  (buf-opts! {:iskeyword- "."})
  (buf-keymaps! {:n {"<localleader>" {"r" {:desc "Run"
                                           :callback #(eval-file (vim.fn.expand "%"))}
                                      "c" {:desc "Search cache"
                                           :callback #(find_files {:cwd (cache-prefix)
                                                                   :hidden true})}}
                     "<Plug>edit#format-file" "gggqG<C-o><C-o>"}}))
