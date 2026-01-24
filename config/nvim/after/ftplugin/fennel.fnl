(local {: buf-opts! : buf-keymaps!} (require :utils))
(import-macros {: set-locally : with-saved : with-saved-view} :utils.macros)

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
                     "<Plug>edit#format-file" #(with-saved-view (vim.cmd "silent! %! fnlfmt -"))}}))
