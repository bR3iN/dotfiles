(local {: buf-opts! : keymaps!} (require :utils))
(import-macros {: with-saved-view} :utils.macros)

(buf-opts! {:commentstring ";; %s" :formatprg "fnlfmt -" :iskeyword- "."})

(keymaps! {:opts {:buffer true}
           :n {:<Plug>edit#format-file #(with-saved-view (vim.cmd "silent! %! fnlfmt -"))}})
