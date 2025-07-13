(local {: autocmd!} (require :utils))

(fn hl []
  (vim.cmd "match TrailingWhitespace /\\v\\s+$/"))

(fn nohl []
  (vim.fn.clearmatches))

(autocmd! :highlight-traling-whitespace
          [{:event :BufEnter :pattern "*" :callback hl}
           {:event :InsertLeave :pattern "*" :callback hl}
           {:event :InsertEnter :pattern "*" :callback nohl}])
