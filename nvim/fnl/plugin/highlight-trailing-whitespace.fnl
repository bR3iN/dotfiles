(local {: augroup!} (require :utils.nvim))

(fn hl []
  (vim.cmd "match TrailingWhitespace /\\v\\s+$/"))

(fn nohl []
  (vim.fn.clearmatches))

(let [autocmd! (augroup! :highlight-traling-whitespace)]
  (autocmd! :BufEnter "*" hl)
  (autocmd! :InsertLeave "*" hl)
  (autocmd! :InsertEnter "*" nohl))
