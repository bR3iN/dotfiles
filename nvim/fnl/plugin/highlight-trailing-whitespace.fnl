(local {: augroup!} (require :utils.nvim))

(let [autocmd! (augroup! :hightlight-traling-whitespace)]
  (autocmd! :InsertLeave "*" #(vim.cmd "match TrailingWhitespace /\\v\\s+$/"))
  (autocmd! :InsertEnter "*" #(vim.fn.clearmatches)))

(vim.api.nvim_set_hl 0 :TrailingWhitespace {:ctermfg 16 :ctermbg 16})
