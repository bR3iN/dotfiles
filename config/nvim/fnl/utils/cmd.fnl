(local M {})

(fn M.toggle-loclist []
  (if (not= 0 (. (vim.fn.getloclist 0 {:winid 0}) :winid))
      (vim.cmd.lclose)
      (vim.cmd.lopen)))

(fn M.toggle-quickfix []
  (if (not= 0 (. (vim.fn.getqflist {:winid 0}) :winid))
      (vim.cmd.cclose)
      (vim.cmd.copen)))

M
