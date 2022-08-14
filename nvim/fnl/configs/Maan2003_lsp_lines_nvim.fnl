(let [{: setup} (require :lsp_lines)]
  (setup))

; Disable virtual_text since it's redundant due to `lsp_lines.nvim`
(vim.diagnostic.config {:virtual_text false})
