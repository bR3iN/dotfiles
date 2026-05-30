(local M {})

(fn M.setup-buf [buf]
  (let [{: start} (require :vim.treesitter)
        {: get_lang : add} (require :vim.treesitter.language)
        ft (. vim.bo buf :filetype)
        lang (get_lang ft)]
    (when (and lang (add lang))
      (when (not= vim.wo.foldmethod :expr)
        (set vim.wo.foldmethod :expr)
        (set vim.wo.foldexpr "v:lua.vim.treesitter.foldexpr()"))
      ;; (set (. vim.bo buf :indentexpr)
              ;;      "v:lua.require'nvmi-treesitter'.indentexpr()")
      ;; Enable treesitter
      (start buf lang)))
  )

M
