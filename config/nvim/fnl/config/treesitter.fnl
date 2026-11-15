(local M {})

(fn M.setup-buf [buf]
  (let [{: start : query} (require :vim.treesitter)
        {: get_lang : add} (require :vim.treesitter.language)
        ft (. vim.bo buf :filetype)
        lang (get_lang ft)]
    (when (and lang
               ;; See if a parser (.so) is present.
               (add lang)
               ;; Only start if highlight rules exist; guards against grammars which support was
               ;; dropped (e.g. tmux), for which a locally compiled parser might still exist.
               (query.get lang :highlights))
      (when (not= vim.wo.foldmethod :expr)
        (set vim.wo.foldmethod :expr)
        (set vim.wo.foldexpr "v:lua.vim.treesitter.foldexpr()"))
      ;; (set (. vim.bo buf :indentexpr)
              ;;      "v:lua.require'nvmi-treesitter'.indentexpr()")
      ;; Enable treesitter
      (start buf lang)))
  )

M
