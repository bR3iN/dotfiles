(use-package yaml-mode)

(use-package haskell-ts-mode
  :config
  (add-hook 'haskell-ts-mode-hook 'prettify-symbols-mode)
  )

;; See https://github.com/rust-lang/rust-mode/issues/541
(use-package rust-mode
  :straight (:build (:not autoloads))
  :init
  (setq rust-mode-treesitter-derive t)
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(with-eval-after-load 'eglot
  (defvar eglot-server-programs)
  (add-to-list 'eglot-server-programs
  	           '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp"))))

(provide 'coding)
