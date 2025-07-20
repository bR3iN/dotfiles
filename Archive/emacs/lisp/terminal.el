;; -*- lexical-binding: t; -*-

(require 'common)

(use-package eat
    :straight (:type git
                     :host codeberg
                     :repo "akib/emacs-eat"
                     :files ("*.el" ("term" "term/*.el") "*.texi"
                                    "*.ti" ("terminfo/e" "terminfo/e/*")
                                    ("terminfo/65" "terminfo/65/*")
                                    ("integration" "integration/*")
                                    (:exclude ".dir-locals.el" "*-s.el")))
    :custom
    (eat-shell "/usr/bin/fish"))

;;(require 'eat)
;; (add-hook 'eat-exec-hook (lambda (&rest_) (eat-char-mode) (insert-cmd)))

(use-package vterm
  :custom
  (vterm-shell "/usr/bin/fish")
  :config
  )

;; (use-package vterm)

(provide 'terminal)
