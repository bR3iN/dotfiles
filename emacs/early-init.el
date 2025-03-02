; Temporarily disable garbage collection for faster startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold 800000)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq package-enable-at-startup nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'default-frame-alist '(left-fringe . 0))
(add-to-list 'default-frame-alist '(right-fringe . 0))
(add-to-list 'default-frame-alist '(internal-border-width . 15))
(add-to-list 'default-frame-alist '(alpha-background . 9))
