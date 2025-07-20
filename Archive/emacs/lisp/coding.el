;; -*- lexical-binding: t; -*-

(require 'common)
(require 'modality)

(use-package treesit
    :ensure nil
    :straight nil
    :config
    (defun -treesit-hook ()
      (let ((settings (treesit-font-lock-rules
                       :language 'cpp
                       :override t
                       :feature 'namespace
                       '((namespace_identifier) @font-lock-namespace-face))))
        (setq treesit-font-lock-settings (append treesit-font-lock-settings settings)
              treesit-font-lock-level 4))
      ;; (push 'namespace (nth 3 treesit-font-lock-feature-list))
      ;; (treesit-font-lock-recompute-features '(namespace))
      )
    (add-hook 'c++-ts-mode-hook #'-treesit-hook)
    (add-hook 'rust-ts-mode-hook #'-treesit-hook)
    )

;; Automatically install missing ts-grammars and use -ts-modes
(use-package treesit-auto
    ;; :custom
    ;; (treesit-auto-install 'prompt)
    :straight t
    :config
    ;; Wants to register builtin rust-ts-mode, conflicting with rust-mode above which already uses ts
    (treesit-auto-add-to-auto-mode-alist)
    (setq auto-mode-alist (cl-delete-if
                           (lambda (el)
                             (eq (cdr el) 'rust-ts-mode))
                           auto-mode-alist))
    (global-treesit-auto-mode)
    )

;; Builtin LSP server
(use-package eglot
    :straight t
    :demand t
    :hook
    (haskell-ts-mode . eglot-ensure)
    (rust-ts-mode . eglot-ensure)
    (python-ts-mode . eglot-ensure)
    :config

    (dolist (el '((haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp"))
                  (haskell-mode . ("haskell-language-server-wrapper" "--lsp"))
                  ))
      (add-to-list 'eglot-server-programs el))
    
    (setq-default
     eglot-workspace-configuration
     '(:haskell (
                 ;; :plugin
                 ;; (:stan (:globalOn t))
                 :formattingProvider "ormolu")))
    
    ;; Fixes performance problems for chatty servers
    (fset #'jsonrpc--log-event #'ignore)
    :custom
    (eglot-events-buffer-size 0)
    (eglot-report-progress nil)
    (eglot-autoshutdown t)
    (eglot-confirm-server-initiated-edits nil)
    (eglot-code-action-indicator ""))

(use-package dape
    :straight t
    ;; :hook
    ;; (kill-emacs . dape-breakpoint-save)
    ;; (after-init . dape-breakpoint-load)
    :config

    
    )

;; (use-package minimap)

(use-package editorconfig
    :straight nil
    :hook
    prog-mode)

(use-package magit)

;; (use-package eglot-booster
;;   :straight (:host github :repo "jdtsmith/eglot-booster")
;;   :after eglot
;;   :config (eglot-booster-mode 1))

(use-package eglot-hierarchy
  :straight (:host github :repo "dolmens/eglot-hierarchy"))

(use-package yaml-mode)

(defun use-default-forward-sexp ()
  (setq-local forward-sexp-function nil))

(use-package haskell-ts-mode
    :hook
  (haskell-ts-mode . use-default-forward-sexp)
  :config
  ;; (add-hook 'haskell-ts-mode-hook 'use-default-forward-sexp)
  (setq haskell-ts-use-indent t)

  ;; Overwrite as usage of stack's ghci is otherwise not configurable
  (defun run-haskell ()
    "Run an inferior Haskell process."
    (interactive)
    (let ((buffer (concat "*" haskell-ts-ghci-buffer-name "*")))
      (pop-to-buffer-same-window
       (if (comint-check-proc buffer)
           buffer
         (let ((buf (make-comint haskell-ts-ghci-buffer-name "stack" nil "ghci" buffer-file-name)))
           (with-current-buffer buf
             (setq-local comint-prompt-regexp "λ ❯ "
                         comint-prompt-read-only t))
           buf)))))


  )

(use-package consult-hoogle
    :config
  (require 'hoogle-buffer)
  (map haskell-ts-mode-map
    `(("C-c C-S-s" . hoogle-buffer-project)
      ("C-c C-s" . consult-hoogle-project)))
  :custom
  (hoogle-base-project-args '("stack" "hoogle" "--"
                              "--jsonl" "-q" "--count=250")))

;; See https://github.com/rust-lang/rust-mode/issues/541
(use-package rust-mode
    :straight (:build (:not autoloads))
    :config
    (setq rust-mode-treesitter-derive t)
    (autoload 'rust-mode "rust-mode" nil t)
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))



;; Eldoc config
(setq eldoc-display-functions (remove #'eldoc-display-in-echo-area eldoc-display-functions)
      eldoc-echo-area-prefer-doc-buffer t
      )
(remove-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)

(use-package eldoc-box
  :config
  (set-face-attribute 'eldoc-box-border nil :background (base16-get :bg3))
  (set-face-attribute 'eldoc-box-body   nil :background (base16-get :bg3))
  (defun -customize-eldoc-box (main-frame)
    (modify-frame-parameters nil `((internal-border-width . 5)
                                   ))
    )
  (add-hook 'eldoc-box-frame-hook #'-customize-eldoc-box)
  )


;; Flymake
(use-package flymake
  :straight nil
  :config
  (setq flymake-indicator-type 'margins))

;; (use-package flymake-posfrmae)


;; Toggling face of comments
;; FIXME: Breaks when evaluated as a defun as it seems to not bind `last' lexically then.
(let ((last `(:foreground ,(color-lighten-name (rgb-mix (base16-get :fg1) (base16-get :green) 0.3) 0.2))))
  (defun toggle-comments ()
    (interactive)
    (let ((curr (custom-face-attributes-get 'font-lock-comment-face nil)))
      (apply #'set-face-attribute 'font-lock-comment-face nil last)
      (setq last curr))))

(setq-default indent-tabs-mode nil
              tab-width 4)

(use-package graphql-ts-mode
    :mode ("\\.graphql\\'" "\\.gql\\'")
    :init
    (with-eval-after-load 'treesit
      (add-to-list 'treesit-language-source-alist
                   '(graphql "https://github.com/bkegley/tree-sitter-graphql"))))



(add-hook 'text-mode-hook (nlambda setup-text-mode ()
                            (setq left-margin-width 1
                                  right-margin-width 1
                                  line-prefix ""
                                  wrap-prefix "")))



(add-hook 'prog-mode-hook (nlambda setup-prog-mode ()
                            (let ((prefix ;; (propertize "" 'face `(:foreground ,(base16-get :bg1)))
                                   ""
                                   ))
                              (setq line-prefix prefix
                                    wrap-prefix prefix))
                            ;; (keymap-local-set "<remap> <newline>" #'comment-indent-new-line)
                            ))

(use-package aggressive-indent
    :hook
  emacs-lisp-mode)

;; TODO: document what this does again
(use-package elisp-slime-nav
    :config
  (require 'elisp-slime-nav)
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(add-hook 'emacs-lisp-mode-hook
          (nlambda setup-elisp-mode ()
            (setq lisp-indent-function 'common-lisp-indent-function)))

;; (use-package erefactor)


;; Experiment: let each window have its own paren-overlay
;; Likely not that straightforward as vars are buffer-local, not window-local.
;; (with-eval-after-load 'paren
;;   (make-variable-buffer-local 'show-paren--overlay)
;;   (make-variable-buffer-local 'show-paren--overlay-1)

;;   (advice-add 'show-paren-function
;;               :after (nlambda show-parens-only-in-window (&rest _)
;;                        (let ((win (selected-window)))
;;                          (dolist (overlay `(,show-paren--overlay ,show-paren--overlay-1))
;;                            (when (overlayp overlay)
;;                              (overlay-put overlay 'window win))))))

;;   (add-hook 'show-paren-mode-hook (nlambda localize-paren-overlays (&rest _)
;;                                     (setq show-paren--overlay show-paren--overlay
;;                                           show-paren--overlay-1 show-paren--overlay-1)))

;;   )


(with-eval-after-load 'meow
  (advice-add #'meow--edebug-hook-function
              :override
              (lambda ()
                (if (bound-and-true-p edebug-mode)
                    (meow--switch-state 'insert)
                  (meow--switch-to-normal)))))

;; Python

(use-package pet
    :config
  (defun pet-setup ()
    (pet-mode)
    (pet-eglot-setup)
    (add-to-list 'eglot-server-programs `((python-mode python-ts-mode)
                                          ,(pet-executable-find "basedpyright-langserver") "--stdio"))
    )
  (add-hook 'python-base-mode-hook #'pet-setup -10))

(provide 'coding)
