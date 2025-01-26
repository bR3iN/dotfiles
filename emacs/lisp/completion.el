;; -*- lexical-binding: t; -*-

; TODO: find out why corfu feels more sluggish than company with eglot completion
; (use-package corfu
;   :config
;   (global-corfu-mode 1)
;   (setq
;     ; corfu-auto t
; 	; corfu-quit-no-match 'separator
; 	; corfu-auto-prefix 3
;     ; corfu-auto-delay 0.1
; 	; corfu-cycle t
;     )
;   (keymap-unset corfu-map "RET")
;   (keymap-set corfu-map "C-l" 'corfu-insert))

(use-package company
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        ;;company-insertion-on-trigger
	;; Disables company
	; icons-format-margin-function nil
        )
  (keymap-unset company-active-map "<return>")
  (keymap-unset company-active-map "RET")
  ;; (keymap-unset company-active-map "C-m")
  (keymap-unset company-active-map "C-w")
  (keymap-set company-active-map "C-l" 'company-complete-selection)
  (keymap-set company-active-map "<tab>" 'company-complete-common)
  (keymap-set company-active-map "C-i" 'company-show-location)
  )

; (use-package cape
;   :config
;   ;; Get updated completion table from server during completion, instead of only once at the beginning
;   (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))
				   (command (styles initials basic)))))


(use-package marginalia
  :init
  (marginalia-mode 1))

;; (use-package vertico-posframe
;;   :after vertico
;;   :config
;;   (vertico-posframe-mode -1)
;; )

(use-package vertico
  :init
  (vertico-mode 1)
  ;; (vertico-reverse-mode 1)
  (setq vertico-cycle t
        vertico-preselect 'first)
  (keymap-set vertico-map "<remap> <minibuffer-complete>" 'vertico-insert)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; (keymap-set vertico-map "RET" 'vertico-exit-input)
  (setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
  )


(use-package consult
             :config
             (setq
               ; xref-show-xrefs-function #'consult-xref
               ; xref-show-definitions-function #'consult-xref
               consult-async-min-input 2
               )
             (recentf-mode))

(use-package yasnippet
  :config
  (yas-global-mode)
  ;; (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; (add-hook 'text-mode-hook 'yas-minor-mode)
  )

(keymap-set minibuffer-local-map "C-j" "M-<down>")
(keymap-set read--expression-map "C-j" "M-<down>")
(keymap-set minibuffer-local-map "C-k" "M-<up>")
(keymap-set minibuffer-local-map "C-d" "C-M-v")
(keymap-set minibuffer-local-map "C-u" "C-M-S-v")
(keymap-set minibuffer-local-map "C-p" 'previous-line-or-history-element)
(keymap-set minibuffer-local-map "C-n" 'next-line-or-history-element)
(keymap-set minibuffer-local-map "C-l" 'minibuffer-complete)
(keymap-set minibuffer-local-map "C-;" 'minibuffer-complete-and-exit)

(use-package embark
  :ensure t

  :bind
  (("C-a" . embark-act)         ;; pick some comfortable binding
   ("M-a" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(provide 'completion)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "completion--"))
;; End:
