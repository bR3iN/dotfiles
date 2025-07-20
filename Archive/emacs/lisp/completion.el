;; -*- lexical-binding: t; -*-

(require 'common)
;; (eval-when-compile
;;   (require 'straight)

;;   (setq straight-use-package-by-default t
;;         ;; Workaround for https://github.com/radian-software/straight.el/issues/1022#issuecomment-2614489883
;;         straight-cache-autoloads nil
;;         ;; Also load packages if they have e.g. :hook declarations
;;         use-package-always-demand t))

(setopt text-mode-ispell-word-completion nil)

(use-package corfu
    ;; :hook ((corfu-mode . corfu-popupinfo-mode))
    :bind (:map corfu-map
                ("<remap> <move-end-of-line>" . corfu-quit)
                ("<remap> <next-line-or-history-element>" . corfu-next)
                ("<remap> <previous-line-or-history-element>" . corfu-previous))
    :config
    (setq
     corfu-auto t
     corfu-quit-no-match 'separator
     corfu-auto-prefix 1
     corfu-auto-delay 0.05
     corfu-bar-width 0.5
     global-corfu-minibuffer t
     ;; FIXME: figure out how to configure its background (seemingly fringe font) without defining it globally
     corfu-right-margin-width 0
     corfu-left-margin-width 0
     corfu-cycle t
     corfu-popupinfo-delay '(0.2 . 0.1)
     corfu-popupinfo-hide nil
     )
    

    (set-face-attribute 'corfu-default nil :background (base16-get :bg1))
    (set-face-attribute 'corfu-current nil :background (base16-get :bg2) :foreground (base16-get :cyan))
    (set-face-attribute 'corfu-border nil :background (base16-get :bg3))
    
    (set-face-attribute 'corfu-bar nil :background (base16-get :fg0))
    
    (keymap-unset corfu-map "RET")
    (keymap-set corfu-map "C-l" 'corfu-insert)

    (corfu-popupinfo-mode)
    (global-corfu-mode)
    
    (setf
     
     (alist-get 'line-prefix corfu--buffer-parameters) ""
     (alist-get 'wrap-prefix corfu--buffer-parameters) ""
     (alist-get 'line-prefix corfu-popupinfo--buffer-parameters) ""
     (alist-get 'wrap-prefix corfu-popupinfo--buffer-parameters) ""
     
     (alist-get 'child-frame-border-width corfu--frame-parameters) 2
     (alist-get 'internal-border-width corfu--frame-parameters) 2
     ;; (alist-get 'outer-border-width corfu--frame-parameters) 0
     ;; (alist-get 'border-width corfu--frame-parameters) 0
     
     )
    
    (defvar -corfu-preview-buffer "*Help-Preview*")

    (advice-add
     'elisp--company-doc-buffer
     :around (lambda (orig &rest args)
               (cl-letf (((symbol-function 'help-buffer)
                          (lambda (&rest _)
                            (get-buffer-create -corfu-preview-buffer))))
                 (apply orig args))))

    (advice-add
     'corfu--done
     :after (lambda (&rest _)
              (when (get-buffer -corfu-preview-buffer)
                (kill-buffer -corfu-preview-buffer))))


    ;; (corfu-next)
    ;; (corfu-sen)

    ;; (def)
    )


;; (use-package nerd-icons-corfu
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


(require 'cl-lib)

;; (use-package company
;;   :config
;;   (global-company-mode 1)
;;   (setq company-echo-delay 99
;;         company-tooltip-idle-delay 99
;;         company-idle-delay 0.0

;;         company-minimum-prefix-length 2
;;         company-selection-wrap-around t
;;         ;;company-insertion-on-trigger
;; 	;; Disables company
;; 	; icons-format-margin-function nil
;;         )


;;   ;; (set-face-attribute 'company-tooltip nil :background (base16-get :bg2))

;;   (keymap-unset company-active-map "<return>")
;;   (keymap-unset company-active-map "RET")
;;   ;; (keymap-unset company-active-map "C-m")
;;   (keymap-unset company-active-map "C-w")
;;   (keymap-set company-active-map "C-l" 'company-complete-selection)
;;   (keymap-set company-active-map "C-e" 'company-abort)
;;   (keymap-set company-active-map "<tab>" 'company-complete-common)
;;   (keymap-set company-active-map "C-i" 'company-show-location)
;;   )

;; ;; FIXME have cmds scroll the help if ilable (or just when in insert mode?)
;; (use-package company-posframe
;;   :hook (company-mode . company-posframe-mode)
;;   :config
;;   (setq company-posframe-show-params           '(:internal-border-width 10)
;;         company-posframe-quickhelp-show-params (append '(:internal-border-width 10)
;;                                                        company-posframe-quickhelp-show-params)
;;         company-posframe-quickhelp-show-header nil
;;         company-posframe-quickhelp-delay 0.1
;;         company-posframe-quickhelp-x-offset 10
;;         )
;;   (set-face-attribute 'company-posframe-quickhelp nil
;;                       :background (base16-get :bg3))
;;   )


;; (use-package company-box
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-doc-delay 0.5))


(use-package cape
    :config
  ;; Get updated completion table from server during completion, instead of only once at the beginning
  (advice-add 'eglot-completion-at-point
              :around #'cape-wrap-buster)
  (add-hook 'completion-at-point-functions #'cape-file))

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
				                   (command (styles initials basic))))

  :config

  ;; Dynamic overrides for separator by mode or predicate
  (defvar orderless-separator-overrides
    '((lisp-data-mode . "[ -]")))
  
  (when t
    ;; Better integration with company
    (defun -split-with-custom-separator (string)
      (if-let ((sep (cl-some (lambda (el)
                               (let ((pred (car el))
                                     (sep (cdr el)))
                                 (when (cond
                                         ((symbolp pred) (derived-mode-p pred))
                                         ((functionp pred) (funcall pred))
                                         (t (error (format "Bad predicate: %s" pred))))
                                   sep)))
                             orderless-separator-overrides)
                    ))
          (cond
            ((stringp sep) (split-string string sep))
            ((functionp sep) (funcall sep string))
            (t (error (format "Bad separator: %s" sep))))
        (orderless-escapable-split-on-space string)))

    (setq orderless-component-separator #'-split-with-custom-separator)


    ;; Fix match highlighting
    (advice-add
     'company-capf--candidates
     :around (defun -capf-candidates (fn &rest args)
               (let ((orderless-match-faces [completions-common-part]))
                 (apply fn args))))
    ))



(use-package marginalia
    :config
  (marginalia-mode 1))

;; (use-package vertico-posframe
;;   :after vertico
;;   :config
;;   (vertico-posframe-mode -1)
;; )

(use-package vertico
    :config
  (vertico-mode 1)
  ;; (vertico-reverse-mode 1)
  (setq vertico-cycle t
        vertico-preselect 'first)
  (keymap-set vertico-map "<remap> <minibuffer-complete>" 'vertico-insert)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Use <tab> instead of TAB to not conflict with C-i bindings
  (keymap-set vertico-map "<tab>" 'vertico-insert)
  (keymap-unset vertico-map "TAB")
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  )


(use-package consult
    :custom-face
  ;; Presumably defined by haskell-mode
  (haskell-keyword-face ((t (:inherit font-lock-keyword-face))))
  (haskell-quasi-quote-face ((t (:inherit font-lock-string-face))))
  :config
  (setq
   ;; To use next-error-buffer, use embark -> [E]xport
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref
   consult-async-min-input 2)
  (recentf-mode))

(use-package yasnippet
    :config
  (yas-global-mode)
  ;; (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; (add-hook 'text-mode-hook 'yas-minor-mode)
  )


(defun corfu-active-p ()
  (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))
;; (keymap-set minibuffer-local-map "C-j" "M-<down>")
;; (keymap-set read--expression-map "C-j" "M-<down>")
;; (keymap-set minibuffer-local-map "C-k" "M-<up>")
(keymap-set minibuffer-local-map "C-d" "C-M-V")
(keymap-set minibuffer-local-map "C-u" "C-M-S-v")

(keymap-set minibuffer-local-map "C-SPC" #'completion-at-point)
(keymap-set minibuffer-local-map "C-p" #'previous-line-or-history-element)
(keymap-set minibuffer-local-map "C-n" #'next-line-or-history-element)

(keymap-set minibuffer-local-map "C-l" 'minibuffer-complete)
(keymap-set minibuffer-local-map "C-;" 'minibuffer-complete-and-exit)


(use-package embark
    :after embark-consult ; prevents warning
    :bind
    (
     ;; ("C-i" . embark-act)         ;; pick some comfortable binding
     ;; ("M-i" . embark-dwim)        ;; good alternative: M-.
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
    (keymap-set embark-buffer-map "d" (nlambda embark-display-buffer (buffer)
                                        (display-buffer buffer)))
    
    ;; Hide the mode line of the Embark live/completions buffers
    ;; (add-to-list 'display-buffer-alist
    ;;              '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
    ;;                nil
    ;;                (window-parameters (mode-line-format . none))))
    )

(use-package embark-consult
    :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(provide 'completion)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "completion--"))
;; End:
