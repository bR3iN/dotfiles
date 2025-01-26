;; -*- lexical-binding: t; -*-

;; Bootstrap straight.el and activate use-package integration and autoinstallation
(require 'bootstrap/straight)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package org
  :config
  (setq org-startup-truncated nil))

(defvar rerequire-config t)

(defun rerequire (feature &rest rest)
  (when (featurep feature)
    (unload-feature feature t))
  (apply #'require feature rest))

(require 'cl-lib)
(rerequire 'utils)
(rerequire 'protect-windows)
(rerequire 'dedicated-windows)
(rerequire 'color-utils)
(rerequire 'base16-colors)
(rerequire 'appearance)
(rerequire 'traces)
(rerequire 'ts-nav)
(rerequire 'thing-defs)
(rerequire 'thing-nav)
(rerequire 'restore-region)
(rerequire 'scroll-mode)
(rerequire 'completion)
(rerequire 'dot-repeat)
(rerequire 'zk-emacs)
(rerequire 'prog-buffers)
(rerequire 'winmax)
(rerequire 'up-and-down)
(rerequire 'dymap)
;;(rerequire 'auto-mark-mode)


(defun -unmaximize-window-advice (&rest _)
  (winmax-restore))

(dolist (sym '(;; select-window
               mouse-set-point
               split-window
               windmove-up
               windmove-down
               windmove-right
               windmove-left
               ))
  (advice-add sym :before #'-unmaximize-window-advice))

;; TODO: try out spacious-padding mode

(defun keymap-multiset (map binding-alist)
  (pcase-dolist (`(,key . ,cmd) binding-alist)
    (keymap-set map key cmd)))

(defvar -windmove-bindings '(("M-h" . windmove-left)
                             ("M-j" . windmove-down)
                             ("M-k" . windmove-up)
                             ("M-l" . windmove-right)))

(keymap-multiset global-map -windmove-bindings)


;; TODO: desktop-save asks for custom path
(use-package desktop
  :config
  (setq desktop-path '("~/.local/share/emacs/desktop")))

;; Wrap at word boundary and fall back to truncatation in small windows
(setq-default
 word-wrap t
 ;; truncate-lines t
 truncate-partial-width-windows window-min-width
 ;; TODO: Nice to have a space here, but company-mode has a problem with this.
 line-prefix nil
 wrap-prefix nil
 )

(use-package eat
  :straight (:type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el"))))
(require 'eat)
(add-hook 'eat-exec-hook (lambda (&rest_) (eat-char-mode)))

;; ;; TODO try out this (mode) together with visual line navigation (thing)
;; (use-package adaptive-wrap
;;   :config
;;   (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
;;   (global-visual-line-mode))


;; Have help links replace the help buffer in its window
(setq help-window-keep-selected t)

(defun smart-undo (&optional n)
  "undo-tree-undo for positive numeric prefixes, undo-tree-redo for negative ones."
  (interactive "*p")
  (let ((n (or n 1)))
    (if (< n 0) (undo-tree-redo (- n))
      (undo-tree-undo n))))


(defun reload-config ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun starting-dot-repeat (cmd)
  (adviced cmd
           :what "Starting `dot-repeat`."
           :before (lambda (&rest _)
                     (start-dot-repeat))))





;; Treat "C-[" the same as escape instead of an alternative "M-" prefix.
(key-translate "C-[" "<escape>")

;; Useful to see default keybindings; remove after stabilizing keybindings.
(use-package which-key
  :commands which-key-mode)

(let ((-last-fg (color-lighten-name (rgb-mix (base16-get :fg1) (base16-get :green) 0.3) 0.2))
      (-last-weight 'bold))
  (defun toggle-comments ()
    (interactive)
    (let ((curr-fg (face-attribute 'font-lock-comment-face :foreground nil t))
          (curr-weight (face-attribute 'font-lock-comment-face :weight nil t))
          (new-fg -last-fg)
          (new-weigth -last-weight))
      (setq -last-fg curr-fg
            -last-weight curr-weight)
      (set-face-attribute 'font-lock-comment-face nil :foreground new-fg :weight new-weigth))))

(setq-default indent-tabs-mode nil
              tab-width 4)

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

(global-auto-revert-mode t)

;; Going up and down

(cl-flet ((set-mark (lambda (pos)
                      (if (region-active-p)
                          (set-mark pos)
                        (push-mark pos t t))))
          (go-up (lambda (n &optional callback)
                   (sp-up-sexp n t)
                   (when callback
                     (funcall callback
                              (save-excursion
                                (sp-forward-sexp (- (sign n)))
                                (point))))))
          (go-down (lambda (n &optional callback)
                     (sp-down-sexp n)
                     (when callback
                       (funcall callback
                                (save-excursion
                                  (sp-up-sexp (sign n) t)
                                  (sp-down-sexp (- (sign n)))
                                  (point)))))))

  (defun down-cmd (&optional n)
    (interactive "p")
    (sp-down-sexp (or n 1)))

  (defun up-cmd (&optional n)
    (interactive "p")
    (sp-up-sexp (or n 1) t))

  (defun end-of-inner (&optional n)
    (interactive "p")
    (if (< n 0)
        (progn
          (up-cmd n)
          (sp-backward-sexp)
          (down-cmd))
      (up-cmd n)
      (down-cmd -1)))

  (defun down-traced (&optional n)
    (interactive "p")
    (go-down (or n 1) #'request-cmd-trace))

  (defun up-traced (&optional n)
    (interactive "p")
    (go-up (or n 1) #'request-cmd-trace))

  (defun down-marked (&optional n)
    (interactive "p")
    (go-down (or n 1) #'set-mark))

  (defun up-marked (&optional n)
    (interactive "p")
    (go-up (or n 1) #'set-mark))

  (defun mark-inner (&optional n)
    (interactive "p")
    (let ((n (or n 1)))
      (sp-up-sexp n t)
      (go-down (- (sign n)) #'set-mark))))


(setq enable-recursive-minibuffers t)
(minibuffer-electric-default-mode t)


(use-package hydra
  :config
  (setq hydra-is-helpful nil)
  )


;; See https://github.com/rust-lang/rust-mode/issues/541
(use-package rust-mode
  :straight (:build (:not autoloads))
  :init
  (setq rust-mode-treesitter-derive t)
  (autoload 'rust-mode "rust-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))


(setq treesit-font-lock-level 4)

;; Automatically install missing ts-grammars and use -ts-modes
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Wants to register builtin rust-ts-mode, conflicting with rust-mode above which already uses ts
  ;; (treesit-auto-add-to-auto-mode-alist t)
                                        ;; (global-treesit-auto-mode)
  )

(keymap-global-set "M-/" 'xref-find-references)
(keymap-global-set "M-s" 'isearch-forward-regexp)
(keymap-global-set "M-r" 'isearch-backward-regexp)


;; TODO: document what this does again
(use-package elisp-slime-nav
  :config
  (require 'elisp-slime-nav)
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'elisp-slime-nav-mode)))

(ido-mode -1) ;; TODO: needed?
(keymap-set global-map "C-w" 'backward-kill-word)

;; TODO: check if needed, possibly generalize scroll-mode for other-window?
(setq help-window-select t)

;; (setq delete-pair-blink-delay nil)

(setq isearch-wrap-pause 'no-ding)
(setq isearch-repeat-on-direction-change t)
(keymap-set isearch-mode-map "C-n" #'isearch-ring-advance)
(keymap-set isearch-mode-map "C-p" #'isearch-ring-retreat)
(keymap-set isearch-mode-map "<backspace>" #'isearch-del-char)
(keymap-set isearch-mode-map "C-l" "RET")
(keymap-set minibuffer-local-isearch-map "C-f" #'isearch-repeat-forward)
(keymap-set minibuffer-local-isearch-map "C-b" #'isearch-repeat-backward)

;; Recenter if search moved off-screen
(add-hook 'isearch-update-post-hook (lambda ()
				                      (when (not (pos-visible-in-window-p))
					                    (recenter))))





;; (keymap-global-set "C-]" "RET")

;; (use-package dired
;;   :straight nil
;;   :hook (dired-mode . (lambda () (dired-omit-mode)))
;;   :config
;;   ; (defun dired-up ()
;;   ;   (interactive)
;;   ;   (if (eq major-mode 'dired-mode) (dired-up-directory)
;; 	  ; (when-let* ((current-file (buffer-file-name))
;;   ;              (current-dir (file-name-directory current-file)))
;;   ;             (dired current-dir))))
;;   ;; TODO: see if SPC x j is good enough
;;   (keymap-set dired-mode-map "_" #'dired-))

;; TODO: Could motion state be replaced with normal state and become more minimal to be more usable here?
(use-package dirvish
  :hook (dired-mode . (lambda () (dired-omit-mode)))
  :config
  (setq dirvish-hide-details nil
        dirvish-reuse-session nil)
  (dirvish-override-dired-mode))

(defun my-reset ()
  (interactive)
  ;; (set-current-thing 'word)
  (clear-current-trace)
  (deactivate-mark)
  (delete-overlay mouse-secondary-overlay))


(defvar-local last-state nil)

(defun -state-switch-hook (new-state)
  (pcase new-state
    ('normal (end-dot-repeat nil))
    ('insert (start-dot-repeat))
    ('visual (start-dot-repeat)))
  (setq last-state new-state))

(add-hook 'meow-switch-state-hook #'-state-switch-hook)


;; Traces yanks
(defun trace-region (&rest _)
  (trace-range (mark) (point)))

(advice-add 'yank :after #'trace-region)
(advice-add 'yank-pop :after #'trace-region)

(advice-add 'kill-ring-save
            :after (lambda (beg end &rest _)
                     (pulse-momentary-highlight-region beg end)))

;;Trace insertions
(defvar-local change-range nil)  ; Start + Offset
(defvar-local had-changes nil)

(defvar-local change-list '())
(defvar-local change-list-cursor (create-cursor change-list))

;; TODO: doesn't respect undos, not sure about feasability
(defun goto-last-change (n)
  (interactive "p")
  (let* ((last-change (cursor-get change-list-cursor)))
    (cursor-move n change-list-cursor)
    (goto-char last-change)))

(defun track-changes-to-highlight (start end del-len)
  (setq had-changes t)
  (when change-range
    (let* ((insert-len (- end start))
           (offset-delta (- insert-len del-len))
           (current-start (car change-range))
           (current-offset (cdr change-range)))
      (when (< start current-start)
        (setcar change-range start))
      (when (not (= offset-delta 0))
        ;; If the offset becomes negative, this is already reflected by the
        ;; change of the start above.
        (setcdr change-range (max 0 (+ current-offset offset-delta)))))))

(add-hook 'after-change-functions #'track-changes-to-highlight)

(defun trace-insertion-hook (new-state)
  (pcase new-state
    ('insert (setq change-range (cons (+ (point) 1) 0)))
    ('normal (when (and
                    (equal last-state 'insert)
                    ;; change-range
                    )
               ;; Trace changed region
               (let* ((start (car change-range))
                      (offset (cdr change-range)))
                 (when had-changes
                   (trace-range start (+ start offset))
                   (push (+ start offset) change-list)
                   (setq change-list-cursor (create-cursor change-list)))))
             (setq change-range nil
                   had-changes nil))))

(add-hook 'meow-switch-state-hook #'trace-insertion-hook -50)


(defun insert-at-pt (&optional n)
  ;; TODO: really needed? For region, oi would do this
  "Insert at point; with negative prefix argument, insert at the mark or start of the current trace, if available."
  (interactive "p")
  (when-let ((guard (< (or n 1) 0))
             (range (try-get-st)))
    (goto-char (car range)))
  (deactivate-mark)
  (meow-insert-mode))



(defun smart-yank (&optional n)
  (interactive "*p")
  (if (eq last-command 'yank) (funcall 'yank-pop n)
    (funcall 'yank n)))


(defun smart-replace (&optional n)
  "Yanks, replacing active traces or regions."
  (interactive "*p")
  (if (eq last-command 'yank) (funcall 'yank-pop n)
    (when-let* ((maybe-st (try-get-st))
                (start (car maybe-st))
                (end (cdr maybe-st)))
      (kill-region start end)
      (setq n (+ n 1)))
    (funcall 'yank n)))


(defun my-till (n target)
  (interactive (list (prefix-numeric-value current-prefix-arg) (string (read-char))))
  (let ((case-fold-search nil)
        (orig (point))
        (direction (/ n (abs n))))
    (forward-char direction)
    (search-forward target nil t n)
    (forward-char (- direction))))


(defun my-find (n target)
  (interactive (list (prefix-numeric-value current-prefix-arg) (string (read-char))))
  (let ((case-fold-search nil))
    (search-forward target nil t n)))


(setq repeat-message-function #'ignore)


(defun search-cmd (&optional n)
  (interactive "p")
  (isearch-mode (> n 0) nil nil nil)
  (set-current-thing 'search)
  (when (region-active-p)
    (deactivate-mark)
    (isearch-yank-string (buffer-substring-no-properties (region-beginning) (region-end)))))


(defun regexp-search-cmd (&optional n)
  (interactive "p")
  (set-current-thing 'regexp-search)
  (if (< n 0) (isearch-backward-regexp)
    (isearch-forward-regexp)))


(use-package meow
  :after (avy org-journal)
  :config

  ;; General configuration

  (alist-map
   (lambda (mode state)
     (setf (alist-get mode meow-mode-state-list) state))
   `((help-mode . motion)
     (eshell-mode . insert)
     (debugger-mode . insert)
     (dired-mode . transparent)
     (eat-mode . motion)))

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (setq meow-keypad-self-insert-undefined nil)
                                        ;; Prefix is bound in leader mode
  (setq meow-motion-remap-prefix "H-a")

  ;; TODO: decide how far to couple to meow as a framework
  (defmacro def-state (name-lit indicator desc &optional transparent)
    (let* ((name (symbol-name name-lit))
	       (keymap (intern (concat "meow-" name "-state-keymap"))))
      `(progn
         (setq ,keymap (make-keymap))
         (when (not ,transparent)
           (suppress-keymap ,keymap t))
         (meow-define-state ,name-lit
           ,desc
           :lighter ,(concat " [" indicator "]")
           :keymap ,keymap))))

  (def-state transparent "T" "Transparent")

  ;; Visual Mode; automatically entered and left when activating and deactivating region.
  (def-state visual "V" "State during active selection")

  ;; Keep track of last pre-visual mode and re-enter it when deactivating mark
  (defvar-local -pre-visual-state nil)

  (defun -enter-visual-state-hook ()
    (let ((state (meow--current-state)))
      (when (member state '(normal motion))
        (save-local-win-pos)
                                        ; (setq -pre-visual-point (point))
        (setq -pre-visual-state state)
        (clear-current-trace)
        (meow--switch-state 'visual))))

  (defun -leave-visual-state-hook ()
    (when (equal (meow--current-state) 'visual)
                                        ; (setq -pre-visual-point nil)
      (meow--switch-state -pre-visual-state)
      (setq -pre-visual-state nil)))

  (add-hook 'activate-mark-hook #'-enter-visual-state-hook)
  (add-hook 'deactivate-mark-hook #'-leave-visual-state-hook)

  (defun abort-visual ()
    (interactive)
    (deactivate-mark)
    (restore-local-win-pos))


  (defun entering-insert-state (cmd)
    (adviced cmd
             :what "Enters insert state."
             :after (lambda (&rest _) (meow-insert-mode))))


  (defun open-line-below ()
    (interactive)
    (deactivate-mark)
    (end-of-line)
    (newline-and-indent)
    (meow-insert-mode))

  (defun open-line-above ()
    (interactive)
    (deactivate-mark)
    (beginning-of-line)
    (if (bobp) (open-line 1)
      (forward-line -1)
      (end-of-line)
      (newline-and-indent))
    (meow-insert-mode))


  (defun insert-at-beginning-of-thing (thing &optional n)
    (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
    (unless (forward-to-nearest-thing-at-point thing (- n))
      (error "No thing found"))
    (meow-insert-mode))

  (defun insert-at-end-of-thing (thing &optional n)
    (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
    (unless (forward-to-nearest-thing-at-point thing n)
      (error "No thing found"))
    (meow-insert-mode))

  ;; Keybindings
  (let* ((digit-bindings 
          `(("-" . negative-argument)
            ("1" . digit-argument)
            ("2" . digit-argument)
            ("3" . digit-argument)
            ("4" . digit-argument)
            ("5" . digit-argument)
            ("6" . digit-argument)
            ("7" . digit-argument)
            ("8" . digit-argument)
            ("9" . digit-argument)
            ("0" . digit-argument)))
         
         (xref-bindings
          `(("M-." . ,(with-let ((xref-show-definitions-function #'consult-xref)
                                 (xref-prompt-for-identifier nil))
                                xref-find-definitions))

            ("M-/" . ,(with-let ((xref-show-xrefs-function #'consult-xref)
                                 (xref-prompt-for-identifier nil))
                                xref-find-references))

            ("g M-." . ,(with-let ((xref-auto-jump-to-first-definition t)
                                   (xref-prompt-for-identifier nil))
                                  xref-find-definitions))

            ("g M-/" . ,(with-let ((xref-auto-jump-to-first-xref t)
                                   (xref-prompt-for-identifier nil))
                                  xref-find-references))))

         (window-nav-keys
          '(;; Move window focus
            ("h" . windmove-left)
            ("j" . windmove-down)
            ("k" . windmove-up)
            ("l" . windmove-right)
            ("<left>" . windmove-left)
            ("<down>" . windmove-down)
            ("<up>" . windmove-up)
            ("<down>" . windmove-right)
            ;; Move windows
            ("H" . windmove-swap-states-left)
            ("J" . windmove-swap-states-down)
            ("K" . windmove-swap-states-up)
            ("L" . windmove-swap-states-right)
            ("S-<left>" . windmove-swap-states-left)
            ("S-<down>" . windmove-swap-states-down)
            ("S-<up>" . windmove-swap-states-up)
            ("S-<down>" . windmove-swap-states-right)
            ;; Split windows
            ("s" . my-split-window-below)
            ("v" . my-split-window-right)
            ;; Focus specific window
            ("w" . other-window)
            ("m" . switch-to-minibuffer)
            ("e" . switch-to-error-buffer)
            ;; Kill window
            ("q" . delete-window)
            ("<escape>" . ignore)))

         (thing-nav-basic
          `(("n" . next-thing)
            ("p" . previous-thing)
            ("f" . end-of-thing-cmd)
            ("b" . beginning-of-thing-cmd)
            ("m" . mark-thing)
            
            ("N" . ,(with-new-thing #'next-thing))
            ("P" . ,(with-new-thing #'previous-thing))
            ("F" . ,(with-new-thing #'end-of-thing-cmd))
            ("B" . ,(with-new-thing #'beginning-of-thing-cmd))
            ("M" . ,(with-new-thing #'mark-thing))
            
            ("C-n" . ,(with-tmp-thing #'next-thing))
            ("C-p" . ,(with-tmp-thing #'previous-thing))
            ("C-f" . ,(with-tmp-thing #'end-of-thing-cmd))
            ("C-b" . ,(with-tmp-thing #'beginning-of-thing-cmd))
            ("C-m" . ,(with-tmp-thing #'mark-thing))

            ("M-n" . sp-next-sexp)
            ("M-p" . sp-previous-sexp)
            ("M-f" . sp-forward-sexp)
            ("M-b" . sp-backward-sexp)
            ("M-d" . sp-down-sexp)
            ("M-u" . sp-up-sexp)))

         (thing-nav-tracing
          `(("n" . ,(traced-motion #'next-thing))
            ("p" . ,(traced-motion #'previous-thing))
            ("f" . ,(traced-motion #'end-of-thing-cmd))
            ("b" . ,(traced-motion #'beginning-of-thing-cmd))
            ("m" . mark-thing)

            ("N" . ,(with-new-thing (traced-motion #'next-thing)))
            ("P" . ,(with-new-thing (traced-motion #'previous-thing)))
            ("F" . ,(with-new-thing (traced-motion #'end-of-thing-cmd)))
            ("B" . ,(with-new-thing (traced-motion #'beginning-of-thing-cmd)))
            ("M" . ,(with-new-thing #'mark-thing))
            
            ("C-n" . ,(with-tmp-thing (traced-motion #'next-thing)))
            ("C-p" . ,(with-tmp-thing (traced-motion #'previous-thing)))
            ("C-f" . ,(with-tmp-thing (traced-motion #'end-of-thing-cmd)))
            ("C-b" . ,(with-tmp-thing (traced-motion #'beginning-of-thing-cmd)))
            
            ("M-n" . ,(traced-motion #'sp-next-sexp))
            ("M-p" . ,(traced-motion #'sp-previous-sexp))
            ("M-f" . ,(traced-motion #'sp-forward-sexp))
            ("M-b" . ,(traced-motion #'sp-backward-sexp))
            ("M-e" . end-of-inner)
            ("M-d" . down-traced)
            ("M-u" . up-traced)
            
            ("e" . ,(with-tmp-thing (act-on-tap-alt2 (lambda (_ end)
                                                       (push-mark nil t t)
                                                       (goto-char end)))))))

         (thing-nav-marking
          `(("M-i" . ,#'mark-inner)
            ("C-j" . ,(setting-current-thing 'sexp #'down-marked))
            ("C-k" . ,(setting-current-thing 'sexp #'up-marked))
            ("M-m" . ,(with-new-thing #'mark-thing 'sexp))
            ("C-j" . down-marked)
            ("C-k" . up-marked)))

         (basic-navigation
          `(;; Using arrow keys avoids conflicts with rebinded "C-n/p"
            ("h" . "<left>")
            ("j" . "<down>")
            ("k" . "<up>")
            ("l" . "<right>")

            ("t" . find-char)
            ("/" . search-cmd)
            ("g /" . ,(adviced #'search-cmd
                               :after (lambda (&rest _)
                                        (isearch-ring-retreat))))
            ("?" . regexp-search-cmd)
            ("a" . trace-to-region)

            ("C-d" . sscroll-hp-down)
            ("C-u" . sscroll-hp-up)
            ("g +" . winmax-dwim)
            ("+" . winmax2-max)
            ("C-o" . pop-to-mark-command)
            ("M-o" . pop-global-mark)
            ("G" . ,(traced-motion-if-local #'avy-goto-char-2))
            ("[" . previous-error)
            ("]" . next-error)

            ;; Window nav keys under C-w prefix
            ,@(alist-map (lambda (key cmd)
                           (cons (concat "C-w " key) cmd))
                         window-nav-keys)
            ,@(alist-map (lambda (key cmd)
                           (cons (concat "w " key) cmd))
                         window-nav-keys)
            
            )))

    (cl-flet ((map (keymap bindings)
                (if (symbolp keymap)
                    (apply #'meow-define-keys keymap bindings)
                  (pcase-dolist (`(,key . ,cmd) bindings)
                    (keymap-set keymap key cmd)))))
      
      (map 'normal
           `(,@digit-bindings
             ,@-windmove-bindings
             ,@basic-navigation
             ,@thing-nav-tracing
             ,@thing-nav-marking
             ("i" . insert-at-pt)
             ("<escape>" . my-reset)

             ;; FIXME: eval/replace
             ("'" . toggle-thing-hints)
             ("C-'" . ,(with-tmp-thing #'hint-thing))
             ("Q" . delete-window)
             ("g Q" . kill-current-buffer)

             ("g R" . reload-config)          
             ("I" . ,(with-tmp-thing #'insert-at-beginning-of-thing))
             ("A" . ,(with-tmp-thing #'insert-at-end-of-thing))

             ("." . dot-repeat)
             ("C-." . prev-dot-repeat)
             ("C-," . next-dot-repeat)

             ("x" . delete-forward-char)
             ("X" . delete-backward-char)

             ("d" . delete-forward-char)
             
             ("c" . ,(entering-insert-state #'sp-delete-char))
             ;; TODO: jump if eotp
             ("C" . ,(lambda (n)
                       (interactive "p")
                       (let ((bounds (bounds-of-next-thing-at-point current-thing n)))
                         (sp-kill-region (car bounds) (cdr bounds))
                         (meow-insert-mode))))
             ("g ;" . goto-last-change)
             ("g v" . st-to-secondary)

             ("C-v" . smart-yank)
             ("V" . smart-replace)

             ("u" . undo-tree-undo)
             ("C-r" . undo-tree-redo)
             ("g u" . undo-tree-visualize)

             ("S" . rewrap)
             ("s" . wrap)
             
             ("Z" . start-scroll-mode)
             ("z" . ,scroll-keys)

             ;; Unclear TODO
             ("g g" . consult-goto-line)
             ("G" . meow-grab)
             ("J" . ,(with-tmp-thing #'join-things))
             ("g J" . ,(with-tmp-thing #'join-things-at))

             ("<return>" . open-line-below)
             ("S-<return>" . open-line-above)

             ("TAB" . ,(with-tmp-thing (act-on-tap-region #'indent-region) nil t))

             ("C-/" . ,(with-tmp-thing
                        (act-on-tap-region #'comment-or-uncomment-region) nil t))

             ;; Activating region
             ("g s" . restore-region)
             ("v" . smart-yank)
             ("C-m" . set-mark-command)
             ("g C-m" . rectangle-mark-mode)
             
             ("o" . reverse-st)))
      
      (map 'visual
           `(,@digit-bindings
             ,@-windmove-bindings
             ,@basic-navigation
             ,@thing-nav-basic
             ,@thing-nav-marking

             ("<escape>" . (lambda ()
                             (interactive)
                             (deactivate-mark)))
             ("z" . pop-region)
             ("q" . abort-visual)

             ;; Misc navigation
             ("SPC" . meow-keypad)
             ("o" . reverse-st)

             ;; Act on region
             ("/" . search-cmd)
             ("s" . wrap)
             ("u" . smart-undo)
             ("d" . kill-region)
             ("D" . kill-unwrap-region)
             ("C" . kill-ring-save)
             ("y" . kill-ring-save)
             ("=" . indent-region)
             ("C-/" . comment-dwim)
             ("c" . ,(entering-insert-state #'kill-region))
             ("v" . smart-replace)))
      
      (map 'insert
           `(,@-windmove-bindings
             ("C-v" . smart-yank)))
      
      (map 'motion
           `(,@digit-bindings
             ,@-windmove-bindings
             ,@basic-navigation
             ,@thing-nav-tracing
             ,@thing-nav-marking
             ;;Meow has "last-buffer" here by default
             ("<escape>" . nil)))
      

      (map eat-mode-map
           `(("i" . eat-char-mode)))
      
      (map eat-char-mode-map
           `(,@-windmove-bindings
             ("M-x" . execute-extended-command)
             ("C-h" . ,help-map)
             ("<escape>" . eat-emacs-mode)
             ("M-<escape>" . ,(with-let ((last-command-event 27))
                                        eat-self-input))))
      
      ;; (setq eat-semi-char-mode-map eat-char-mode-map)

      (map mode-specific-map
           `(("C-a" . ,meow-motion-remap-prefix)
             ("a" . mode-line-other-buffer)
             ("w" . save-buffer)
             ("b" . consult-buffer)
             ("B" . consult-project-buffer)
             ("s" . set-variable)

             ("t c" . toggle-comments)
             ("t h" . eglot-inlay-hints-mode)
             ("t p" . prog-buffers-mode)

             ("r l" . eglot-code-actions)
             ("r n" . eglot-rename)
             ("r f" . eglot-format)
             ("r q" . eglot-code-action-quickfix)
             ("r x" . eglot-code-action-extract)
             ("r i" . eglot-code-action-inline)
             ("r I" . eglot-code-action-organize-imports)
             ("r w" . eglot-code-action-rewrite)

             ("o j" . ,(entering-insert-state #'org-journal-new-entry))
             ("o i" . ,(lambda ()
                         (interactive)
                         (find-file user-init-file)))
             ("o t" . eat)
             ("o d" . dired-jump)
             ("l s" . eglot)
             ("l x" . eglot-shutdown)

             ("l d" . ,(with-let ((xref-show-xrefs-function #'consult-xref)
                                  (xref-prompt-for-identifier nil))
                                 eglot-find-declaration))
             ("l t" . ,(with-let ((xref-show-xrefs-function #'consult-xref)
                                  (xref-prompt-for-identifier nil))
                                 eglot-find-typeDefinition))
             ("l i" . ,(with-let ((xref-show-xrefs-function #'consult-xref)
                                  (xref-prompt-for-identifier nil))
                                 eglot-find-implementation))

             ("l D" . ,(with-let ((xref-auto-jump-to-first-definition t)
                                  (xref-prompt-for-identifier nil))
                                 eglot-find-declaration))
             ("l T" . ,(with-let ((xref-auto-jump-to-first-definition t)
                                  (xref-prompt-for-identifier nil))
                                 eglot-find-typeDefinition))
             ("l I" . ,(with-let ((xref-auto-jump-to-first-definition t)
                                  (xref-prompt-for-identifier nil))
                                 eglot-find-implementation))

             ("f f" . consult-fd)
             ("f b" . consult-recent-files)
             ("f g" . consult-ripgrep)
             ("f G" . consult-git-grep)
             ("f l" . consult-line)
             ("f L" . (lambda ()
                        (interactive)
                        (consult-line-multi t)))

             ("o n" . (lambda ()
                        (interactive)
                        (let ((consult-async-min-input 1))
                          (consult-fd "~/Notes"))))

             ("p f" . project-find-file)
             ("p F" . project-or-external-find-file)
             ("p g" . project-find-regexp)
             ("p G" . project-or-external-find-regexp)
             ("p c" . project-compile)
             ("p ." . project-dired)
             ("p d" . project-find-dir)
             ("p r" . project-query-replace-regexp)
             ("p s" . project-eshell)

             ("p s" . profiler-start)
             ("p e" . profiler-stop)
             ("p r" . profiler-report)))

      (map 'transparent
           `(,@-windmove-bindings
             ("<escape>" . ,(dynmap `((t . meow-motion-mode))))
             ("C-<space>" . meow-keypad)))))

  (meow-global-mode 1))


(use-package avy)


(global-display-line-numbers-mode 1)
(setq
 display-line-numbers-widen t
 display-line-numbers-type 'relative
 display-line-numbers-current-absolute t
 display-line-numbers-width-start t)

(use-package treesit
  :straight nil
  :config
  )

(use-package undo-tree
  :config
  ;; undo-tree uses this as a heuristic to detect incompatible major modes which of course makes no sense if we remap C-/ manually
  (advice-add 'undo-tree-overridden-undo-bindings-p :override #'ignore)
  (global-undo-tree-mode 1)

  (setq undo-tree-visualizer-diff t)
  ;; Persist undo history
  (let ((undo-hist-dir "~/.local/share/emacsundo"))
    (unless (file-directory-p undo-hist-dir)
      (make-directory undo-hist-dir t))
    (setq undo-tree-auto-save-history t
          undo-tree-history-directory-alist `(("." . ,undo-hist-dir)))))

(setq inhibit-startup-screen t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)
(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)

;; Builtin LSP server
(use-package eglot
  :straight nil
  ;; :hook (prog-mode . eglot-ensure)
  :config
  (setq 
   eglot-events-buffer-size 0
   eglot-report-progress nil
   )
  (fset #'jsonrpc--log-event #'ignore)
  ;; (setf (plist-get eglot-events-buffer-config :size) 0)
  ;; (add-hook 'rust-mode-hook 'eglot-ensure)
  ;; (add-hook 'prog-mode-hook 'eglot-ensure)
  )

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode 1))

(setq org-directory "~/Notes")

(use-package markdown-mode
  :after zk-emacs
  :config
  (setq markdown-enable-math t)

  ;; (defun zk-screenshot ()
  ;;   (interactive)
  ;;   (let ((basename)
  ;;         (path (concat "~/Zettelkasten/screenshots/" filename ".png")))
  ;;     (shell-command (concat "grim -g $(slurp) " path))
  ;;     ))

  (rerequire 'emanote-live)
  (let ((zk-dir "~/Zettelkasten"))
    (zk-register zk-dir)
    (emanote-live-register zk-dir))
  )

(use-package org-journal
  :config
  (setq org-journal-dir "~/Journal"))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.3)
  (dimmer-mode +1))

(use-package rainbow-mode
  :config
  (rainbow-mode))


(dolist (el
         ;; Right side
         `(("\\*Help\\*" display-buffer-in-side-window
               (side . right) (slot . -2) (dedicated . t))
              ("\\*eldoc" display-buffer-in-side-window
               (side . right) (slot . -1) (dedicated . t))

              ;; Bottom
              ("\\*eat" display-buffer-in-side-window
               (side . bottom) (slot . 0) (dedicated . t))
              ("\\*compilation\\*" display-buffer-in-side-window
               (side . bottom) (slot . 1) (dedicated . t))
              ))
  (add-to-list 'display-buffer-alist el))

(defvar test-frame nil)


;; Local Variables:
;; read-symbol-shorthands: (("-" . "init--"))
;; End:
