;; -*- lexical-binding: t; -*-

(require 'common)

(use-package compile-angel
    :custom
  (compile-angel-verbose t)
  :config
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)
  )
;; Otherwise compiling parts of the emacs config on save always pops up
;; the compilation log buffer. TODO: Either get rid of all the warnings
;; or set in elisp mode.
(setq warning-minimum-level :error)

;; Require very early; otherwise an older version is required and dape breaks
(straight-use-package 'jsonrpc)
(straight-use-package 'eglot)
(straight-use-package 'eldoc)
(straight-use-package 'org)

(use-package eglot
    :straight t)

(require 'jsonrpc)
(require 'eglot)
;; (require 'org)

(defun rerequire (feature &rest rest)
  (when (featurep feature)
    (unload-feature feature t))
  (apply #'require feature rest))

;; Require early so that other minor modes have the chance to overwrite its keybinds
(require 'modality)
(require 'base16-colors)
(require 'completion)
(require 'traces)
(require 'ts-nav)
(require 'thing-defs)
(require 'thing-nav)
(require 'restore-region)
(require 'scroll-mode)
(require 'dot-repeat)
(require 'multi-select)
(require 'multi-insert)
(require 'winmax)
(require 'up-and-down)
(require 'dymap)
(require 'text-editing)
(require 'coding)
(require 'terminal)
(require 'undo)

;; (unless (server-running-p)
;;   (server-start))



;; Setup keybindings and appearance last when all commands and faces referenced exist.
(with-eval-after-load 'init
  (require 'ui)
  (require 'appearance)
  (require 'keybindings))



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

(use-package diff-hl
    :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-dired-mode)
  ;; (setf (alist-get 'change diff-hl-margin-symbols-alist) "*")
  )

;; Colorize compilation output
;; (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; TODO: desktop-save asks for custom path
(use-package desktop
    :config
  (setq desktop-path '("~/.local/share/emacs/desktop")))

;; Wrap at word boundary and fall back to truncatation in small windows
(setq-default
 word-wrap t
 ;; truncate-lines t
 truncate-partial-width-windows window-min-width
 ;; line-prefix ""
 ;; wrap-prefix ""
 )

;; TODO: ace bindings to "pop buffer to other window"
(use-package ace-window
    :custom
  (aw-dispatch-always t)
  (aw-scope 'frame)
  (aw-background nil)
  (aw-minibuffer-flag t)
  (aw-display-mode-overlay nil)
  (aw-ignored-on t)
  (aw-ignored-buffers '(minibuffer-mode))
  :config
  ;; Required for our modeline integration, taken from aws internal logic for its minor mode doing a similar thing.
  (add-hook 'window-configuration-change-hook 'aw-update)
  (add-hook 'after-make-frame-functions 'aw--after-make-frame t)

  ;; Workaround for https://github.com/abo-abo/ace-window/issues/249
  (defun aw-update ()
    "Update ace-window-path window parameter for all windows.

Ensure all windows are labeled so the user can select a specific
one, even from the set of windows typically ignored when making a
window list."
    (let ((ignore-window-parameters t))
      (avy-traverse
       (avy-tree (aw-window-list) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (propertize
           (apply #'string (reverse path))
           'face 'aw-mode-line-face))))))

  (defun aw-move-into-vsplit (window)
    (let ((old-win (selected-window)))
      (select-window window)
      (let ((new-win (split-window-horizontally)))
        (window-swap-states old-win new-win)
        (delete-window old-win))))

  (defun aw-move-into-hsplit (window)
    (let ((old-win (selected-window)))
      (select-window window)
      (let ((new-win (split-window-vertically)))
        (window-swap-states old-win new-win)
        (delete-window old-win))))

  ;; Customize dispatch table
  (defun aw-bind (key action)
    (setf (alist-get key aw-dispatch-alist nil t) action))
  
  (aw-bind ?v '(aw-split-window-horz "V-Split Window"))
  (aw-bind ?s '(aw-split-window-vert "H-Split Window"))
  (aw-bind ?V '(aw-move-into-vsplit "Move into V-Split"))
  (aw-bind ?S '(aw-move-into-hsplit "Move into H-Split"))
  (aw-bind ?b nil) ; Original position of v-split

  (aw-bind ?k '(aw-delete-window "Delete Window"))
  (aw-bind ?x nil)

  (aw-bind ?Q '(delete-other-windows "Delete Other Windows"))
  (aw-bind ?o nil)

  (aw-bind ?b '(aw-switch-buffer-in-window "Select Buffer"))
  (aw-bind ?j nil)

  (aw-bind ?B '(aw-switch-buffer-other-window "Select Buffer Other Window"))
  (aw-bind ?u nil)

  (aw-bind ?t '(aw-swap-window "Swap Windows"))
  (aw-bind ?m '(switch-to-minibuffer))

  ;; Use consult-buffer for switching buffers
  (advice-add 'aw--switch-buffer
              :around (lambda (orig &rest args)
                        (cond ((functionp 'consult-buffer) (call-interactively #'consult-buffer))
                              (apply orig args)))))

;; (use-package eyebrowse
;;   :config
;;   (eyebrowse-mode t))

;; ;; TODO try out this (mode) together al line navigation (thing)
;; (use-package adaptive-wrap
;;   :config
;;   (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
;;   (global-visual-line-mode))

(setq kill-buffer-delete-auto-save-files t)


(use-package org
    :config
  (setq org-startup-truncated nil))

(use-package org-journal
    :config
  (setq org-journal-dir "~/Journal"))

;; Have help links replace the help buffer in its window
(setq help-window-keep-selected t)

(defun reload-config ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun starting-dot-repeat (cmd)
  (adviced cmd
           :what "Starting `dot-repeat`."
           :before (lambda (&rest _)
                     (start-dot-repeat))))



;; Useful to see default keybindings; remove after stabilizing keybindings.
(use-package which-key
    :custom
  (which-key-show-transient-maps t)
  (which-key-idle-delay 0.5)
  :commands which-key-mode
  :config
  (which-key-mode))

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  ;; TODO: See how this behaves with latency
  ;; (super-save-remote-files nil)
  ;; Disable builtin autosave and backups
  ;; (auto-save-default nil)
  :config
  (super-save-mode +1)
  ;; Only auto save in normal state (TODO: also motion state)
  (add-to-list 'super-save-predicates (nlambda -save-in-normal-only ()
                                        (if (boundp 'meow--current-state)
                                            (equal meow--current-state 'normal)
                                          t))))

(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backup/" user-emacs-directory))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))

(global-auto-revert-mode t)

;; Going up and down




(setq enable-recursive-minibuffers t)
(minibuffer-electric-default-mode t)


(use-package hydra
    :config
  (setq hydra-is-helpful t)
  )

(use-package crux)

(ido-mode -1) ;; TODO: needed?

;; TODO: check if needed, possibly generalize scroll-mode for other-window?
(setq help-window-select t)

;; (setq delete-pair-blink-delay nil)

(setq isearch-wrap-pause 'no-ding)
(setq isearch-repeat-on-direction-change t)

;; Recenter if search moved off-screen
(add-hook 'isearch-update-post-hook (lambda ()
				                      (when (not (pos-visible-in-window-p))
					                    (recenter))))

;; TODO: might misbehave in regexp search, c.f. isearch-yank-string's implementation
(defun isearch-set-search-string (string)
  (setq isearch-string string
        isearch-message (mapconcat #'isearch-text-char-description string ""))
  ;; Copied from `isearch-del-char', apparently `isearch-search-and-update' has
  ;; problems with deletions.
  (funcall (or isearch-message-function #'isearch-message) nil t)
  (if isearch-other-end (goto-char isearch-other-end))
  (isearch-search)
  (isearch-push-state)
  (isearch-update))

(defmacro with-isearch-string-as-buffer (&rest body)
  (declare (indent 0))
  `(let ((out (with-temp-buffer
                (insert isearch-string)
                (end-of-buffer)
                (let ((res (progn ,@body)))
                  (cons res (buffer-substring-no-properties 1 (point-max)))))))
     (isearch-set-search-string (cdr out))
     (car out)))

(defun isearch-kill-word (&optional n)
  (interactive "p")
  (with-isearch-string-as-buffer
    (backward-kill-word (or n 1))))


(defun isearch-search-fun-ms-aware ()
  "When a multi selection is active, only search inside its bounds."
  (let ((inner-fn (isearch-search-fun-default)))
    (cond          
      ((has-multi-selection-p) (isearch-search-fun-in-noncontiguous-region
                                inner-fn
                                (reverse ms--selections)))
      (t inner-fn))))

(defun isearch-exit-ms-aware ()
  "Exit isearch, if there is an active multi selection, refine it to matches."
  (interactive)
  (call-interactively #'isearch-exit)
  (set-current-thing (if isearch-regexp
                         'regexp-search
                       'search))
  (when (has-multi-selection-p)
    (multi-select-refine-by-thing current-thing)))

(defun isearch-exit-and-refine ()
  "Exit isearch, if there is an active multi selection, refine it to matches."
  (interactive)
  (call-interactively #'isearch-exit)
  (set-current-thing (if isearch-regexp
                         'regexp-search
                       'search))
  (if (has-multi-selection-p)
      (multi-select-refine-by-thing current-thing)
    (multi-select-things-in-region current-thing (point-min) (point-max))))

(defun isearch-start-with-string (string &optional backwards)
  (isearch-mode (not backwards))
  (isearch-set-search-string string))

(defun isearch-forward-region ()
  "Start isearch with current region as search string"
  (interactive)
  (let ((string (region-text)))
    (deactivate-mark)
    (isearch-start-with-string string)))

(defun isearch-backward-region ()
  "Start isearch with current region as search string"
  (interactive)
  (let ((string (region-text)))
    (deactivate-mark)
    (isearch-start-with-string string t)))

(setq isearch-search-fun-function #'isearch-search-fun-ms-aware)


(setq select-enable-clipboard nil)

(advice-add 'get-register
            :around (nlambda overloaded-get-register-advice (orig register)
                      (pcase register
                        (?- (gui-get-selection))
                        (59 (current-kill 0 t)) ; ;
                        ;; (32 (gui-get-selection)) ; "
                        (_ (funcall orig register)))))

(advice-add 'set-register
            :around (nlambda overloaded-set-register-advice (orig register value)
                      (pcase register
                        (?- (gui-set-selection nil value))
                        (59 (kill-new value)) ; ;
                        (_ (funcall orig register value)))))


;; TODO: Could motion state be replaced with normal state and become more minimal to be more usable here?
(use-package dirvish
    :hook (dired-mode . (lambda () (dired-omit-mode)))
    :config
    (setq dirvish-hide-details t
          dirvish-mode-line-height (pixel-line-height)
          dirvish-header-line-height (pixel-line-height)
          dirvish-reuse-session nil)

    ;; TODO: After following file in dirvish-side, window is not dedicated anymore
    (defun -dv-setup-hook ())
    (add-hook 'dired-setup-hook #'-dv-setup-hook)
    
    ;; (add-hook 'dirvish-side)
    (dirvish-side-follow-mode +1)
    (dirvish-override-dired-mode))

(defun my-reset ()
  (interactive)
  (set-current-thing 'sexp)
  (clear-current-trace)
  (deactivate-mark)
  ;; (minibuffer-keyboard-quit)
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
                    (derived-mode-p '(prog-mode text-mode))
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
  (interactive "*p")
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
    (if (region-active-p) 
        (progn
          (kill-region (region-beginning) (region-end))
          (rotate-yank-pointer 1)
          (yank n)
          (rotate-yank-pointer -1))
      (yank n))))



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


;; General configuration



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



(defun forward-to-nearest-thing-at-point (thing n) 
  (when-let ((bounds (bounds-of-nearest-thing-at-point thing n)))
    (goto-char (if (> n 0) (cdr bounds) (car bounds)))))

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




(use-package avy)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (global-display-line-numbers-mode 1)
(setq
 display-line-numbers-widen t
 display-line-numbers-type 'relative
 display-line-numbers-current-asolute t
 display-line-numbers-width-start t)


(setq inhibit-startup-screen t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)
(put 'set-goal-column 'disabled nil)

(put 'narrow-to-region 'disabled nil)


;; Have insert-register jump to saved point instead of inserting it
(cl-defmethod register-val-insert ((val marker))
  (register-val-jump-to val nil))

(use-package rainbow-mode
    :config
  (rainbow-mode))


(winner-mode)

;; Close eat buffer on regular exits
(defun -eat-exit-hook (process)
  (when (= (process-exit-status process) 0)
    (kill-buffer)))
(add-hook 'eat-exit-hook #'-eat-exit-hook)

;; (add-hook 'eat--char-mode-hook (lambda (&rest _)i
;;                                  (meow-insert-mode)))
;; (add-hook 'eat--emacs-mode-hook (lambda (&rest _)
;;                                   (if (equal meow--current-state 'insert)
;;                                       (meow-insert-exit)
;;                                     (meow-normal-mode))))

(defvar-local offset-cursor--did-not-move nil)
(defun offset-cursor-pre-cmd ())
(defun offset-cursor-post-cmd ())

(define-minor-mode
    offset-cursor-mode
    "Display cursor vim-like offset to the left."
  :lighter nil
  (if offset-cursor-mode
      (progn
        (add-hook 'pre-command-hook #'offset-cursor-pre-cmd nil t)
        (add-hook 'post-command-hook #'offset-cursor-post-cmd nil t))
    (remove-hook 'pre-command-hook #'offset-cursor-pre-cmd t)
    (remove-hook 'post-command-hook #'offset-cursor-post-cmd t)))

;; Always follow compilation output. Even in the case of errors, as causes warnings (whihc are not differentiated) to hide further output
(setq compilation-scroll-output t)

;; (setq line-move-visual t)

;; (use-package isearch+)

(put 'downcase-region 'disabled nil)

(require 'ui)

(provide 'init)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "init--"))
;; End:
