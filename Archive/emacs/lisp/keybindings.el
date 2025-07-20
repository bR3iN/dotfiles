;; -*- lexical-binding: t; -*-

(setq viper-mode nil)
(require 'viper)

;; ; - command changing buffer or compilation
;; ? - find/goto things
;; / - settings, e.g. toggling truncation, inlay hints

;; Treat "C-[" the same as escape instead of an alternative "M-" prefix.
(defun gui-key-tranlations ()
  (key-translate "C-[" "<escape>"))
(add-hook 'after-make-frame-functions #'gui-key-tranlations)
(gui-key-tranlations)

;; TODO: does this override the translation above and can the latter be removed?
;; (key-translate "ESC" "C-g")

(require 'common)
(require 'modality)

(defun store-last-kill-in-register (register)
  "Store the most recent kill (first entry of `kill-ring`) into REGISTER."
  (interactive (list (read-char "Store last kill in register: ")))
  (let ((text (current-kill 0 t)))   ; get the most recent entry without advancing yank pointer
    (set-register register text)))

;; (defun typing-keys (keys)
;;   "Return a symbol suitable to be set as a key definition to act as typing KEYS."
;;   (let ((sym (make-symbol "<prefix>")))
;;     (fset sym (lambda ()
;;                 (interactive)
;;                 (let ((keymap (key-binding (read-kbd-macro keys)))
;;                       (mapsym (make-symbol "dynamic-prefix-map")))
;;                   (fset mapsym keymap)
;;                   (set mapsym keymap)
;;                   (keymap-set mapsym "C-h" prefix-help-command)
;;                   (set-transient-map mapsym)
;;                   (when which-key-mode
;;                     (which-key-show-keymap mapsym t))
;;                   )))
;;     (put sym 'function-documentation
;;          (format "Set typed keybind prefix to %s" keys))
;;     sym))

(defun keymap-from-alist (key-alist)
  (let ((keymap (make-sparse-keymap)))
    (map keymap key-alist)
    keymap))

(defun with-prefix (prefix keys)
  (mapcar (pcase-lambda (`(,key . ,cmd))
              (cons (concat prefix key) cmd))
          keys))

(defun prefix-keymap (prefix)
  (key-binding (read-kbd-macro keys)))


(defun bind-from (keys &optional toggled)
  "Return a symbol suitable to be set as a key definition to act as typing KEYS."
  `(menu-item
    ,"" nil
    :filter ,(lambda (&rest _)
               (if toggled
                   (lambda ()
                     (interactive)
                     (meow-keypad-start-with keys))
                 (meow--exit-keypad-state)
                 (key-binding (read-kbd-macro keys))))))

(defmacro lazydef (&rest body)
  `(let ((as-fn (lambda (&rest ,(make-symbol "args"))
                  ,@body)))
     `(menu-item
       ,"" nil
       :filter ,as-fn)))

(defmacro dynk (&rest clauses)
  `(let ((as-fn (lambda (&rest ,(make-symbol "args"))
                  (cond
                    ,@clauses))))
     `(menu-item
       ,"" nil
       :filter ,as-fn)))

(defun key-by-state (&rest by-state-alist)
  (print by-state-alist)
  `(menu-item
    ,"" nil
    :filter ,(lambda (&rest _)
               (cl-some
                (pcase-lambda (`(,state . ,def)
                                (print state)
                                (when (memq state `(t ,meow--current-state))
                                  def)))
                by-state-alist))))
(letrec ((unstick nil)
         (map (keymap-from-alist
               `(("f" . end-of-thing-cmd-traced)
                 ("b" . beginning-of-thing-cmd-traced)
                 ("n" . next-thing-traced)
                 ("p" . previous-thing-traced)
                 ("m" . ,(adviced #'mark-thing
                                  :after (lambda (&rest _)
                                           (funcall unstick))))
                 ("s" . multi-select-things-at-point)
                 ("C-t" . (lambda ()
                            (interactive)
                            (transpose-things current-thing nil)))
                 ("M-t" . (lambda ()
                            (interactive)
                            (transpose-things current-thing t)))
                 ))))
  
  (defun sticking-thing (init-cmd)
    (adviced (with-new-thing init-cmd)
             :after (lambda (&rest _)
                      (setq unstick
                            (set-transient-map
                             map t #'pop-current-thing
                             (format "Sticky %s" current-thing)))))))



(defun traced-in-normal (cmd)
  (dispatch
   `(state normal ,(traced-motion cmd))
   `(t ,cmd)))

(defun insert-beginning-of-region (start end)
  (interactive (list (region-beginning) (region-end)))
  (goto-char start)
  (meow-insert-mode))

(defun insert-end-of-region (start end)
  (interactive (list (region-beginning) (region-end)))
  (goto-char end)
  (meow-insert-mode))

(defun my/mark-sexp (&optional n)
  (interactive "p")
  (mark-things 'sexp (or n 1)))

(defun insert-cmd ()
  (interactive)
  ;; (call-interactively (with-thing-region #'kill-region))
  (if multi-insert-cursors
      (pop-into-multi-insert)
    (meow--switch-state 'insert)))


(defun marked-motion (cmd)
  (adviced cmd
           :what "Marked motion."
           :before (lambda (&rest _)
                     (push-mark nil t t))))

(dolist (cmd '(end-of-thing-cmd beginning-of-thing-cmd
               next-thing previous-thing))
  (pcase-dolist (`(,suffix . ,fun) `(("traced" . traced-motion)
                                     ("marked" . marked-motion)))
    (let ((sym (intern (concat (symbol-name cmd) "-" suffix))))
      (fset sym (funcall fun cmd)))))


(defvar -windmove-bindings '(("M-h" . windmove-left)
                             ("M-j" . windmove-down)
                             ("M-k" . windmove-up)
                             ("M-l" . windmove-right)))

(defun setting-thing (thing)
  (lambda ()
    (interactive)
    (setq current-thing thing)))

;; (keymap-global-set "M-/" 'xref-find-references)
;; (keymap-global-set "M-s" 'isearch-forward-regexp)
;; (keymap-global-set "M-r" 'isearch-backward-regexp)

;; (keymap-set global-map "C-w" 'backward-kill-word)
;; Doesn't work
;; (keymap-set isearch-mode-map "C-w" 'backward-kill-word)
(map isearch-mode-map
  `(("C-w" . isearch-kill-word)
    ("C-n" . isearch-ring-advance)
    ("C-p" . isearch-ring-retreat)
    ("<return>" . isearch-exit)
    ("C-<return>" . isearch-exit-and-refine)
    ("<escape>" . isearch-abort)
    ("<backspace>" . isearch-del-char)
    ("C-l" . "RET")))
(keymap-set minibuffer-local-isearch-map "C-f" #'isearch-repeat-forward)
(keymap-set minibuffer-local-isearch-map "C-b" #'isearch-repeat-backward)

;; (map (defvar-keymap tab-map)
;;   `(("c" . tab-new)
;;     ("q" . tab-close)
;;     ("n" . tab-next)
;;     ("p" . tab-previous)
;;     ("b" . switch-to-buffer-other-tab)
;;     ("r" . tab-rename)
;;     ("f" . find-file-other-tab)
;;     ("o" . tab-close-other)
;;     ("m" . tab-move)))

(map project-prefix-map
  `(("l" . (lambda ()
             (interactive)
             (consult-line-multi nil)))
    ("t" . eat-project)
    ("C-b" . nil)
    ("B" . project-list-buffers)
    ("c" . project-recompile)
    ("C" . project-compile)
    ))


;; (map (defvar-keymap actions-map)
;;   `(("x" t )
;;     ))

(map `(,minibuffer-mode-map)
  `(("C-w" . backward-kill-word)))

(letrec (;; Keybinding Groups
         (digit-bindings
          `(("-" . negative-argument)
            ;; ("\\" . universal-argument)
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

            ;; ("u" . winner-undo)
            ;; ("r" . winner-redo)
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
            ;; ("o" . other-window)
            ;; ("m" . switch-to-minibuffer)
            ;; ("e" . switch-to-error-buffer)
            ;; Kill window
            ("q" . delete-window)
            ("<escape>" . ignore)))

         (multi-select-actions
          `(("d" . ,(multi-select-cmd-each #'kill-region))
            ("<tab>" . ,(multi-select-cmd-each #'indent-region))
            ("<escape>" . ignore)
            ("e" . multi-wrap)
            ("r" . ,(with-new-thing #'multi-select-refine-by-thing))
            ("i" . multi-select-change)
            ("I" . multi-select-insert)
            ("A" . multi-select-append)
            
            (";" . ,(multi-select-cmd-each #'comment-or-uncomment-region))
            ("C-;" . ,(multi-select-cmd-each #'comment-region))
            ("M-;" . ,(multi-select-cmd-each #'uncomment-region))
            
            ("m" . ,(lambda ()
                      "Merge selections into one and choose another action."
                      (interactive)
                      (multi-select-merge-selections)
                      (set-transient-map multi-select-keymap
                                         nil nil "Act on merged selection")))
            ("M" . multi-select-into-region)
            ))

         (multi-select-keymap
          (keymap-from-alist multi-select-actions))

         (local-options
          `(("l s" . eglot)
            ("l x" . eglot-shutdown)
            ("l h" . eglot-inlay-hints-mode)
            ("w" . toggle-truncate-lines)
            ("g" . glasses-mode)
            ("e" . ,(lambda ()
                      (interactive)
                      (setq-local flymake-show-diagnostics-at-end-of-line
                                  (not flymake-show-diagnostics-at-end-of-line))
                      ;; Restart mode to make change active
                      (when flymake-mode
                        (flymake-mode -1)
                        (flymake-mode +1))))
            )))

  (map universal-argument-map
    `(("\\" . universal-argument-more)))

  ;; Overlay on "basis-sexp" bindings?
  (with-eval-after-load 'hydra
    (mapply #'defhydra
            `(hydra-thing-nav-normal
              (:post (when (region-active-p)
                       (print "yes")))
              "Thing navigation (normal mode)"
              ("f" end-of-thing-cmd-traced)
              ("b" beginning-of-thing-cmd-traced)
              ("n" next-thing-traced)
              ("p" previous-thing-traced)
              ("F" end-of-thing-cmd-marked
                   :exit t)
              ("B" beginning-of-thing-cmd-marked
                   :exit t)
              ("N" next-thing-marked
                   :exit t)
              ("P" previous-thing-marked
                   :exit t)
              ("m" mark-thing
                   :exit t)

              ;; ("o" reverse-st)
              ("<escape>" nil :exit t)
              ))

    (mapply #'defhydra
            `(hydra-thing-nav-visual
              (:pre (message "v"))
              "Thing navigation (visual mode)"
              ("f" end-of-thing-cmd)
              ("b" beginning-of-thing-cmd)
              ("n" next-thing)
              ("p" previous-thing)
              ("o" reverse-st)
              ("m" mark-thing)
              ("<escape>" nil :exit t)
              ))

                                        ; (mapply #'defhydra
    ;;         `(hydra-sexp-nav
    ;;           ()
    ;;           "Expression based navigation"
    ;;           ("f" ,(traced-in-normal #'sp-forward-sexp))
    ;;           ("b" ,(traced-in-normal #'sp-backward-sexp))
    ;;           ("n" ,(traced-in-normal #'sp-next-sexp))
    ;;           ("p" ,(traced-in-normal #'sp-previous-sexp))
    ;;           ("m" my/mark-sexp)
    ;;           ("o" reverse-st)
    ;;           ("h" ,(dispatch
    ;;                  `(state normal backward-down-traced)
    ;;                  `(t backward-down)))
    ;;           ("k" ,(dispatch
    ;;                  `(state normal backward-up-traced)
    ;;                  `(t backward-up)))
    ;;           ("j" ,(dispatch
    ;;                  `(state normal forward-down-traced)
    ;;                  `(t forward-down)))
    ;;           ("l" ,(dispatch
    ;;                  `(state normal forward-up-traced)
    ;;                  `(t forward-up)))
    ;;           ("<escape>" nil :exit t)
    ;;           ))
    )

  (with-eval-after-load 'hydra
    )
  
  ;; Actual Keybindings
  ;; Empty: qtg
  ;; unsure: 'axz,.[]\/
  ;; To add:
  ;; vc under g?
  ;; register usage
  ;; macros
  ;; to reduct Ctrl+ bindings, but low prio
  ;; search
  ;; undo/redo
  (map 'normal
    `(,@digit-bindings
      ,@(alist-map (lambda (key cmd)
                     (cons (concat "C-w " key) cmd))
                   window-nav-keys)

      ("`" . abort-recursive-edit)

      ("h" . "<left>")
      ("l" . "<right>")
      
      ("j" . "<down>")
      ("k" . "<up>")

      ("gj" . (with-let ((line-move-visual nil))
                next-line))
      ("gk" . (with-let ((line-move-visual nil))
                previous-line))

      ("d" . delete-forward-char)

      ("i" .  ,(lambda ()
                 (interactive)
                 (goto-char (car (current-selection)))
                 (meow-insert-mode)))
      ("a" .  ,(lambda ()
                 (interactive)
                 (goto-char (cdr (current-selection)))
                 (meow-insert-mode)))
      
      ("I" . ,(entering-insert-state #'beginning-of-line-text))
      ("A" . ,(entering-insert-state #'end-of-line))

      ("o" . ,(entering-insert-state #'crux-smart-open-line))
      ("O" . ,(entering-insert-state #'crux-smart-open-line-above))

      ("w" . ,(tracing-last (lambda ()
                              (right-char)
                              (viper-forward-word nil)
                              (left-char))
                            t))
      ("W" . ,(tracing-last (lambda ()
                              (right-char)
                              (viper-forward-Word nil)
                              (left-char))
                            t))
      
      ("e" . ,(tracing-last (lambda () (viper-end-of-word nil)) t))
      ("E" . ,(tracing-last (lambda () (viper-end-of-Word nil)) t))
      
      ("b" . ,(tracing-last (lambda () (viper-backward-word nil)) t))
      ("B" . ,(tracing-last (lambda () (viper-backward-Word nil)) t))

      ("v" . ,(lambda ()
                (interactive)
                (if (current-trace-p)
                    (trace-to-region)
                  (push-mark (point) t t))))

      ("x" . (lambda (&optional n)
               (interactive "p")
               (if (not (equal (cmd-trace-type) 'line))
                   (progn
                     (request-cmd-trace
                      (save-excursion (beginning-of-line) (point))
                      nil 'line)
                     (end-of-line n))
                 (request-cmd-trace
                  (current-trace-get 'first)
                  nil 'line)
                 (goto-char (current-trace-get 'last))
                 (end-of-line (1+ n)))))

      ("c" . ,(entering-insert-state
               (acting-on-selection #'kill-region)))

      ("d" . ,(acting-on-selection #'kill-region))

      (";" . clear-current-trace)
      ("y" . ,(acting-on-selection #'kill-ring-save))

      ;; FIXME: add register mechanism
      ("/" . isearch-forward-regexp)
      ("?" . isearch-backward-regexp)
      ("n" . ,(with-tmp-thing #'next-thing 'regexp-search))
      ("N" . ,(with-tmp-thing #'previous-thing 'regexp-search))
      
      
      ;; FIXME: needs to handle line-yanks differently + p
      ("p" . yank)

      ("u" . undo-only)
      ("r" . undo-redo)

      ("," . multi-insert-clear-cursors)
      
      ("'" . hint-current-thing-mode)
      

      ;; TODO: sticky
      ("C-t" . (lambda ()
                 (interactive)
                 (transpose-things current-thing nil)))
      ("M-t" . (lambda ()
                 (interactive)
                 (transpose-things current-thing t)))

      ("C-h" . backward-down-traced)
      ("C-j" . forward-down-traced)
      ("C-k" . backward-up-traced)
      ("C-l" . forward-up-traced)
      ("M-h" . backward-down-marked)
      ("M-j" . forward-down-marked)
      ("M-k" . backward-up-marked)
      ("M-l" . forward-up-marked)

      ;; FIXME: use flymake thing instead
      ("M-f" . flymake-goto-next-error)
      ("M-b" . flymake-goto-prev-error)

      ;;("d" . smooth-scroll-half-window-down)
      
      ;;("u" . smooth-scroll-half-window-up)

      ("r" . kmacro-end-or-call-macro)
      ("R" . kmacro-to-register)
      
      ;; ("g d" . ,(with-let ((xref-show-definitions-function #'consult-xref))
      ;;             xref-find-definitions))

      ;; ("g D" . ,(with-let ((xref-auto-jump-to-first-definition t))
      ;;             xref-find-definitions))

      ;; ("g r" . ,(with-let ((xref-show-xrefs-function #'consult-xref))
      ;;             xref-find-references))

      ;; ("g R" . ,(with-let ((xref-auto-jump-to-first-xref t))
      ;;             xref-find-references))

      ;; ("g t" . ,(with-let ((xref-show-xrefs-function #'consult-xref))
      ;;             eglot-find-typeDefinition))

      ;; ("g T" . ,(with-let ((xref-auto-jump-to-first-definition t))
      ;;             eglot-find-typeDefinition))    

      ("g l" . consult-goto-line)
      ("g g" . consult-line)
      ("g p" . ,(traced-motion
                 (with-let-new ((avy-all-windows nil))
                   #'avy-goto-char-2)))

      ("g ;" . goto-last-change)

      ("g e" . consult-flymake)
      ("g s" . imenu)
      ("g r" . xref-find-references)
      ("g d" . xref-find-definitions)
      ("g c" . eglot-find-declaration)
      ("g t" . eglot-find-typeDefinition)
      ("g i" . eglot-find-implementation)
      ("g f" . find-file-at-point)
      ("g E" . flymake-show-buffer-diagnostics)

      ;; ("a" . trace-to-region)

      ;; ("c" . multi-insert-toggle-cursor)
      ;; ("C" . multi-insert-clear-cursors)

      ;; Can be distinguished not to RET, but to <return>
      ("C-m" . push-mark-command)
      ("M-m" . rectangle-mark-mode)

      ("<escape>" . my-reset)

      ;; ("RET" . ignore)


      ;; ("g R" . reload-config)
      ;; ("g K" . (lambda ()
      ;;            (interactive)
      ;;            (load-file "~/.config/emacs/lisp/keybindings.el")))

      ;; ("," . kmacro-end-or-call-macro)
      ("." . dot-repeat)
      ;; ("." . kmacro-call-macro)
      ("C-." . prev-dot-repeat)
      ("C-," . next-dot-repeat)

      ,@(with-prefix "\\ " local-options)

      (": r" . kmacro-start-macro)
      (": Q" . kill-current-buffer)
      (": f" . eglot-format)
      (": a" . eglot-code-actions)
      (": q" . eglot-code-action-quickfix)
      (": x" . eglot-code-action-extract)
      (": i" . eglot-code-action-inline)
      (": I" . eglot-code-action-organize-imports)
      (": w" . eglot-code-action-rewrite)
      (": n" . eglot-rename)
      (": b" . dape-breakpoint-toggle)
      (": j" . join-things-at)
      (": e" . wrap)
      (": E" . rewrap)
      (": u" . unwrap)


      ;; ("?" . eldoc-box-help-at-point)
      ;; ("<" . (menu-item
      ;;         ,"" nil
      ;;         :filter ,(lambda (&rest args)
      ;;                    (print current-prefix-arg))))
      ;; ("z" . undo-only)
      ;; ("y" . undo-redo)

      ;; (">" . ,(lambda (&rest args)
      ;;           (interactive)
      ;;           (print current-prefix-arg)))

      ;; ("t" . ts-nav-start-on-point)

      ("i" . insert-cmd)

      ;; FIXME: use repeatable yank here instead
      ("v" . smart-yank)
      ("V" . store-last-kill-in-register)
      ("C-y" . insert-register)
      ("C-v" . clipboard-yank)
      
      ;; ("I" . ,(act-on-thing-new #'insert-beginning-of-region))
      ;; ("A" . ,(act-on-thing-new #'insert-end-of-region))

      ;; FIXME vc on g prefix?
      ;; ("v o" . magit)

      ;; ("s" . set-current-thing)

      ;; FIXME: multi-cursor on selection, "normal" cursor when only one, replaceing I/A?
      ;; also, free S for mark+next/prev wide selection by having e or E act as S if no selection is present?
      ("<tab>" . multi-select-things-at-point)
      ("S-<tab>" . ,(with-tmp-thing
                        (lambda ()
                          (interactive)
                          (call-interactively #'multi-select-things-at-point))))

      ;; TODO: If good, use in visual state for pop-region
      ;; Or insteal C-[ with C-] reversing?
      ("<backspace>" . multi-select-pop)
      ("C-<backspace>" . multi-select-clear)
      
      ("]" . multi-select-extend-by-thing)
      ("[" . multi-select-extend-by-thing-reverse)

      ;; TODO: doesn't make sense, too similar to M
      ("E" . ,(lazydef
               (if (has-multi-selection-p)
                   #'multi-select-into-region
                 (with-tmp-thing
                     (lambda ()
                       (interactive)
                       (call-interactively #'multi-select-things-at-point)
                       ;; Reuses prefix arg, but only uses its for getting a direction,
                       ;; so we actually want to use it here.
                       (call-interactively #'multi-select-into-region))))))
      ("e" . ,(lazydef
               (if (has-multi-selection-p)
                   (keymap-from-alist multi-select-actions)
                 (with-tmp-thing
                     (lambda ()
                       (interactive)
                       (call-interactively #'multi-select-things-at-point)
                       (set-transient-map (keymap-from-alist multi-select-actions)
                                          nil nil "Act on multi-selection"))))))
      
      ;; ("f" . ,(identity #'end-of-thing-cmd-traced))
      ;; ("b" . ,(identity #'beginning-of-thing-cmd-traced))
      ;; ("n" . ,(identity #'next-thing-traced))
      ;; ("p" . ,(identity #'previous-thing-traced))
      ;; FIXME: add variant with included n/p movement as "outer" version?
      ;; ("m" . mark-things)
      ;; ("M" . ,(with-new-thing #'mark-things))

      ("C-f" . ,(with-tmp-thing #'end-of-thing-cmd-traced))
      ("C-b" . ,(with-tmp-thing #'beginning-of-thing-cmd-traced))
      ("C-n" . ,(with-tmp-thing #'next-thing-traced))
      ("C-p" . ,(with-tmp-thing #'previous-thing-traced))
      
      ;; FIXME: use smooth scrolling for this
      ;; ("C-z" . recenter)
      ;; ("z b" . ,(lambda ()
      ;;             (interactive)
      ;;             (recenter-top-bottom -1)))
      ;; ("z t" . ,(lambda ()
      ;;             (interactive)
      ;;             (recenter-top-bottom 0)))

      ))

  (map 'visual
    `(,@digit-bindings
      ("h" . "<left>")
      ("j" . "<down>")
      ("k" . "<up>")
      ("l" . "<right>")

      ;; TODO: pop-region, consider storing also window position for this

      ;; ("G" . st-to-secondary)
      ;; ("M" . swap-st-with-secondary)

      ;; ("t" . ,(with-new-thing #'ignore))
      ("m" . mark-things)

      ("n" . next-thing)
      ("p" . previous-thing)
      ("f" . end-of-thing-cmd)
      ("b" . beginning-of-thing-cmd)

      ("]" . mark-next-thing)
      ("[" . mark-previous-thing)

      ("C-h" . ,(with-new-thing #'backward-down 'sexp))
      ("C-j" . ,(with-new-thing #'forward-down 'sexp))
      ("C-k" . ,(with-new-thing #'backward-up 'sexp))
      ("C-l" . ,(with-new-thing #'forward-up 'sexp))
      ;; FIXME: Seems one of the up/down commands behaves differently when region is active
      ("M-h" . ,(with-new-thing #'backward-down-marked 'sexp))
      ("M-j" . ,(with-new-thing #'forward-down-marked 'sexp))
      ("M-k" . ,(with-new-thing #'backward-up-marked 'sexp))
      ("M-l" . ,(with-new-thing #'forward-up-marked 'sexp))

      ("C-s" . isearch-forward-region)
      ("C-r" . isearch-backward-region)

      ("R" . ,(with-new-thing #'multi-select-things-in-region))

      ("<escape>" . (lambda ()
                      (interactive)
                      (deactivate-mark)))

      
      ("q" . abort-visual)

      ("SPC" . meow-keypad)
      ("o" . exchange-point-and-mark)
      
      ("s" . multi-select-region)
      ("S" . set-current-thing)

      ;; Act on region
      ("e" . wrap)
      ("x" . kill-region)
      ("d" . delete-region)
      ("TAB" . indent-region)
      (";" . comment-dwim)
      ("i" . ,(entering-insert-state #'kill-region))
      ("c" . kill-ring-save)
      ("v" . smart-replace)))

  (map 'visrec
    `(,@digit-bindings
      ("h" . "<left>")
      ("j" . "<down>")
      ("k" . "<up>")
      ("l" . "<right>")

      ("s" . ,(with-new-thing #'ignore))
      ("m" . mark-things)

      ;; TODO: rename-thing-t-point

      ("n" . next-thing)
      ("p" . previous-thing)
      ("f" . end-of-thing-cmd)
      ("b" . beginning-of-thing-cmd)

      ("C-h" . ,(with-new-thing #'backward-down 'sexp))
      ("C-j" . ,(with-new-thing #'forward-down 'sexp))
      ("C-k" . ,(with-new-thing #'backward-up 'sexp))
      ("C-l" . ,(with-new-thing #'forward-up 'sexp))

      ("<escape>" . (lambda ()
                      (interactive)
                      (deactivate-mark)))

      ("q" . abort-visual)
      ("SPC" . meow-keypad)

      ;; TODO: upper/lower
      ("w" . kill-ring-save)
      ("o" . exchange-point-and-mark)
      ("I" . string-insert-rectangle)
      ("i" . string-rectangle)
      ("y" . yank-rectangle)
      ;; ("#" . rectangle-number-lines)
      ;; ("x" . delete-whitespace-rectangle)
      ;; ("X" . delimit-columns-rectangle)
      ;; ("c" . clear-rectangle)
      ("d" . kill-rectangle)
      ;; ("O" . open-rectangle)
      ))

  (map 'insert
    `(
      ;; Note: as few possible here to be as "transparent" as possible.
      ;; ("C-g" . meow-insert-exit)
      ;; ("C-n" . company-complete)
      ;; ("<return>" . ,(dispatch
      ;; `(mode prog-mode comment-indent-new-line)
      ;; ))
      ;; ("<return>" . newline)
      ))

  (map 'motion
    `(,@(alist-map (lambda (key cmd)
                     (cons (concat "C-w " key) cmd))
                   window-nav-keys)
        ("h" . "<left>")
        ("j" . "<down>")
        ("k" . "<up>")
        ("l" . "<right>")
        ("SPC" . meow-keypad)
        ;; Bound by meow to alt-buffer, unbind it
        ("<escape>" . nil)))

  (map global-map
    `(,@-windmove-bindings
      ("C-]" . embark-act)
      ("M-]" . embark-dwim)
      ("M-o" . pop-to-mark-command)
      ("C-o" . pop-global-mark)
      ("C-`" . abort-recursive-edit)
      ("C-d" . smooth-scroll-half-window-down)
      ("C-u" . smooth-scroll-half-window-up)
      ("C-SPC" . meow-keypad)
      ("TAB" . tab-to-tab-stop)

      ;; Experimentally here instead of in normal state map
      ("M-n" . next-error)
      ("M-p" . previous-error)

      ("C-/" . undo-only)
      ("M-/" . undo-redo)
      ("C-M-/" . vundo)
      
      ;; TODO: Have this toggle max instead
      ;; ("C-=" . winmax2-max)
      ))

  (map tab-prefix-map
    `(("i" . tab-previous)))

  ;; (setq meow-use-keypad-when-execute-kbd nil)
  (map mode-specific-map
    `(("C-d" . ,meow-motion-remap-prefix)
      ;; ("a" . mode-line-other-buffer)
      ;; ("w" . ace-window) ;; in normal?
      ("i" . ace-window)
      ("b" . consult-buffer)

      ("r" . ,ctl-x-r-map)
      ("t" . ,tab-prefix-map)
      ("v" . ,vc-prefix-map)
      ("k" . ,kmacro-keymap)
      ("d" . ,(bind-from "C-x C-a"))

      ;; FIXME: nicer pulsing (faster/lighter face)
      ("1" . ,(setting-thing 'word))
      ("2" . ,(setting-thing 'symbol))
      ("3" . ,(setting-thing 'inner-line))
      ("4" . ,(setting-thing 'line))
      ("7" . ,(setting-thing 'paragraph))
      ("8" . ,(setting-thing 'defun))

      ("s" . set-current-thing)
      
      ;; Configured by globl state, so here instead of in normal state map
      ("; c" . recompile)
      ("; C" . compile)

      ;; Change settings
      ("\\ m" . consult-minor-mode-menu)
      ("\\ c" . toggle-comments)
      ("\\ d" . dirvish-side)
      ("\\ D" . toggle-debug-on-error)
      ("\\ s" . window-toggle-side-windows)
      ("\\ v" . set-variable)
      ("\\ e" . ,(lambda ()
                   (interactive)
                   (setq flymake-show-diagnostics-at-end-of-line
                         (not flymake-show-diagnostics-at-end-of-line))
                   ;; Restart mode to make change active
                   (when flymake-mode
                     (flymake-mode -1)
                     (flymake-mode +1))))
      
      ,@(with-prefix "? " local-options)

      ;; Open things
      ;; ("o w" . other-window)
      ("o a" . mode-line-other-buffer)
      ("o j" . ,(entering-insert-state #'org-journal-new-entry))
      ;; ("o i" . ,(lambda ()
      ;;             (interactive)
      ;;             (find-file user-init-file)))
      ("o t" . eat)
      ("o d" . eldoc-doc-buffer)
      ;; ("o h" . eglot-hierarchy-both)
      ;; ("o u" . dired-jump)
      
      ("o m" . ,(lambda ()
                  (interactive)
                  (when-let ((win (display-buffer "*Messages*")))
                    (with-selected-window win
                      (goto-char (point-max))))))
      
      ("o i" . pop-to-mark-command)
      ("o u" . pop-global-mark)

      ("o f" . ,(lambda ()
                  (interactive)
                  (consult-fd (if (derived-mode-p 'dired-mode)
                                  dired-directory nil))))
      ("o p" . avy-goto-char-2)

      ;; ("f b" . consult-recent-files)
      ("o g" . ,(lambda ()
                  (interactive)
                  (consult-ripgrep (if (derived-mode-p 'dired-mode)
                                       dired-directory nil))))
      
      ("o G" . ,(lambda ()
                  (interactive)
                  (consult-git-grep (if (derived-mode-p 'dired-mode)
                                        dired-directory nil))))
      
      ("o l" . (lambda ()
                 (interactive)
                 (consult-line-multi t)))
      
      ("o n" . (lambda ()
                 (interactive)
                 (let ((consult-async-min-input 1))
                   (consult-fd "~/Notes"))))

      ("a" . avy-goto-char-2)


      ("]" . next-error)
      ("[" . previous-error)
      ("=" . recenter-current-error)

      ("p" . ,project-prefix-map)

      ("P s" . profiler-start)
      ("P e" . profiler-stop)
      ("P r" . profiler-report)
      )))

;; TODO: broken
;; (map tab-bar-mode-map
;;   `("M-<tab>" . tab-previous))

(with-eval-after-load 'vterm
  (map vterm-copy-mode-map
    `(("<remap> <insert-cmd>" . ,(adviced
                                  #'insert-cmd
                                  :what "Switching off `vterm-copy-mode'"
                                  :before (lambda (&rest _)
                                            (vterm-copy-mode -1))))))
  (map vterm-mode-map
    `(("<remap> <meow-insert-exit>" . ,(adviced
                                        #'meow-insert-exit
                                        :what "Switching on `vterm-copy-mode'"
                                        :before (lambda (&rest _)
                                                  (vterm-copy-mode +1)))))))

(map eat-mode-map
  `(("<remap> <insert-cmd>" . ,(adviced
                                #'insert-cmd
                                :what "Switching to `eat-char-mode'"
                                :before (lambda (&rest _)
                                          (eat-char-mode)
                                          (eat--synchronize-scroll '(buffer)))))
    ("<remap> <meow-insert-exit>" . ,(adviced
                                      #'meow-insert-exit
                                      :what "Switching to `eat-emacs-mode'"
                                      :before (lambda (&rest _)
                                                (eat-emacs-mode))))))

(map eat-char-mode-map
  `(,@-windmove-bindings
    ("C-v" . eat-yank)
    ("M-x" . execute-extended-command)
    ("C-h" . ,help-map)
    ("C-c" . nil)
    ("C-c C-c" . self-insert-command)
    
    
    ("M-[" . ,(with-let ((last-command-event 27))
                self-insert-command))
    ("C-w" . eat-self-input)))


(map-in-mode 'prog-mode
  `(("C-<return>" . crux-smart-open-line)
    ("M-<return>" . crux-smart-open-line-above)
    ("C-k" . eldoc-box-help-at-point)

    ("M-n" . ts-nav-next-cousin)
    ("M-p" . ts-nav-prev-cousin)
    ;; ("M-m" . ,(with-new-thing #'mark-things 'word))

    ("<remap> <newline>" . comment-indent-new-line)
    ("C-;" . comment-dwim)))

(map-in-mode 'comint-mode
  `(("<remap> <mouse-set-point>" . ,(adviced
                                     #'mouse-set-point
                                     :what "Switches to normal mode."
                                     :after (lambda (&rest _)
                                              (when (equal meow--current-state 'insert)
                                                (meow-insert-exit))))) 
    ("<remap> <insert-cmd>" . ,(adviced
                                #'insert-cmd
                                :what "Goes to process mark"
                                :after (lambda (&rest _)
                                         (end-of-buffer))))))


(map-in-mode '(text-mode prog-mode)
  `(("C-y" . smart-yank)
    ("C-<return>" . open-line)
    ("C-w" . sp-backward-delete-word)))



;; working/used?
(define-minor-mode my-remaps-mode
    "Remaps of vanilla commands to plugin provided ones. Packaged as minor mode to allow for an easier reversal."
  :global t
  :keymap (define-keymap :name "my remaps"
            "<remap> <switch-to-buffer>" #'consult-buffer))

(my-remaps-mode)


(provide 'keybindings)
