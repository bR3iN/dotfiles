;; -*- lexical-binding: t; -*-

(require 'common)

(use-package meow
    :config

  (pcase-dolist
      (`(,mode . ,state)
        `((help-mode . motion)
          (eshell-mode . motion)
          (debugger-mode . motion)
          (messages-buffer-mode . normal)
          (eat-mode . insert)
          (comint-mode . insert)
          (vterm-mode . insert)
          (dape-repl-mode . insert)))
    (setf (alist-get mode meow-mode-state-list) state))
  

  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (setq meow-keypad-self-insert-undefined nil)
  ;; Prefix is bound in leader mode
  (setq meow-motion-remap-prefix "H-a")

  ;; TODO: decide how far to couple to meow as a framework; is `map' in keybindings.el and the meow-<state>-mode functions enough?
  
  ; (defmacro def-state (name-lit indicator desc &optional transparent)
  ;   (let* ((name (symbol-name name-lit))
  ;          (keymap (intern (concat "meow-" name "-state-keymap"))))
  ;     `(progn
  ;        (setq ,keymap (make-keymap))
  ;        (when (not ,transparent)
  ;          (suppress-keymap ,keymap t))
  ;        (meow-define-state ,name-lit
  ;                           ,desc
  ;                           :lighter ,(concat " [" indicator "]")
  ;                           :face 'meow-visual-cursor
  ;                           :keymap ,keymap))))


  ;; (set-face-attribute 'meow-insert-cursor nil :background (base16-get :yellow))
  ;; (set-face-attribute 'meow-normal-cursor nil :background (base16-get :green))
  ;; (set-face-attribute 'meow-visual-cursor nil :background (base16-get :cyan))

  ;; Keep track of last pre-visual mode and re-enter it when deactivating mark
  (defvar-local meow--pre-visual-state nil)

  (defun meow--enter-visual-state-hook ()
    (let ((state (meow--current-state)))
      (when (member state '(normal motion insert))
        (save-local-win-pos)
        (setq meow--pre-visual-state state)
        (clear-current-trace)
        (meow--switch-state (if rectangle-mark-mode
                                'visrec
                              'visual)))))

  (defun meow--leave-visual-state-hook ()
    (when (memq meow--current-state '(visual visrec))
      (meow--switch-state meow--pre-visual-state)
      (setq meow--pre-visual-state nil)))

  (add-hook 'activate-mark-hook #'meow--enter-visual-state-hook)
  (add-hook 'deactivate-mark-hook #'meow--leave-visual-state-hook)

  (defun abort-visual ()
    (interactive)
    (deactivate-mark)
    (restore-local-win-pos))

  (defun entering-insert-state (cmd)
    (adviced cmd
             :what "Enters insert state."
             :after (lambda (&rest _) (meow-insert-mode))))


  (meow-global-mode 1))


;; FIXME: Super finicky with native-compilation
(eval
 '(with-eval-after-load 'meow-helpers
   ;; Visual Mode; automatically entered and left when activating and deactivating region.
   (setq meow-visual-state-keymap (make-keymap))
   (meow-define-state
       visual
     "State during active selection"
     :lighter "[V]"
     :keymap meow-visual-state-keymap
     :face meow-visual-cursor)

   (setq meow-visrec-state-keymap (make-keymap))
   (meow-define-state
       visrec
     "State during active selection"
     :lighter "[R]"
     :keymap meow-visrec-state-keymap
     :face meow-visual-cursor)

   ;; Has to be required somewhere
   (require 'rect)
   ))

(provide 'modality)
