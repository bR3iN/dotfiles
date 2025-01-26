;; -*- lexical-binding: t; -*-

;; TODO: rename module kmacro -> kbd-macro

(require 'utils)

; (defun -macro-name (name)
;   (intern (concat (symbol-name -macro) "-" name)))

; (defun -end-macro (&optional name)
;   ())


(defvar -macro-mode-line-saved nil)
(defun -hide-or-restore-macro-mode-line ()
  (swap mode-line-defining-kbd-macro -macro-mode-line-saved))


(defvar is-implicit-kbd-macro nil)

(defvar-local -implicit-kmacro nil
              "Cons cell of prefix arg of initial command (as that is not captured by the macro) and the current ")


(defun start-implicit-kmacro ()
  (when (and (not defining-kbd-macro)
             (not executing-kbd-macro))
    (setq is-implicit-kbd-macro t)
    ; (setq -captured-vars (mapcar (lambda (sym) (symbol-value sym))))
    (push (this-command-keys-vector) -implicit-kmacro)
    ; (print -implicit-kmacro)
    (start-kbd-macro nil)))

(defun end-implicit-kmacro ()
  (when (and defining-kbd-macro is-implicit-kbd-macro)
    (-suspend-implicit-kmacro)
    ; (print (this-command-keys))
    (push (let ((this (this-command-keys-vector)))
            (if (length> this 0) this
              -last-cmd-keys-vector))
            -implicit-kmacro)
    (let ((kbd-macro (apply #'vconcat (reverse -implicit-kmacro))))
      ; (print kbd-macro)
      (setq last-kbd-macro kbd-macro)
      (kmacro-push-ring kbd-macro)
      (setq -implicit-kmacro '()))))

(defun -suspend-implicit-kmacro ()
  ; (print "suspended")
  (let ((last-kbd-macro last-kbd-macro))
    (end-kbd-macro)
    (setq is-implicit-kbd-macro nil)
    (push last-kbd-macro -implicit-kmacro)))

(defun -continue-implicit-kmacro ()
  (print "continued")
  (setq is-implicit-kbd-macro t)
  (start-kbd-macro nil))

(defun abort-implicit-kmacro ()
  (print "aborted")
  (end-kbd-macro)
  (setq is-implicit-kbd-macro nil)
  (kmacro-pop-ring))

(advice-add 'kmacro-start-macro
            :before (lambda (&rest _)
                      (when (and defining-kbd-macro is-implicit-kbd-macro)
                        (abort-implicit-kmacro))))

(defvar -last-cmd-keys-vector nil)
(defun -track-last-cmd-keys-vector ()
  (setq -last-cmd-keys-vector (this-command-keys-vector)))

(add-hook 'post-command-hook #'-track-last-cmd-keys-vector)


(defvar-local -current-dot-cmd nil)

(defun dot-repeat (&optional n)
  (let ((n (or n 1)))
    (when (< n 1) (error "numeric prefix must be positive"))
    (when (not -current-dot-cmd) (error "nothing to repeat"))
    (dotimes (_ n)

; (defun start-or-end-kmacro ()
;   (if (or (not defining-kbd-macro)
;           (and defining-kbd-macro -is-im
;   )

(provide 'kmacro-utils)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "kmacro-utils--"))
;; End:
