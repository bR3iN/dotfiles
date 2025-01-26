;; -*- lexical-binding: t; -*-

(require 'utils)

(defvar -protected-windows '())

;; Callback instead as optional arg?
(defun protect-window (window &optional other-window-on-quit-instead)
  (setf (alist-get window -protected-windows) other-window-on-quit-instead))

(defun unprotect-window (window)
  (setf (alist-get window -protected-windows nil t) nil))

(defun -quit-window-advice (&optional _kill curr-win)
  (let ((curr-win (or curr-win (selected-window))))
    (alist-map
      (lambda (win other-window-on-quit)
        (when (eq win curr-win)
          (when other-window-on-quit
            (other-window))
          (error "window is protected and can't be killed")))
      -protected-windows)))

(advice-add 'quit-window
            :before #'-quit-window-advice)

(provide 'protect-windows)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "protect-windows--"))
;; End:
