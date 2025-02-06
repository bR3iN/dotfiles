;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun -test-cond (c)
  (cond
   ((booleanp c) c)
   ((and (symbolp c) (memq c minor-mode-list)) (symbol-value c))
   ((symbolp c) (equal major-mode c))
   ((functionp c) (funcall c))))

(defun mode-p (mode)
  (lambda ()
    (equal major-mode mode)))

(defun true-p (v)
  (lambda () v))

;; TODO: named inconsistently
(defun dynmap (map-alist)
  (lambda ()
    (interactive)
    (if-let ((cmd (cl-assoc-if #'-test-cond map-alist)))
        (call-interactively (cdr cmd))
      (error "No keybinding valid in current context"))))

(provide 'dymap)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "dymap--"))
;; End:
