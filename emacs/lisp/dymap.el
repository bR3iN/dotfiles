;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun -test-cond (c)
  (cond
   ((booleanp c) c)
   ((functionp c) (funcall c))
   ((symbolp c) (equal major-mode c))))


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
