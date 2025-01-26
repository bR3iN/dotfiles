;; -*- lexical-binding: t; -*-

; (require 'utils)

(defvar-local -region-history '())

(defun -save-region-history ()
  (when (region-active-p)
    (let ((current-region (cons (mark) (point))))
      (when (not (equal current-region (car -region-history)))
        (push current-region -region-history)))))

(add-hook 'post-command-hook #'-save-region-history)

(defun pop-region ()
  (interactive)
  (pop -region-history)
  (when-let* ((region (car -region-history)))
             (set-mark (car region))
             (goto-char (cdr region))))

(defun restore-region ()
  (interactive)
  (when-let* ((region (car -region-history)))
             (push-mark (car region) t (not (region-active-p)))
             (goto-char (cdr region))))

(provide 'restore-region)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "restore-region--"))
;; End:
