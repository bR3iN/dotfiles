;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar -pre-cmd-state nil)

(defun -pre-cmd-hook ()
  (let ((window (selected-window))
        (buffer (window-buffer))
        (window-start (window-start))
        (pt (point))
        (mk (mark))
        (mk-ring-top (car mark-ring)))
    (setq -pre-cmd-state `(:window ,window
                         :window-start ,window-start
                         :buffer ,buffer
                         :point ,pt
                         :mark ,mk
                         :mark-ring ,mk-ring-top))))

(defun -post-cmd-hook ()
  (cl-flet ((r (sym)
              (plist-get -pre-cmd-state sym)))
    (when (and (equal (r :window) (selected-window))
               (equal (r :buffer) (window-buffer))
               (equal (r :mark-ring) (car mark-ring))
               (equal (r :mark) (mark))
               (not (equal (r :point) (point)))
               (not (equal (r :window-start) (window-start))))
      (message "auto-marked")
      (push-mark (r :point) t nil))))

(add-hook 'pre-command-hook #'-pre-cmd-hook)
(add-hook 'post-command-hook #'-post-cmd-hook)
(provide 'auto-mark-mode)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "auto-mark-mode--"))
;; End:
