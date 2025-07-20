;; -*- lexical-binding: t; -*-

;; Idea: Show only filename in all modelines until next cmd is executed

(defvar qbp--orig-mode-lines nil)

(defun qbp--set-mode-line (win mode-line)
  (with-selected-window win
    (setq-local mode-line-format mode-line)))

(defun qbp--restore ()
  (when qbp--orig-mode-lines
    (pcase-dolist (`(,win . ,mode-line) qbp--orig-mode-lines)
      (when (window-live-p win)
        (qbp--set-mode-line win mode-line)))
    (setq qbp--orig-mode-lines nil)))

(add-hook 'pre-command-hook 'qbp--restore)

(defun qbp--mode-line-segment ()
  '(" test "))

(defun qbp-modeline ()
  (interactive)
  (let ((wins (window-list))
        (mode-line '((:eval (qbp--mode-line-segment)))))
    (setq qbp--orig-mode-lines (mapcar
                                (lambda (win)
                                  `(,win . ,(with-selected-window win
                                              mode-line-format)))
                                wins))
    (dolist (win wins)
      (qbp--set-mode-line win mode-line))))


(provide 'quick-buffer-paths)
