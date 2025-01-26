;; -*- lexical-binding: t; -*-

(defvar -pre-winmax-winconfig nil)

(defun winmax-active-p ()
  (if -pre-winmax-winconfig t nil))

(defun winmax-max (&optional win)
  (interactive)
  (let ((win (window-normalize-window win t)))
    (-save-winconfig)
    (delete-other-windows win)))

(defun winmax-toggle (&optional win)
  (interactive)
  (if (winmax-active-p) (winmax-restore)
    (winmax-max)))

(defun winmax-dwim (&optional n win)
  "When there is only one window, try to restore the original unmaximized window layout. If there are multiple windows, maximize along the next N splits if N is positive, fully maximize the current window if N is zero and restore the original layout when N is negative."
  (interactive "p")
  (let ((win (window-normalize-window win t))
        (parent (window-parent win))
        (n (or n 1)))
    (cond
     ((and parent (> n 0)) (winmax-split n win))
     ((and parent (= n 0)) (winmax-max))
     ;; No parent or negative N
     (t (winmax-restore)))))

(defun winmax-split (&optional n win)
  (interactive "p")
  (unless (> n 0)
    (error "numeric argument must be positive"))
  (let ((win (window-normalize-window win t))
        (parent (window-parent win)))
    (when parent
      (-save-winconfig))
    (while (> n 0)
      (unless parent
        (error "No further window split"))
      (-delete-children-not win parent)
      (setq parent (window-parent win)
            n (1- n)))))

(defun -delete-children-not (win parent)
  (let ((curr-win (window-child parent))
        (mb-win (minibuffer-window)))
    (while curr-win
      ;; Have to get sibling before deleting curr-win
      (let ((next (window-next-sibling curr-win)))
        (unless (or (equal win curr-win)
                    (and mb-win (equal mb-win curr-win)))
          (delete-window curr-win))
        (setq curr-win next)))))

(defun -save-winconfig ()
  (unless -pre-winmax-winconfig
    (setq -pre-winmax-winconfig (current-window-configuration))))

(defun winmax-restore (&optional interactive)
  (interactive '(t))
  (if (not -pre-winmax-winconfig) (when interactive
                                    (error "Nothing to restore"))
    (set-window-configuration -pre-winmax-winconfig)
    (setq -pre-winmax-winconfig nil)))

(defvar -saved-winconfig nil)

(defun winmax2-max (&optional window)
  (interactive)
  (winmax-restore)
  (-save-winconfig)
  (maximize-window window))

(provide 'winmax)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "winmax--"))
;; End:
