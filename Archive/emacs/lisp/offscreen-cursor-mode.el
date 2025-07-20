;; -*- lexical-binding: t; -*-

(require 'cl-lib)

;; TODO: allow configuration of multiple cursor-faces to hide/reset
(defvar oc-cursor-faces '(cursor meow-normal-cursor))


(cl-defstruct oc--snapshot
  point
  mark
  region-active
  overlays
  window-start)

(defvar-local oc--current-snapshot nil)

(defun oc--has-snapshot-p ()
  (when oc--current-snapshot t))

(defun oc--create-cursor-overlay (pos)
  (let ((overlay (make-overlay pos (1+ pos))))
    (overlay-put overlay 'face (list :background (face-attribute 'cursor :background nil t)
                                     :foreground (face-attribute 'default :background nil t)))
    (overlay-put overlay 'window (selected-window))
    overlay))

(defun oc--create-region-overlay (beg end)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'region)
    (overlay-put overlay 'window (selected-window))
    overlay))

(defun oc--create-snapshot ()
  (when oc--current-snapshot
    (error "Snapshot already exists for this buffer"))
  (let* ((region-active (region-active-p))
         (pt (point))
         (mk (mark))
         (overlays `(,(oc--create-cursor-overlay pt)
                     ,@(when region-active
                         `(,(oc--create-region-overlay pt mk))))))
    (when region-active
      (deactivate-mark))
    (setq oc--current-snapshot (make-oc--snapshot
                                :point pt
                                :mark mk
                                :region-active region-active
                                :overlays overlays
                                :window-start (window-start)))))

(defun oc--restore-snapshot ()
  (pcase oc--current-snapshot
    ('nil (error "No current snapshot"))
    ((cl-struct oc--snapshot (point pt) (mark mk)
                region-active window-start overlays)
     (dolist (overlay overlays)
       (delete-overlay overlay))
     (set-window-start (selected-window) window-start)
     (push-mark mk t region-active)
     (goto-char pt)))
  (setq oc--current-snapshot nil))

(defun oc--drop-snapshot ()
  (pcase oc--current-snapshot
    ((cl-struct oc--snapshot overlays)
     (dolist (overlay overlays)
       (delete-overlay overlay))))
  (setq oc--current-snapshot nil))


(defvar oc--hidden-cursors '()
  "Alist of windows which cursor we hid together with its old `cursor-type' value.'")

(defun oc--cursor-hidden-p ()
  "Did we hide the cursor in the current window?"
  (when (alist-get (selected-window) oc--hidden-cursors)
    t))

(defun oc--hide-cursor ()
  "Hide cursor in current window."
  (let ((win (selected-window)))
    (if (alist-get win oc--hidden-cursors)
        (error "Cursor already hidden")
      (setf (alist-get win oc--hidden-cursors) (window-cursor-type))
      (set-window-cursor-type win nil))))

(defun oc--unhide-cursor ()
  (let* ((win (selected-window))
         (el (alist-get win oc--hidden-cursors)))
    (unless el
      (error "Cursor not hidden"))
    (set-window-cursor-type win el)
    (setf (alist-get win oc--hidden-cursors nil t) nil)))

(defun oc--refresh-cursor-visibility (&rest _)
  (if (oc--has-snapshot-p)
      (unless (oc--cursor-hidden-p)
        (oc--hide-cursor))
    (when (oc--cursor-hidden-p)
      (oc--unhide-cursor))))

(add-hook 'post-command-hook #'oc--refresh-cursor-visibility)

(define-minor-mode offscreen-cursor-mode
  ""
  :lighter " oc"
  (if offscreen-cursor-mode
      (progn
        )
    (when (oc--has-snapshot-p)
      (oc--drop-snapshot)
      (oc--refresh-cursor-visibility))))

(defun oc-detach-cursor()
  )


(provide 'offscreen-cursor-mode)
