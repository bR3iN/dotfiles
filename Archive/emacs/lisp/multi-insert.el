;; -*- lexical-binding: t; -*-

(require 'common)

(define-minor-mode multi-insert-mode
    "Multi insert"
  :lighter " mi"
  (if multi-insert-mode
      (if (not multi-insert-cursors)
          (meow--switch-state 'insert)
        (setq mi--point-anchor (point-marker))
        (add-hook 'after-change-functions #'mi--after-change-hook nil t)
        (add-hook 'before-change-functions #'mi--before-change-hook nil t)
        (meow--switch-state 'insert)
        (hook-once 'meow-switch-state-hook (lambda (state)
                                             (unless (equal state 'insert)
                                               (multi-insert-clear-cursors)
                                               (multi-insert-mode -1)))
                   nil t))
    (remove-hook 'after-change-functions #'mi--after-change-hook t)
    (remove-hook 'before-change-functions #'mi--before-change-hook t)
    (setq mi--point-anchor nil)))

(defun pop-into-multi-insert ()
  (interactive)
  (setq multi-insert-cursors (sort multi-insert-cursors))
  (goto-char (pop multi-insert-cursors))
  (multi-insert-mode))

(defun multi-insert-at-pt ()
  (interactive)
  (setq multi-insert-cursors (sort multi-insert-cursors))
  (multi-insert-mode))

(cl-defstruct mi--change
  offset-to-anchor
  inserted-text
  replaced-len)

(defun mi--replay-change (change anchor)
  (let* ((mi--replaying-change t)
         (offset (mi--change-offset-to-anchor change))
         (start (+ anchor offset)))
    (when-let ((len (mi--change-replaced-len change)))
      ;; Move anchor mirroring how the point anchor behaves when "deleting" through it
      (when (> len offset)
        (set-marker anchor (max 0 (- anchor (- len offset)))))
      (delete-region (- start len) start)
      (setq start (- start len)))
    (when-let ((text (mi--change-inserted-text change)))
      (save-excursion
        (goto-char start)
        (insert text)))))

(defun mi--change-from-hook (pre-change-anchor beg end len)
  (make-mi--change
   :offset-to-anchor (- (+ beg len) pre-change-anchor)
   :inserted-text (unless (equal beg end)
                    (buffer-substring-no-properties beg end))
   :replaced-len len))

(defvar-local mi--pre-change-point-anchor nil)

(defun mi--before-change-hook (&rest _)
  (setq mi--pre-change-point-anchor (marker-position mi--point-anchor)))

(defun mi--after-change-hook (beg end len)
  ;; (print (list beg end len))
  (unless mi--replaying-change
    (let ((change (mi--change-from-hook mi--pre-change-point-anchor
                                        beg end len)))
      (dolist (cursor multi-insert-cursors)
        (mi--replay-change change cursor))
      )))

(defface mi--bar-cursor
    `((t :inherit cursor))
  "")

(defface mi--line-cursor
    `((t))
  "")

(defvar-local mi--cursor-overlays '())

(defun mi--clear-overlays ()
  (dolist (overlay mi--cursor-overlays)
    (delete-overlay overlay))
  (setq mi--cursor-overlays '()))


(defun mi--cursor-face ()
  (if (equal meow--current-state 'insert)
      
      'mi--bar-cursor))

(defun mi--update-line-face ()
  (let ((color (face-attribute 'cursor :background nil t)))
    (set-face-attribute 'mi--line-cursor nil
                        :box `(:line-width -1))))

(defun mi--make-cursor-overlay (pos)
  (if (equal meow--current-state 'insert)
      (if (save-excursion (goto-char pos) (eolp))
          (let ((overlay (make-overlay pos pos)))
            (overlay-put overlay 'after-string "|")
            overlay)
        (let ((overlay (make-overlay pos (1+ pos))))
          (overlay-put overlay 'face 'mi--line-cursor)
          overlay))
    (let ((overlay (make-overlay pos (1+ pos))))
      (overlay-put overlay 'face 'mi--bar-cursor)
      overlay))
  )

(defun mi--render-cursors ()
  (mi--clear-overlays)
  (mi--update-line-face)  
  (let ((face 'mi--bar-cursor))
    (dolist (cursor multi-insert-cursors)
      (let* ((offset (if mi--point-anchor
                         (- (point) mi--point-anchor)
                       0))
             (pos (+ cursor offset))
             (overlay (make-overlay pos (1+ pos))))
        (overlay-put overlay 'face 'mi--bar-cursor)
        (push overlay mi--cursor-overlays)))))

(add-hook 'post-command-hook #'mi--render-cursors)

(defun multi-insert-create-cursor (pos)
  (interactive "d")
  (add-to-list 'multi-insert-cursors (marker-with-type pos)))

(defun multi-insert-toggle-cursor (pos)
  (interactive "d")
  (mi--toggle-insert-cursor (marker-with-type pos)))

(defun multi-insert-clear-cursors ()
  (interactive)
  (setq multi-insert-cursors '()))


(defun mi--toggle-insert-cursor (cursor)
  (let ((last-node nil)
        (curr-node multi-insert-cursors))
    ;; Traverse strictly previous cursorements
    (while (and curr-node (value< (car curr-node) cursor))
      (setq last-node curr-node
            curr-node (cdr curr-node)))
    ;; Insert or remove
    (if (and curr-node (equal (car curr-node) cursor))
        (setq curr-node (cdr curr-node))
      (setq curr-node (cons cursor curr-node)))
    ;; Patch together list
    (if last-node
        (setcdr last-node curr-node)
      (setq multi-insert-cursors curr-node))))


(defvar-local multi-insert-cursors '())
(defvar-local mi--point-anchor nil)
(defvar-local mi--replaying-change nil)

(provide 'multi-insert)
