;; -*- lexical-binding: t; -*-

(require 'utils)
(require 'color-utils)

(defvar ts-named-only t)
(defvar ts-highlight-children t)

(let* ((cursor (colored-region :green 0.15 10))
       (child-even cursor)
       (child-odd cursor)
       ; (child-even (colored-region :yellow 0.15 -10))
       ; (child-odd (colored-region :yellow 0.15 -10))
       (sibling-even (color-of-region 25))
       (sibling-odd (colored-region :green 0.1 30)))
  (defface ts-nav-cursor
           `((t :inherit region :background ,cursor))
           "Cursor during treesitter navigation")
  (defface ts-nav-child-even
           `((t :inherit region :background ,child-even))
           "Even child nodes during treesitter navigation")
  (defface ts-nav-child-odd
           `((t :inherit region :background ,child-odd))
           "Odd child nodes during treesitter navigaton")
  (defface ts-nav-sibling-even
           `((t :background ,sibling-even))
           "Even sibling nodes during treesitter navigation")
  (defface ts-nav-sibling-odd
           `((t :background ,sibling-odd))
           "Odd sibling nodes during treesitter navigaton"))


(defvar-local ts--nav-cursor nil)

(defun ts-get-current-node ()
  (when ts--nav-cursor
    (nth 0 ts--nav-cursor)))

(defun ts-get-children ()
  (when ts--nav-cursor
    (nth 1 ts--nav-cursor)))


; (defun ts--get-current-node-overlay ()
;   (when ts--nav-cursor
;     (cdr ts--nav-cursor)))


(defun ts--has-cursor-overlay-p ()
  (and (boolify ts--nav-cursor)
       (boolify (nth 2 ts--nav-cursor))))


(defun ts-delete-cursor-overlay ()
  (when (ts--has-cursor-overlay-p)
    (mapcar #'delete-overlay (nth 2 ts--nav-cursor))
    (setf (nth 2 ts--nav-cursor) nil)))


(defun ts--type-to-priority (type)
  (pcase type
         ('cursor 2)
         ('child 3)
         ('sibling 1)))


(defun ts--overlay-node (face node type)
  (let* ((start (treesit-node-start node))
         (end (treesit-node-end node))
         (overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'priority (ts--type-to-priority type))
    overlay))


(defun ts--is-named (node)
  (treesit-node-check node 'named))


(defun ts--named-closure (node)
  (treesit-parent-until node #'ts--is-named t))


(defun ts--restore-cursor-overlay ()
  (when (not (ts--has-cursor-overlay-p))
    (let* ((node (nth 0 ts--nav-cursor))
           (is-even nil)
           (siblings (when-let ((parent (treesit-node-parent node)))
                               (treesit-node-children parent ts-named-only)))
           (sibling-overlays (mapcar
                               (lambda (sibling)
                                 (setq is-even (not is-even))
                                 (ts--overlay-node
                                   (if is-even 'ts-nav-sibling-even
                                     'ts-nav-sibling-odd)
                                   sibling
                                   'sibling))
                               siblings))
           (is-even nil)
           (children (nth 1 ts--nav-cursor))
           (child-overlays (when ts-highlight-children
                             (mapcar
                               (lambda (child)
                                 (setq is-even (not is-even))
                                 (ts--overlay-node
                                   (if is-even 'ts-nav-child-even
                                     'ts-nav-child-odd)
                                   child
                                   'child))
                               children)))
           (cursor-overlay (ts--overlay-node 'ts-nav-cursor node 'cursor))
           (overlays (append `(,cursor-overlay) child-overlays sibling-overlays)))
      (setf (nth 2 ts--nav-cursor) overlays))))


(defun ts--set-cursor (node)
  (ts-delete-cursor-overlay)
  (setq ts--nav-cursor (list node (treesit-node-children node ts-named-only) nil))
  (ts--restore-cursor-overlay))


(defvar-local ts--pre-nav-point nil)
(defvar-local ts--nav-activated-region nil)


(defun ts-nav-exit ()
  (interactive)
  (ts-delete-cursor-overlay)
  (setq ts--pre-nav-point nil
        ts--nav-activated-region nil))


(defun ts-nav-abort ()
  (interactive)
  (when ts--nav-activated-region
    (deactivate-mark))
  (let ((pt ts--pre-nav-point))
    (ts-nav-exit)
    (goto-char pt)))


(defun ts-forward-sibling (n)
  (interactive "p")
  (let ((node (ts-get-current-node))
        (forward (> n 0))
        (n (abs n)))
    ; (unless node (error "No next sibling node"))
    (while-let ((guard (> n 0))
                (next (if forward
                        (treesit-node-next-sibling node ts-named-only)
                        (treesit-node-prev-sibling node ts-named-only))))
               (setq node next
                     n (- n 1)))
    (when (not (= n 0))
      (print (concat "No " (if forward "next" "previous") " sibling node")))

    (ts--set-cursor node)
    (goto-char (treesit-node-start node))))


(defun ts-backward-sibling (n)
  (interactive "p")
  (ts-forward-sibling (- n)))


(defun ts-ancestor (n)
  (interactive "p")
  (unless (> n 0) (error "Negative number prefixes are not supported"))
  (let ((node (ts-get-current-node)))
    (while-let ((guard (> n 0))
                (next (treesit-node-parent node)))
               (setq node next
                     n (- n 1)))
    (when (not (= n 0))
      (print "No parent node"))
    (print node)
    (print (treesit-node-start node))
    (ts--set-cursor node)
    (goto-char (treesit-node-start node))))

(defun ts-child (n)
  (interactive "p")
  (unless (> n 0) (error "Negative number prefixes are not supported"))
  (let* ((children (ts-get-children))
         (child (nth (- n 1) children)))
    (if (not child) (error
                      (concat
                        "Child with index " (number-to-string n)
                        " is out of bounds for node with "
                        (number-to-string (length children)) " children"))
      (ts--set-cursor child)
      (goto-char (treesit-node-start child)))))

(defun ts--get-start-node (&optional start-at-leaf)
  (interactive)
  (if start-at-leaf (treesit-node-at (point) nil ts-named-only)
    (let ((start (if (region-active-p) (mark) (point)))
          (end (point)))
      (treesit-node-on start end nil ts-named-only))))

(defun ts-mark-current-node ()
  (interactive)
  (when-let* ((node (ts-get-current-node))
              (start (treesit-node-start node))
              (end (treesit-node-end node)))
  (when (not (region-active-p))
    (setq ts--nav-activated-region t))
  (goto-char end)
  (push-mark start t t)
  node))

; (defun treesit-mark-node-at-point ()
;   (interactive)
;   (when-let* ((node (treesit-node-on (point) (point)))
;               (start (treesit-node-start node))
;               (end (treesit-node-end node)))
;              (ts--set-cursor node)
;              (goto-char end)
;              (push-mark start t t)))


(defun start-ts-nav (&optional start-at-leaf)
  (interactive)
  (if-let ((node (ts--get-start-node start-at-leaf)))
          (let ((pt (point)))
            (push-mark pt t nil)
            ; TODO needed if we push mark? abort could simply pop it? maybe more robust
            (setq ts--pre-nav-point pt)
            (ts--set-cursor node)
            (goto-char (treesit-node-end node))
            (-hydra/body))
          (error "Can't determine start node")))


(defun start-ts-nav-at-leaf ()
  (interactive)
  (start-ts-nav t))


(defun continue-ts-nav ()
  (interactive)
  (if-let ((node (ts-get-current-node)))
          (progn
            (setq ts--pre-nav-point (point))
            (ts--restore-cursor-overlay)
            (goto-char (treesit-node-end node))
            (-hydra/body))
          (error "No previous treesitter navigation")))

(with-eval-after-load
  'hydra
  (defhydra -hydra (:foreign-keys warn
                                        ; :hint nil
                                        )
            "Treesitter Navigation"
            ("m" ts-mark-current-node)
            ("n" ts-forward-sibling)
            ("p" ts-backward-sibling)
            ("a" ts-ancestor)
            ("c" ts-child)
            ("<escape>" ts-nav-exit :exit t)
            ("q" ts-nav-abort :exit t)
            ("C-g" ts-nav-abort :exit t)))

(provide 'ts-nav)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "ts-nav--"))
;; End:
