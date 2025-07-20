;; -*- lexical-binding: t; -*-

(require 'common)

(use-package treesit
    :straight nil)
(eval-and-compile
  (use-package hydra))

(let* ((cursor (colored-region :green 0.15 10))
       (context (colored-region :red 0.15 10))
       (child-even cursor)
       (child-odd cursor)
                                        ; (child-even (colored-region :yellow 0.15 -10))
                                        ; (child-odd (colored-region :yellow 0.15 -10))
       (sibling-even (color-of-region 25))
       (sibling-odd (colored-region :green 0.1 30)))
  (defface ts-nav-cursor
      `((t :inherit region :background ,cursor))
    "Cursor during treesitter navigation")
  (defface ts-nav-context
      `((t :inherit region :background ,context))
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



;; Utilities for treesit nodes

(defun ts-nav--node-bounds (node)
  (cons (treesit-node-start node) (treesit-node-end node)))

(defun ts-nav--bounds-len (bounds)
  (- (cdr bounds) (car bounds)))

(defun ts-nav--node-is-named (node)
  (treesit-node-check node 'named))

(defun ts-nav--equal-bounds-p (node1 node2)
  (equal (ts-nav--node-bounds node1) (ts-nav--node-bounds node2)))

(defun ts-nav--node-move-inner (node forward)
  (if forward (treesit-node-next-sibling node)
    (treesit-node-prev-sibling node)))

;; (defun ts-nav--node-child-id (node)
;;   (when-let ((parent (treesit-node-parent node)))
;;     (catch 'found
;;       (let ((id 0))
;;         (dolist (child (treesit-node-children parent))
;;           (if (treesit-node-eq node child)
;;               (throw 'found (cons id (treesit-node-field-name-for-child node id)))
;;             (setq id (1+ id)))))
;;       (error "Can't find node inside parents children"))))

;; (defun ts-nav--up-node-with-id (node)
;;   (when-let ((parent (treesit-node-parent node)))
;;     (catch 'found
;;       (let ((id 0))
;;         (dolist (child (treesit-node-children parent))
;;           (if (treesit-node-eq node child)
;;               (throw 'found (cons parent
;;                                   (cons id (treesit-node-field-name-for-child node id))))
;;             (setq id (1+ id)))))
;;       (error "Can't find node inside parents children"))))


;; Pseudo nodes

(cl-defstruct ts-nav--pnode
  "Treesitter pseudo node consisting of a maximal set of nodes having the same bounds."
  (nodes '() :read-only t)
  (bounds nil :read-only t)
  (named nil :read-only t))


(defun ts-nav--pnode-new (node)
  "Get the unique `ts-nav--pnode' containing NODE. If NODE is nil, return nil."
  (when node
    (let ((nodes-up'())
          (nodes-down '())
          (bounds (ts-nav--node-bounds node))
          (named nil))
      ;; Gather parents with same bounds
      (let ((curr node))
        (while (and curr
                    (equal (ts-nav--node-bounds curr) bounds))
          (push curr nodes-up)
          (when (ts-nav--node-is-named curr)
            (setq named t))
          (setq curr (treesit-node-parent curr))))
      ;; Gather direct children with same bounds
      (let ((curr node))
        (while curr
          (push curr nodes-down)
          (when (ts-nav--node-is-named curr)
            (setq named t))
          ;; Check if there is a children with the same size
          ;; TODO: It looks like error nodes can have length 0, but double check this.
          (let ((children (treesit-node-children curr)))
            (setq curr nil)
            (dolist (child children)
              (when (equal (ts-nav--node-bounds child) bounds)
                (setq curr child))))))
      (make-ts-nav--pnode
       ;; `cdr' as we recorded `NODE' twice.
       :nodes (append nodes-up (cdr (reverse nodes-down)))
       :bounds bounds
       :named named))))

(defun ts-nav--pnode-any (pnode node-pred)
  "Checks if one treesitter ndoe in PNODE satisfied NODE-PRED."
  (cl-some node-pred (ts-nav--pnode-nodes pnode)))

(defun ts-nav--pnode-first (pnode)
  "Upmost treesitter node part of PNODE."
  (car (ts-nav--pnode-nodes pnode)))

(defun ts-nav--pnode-last (pnode)
  "Downmost treesitter node part of PNODE."
  (car (last (ts-nav--pnode-nodes pnode))))

(defun ts-nav--pnode-start (pnode)
  (car (ts-nav--pnode-bounds pnode)))

(defun ts-nav--pnode-end (pnode)
  (cdr (ts-nav--pnode-bounds pnode)))

(defun ts-nav--pnode-child-count (pnode)
  (thread-first
    pnode
    (ts-nav--pnode-last)
    (treesit-node-child-count)))

(defun ts-nav--pnode-child (pnode n)
  (thread-first
    pnode
    (ts-nav--pnode-last)
    (treesit-node-child n)
    (ts-nav--pnode-new)))

(defun ts-nav--pnode-field-name-for-child (pnode n)
  (thread-first
    pnode
    (ts-nav--pnode-last)
    (treesit-node-field-name-for-child n)))

(defun ts-nav--pnode-child-by-field-name (pnode field-name)
  (thread-first
    pnode
    (ts-nav--pnode-last)
    (treesit-node-child-by-field-name field-name)
    (ts-nav--pnode-new)))

(defun ts-nav--pnode-field-name (pnode)
  (thread-first
    pnode
    (ts-nav--pnode-first)
    (treesit-node-field-name)))

(defun ts-nav--pnode-parent (pnode)
  (thread-first
    pnode
    (ts-nav--pnode-first)
    (treesit-node-parent)
    (ts-nav--pnode-new)))

(defun ts-nav--pnode-children (pnode)
  (thread-last
    pnode
    (ts-nav--pnode-last)
    (treesit-node-children)
    (mapcar #'ts-nav--pnode-new)))


(defun ts-nav--pnode-index (pnode)
  (thread-first
    pnode
    (ts-nav--pnode-first)
    (treesit-node-index)))


(defun ts-nav--pnode-ctx (pnode)
  "Context of PNODE in its parent; returns nil if PNODE is the root, otherwise a cons cell off its child it and field name in its parent node (the latter which might be nil)."
  (cons (ts-nav--pnode-index pnode)
        (ts-nav--pnode-field-name pnode)))


(defun pulse-pnode (pnode)
  (pcase-let ((`(,beg . ,end) (ts-nav--pnode-bounds pnode)))
    (pulse-momentary-highlight-region beg end)))

(cl-defun ts-nav--pnode-step-pre-order (pnode forward &key bounded)
  (or
   (ts-nav--pnode-to-child pnode forward)
   (catch 'stop
     (while (and pnode (or (not bounded)
                           (not (ts-nav--pnode-eq pnode bounded))))
       (if-let ((next (ts-nav--pnode-to-sibling pnode forward)))
           (throw 'stop next)
         (setq pnode (ts-nav--pnode-parent pnode)))))))

(defun ts-nav--pnode-step-reverse-pre-order (pnode forward)
  (if-let ((next (ts-nav--pnode-to-sibling pnode forward)))
      (progn
        (setq pnode next)
        (while-let ((down (ts-nav--pnode-to-child pnode forward)))
          (setq pnode down))
        pnode)
    (ts-nav--pnode-parent pnode)))

(defun ts-nav-test ()
  (interactive)
  (let ((pnode (thread-first
                 (treesit-node-at (point))
                 (ts-nav--get-pnode))))
    (pulse-pnode pnode)))

(defun ts-nav--down-start-node (pos forward)
  (let ((node (ts-nav--pnode-new (treesit-node-at pos))))
    (if forward node
      ))
  )

(defun ts-nav--just-past-pnode-p (pnode pos forward)
  (let ((bounds (ts-nav--pnode-bounds pnode)))
    (= pos (if forward (cdr bounds)
             (car bounds)))))

(defun ts-nav--after-pnode-p (pnode pos forward)
  (let ((bounds (ts-nav--pnode-bounds pnode)))
    (if forward
        (>= pos (cdr bounds))
      (<= pos (car bounds)))))

;; (defun ts-nav--inside-pnode-p (pnode pos forward)
;;   (let ((bounds (ts-nav--pnode-bounds pnode)))
;;     (if forward
;;         (and ()))))

(defun ts-nav--plist-p (pnode)
  (and
   (>= (ts-nav--pnode-child-count pnode) 2)
   (thread-first
     pnode
     (ts-nav--pnode-child 0)
     (ts-nav--left-delimiter-pnode-type-p))
   (thread-first
     pnode
     (ts-nav--pnode-child -1)
     (ts-nav--right-delimiter-pnode-type-p))))


(defun ts-nav--first-delimiter-p (pnode forward)
  (when-let ((parent (ts-nav--pnode-parent pnode))
             (child-count (ts-nav--pnode-child-count parent)))
    (and
     ;; Parent has at least two childs
     (>= (ts-nav--pnode-child-count parent) 2)
     ;; Node has the correct position inside parent
     (= (ts-nav--pnode-index pnode) (if forward 0
                                      (1- child-count)))
     ;; Satisfies predicate for first delimiter
     (ts-nav--first-delimiter-pnode-type-p pnode forward)
     ;; Other outer child is other delimiter
     (thread-first
       (ts-nav--pnode-child parent (if forward -1 0))
       (ts-nav--first-delimiter-pnode-type-p (not forward))))))


(defun ts-nav--enclosing-list (pnode)
  (ts-nav--step-until pnode #'ts-nav--pnode-parent #'ts-nav--plist-p))

(defun ts-nav-up-list-forward (&optional n)
  (interactive "p")
  (let* ((n (or n 1))
         (forward (> n 0))
         (n (abs n))
         (pnode (ts-nav--get-leaf-or-list-ending-after-point forward)))
    (dotimes (_ n)
      (unless (setq pnode (ts-nav--enclosing-list pnode))
        (user-error "Can't go up further"))
      (goto-char (ts-nav--pnode-start pnode (not forward))))))

;; (defun ts-nav-up-list-forward2 (&optional n)
;;   (interactive "p")
;;   (let* ((n (or n 1))
;;          (forward (> n 0))
;;          (n (abs n)))
;;     (unless (= 0 n)
;;       (catch 'stop
;;         (let* ((pt (point))
;;                (pnode (thread-first
;;                         (treesit-node-at pt)
;;                         (ts-nav--pnode-new))))
;;           (setq pnode (or
;;                        (ts-nav--enclosing-list pnode)
;;                        (throw 'stop nil)))
;;           ;; After going up once, see if we point was already at the edge of the list; in this case, go up once more than otherwise.
;;           (unless (let ((bounds (ts-nav--pnode-bounds pnode)))
;;                     (or (= pt (car bounds)) (= pt (cdr bounds))))
;;             (setq n (1- n)))
;;           ;; Go up N times more
;;           (dotimes (_ n)
;;             (setq pnode (or
;;                          (ts-nav--enclosing-list pnode)
;;                          (throw 'stop nil))))
;;           (let ((bounds (ts-nav--pnode-bounds pnode)))
;;             (goto-char (if forward (cdr bounds)
;;                          (car bounds)))))))))

(defun ts-nav-up-list-backward (&optional n)
  (interactive "p")
  (ts-nav-up-list-forward (- (or n 1))))

(defun ts-nav--pnode-start (pnode forward)
  (let ((bounds (ts-nav--pnode-bounds pnode)))
    (if forward (car bounds)
      (cdr bounds))))

;; TODO: rename, doesn't reflect extra things this does.
(defun ts-nav--get-leaf-or-list-ending-after-point (forward)
  (let* ((pt (point))
         (pnode (ts-nav--pnode-new (treesit-node-at pt))))
    (cond
      ((ts-nav--after-pnode-p pnode pt forward)
       (when-let ((new (ts-nav--pnode-step-pre-order pnode forward)))
         (setq pnode new)))
      ((and (= pt (ts-nav--pnode-start pnode forward))
            (ts-nav--first-delimiter-p pnode forward))
       (setq pnode (ts-nav--pnode-parent pnode))))
    pnode))


;; TODO: rename first -> opening
(defun ts-nav--down-list-once2 (forward)
  (let ((pnode (ts-nav--get-leaf-or-list-ending-after-point forward)))
    ;; Traverse in pre-order until at an opening delimiter; don't escape an enclosing list, though.
    (let ((bounded (ts-nav--enclosing-list pnode)))
      (if-let* ((res (ts-nav--step-until pnode
                       (lambda (pnode)
                         (ts-nav--pnode-step-pre-order pnode forward :bounded bounded))
                       (lambda (pnode)
                         (ts-nav--first-delimiter-p pnode forward))
                       ;; We already skipped a node manually above in case we weren't interested in our actual starting pnode.
                       :include-current t))
                (bounds (ts-nav--pnode-bounds res)))
          (goto-char (if forward (cdr bounds)
                       (car bounds)))))))

;; TODO: rename first -> opening

(defun ts-nav--down-list-once (pnode forward)
  ;; Traverse in pre-order until at an opening delimiter; don't escape an enclosing list, though.
  (let ((bounded (ts-nav--enclosing-list pnode)))
    (ts-nav--step-until pnode
      (lambda (pnode)
        (ts-nav--pnode-step-pre-order pnode forward :bounded bounded))
      (lambda (pnode)
        (ts-nav--first-delimiter-p pnode forward))
      :include-current t)))

(defun ts-nav-forward-down-list (&optional n)
  (interactive "p")
  (let* ((n (or n 1))
         (forward (> n 0))
         (n (abs n))
         (pnode (ts-nav--get-leaf-or-list-ending-after-point forward)))
    (dotimes (_ n)
      (unless (setq pnode (ts-nav--down-list-once pnode forward))
        (user-error "Can't go down further"))
      (goto-char (ts-nav--pnode-start pnode (not forward))))))

(defun ts-nav-backward-down-list (&optional n)
  (interactive "p")
  (ts-nav-forward-down-list (- (or n 1))))

(defun ts-nav--list-or-node-p (pnode forward)
  (or (ts-nav--first-delimiter-p pnode forward)
      (and (ts-nav--pnode-named pnode)
           (ts-nav--pnode-leaf-p pnode))))

;; TODO
(defun ts-nav--forward-list-or-node-once (pnode forward)
  (let ((next-p (lambda (pnode)
                  (or (ts-nav--named-le)))))
    (if (ts-nav--plist-p pnode)
        (progn)
      )
    (ts-nav--step-until)))

(defun ts-nav-forward-list-or-node (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (cond ((< n 0) (ts-nav-backward n))
          
          ((> n 0) (let* ((pt (point))
                          (pnode (ts-nav--pnode-new (treesit-node-at pt)))
                          ))))))

(defun ts-nav-backward (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (cond ((< n 0) (ts-nav-forward n))
          ((> n 0) (while ))
          )))


(defun ts-nav--pnode-leaf-p (pnode)
  (= (ts-nav--pnode-child-count pnode) 0))


(defun ts-nav--pnode-unnamed-leaf-p (pnode)
  (and (not (ts-nav--pnode-named pnode))
       (ts-nav--pnode-leaf-p pnode)))


(defun ts-nav--pnode-delimited-p (pnode)
  (and (<= 2 (ts-nav--pnode-child-count pnode))
       (thread-first
         pnode
         (ts-nav--pnode-child 0)
         (ts-nav--pnode-unnamed-leaf-p))
       (thread-first
         pnode
         (ts-nav--pnode-child -1)
         (ts-nav--pnode-unnamed-leaf-p))))


(defun ts-nav--pnode-to-sibling (pnode forward)
  (cl-flet ((node-to-sibling (node)
              (if forward
                  (treesit-node-next-sibling node)
                (treesit-node-prev-sibling node))))
    (thread-first
      pnode
      (ts-nav--pnode-first)
      (node-to-sibling)
      (ts-nav--pnode-new))))

(defun ts-nav--pnode-to-child (pnode forward)
  (ts-nav--pnode-child pnode (if forward 0 -1)))

(cl-defun ts-nav--pnode-to-sibling-until (pnode forward pred &key include-current)
  (unless include-current
    (setq pnode (ts-nav--pnode-to-sibling pnode forward)))
  (catch 'stop
    (while pnode
      (when (funcall pred pnode)
        (throw 'stop pnode))
      (setq pnode (ts-nav--pnode-to-sibling pnode forward)))))


(defun ts-nav--pnode-eq (pnode1 pnode2)
  (treesit-node-eq (ts-nav--pnode-first pnode1) (ts-nav--pnode-first pnode2)))

(defun ts-nav--pnode-types (pnode)
  (thread-last
    pnode
    (ts-nav--pnode-nodes)
    (mapcar #'treesit-node-type)))


(defun ts-nav--pnode-type-in (pnode type-list)
  (cl-intersection (ts-nav--pnode-types pnode) type-list))


(defun ts-nav--ctx-up (pnode)
  (let ((child-id (ts-nav--pnode-index pnode))
        (field-name (ts-nav--pnode-field-name pnode)))
    (list 'up child-id field-name)))

(defun ts-nav--ctx-down (pnode child-id)
  (let ((field-name (ts-nav--pnode-field-name-for-child pnode child-id)))
    (list 'down child-id field-name)))

(defun ts-nav--ctx-to-sibling (forward)
  (list 'move forward))

(defun ts-nav--ctx-cons (ctx ctxs)
  "Return a list logically equivalent to the cons of CTX and CTXS in that it describes the same relative position of two pnodes in the tree, but possibly removes redundancies."
  (if-let ((orig ctxs))
      (pcase ctx
        ('(up . _)
          ;; If CTXS starts with a nonnegative number to-sibling movements followed by a
          ;; down movement, we "cancel" these out.
          (let ((orig ctxs))
            (while (eq (caar ctxs) 'to-sibling)
              (setq ctxs (cdr ctxs)))
            (if (eq (caar ctxs) 'down)
                (cdr ctxs)
              (cons ctx orig))))
        ('(down . _)
          ;; Like above, but with up instead of down
          (let ((orig ctxs))
            (while (eq (caar ctxs) 'to-sibling)
              (setq ctxs (cdr ctxs)))
            (if (eq (caar ctxs) 'up)
                (cdr ctxs)
              (cons ctx orig))))
        ('(to-sibling ,forward)
          (if (equal (caar ctxs) `(to-sibling . ,(not forward)))
              (cdr ctxs)
            ctxs)
          ))
    ;; CTXS is nil
    (list ctx)))

(defun ts-nav--ctx-invert (ctx)
  (pcase ctx
    (`(up ,child-id ,field-name) `(down ,child-id ,field-name))
    (`(down ,child-id ,field-name) `(up ,child-id ,field-name))
    (`(to-sibling ,forward) `(to-sibling ,(not forward)))
    (_ (error "Malformed context %S" ctx))))

(defun ts-nav--ctx-apply (pnode ctx)
  (pcase ctx
    (`(up _ _) (ts-nav--pnode-parent pnode))
    (`(down ,child-id ,field-name) (if field-name
                                       (ts-nav--pnode-child-by-field-name pnode field-name)
                                     (ts-nav--pnode-child pnode child-id)))
    (`(to-sibling ,forward) (ts-nav--pnode-to-sibling pnode forward))
    (_ (error "Malformed context %S" ctx))))

(defun ts-nav--ctx-replay (pnode ctxs)
  (catch 'stop
    (dolist (ctx ctxs)
      (setq pnode (or (ts-nav--ctx-apply pnode ctx)
                      (throw 'stop nil))))))


(defun ts-nav--step-while (current step pred)
  (declare (indent 1))
  (let ((last nil))
    (while (and current (funcall pred current))
      (setq last current
            current (funcall step cstate)))
    last))

(cl-defun ts-nav--step-until (current step pred &key include-current)
  (declare (indent 1))
  (unless include-current
    (setq current (funcall step current)))
  (catch 'stop
    (while current
      (when (funcall pred current)
        (throw 'stop current))
      (setq current (funcall step current)))))



;; Interface to manage node overlays

(defvar-local ts-nav--overlays '())

(defun ts-nav--clear-overlays ()
  (when ts-nav--overlays
    (dolist (overlay ts-nav--overlays)
      (delete-overlay overlay))
    (setq ts-nav--overlays '())))

(defun ts-nav--create-overlay (pnode face-property)
  (let ((overlay (make-overlay (ts-nav--pnode-start pnode) (ts-nav--pnode-end pnode))))
    (overlay-put overlay 'face face-property)
    (overlay-put overlay 'window (selected-window))
    (push overlay ts-nav--overlays)))


;; Cursor

(cl-defstruct ts-nav--cstate
  ;; Best approximation of context, i.e. possibly larger than what is implied by context?
  cursor
  ;; Resolved pnode of CONTEXT if context is "active/decoupled".
  context-pnode
  ;; Description of where cursor is relative to context; list of (string, int)?
  ;; both? Cons of child id and optional name of field?
  ;; Only possibly non-nil if CONTEXT-PNODE is?
  ;; Is nil iff CONTEXT-PNODE is nil or equal to CURSOR
  context
  ;; If point is at end of cursor
  is-forward
  ;; Mark position or list of nodes?
  marked)

;; (advice-add'copy-ts-nav--cstate
;;  :override (lambda (cstate)
;;              (let ((cstate (copy-sequence cstate)))
;;                cstate)))


;; Helper functions to reduce boilerplate code

(cl-defun ts-nav--cstate-with-cursor (cstate cursor)
  (declare (indent 1))
  (let ((cstate (copy-ts-nav--cstate cstate)))
    (setf (ts-nav--cstate-cursor cstate) cursor)
    cstate))


(defun ts-nav--pnode-step-with-ctx (pnode step)
  (pcase step
    ('up `(,(ts-nav--pnode-parent pnode) up . ,(ts-nav--pnode-ctx pnode)))
    (`(down . ,id) (if (stringp id)
                       (when-let ((child (ts-nav--pnode-child-by-field-name pnode id)))
                         `(,child down ,(ts-nav--pnode-index child) . ,id))
                     (when-let ((child (ts-nav--pnode-child pnode id)))
                       `(,child down ,id . ,(ts-nav--pnode-field-name child)))))
    (`(to-sibling . ,forward) (when-let ((sibling (if forward
                                                      (ts-nav--pnode-next-sibling pnode)
                                                    (ts-nav--pnode-prev-sibling pnode))))
                                `(,sibling to-sibling . ,(ts-nav--pnode-ctx pnode))))))

(defun ts-nav--pnode-apply-ctxs (pnode context &optional strict)
  (catch 'stop
    (dolist (ctx context pnode)
      (setq pnode (or
                   (ts-nav--pnode-apply-ctx pnode ctx)
                   (throw 'stop nil))))))

(defun ts-nav--pnode-apply-ctx (pnode ctx &optional strict)
  (pcase ctx
    (`(,child-id . ,field-name) (if (and field-name strict)
                                    (ts-nav--pnode-child-by-field-name pnode field-name)
                                  (ts-nav--pnode-child pnode child-id)))
    (_ (error "Malformed parent context ctx %S" ctx))))


(cl-defun ts-nav--cstate-context-up (cstate)
  ;; Context or cursor. Move, but not through cursor
  (let* ((cstate (copy-ts-nav--cstate cstate))
         (old (or
               (ts-nav--cstate-context-pnode cstate)
               (ts-nav--cstate-cursor cstate))))
    (when-let ((new (ts-nav--pnode-parent old))
               (ctx (ts-nav--pnode-ctx old)))
      (setf (ts-nav--cstate-context-pnode cstate) new)
      (push ctx (ts-nav--cstate-context cstate)))
    cstate))

(cl-defun ts-nav--cstate-context-down (cstate)
  (let* ((cstate (copy-ts-nav--cstate cstate))
         (context (ts-nav--cstate-context cstate)))
    (when-let ((ctx (cdr context)))
      (if (not context)
          ;; If this was the last edge between context and cursor, deactivate cursor
          (setf (ts-nav--cstate-cursor-pnode cstate) nil)
        ;; Otherwise, go downwards towards cursor
        (cl-callf ts-nav--pnode-apply-ctx
            (ts-nav--cstate-context-pnode cstate)
          ctx))
      cstate)))

(cl-defun ts-nav--cstate-context-to-sibling (cstate forward)
  (let* ((cstate (copy-ts-nav--cstate cstate))
         (context (ts-nav--cstate-context cstate)))
    (when-let* (;; Only move if context is not the same as cursor
                (_ context)
                ;; Move context node 
                (new (thread-first
                       cstate
                       (ts-nav--cstate-context-pnode)
                       (ts-nav--pnode-to-sibling forward)))
                ;; Reinterpret cursor from new context-pnode
                (cursor (ts-nav--pnode-apply-ctxs new context)))
      (setf (ts-nav--cstate-context-pnode cstate) new
            (ts-nav--cstate-cursor cstate) cursor)
      cstate)))


(defun ts-nav--cursor-init (&optional pnode)
  "Initialize new cursor in current buffer from PNODE.."
  (when pnode
    (make-ts-nav--cstate
     :cursor pnode
     :is-forward t
     :marked '()
     :context-pnode nil
     :context '())))



(defvar-local ts-nav--cursor-history '())

(defun ts-nav--push-history ()
  (push (copy-ts-nav--cstate ts-nav--cursor) ts-nav--cursor-history))

(defun ts-nav--pop-history()
  (setq ts-nav--cursor (or
                        (pop ts-nav--cursor-history)
                        (error "No cursor in history"))))



;; Low-level cursor API

(defun ts-nav-cursor (&optional interactive)
  "Get pseudo node of current cursor. If INTERACTIVE, print it instead."
  (interactive '(t))
  (if ts-nav--cursor
      (let ((pnode (ts-nav--cstate-cursor ts-nav--cursor)))
        (if interactive
            (message "Cursor: %S" pnode)
          pnode))
    (error "No active cursor")))


;; Primitive cursor actions as a lower-level abstraction to ensure the cursor state is coherent. Indicate success by returning the pnode they moved to on success and otherwise nil, leaving `ts-nav--cursor' untouched in the latter case.

(cl-defun ts-nav--cstate-step-cursor (cstate cursor-step)
  (when-let ((cursor-with-ctx (thread-first
                                cstate
                                (ts-nav--cstate-cursor)
                                (ts-nav--pnode-step-with-ctx cursor-step)))
             (cstate (copy-ts-nav--cstate cstate)))
    ;; Set cursor
    (setf (ts-nav--cstate-cursor cstate) (car cursor-with-ctx))
    ;; When context is active, modify it accordingly
    (when (ts-nav--cstate-context-pnode cstate)
      (pcase (cdr cursor-with-ctx)
        (`(up . _) (unless (pop (ts-nav--cstate-context cstate))
                     (setf (ts-nav--cstate-context-pnode cstate) nil)))
        (`(down . ,ctx) (push ctx (tss-nav--cstate-context cstate)))
        (`(to-sibling . ,ctx) (if (pop (ts-nav--cstate-context cstate))
                                  (push ctx (ts-nav--cstate-context cstate))
                                ;; As above, we were at context and moved to its sibling, deactivate it.
                                (setf (ts-nav--cstate-context-pnode cstate) nil)))))
    cstate))



(cl-defun ts-nav--cursor-to-child (cstate child-id)
  (when-let* ((cstate (copy-ts-nav--cstate cstate))
              (cursor (cl-callf ts-nav--pnode-child
                          (ts-nav--cstate-cursor cstate)
                        child-id)))
    (cl-callf (lambda (context)
                (when context
                  (append context (list (ts-nav--pnode-ctx cursor)))))
        (ts-nav--cstate-context cstate))
    cstate))

(cl-defun ts-nav--cursor-to-parent (cstate)
  (when-let* ((cstate (copy-ts-nav--cstate cstate))
              (cursor (cl-callf ts-nav--pnode-parent
                          (ts-nav--cstate-cursor cstate))))
    (unless (cl-callf butlast
                (ts-nav--cstate-context cstate))
      ;; We went up into the context pnode, deactivate the context.
      (setf (ts-nav--cstate-context-pnode cstate) nil))
    cstate))

(cl-defun ts-nav--cursor-to-sibling (cstate forward)
  (when-let* ((cstate (copy-ts-nav--cstate cstate))
              (cursor (cl-callf ts-nav--pnode-to-sibling
                          (ts-nav--cstate-cursor cstate)
                        forward)))
    (cl-callf (lambda (context)
                (if-let ((up-one (butlast context)))
                    (append up-one (list (ts-nav--pnode-ctx cursor)))
                  (setf (ts-nav--cstate-context-pnode cstate) nil)
                  nil))
        (ts-nav--cstate-context cstate))
    cstate))

;; Composed actions built upon the primitive ones above, erroring when encountering a problem to indicate that the cursor might be left in an intermediate state (so we can revert it appropriately).


(defun ts-nav--cursor-is-p (cstate pnode)
  (and pnode
       (thread-first
         cstate
         (ts-nav--cstate-cursor)
         (ts-nav--pnode-eq pnode))))

(defun ts-nav--cursor-eval-p (pred cstate)
  (and pred
       (thread-last
         cstate
         (ts-nav--cstate-cursor)
         (funcall pred))))


(cl-defun ts-nav--cursor-down (cstate forward)
  "Move CSTATE to first child if FORWARD or its last child otherwise."
  (ts-nav--cursor-to-child cstate (if forward 0 -1)))

(cl-defun ts-nav--cursor-step-while (cstate step pred)
  "Move CSTATE by repeatedly applying STEP to it until it returns nil or (funcall PRED CSTATE) no longer holds, returning the last value of CSTATE for which it did hold (or nil if there is no such value)."
  (declare (indent 1))
  (let ((last nil))
    (while (and cstate (ts-nav--cursor-eval-p pred cstate))
      (setq last cstate
            cstate (funcall step cstate)))
    last))

(cl-defun ts-nav--cursor-step-until (cstate step pred &key include-current)
  "Move CSTATE by repeatedly applying STEP to it until it is nil or (funcall PRED CSTATE) holds and return this value of CSTATE."
  (declare (indent 1))
  (unless include-current
    (setq cstate (funcall step cstate)))
  (catch 'stop
    (while cstate
      (when (ts-nav--cursor-eval-p pred cstate)
        (throw 'stop cstate))
      (setq cstate (funcall step cstate)))))

(cl-defun ts-nav--cursor-step-move (cstate forward &key bounded)
  "Move to the closest sibling of CSTATE or a direct ancestor in the direction specified by FORWARD.
Returns nil if no such node exists. Stays in BOUNDED's subtree if non-nil."
  (catch 'stop
    ;; Go up until we have a sibling in our target direction
    (while cstate
      (if-let ((next (ts-nav--cursor-to-sibling cstate forward)))
          (throw 'stop next)
        (setq cstate (unless (ts-nav--cursor-is-p cstate bounded)
                       (ts-nav--cursor-to-parent cstate)))))))

(cl-defun ts-nav--cursor-move-until (cstate forward pred &key scoped include-current)
  (let ((upper-bound (when scoped
                       (ts-nav--cstate-cursor cstate))))
    (ts-nav--cursor-step-until cstate
      (lambda (cstate)
        (ts-nav--cursor-step-move cstate forward :bounded upper-bound))
      pred :include-current include-current)))

(cl-defun ts-nav--cursor-move-while (cstate forward pred &key scoped)
  (let ((upper-bound (when scoped
                       (ts-nav--cstate-cursor cstate))))
    (ts-nav--cursor-step-while cstate
      (lambda (cstate)
        (ts-nav--cursor-step-move cstate forward :bounded upper-bound)))))

(cl-defun ts-nav--cursor-step-pre-order (cstate forward &key bounded)
  (or
   (ts-nav--cursor-down cstate forward)
   (ts-nav--cursor-step-move cstate forward :bounded bounded)))

(cl-defun ts-nav--cursor-find (cstate forward pred &key scoped include-current)
  (let ((upper-bound (when scoped
                       (ts-nav--cstate-cursor cstate))))
    (ts-nav--cursor-step-until cstate
      (lambda (cstate)
        (ts-nav--cursor-step-pre-order (cstate forward :bounded upper-bound)))
      pred :include-current include-current)))

(cl-defun ts-nav--cursor-step-reverse-pre-order (cstate forward &key bounded)
  ;; Go to first leave of next sibling in target direction; if there is no sibling, go up instead.
  (unless (ts-nav--cursor-is-p cstate bounded)
    (if-let ((next (ts-nav--cursor-to-sibling cstate forward)))
        (progn
          (setq cstate next)
          (while-let ((down (ts-nav--cursor-down cstate forward)))
            (setq cstate down))
          cstate)
      (ts-nav--cursor-up cstate))))

(cl-defun ts-nav--cursor-find-outward (cstate forward pred &key include-current)
  (let ((upper-bound (when scoped
                       (ts-nav--cstate-cursor cstate))))
    (cl-flet ((step (cstate)
                (or
                 (ts-nav--cursor-down cstate forward)
                 (ts-nav--cursor-step-move cstate forward :bounded upper-bound))))
      (unless include-current
        (setq cstate (step cstate)))
      (catch 'stop
        (while cstate
          (when (ts-nav--cursor-eval-p pred cstate)
            (throw 'stop cstate))
          (setq cstate (step cstate)))))))

(defun ts-nav--cursor-ensure-named (cstate forward)
  (ts-nav--cursor-move-until cstate forward #'ts-nav--pnode-named t t))


(defvar-local ts-nav--cursor nil)

(defun ts-nav--redisplay-cursor ()
  (ts-nav--clear-overlays)
  (when ts-nav--cursor
    (ts-nav--create-overlay
     (ts-nav--cstate-cursor ts-nav--cursor)
     'ts-nav-cursor)
    (when-let ((context-pnode (ts-nav--cstate-context-pnode ts-nav--cursor)))
      (ts-nav--create-overlay context-pnode 'ts-nav-context))))

(defun ts-nav--cursor-set (&optional cursor)
  (when ts-nav--cursor
    (ts-nav--push-history))
  (when (setq ts-nav--cursor cursor)
    (ts-nav--redisplay-cursor)))

(defun ts-nav--cursor-get ()
  (or ts-nav--cursor
      (error "No active cursor")))


(defun ts-nav--repeated (n forward backward)
  (declare (indent 1))
  (let ((n (or n 1))
        (cstate (ts-nav--cursor-get)))
    (while (not (= n 0))
      (if (> n 0) 
          (setq cstate (funcall forward cstate)
                n (1- n))
        (setq cstate (funcall backward cstate)
              n (1+ n))))
    (unless cstate
      (error "Bad cursor command"))
    (ts-nav--cursor-set cstate)))


(defun ts-nav--up (&optional n)
  (interactive "p")
  (ts-nav--repeated n
    #'ts-nav--cursor-to-parent
    (lambda (_) (error "Prefix argument must be non-negative"))))

(defun ts-nav--down-once (cstate forward named)
  (when-let ((cstate (ts-nav--cursor-down cstate forward)))
    (if named
        (ts-nav--cursor-move-until cstate forward #'ts-nav--pnode-named :scoped t :include-current t)
      cstate)))

(defun ts-nav--down (n named)
  (ts-nav--repeated n
    (lambda (cstate)
      (ts-nav--down-once cstate t named))
    (lambda (cstate)
      (ts-nav--down-once cstate nil named))))

(defun ts-nav-cursor-down-named (&optional n)
  (interactive "p")
  (ts-nav--down n t))

(defun ts-nav-cursor-down (&optional n)
  (interactive "p")
  (ts-nav--down n nil))

(defun ts-nav--move (n forward pred)
  (ts-nav--repeated n
    (lambda (cstate)
      (or
       (ts-nav--cursor-move-until cstate forward pred)
       (error "No next node")))
    (lambda (cstate)
      (or
       (ts-nav--cursor-move-until cstate (not forward) pred)
       (error "No previous node")))))

(defun ts-nav-cursor-next-named (&optional n)
  (interactive "p")
  (ts-nav--move n t #'ts-nav--pnode-named))

(defun ts-nav-cursor-next (&optional n)
  (interactive "p")
  (ts-nav--move n t #'always))

(defun ts-nav-cursor-prev-named (&optional n)
  (interactive "p")
  (ts-nav--move n nil #'ts-nav--pnode-named))

(defun ts-nav-cursor-prev (&optional n)
  (interactive "p")
  (ts-nav--move n nil #'always))

(defun ts-nav-context-up (&optional n)
  (interactive "p")
  (ts-nav--repeated n
    #'ts-nav--cstate-context-up
    (lambda (_)
      (error "Prefix argument must be nonnegative"))))

(defun ts-nav-context-down (&optional n)
  (interactive "p")
  (ts-nav--repeated n
    #'ts-nav--cstate-context-down
    #'ts-nav--cstate-context-down))

(defun ts-nav--move-context (n forward)
  (ts-nav--repeated n
    (lambda (cstate)
      (or
       (ts-nav--cstate-context-to-sibling cstate forward)
       (error "Can't move context further")))
    (lambda (cstate)
      (or
       (ts-nav--cstate-context-to-sibling cstate (not forward))
       (error "Can't move context further")))))

(defun ts-nav-context-next (&optional n)
  (interactive "p")
  (ts-nav--move-context n t))

(defun ts-nav-context-prev (&optional n)
  (interactive "p")
  (ts-nav--move-context n nil))

(defun ts-nav--left-delimiter-pnode-type-p (pnode)
  (and (not (ts-nav--pnode-named pnode))
       (ts-nav--pnode-leaf-p pnode)))

(defun ts-nav--right-delimiter-pnode-type-p (pnode)
  (and (not (ts-nav--pnode-named pnode))
       (ts-nav--pnode-leaf-p pnode)))

(defun ts-nav--first-delimiter-pnode-type-p (pnode forward)
  (if forward (ts-nav--left-delimiter-pnode-type-p pnode)
    (ts-nav--right-delimiter-pnode-type-p pnode)))

(defun ts-nav-next-cousin (&optional n)
  (interactive "p")
  (let* ((forward (> n 0))
         (pnode (ts-nav--pnode-new (treesit-node-at (point))))
         (ctxs (list (ts-nav--pnode-ctx pnode)))
         (ancestor (ts-nav--pnode-parent pnode)))
    (catch 'stop
      (while ancestor
        ;; See if sibling can interpret ctxs
        (let ((sibling (ts-nav--pnode-to-sibling ancestor forward)))
          (while sibling
            (when-let ((res (funcall #'ts-nav--pnode-apply-ctxs sibling ctxs)))
              (goto-char (ts-nav--pnode-start res t))
              (throw 'stop nil))
            (setq sibling (ts-nav--pnode-to-sibling sibling forward))))
        ;; Otherwise, go up further
        (push (ts-nav--pnode-ctx ancestor) ctxs)
        (setq ancestor (ts-nav--pnode-parent ancestor)))
      (user-error "Can't find cousin"))))

(defun ts-nav-prev-cousin (&optional n)
  (interactive "p")
  (ts-nav-next-cousin (- (or n 1))))


(defun ts-nav--move-similar (n forward)
  (let* ((types (thread-first
                  (ts-nav--cursor-get)
                  (ts-nav--cstate-cursor)
                  (ts-nav--pnode-types))))
    (ts-nav--move n forward (lambda (pnode)
                              (ts-nav--pnode-type-in pnode types)))))

(defun ts-nav-cursor-next-similar (&optional n)
  (interactive "p")
  (ts-nav--move-similar n t))

(defun ts-nav-cursor-prev-similar (&optional n)
  (interactive "p")
  (ts-nav--move-similar n nil))


(defun ts-nav--start-on (bounds)
  (when-let ((node (treesit-node-on (car bounds) (cdr bounds))))
    (thread-first
      node
      (ts-nav--pnode-new)
      (ts-nav--cursor-init)
      (ts-nav--cursor-set))
    (ts-nav--hydra/body)))

(defun ts-nav-start-on-point ()
  (interactive)
  (ts-nav--start-on (cons (point) (1+ (point)))))

(defun ts-nav--bounds-of-line-text (&optional pos)
  (save-excursion
    (when pos
      (goto-char pos))
    (let* ((line-beg (progn
                       (beginning-of-line)
                       (point)))
           (line-end (progn
                       (end-of-line)
                       (point)))
           (inner-end (progn
                        (skip-chars-backward " \t" line-beg)
                        (point)))
           (inner-beg (progn
                        (goto-char line-beg)
                        (skip-chars-forward " \t" line-end)
                        (point))))
      (unless (= inner-end inner-beg)
        (cons inner-beg inner-end)))))

(defun ts-nav-start-on-line ()
  (interactive)
  (if-let ((bounds (ts-nav--bounds-of-line-text)))
      (ts-nav--start-on bounds)
    (error "No nodes on current line")))


(defhydra ts-nav--hydra (:foreign-keys warn
                         :hint t)
  "Treesitter Navigation"
  ;; ("m" ts-mark-current-node)
  
  ("j" ts-nav-context-down)
  ("k" ts-nav-context-up)
  ("h" ts-nav-context-prev)
  ("l" ts-nav-context-next)
  
  ;; ("f" ts-nav-cursor-test)
  ("n" ts-nav-cursor-next-named)
  ("p" ts-nav-cursor-prev-named)
  ("N" ts-nav-cursor-next)
  ("P" ts-nav-cursor-prev)
  ("C-n" ts-nav-cursor-next-similar)
  ("C-p" ts-nav-cursor-prev-similar)
  ("u" ts-nav--up)
  ("d" ts-nav-cursor-down-named)
  ("D" ts-nav-cursor-down)
  ("?" ts-nav-cursor)
  ("q" ignore :exit t)
  ("<escape>" ignore :exit t))

(provide 'ts-nav)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "ts-nav--"))
;; End:
