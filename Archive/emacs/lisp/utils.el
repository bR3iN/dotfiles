;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun create-cursor (ls)
  ; Pointer to current node and stack of previous nodes
  (cons ls nil))

(defun cursor-pop (cursor)
  (prog1
    (cursor-get cursor)
    (cursor-move 1 cursor)))

(defun cursor-get (cursor)
  (let ((curr-node (car cursor)))
    (if (not curr-node) (error "List is empty")
      (car curr-node))))

(defun cursor-move (n cursor)
  (when (not (car cursor))
    (error "List is empty"))

  (defun has-next ()
    (car cursor))
  (defun has-prev ()
    (consp (cdr cursor)))
  (defun increment-unchecked ()
    (push (pop (car cursor)) (cdr cursor)))
  (defun decrement-unchecked ()
    (push (pop (cdr cursor)) (car cursor)))

  (cond
    ((> n 0) (while (and (has-next) (> n 0))
                    (increment-unchecked)
                    (setq n (- n 1)))
             (when (not (= n 0))
               (print "Reached end of list")))
    ((< n 0) (while (and (has-prev) (< n 0))
                    (decrement-unchecked)
                    (setq n (+ n 1)))
             (when (not (= n 0))
               (print "Reached beginning of list"))))
  (= n 0))


(defun boolify (nearly-bool)
  (if nearly-bool t nil))

(defun evenp (n)
  (= (mod n 2) 0))

(defun oddp (n)
  (= (mod n 2) 1))

(defun mfuncall (macro &rest args)
  "Useful for evaluating the arg list before expanding the macro."
  (mapply macro args))

(defun mapply (macro args)
  "Useful for evaluating the arg list before expanding the macro."
  (eval `(,macro ,@args)))

(defmacro nlambda (name arglist &rest body)
  (declare (indent defun))
  `(progn
     (fset ',name (lambda ,arglist ,@body))
     ',name))

(defmacro slambda (name arglist &rest body)
  (declare (indent defun))
  (let ((sym (make-symbol (symbol-name name))))
    `(progn
       (fset ,sym (lambda ,arglist ,@body))
       ,sym)))

(defun named (name func)
  "Names an anymous function, used for example in describe-key.\n This is done by setting the function as the value of and uninternalized symbol and returning said symbol."
  (let ((sym (make-symbol name)))
    (fset sym func)
    sym))

(defun unnamed (func)
  "Gives FUNC a default name for display in describe-key, so it's not cluttered by the closure's definition."
  (named "UNNAMED-FUNCTION" func))


(defun into-sym (func)
  (if (symbolp func) func
    (named "ANONYMOUS-FUNCTION" func)))

(defun with-docs-from (metadata-providing func &optiona extra-docs)
  (let* ((docstring (or (documentation cmd) "No documentation available."))
         (docstring (if extra-docs
                        (concat extra-docs "\n\n" docstring)
                      docstring)))
    (into-sym
     (eval `(lambda (&rest args)
              ,docstring
              (apply #',func args))))))

(defun adviced (cmd &rest config)
  "Inline advice for wrapping commands while preserving documentation and interactive behaviour."
  (let ((before nil)
        (after nil)
        (around nil)
        (interactive (interactive-form cmd))
        (what "No description."))

    (while config
      (let ((kw (pop config))
            (val (pop config)))
        (pcase kw
          (:before (setq before val))
          (:after (setq after val))
          (:around (setq around val))
          (:what (setq what val))
          (:interactive (setq interactive val))
          (_ (error
              (concat "unexpected keyword: "
                      (prin1-to-string kw)))))))

    (when (and around (or before after))
      (error "Can't use :around together with :before or :after"))

    (into-sym
     (eval
      `(lambda (&rest args)
         ,(let ((native (or (documentation cmd) "Not documented.")))
            (concat "Adviced binding: " what "\n\n" native))
         ,interactive
         (setq this-command #',cmd)
         (if #',around (apply #',around #',cmd args)
           (when #',before (apply #',before args))
           (apply #',cmd args)
           (when #',after (apply #',after args))))))))

(defmacro swap (fst snd)
  (let ((tmp (make-symbol "tmp")))
    `(let ((,tmp ,snd))
       (setq ,snd ,fst
             ,fst ,tmp))))


(cl-defmacro with-let (bindings cmd &key what)
  (declare (indent 1))
  (let ((name (if (symbolp cmd) (symbol-name cmd) "UNNAMED-FUNCTION"))
        (docs (concat
               (or what "Executing inside custom let-binding.")
               "\n\n"
               (or (documentation cmd) "Not documented."))))
    `(named ,name
            (lambda ()
              ,docs
              (interactive)
              ;; Fixes in particular wrapped xref-find-<...> not being
              ;; respected by `xref-prompt-for-identifier'.
              (setq this-command #',cmd)
              (let ,bindings
                (call-interactively #',cmd))))))

(cl-defmacro with-let-new (bindings cmd &key what)
  `(lambda ()
     (interactive)
     (let ,bindings
       (call-interactively ,cmd))))

(defun send-text (range goal)
    (save-excursion
      (let* ((start (car range))
             (end (cdr range))
             (text (buffer-substring-no-properties start end)))
        (delete-region start end)
        (when (>= goal end)
          (setq goal (- goal (- end start))))
        (goto-char goal)
        (insert text))))


(defun swap-ranges (range1 range2)
  "Swapping overlapping ranges is UB"
  (let* ((ordered (< (car range1) (car range2)))
         (lrange (if ordered range1 range2))
         (urange (if ordered range2 range1)))
    (send-text lrange (car urange))
    (send-text urange (car lrange))))


(defun pos-prefix-arg-p ()
  (> (prefix-numeric-value current-prefix-arg) 0))

(defvar-local -local-starting-point nil)

(defun save-local-win-pos ()
  (setq -starting-point
        (list (window-start) (point))))

(defun restore-local-win-pos ()
  (pcase-let ((`(,wstart ,wpt) -starting-point))
             (goto-char wpt)
             (set-window-start nil wstart)))

(defvar -starting-point nil)

(defun save-win-pos ()
  (setq -starting-point
        (list (selected-window) (window-start) (point))))

(defun restore-win-pos ()
  (pcase-let ((`(,window ,wstart ,wpt) -starting-point))
             (select-window window)
             (goto-char wpt)
             (set-window-start window wstart)))

(defun sign (n)
  (if (< n 0) -1 +1))

(defun inc-abs (n)
  (cond ((> n 0) (1+ n))
        ((< n 0) (1- n))
        (t 0)))

(defun dec-abs (n)
  (cond ((> n 0) (1- n))
        ((< n 0) (1+ n))
        (t 0)))

; (defmacro with-saved())

(defun region-text ()
  (unless (region-active-p)
    (user-error "No active region"))
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun alist-map (func alist)
  (mapcar
   (lambda (el)
     (funcall func (car el) (cdr el)))
   alist))

(defun -dispatch-eval (kind cond)
  (pcase kind
    ('mode (derived-mode-p cond))
    ('state (equal cond meow--current-state))
    ('pred (funcall cond))
    (t (error "invalid dispatch kind %s" cond))))

(defun dispatch (&rest args)
  (lambda ()
    (interactive)
    (if-let ((el (cl-find-if
                  (lambda (case)
                    (pcase case
                      (`(t ,cmd) cmd)
                      (`(,kind ,cond ,cmd)
                       (when (-dispatch-eval kind cond) cmd))
                      (t (error "malformed dispatch case"))))
                  args)))
        (call-interactively (cdr el))
      (let ((keys (this-command-keys)))
        (setq unread-command-events
              (append (listify-key-sequence keys) unread-command-events)))
      ;; (error "no valid binding in current context")
      )))


(defun map (keymaps bindings)
  (declare (indent 1))
  (dolist (keymap (if (and (listp keymaps) (not (keymapp keymaps)))
                      keymaps
                    (list keymaps)))
    (condition-case err
        (if (symbolp keymap)
            (apply #'meow-define-keys keymap bindings)
          (pcase-dolist (`(,key . ,cmd) bindings)
            (keymap-set keymap key cmd)))
      (error (message "Error while setting keybindings in map %s: %s" keymap err)))))


(defun map-in-mode (mode bindings)
  "Creates bindings in all modes deriving from MODE."
  (declare (indent 1))
  (let ((modes (if (listp mode) mode
                 (list mode))))
    (dolist (mode modes)
      (let ((sym (intern (concat (symbol-name mode) "-custom-keybinds"))))
        (fset sym (lambda (&rest _)
                    (pcase-dolist (`(,key . ,cmd) bindings)
                      (keymap-local-set key cmd))))
        (add-hook (derived-mode-hook-name mode) sym)))))


(defun hook-once (hook fn &optional depth local)
  "Add FN to HOOK, but only for the next invocation."
  (let ((sym (make-symbol "once-hook")))
    (fset sym (lambda (&rest r)
                (remove-hook hook sym local)
                (apply fn r)))
    (add-hook hook sym depth local)))

(defun hook-while (hook fn &optional depth local)
  "Add FN to HOOK, but remove it once it returns nil."
  (let ((sym (make-symbol "while-hook")))
    (fset sym (lambda (&rest r)
                (unless (apply fn r)
                  (remove-hook hook sym local))))
    (add-hook hook sym depth local)))


(defun marker-with-type (pos &optional type)
  (let ((marker (make-marker)))
    (set-marker marker pos)
    (set-marker-insertion-type marker type)
    marker))

(defun bounds-intersect-p (lhs rhs)
  "Check if two bounds LHS and RHS intersect."
  (and (< (car rhs) (cdr lhs))
       (< (car lhs) (cdr lhs))))

(defun subbounds-p (inner outer)
  (and (>= (car inner) (car outer))
       (<= (cdr inner) (cdr outer))))

(defun bounds-union (lhs rhs)
  "Smallest bounds containing LHS and RHS"
  (cons (marker-with-type (min (car lhs) (car rhs)) nil)
        (marker-with-type (max (cdr lhs) (cdr rhs)) t)))

(defun mark-bounds (bounds &optional beg-type end-type)
  (cons (marker-with-type (car bounds) beg-type)
        (marker-with-type (cdr bounds) end-type)))


(provide 'utils)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "scroll-mode--"))
;; End:
