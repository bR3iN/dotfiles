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
  (let ((name (if (symbolp cmd) (symbol-name cmd) "UNNAMED-FUNCTION"))
        (docs (concat
               (or what "Executing inside custom let-binding.")
               "\n\n"
               (or (documentation cmd) "Not documented."))))
    `(named ,name
            (lambda ()
              ,docs
              (interactive)
              (let ,bindings
                (call-interactively #',cmd))))))

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
  (buffer-substring-no-properties (region-beginning) (region-end)))

(defun alist-map (func alist)
  (mapcar
    (lambda (el)
      (funcall func (car el) (cdr el)))
    alist))

(provide 'utils)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "scroll-mode--"))
;; End:
