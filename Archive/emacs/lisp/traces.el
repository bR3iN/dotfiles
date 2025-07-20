;; -*- lexical-binding: t; -*-

(require 'common)

;; Low-level API

(defface traces
    `((t ;:inherit region
       :background ,(colored-region :green 0.05 15)))
  "Faces for traces")


(defvar-local -current-trace nil)


(defun -set-current-trace (trace)
  (when -current-trace
    (delete-overlay (car -current-trace)))
  (setq -current-trace trace))


(defun -create-trace (old-pos pos)
  (let* ((is-forward (<= old-pos pos))
         (start (if is-forward old-pos pos))
         (end (if is-forward pos old-pos))
         (overlay (make-overlay start end)))
    (overlay-put overlay 'face 'traces)
    `(,overlay . ,is-forward)))


(defun trace-range (start end)
  "Create a trace from START to END, replacing a possibly active trace."
  (when trace-mode
    (-set-current-trace (-create-trace start end))))


(defun clear-current-trace ()
  "Possibly remove the active trace, does nothing if there is none."
  (interactive)
  (-set-current-trace nil))


(defun current-trace-p ()
  "Is there a currently active (i.e. visible) trace?"
  (boolify -current-trace))


(defun current-trace-get (what)
  "Query a property of the current trace, possibly values for WHAT are 'start, 'end, 'first and 'last. Returns nil iff (not (current-trace-p))."
  (when -current-trace
    (-trace-get what -current-trace)))


(defun -trace-get (what trace)
  (let* ((is-forward (cdr trace))
         (overlay (car trace)))
    (pcase what
           ; ('active (boolify (overlay-buffer overlay)))
           ('start (if is-forward
                     (overlay-start overlay)
                     (overlay-end overlay)))
           ('end (if is-forward
                   (overlay-end overlay)
                   (overlay-start overlay)))
           ('first (overlay-start overlay))
           ('last (overlay-end overlay)))))


; High level API

(defun try-get-st ()
  "Returns the current [s]election or [t]race if existent."
  (cond ((region-active-p) (cons (mark) (point)))
        ((current-trace-p) (cons (current-trace-get 'start )
						(current-trace-get 'end )))
        (t nil)))


(defun act-on-trace (func)
  (pcase (try-get-st)
    (`(,start . ,end) (funcall func start end))
    ('nil (error "No active trace"))))


(defun tracing-last (step &optional no-arg)
  (lambda (&optional n)
    (interactive "p")
    (let* ((n (or n 1))
           (direction (if (< n 0) -1 +1))
           (n (abs n)))
      (when (> n 1)
        (if no-arg
            (dotimes (_ (1- n))
              (funcall step))
          (funcall step (* direction (1- n)))))
      (request-cmd-trace (point))
      (if no-arg
          (funcall step)
        (funcall step direction)))))


(defun current-selection ()
  (cond ((region-active-p) (cons (mark) (point)))
        ((current-trace-p) (cons (current-trace-get 'start )
						         (current-trace-get 'end )))
        (t (cons (point) (1+ (point))))))

(defun acting-on-selection (cmd)
  (lambda ()
    (interactive)
    (let ((selection (current-selection)))
      (funcall cmd (car selection) (cdr selection)))))

                                        ; (defun act-on-st (func &optional fallback finally)
                                        ;   (cl-flet ((try-dispatch (lambda (fn)
                                        ;                             (when fn
                                        ;                               (if (commandp fn)
                                        ;                                   (call-interactively fn)
                                        ;                                 (funcall fn)))))
                                        ;             (func func))
                                        ;     (lambda ()
                                        ;       (interactive)
                                        ;       (if-let* ((maybe-st (try-get-st))
                                        ;                 (start (car maybe-st))
                                        ;                 (end (cdr maybe-st)))
                                        ;                (func start end)
                                        ;                (try-dispatch fallback))
                                        ;       (try-dispatch finally))))


(defun region-to-trace ()
  (interactive)
  (unless (region-active-p) (error "No active region"))
  (deactivate-mark)
  (trace-range (mark) (point)))


(defun trace-to-region ()
  (interactive)
  (unless (current-trace-p) (error "No active trace"))
  (let ((start (current-trace-get 'start ))
        (end (current-trace-get 'end )))
    (clear-current-trace)
    (push-mark start t t)
    (goto-char end)))


(defun st-to-secondary ()
  (interactive)
  (cl-flet ((set-second (lambda (first last)
                          (delete-overlay mouse-secondary-overlay)
                          (move-overlay mouse-secondary-overlay first last))))
    (cond
     ((region-active-p) (let ((pt (point))
                              (mk (mark)))
                          (deactivate-mark)
                          (set-second (min pt mk) (max pt mk))))
     ((current-trace-p)
      (let ((first (current-trace-get 'first))
            (last (current-trace-get 'last)))
        (clear-current-trace)
        (set-second first last))))))

(defun swap-st-with-secondary ()
  (interactive)
  (when (current-trace-p)
    (trace-to-region))
  ;; TODO: decouple from meow
  (meow-swap-grab))


(defun start-selection ()
  "Convert an existing motion trace into the current selection or start a new selection at point."
  (interactive)
  (cond ((region-active-p) nil)
        ((current-trace-p) (trace-to-region))
        (t (set-mark-command nil))))


(defun reverse-st ()
  (interactive)
  (pcase (try-get-st)
    (`(,start . ,end) (set-mark end) (goto-char start))
    ('nil (when-let ((mk (mark))
                     (pt (point)))
            (set-mark end)
            (goto-char mk)))))

(defun reverse-trace ()
  (interactive)
  (unless (current-trace-p) (error "No active trace"))
  (let ((start (current-trace-get 'start))
        (end (current-trace-get 'end)))
    (request-cmd-trace end)
    (goto-char start)))


(defun reverse-trace-if-backward ()
  (interactive)
  (unless (current-trace-p) (error "No active trace"))
  (let ((start (current-trace-get 'start))
        (end (current-trace-get 'end)))
    (when (> start end)
      (request-cmd-trace end)
      (goto-char start))))

                                        ; Instrumentation of commands

                                        ; (defvar traces-enabled-p nil
                                        ;   "Predicate that has to be nil or evaluate to something true for command traces to be created")

                                        ; nil if no trace was requested, cons cell consisting of a predicate and an integer or integer-valued function.
(defvar-local -cmd-trace-start nil)
(defvar-local -cmd-trace-type nil)

                                        ; (defun -test-pred (pred)
                                        ;   (or (not pred) (funcall pred)))

(defun request-cmd-trace (&optional start pred type)
  "START can be nil to use point, an integer or a integer-valued function. PRED is a predicate guarding the creation of the trace. A function-valued START and a non-nil PRED are evaluated after the current command finished."
  (let ((start (or start (point))))
    (setq -cmd-trace-start (cons (or pred #'always) start)
          -cmd-trace-type type))
  ;; (when (-test-pred traces-enabled-p)
  ;;   (setq -cmd-trace-start (or start 'pre-cmd-pt))
  ;;   )
  )

(defun cmd-trace-type ()
  -cmd-trace-type)

(defvar-local -pre-cmd-pt nil)
(defvar -pre-cmd-buf nil)

(defun -pre-cmd-hook ()
  (setq -pre-cmd-pt (point)
        -pre-cmd-buf (current-buffer)))

(add-hook 'pre-command-hook #'-pre-cmd-hook)

(defvar trace-fallback nil)

(defun -post-cmd-hook ()
  (if-let ((start (when (and -cmd-trace-start
                             (funcall (car -cmd-trace-start)))
                    (cdr -cmd-trace-start))))
      (trace-range
       (if (functionp start) (funcall start) start)
       (point))
    (when (and
           ;; (eq -pre-cmd-buf (current-buffer))
           (not (eq -pre-cmd-pt (point))))
      (setq -cmd-trace-type nil)
      (clear-current-trace)))
  (setq -cmd-trace-start nil)
  (when (and (not (current-trace-p))
             trace-fallback)
    (when-let ((bounds (funcall trace-fallback)))
      (trace-range (car bounds) (cdr bounds)))))

                                        ; (if-let ((start (if (eq -cmd-trace-start 'pre-cmd-pt) -pre-cmd-pt
                                        ;                   -cmd-trace-start)))
                                        ;         (trace-range start (point))
                                        ;         (when (and (eq -pre-cmd-buf (current-buffer))
                                        ;                    (not (eq -pre-cmd-pt (point))))
                                        ;           (clear-current-trace))
                                        ;         )

(define-minor-mode trace-mode
    "show traces"
  :lighter nil
  (if trace-mode
      (progn
        (setq -cmd-trace-start nil)
        (add-hook 'post-command-hook #'-post-cmd-hook nil t))
    (clear-current-trace)
    (remove-hook 'post-command-hook #'-post-cmd-hook t)))

(define-global-minor-mode global-trace-mode
    trace-mode
  (lambda ()
    (when (derived-mode-p '(prog-mode text-mode))
      (trace-mode))))
(global-trace-mode)

                                        ; (defun -post-change-hook (&rest _)
                                        ;   (clear-current-trace))

                                        ; (add-hook 'after-change-functions #'-post-change-hook)

                                        ; (defun -advice-motion (cmd &optional pred)
                                        ;   (advice-add
                                        ;     cmd
                                        ;     :before (lambda (&rest _)
                                        ;              (when (called-interactively-p)
                                        ;                (request-cmd-trace)))
                                        ;     '((name . "trace-motion")
                                        ;       (replace . t))))

                                        ; (defun trace-motion (cmd)
                                        ;   "Advices CMD to leave a trace. If CMD changes the current buffer, the trace will be created the next time a command ends up in the current buffer."
                                        ;   (-advice-motion cmd))

                                        ; (defun trace-cmd-if-local (cmd)
                                        ;   "Advices CMD to leave a trace, but only if CMD stays in the current buffer."
                                        ;   (-advice-motion cmd (lambda ()
                                        ;                         (eq -pre-cmd-buf (current-buffer)))))


(defun traced-motion (cmd)
  (adviced cmd
           :what "Traced motion."
           :before (lambda (&rest _)
                     (request-cmd-trace))))


(defun traced-motion-if-local (cmd)
  (adviced cmd
           :what "Traced motion if command doesn't leave the current buffer."
           :before (lambda (&rest _)
                     (request-cmd-trace
                       nil
                       (lambda ()
                         (eq -pre-cmd-buf (current-buffer)))))))

(defun activating-trace (cmd)
  (adviced cmd
           :what "Activates trace beforehand."
           :before (lambda (&rest _)
                     (trace-to-region))))


(defun with-trace-as-region (cmd)
  (adviced cmd
           :what "Activates trace during execution."
           :before (lambda (&rest _)
                     (trace-to-region))
           :after (lambda (&rest _)
                     (region-to-trace))))


(provide 'traces)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "traces--"))
;; End:
