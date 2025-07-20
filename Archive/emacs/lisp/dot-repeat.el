;; -*- lexical-binding: t; -*-

(require 'common)
(require 'ring)
(require 'il)

(defvar -dot-repeat-debug nil)

(defmacro -log! (&rest fmt)
  `(when -dot-repeat-debug
     (print (format ,@fmt))))


(defvar -current-prefix-keys '())

(let ((last-command-keys nil))
  (defun -track-prefix-keys ()
    ;; The last command added to the prefix iff we have current-prefix-arg
    (if prefix-arg
        (push last-command-keys -current-prefix-keys)
      (setq -current-prefix-keys nil))
    (setq last-command-keys (this-command-keys-vector))))

(add-hook 'pre-command-hook #'-track-prefix-keys)


;; Low-level interface and hooks of dot repeat engine

(defvar-local -last-dot-repeat nil
              "Macro replayed by `dot-repeat` command.")

(defvar-local -current-dot-repeat '())
(defvar-local -current-dot-repeat-had-changes nil
  "Tracks if a currently recorded dot-repeat did change the buffer so we don't record no-ops")

(defvar-local -recording-dot-repeat nil
  "Indicates if we are currently recording")

(defun -abort-recording ()
  (-log! "aborted recording")
  (setq -current-dot-repeat '()
        -current-dot-repeat-had-changes nil
        -recording-dot-repeat nil))

(defun -start-recording ()
  (-log! "started recording")
  (setq -recording-dot-repeat t))

(defun -stop-recording ()
  (-log! "stopped recording")
  (prog2
      (when -current-dot-repeat-had-changes
        (let ((macro (apply #'vconcat (reverse -current-dot-repeat))))
          (-log! "Macro: %s" (format-kbd-macro macro))
          (setq -last-dot-repeat macro)))
      -current-dot-repeat-had-changes
    (setq -current-dot-repeat nil
          -current-dot-repeat-had-changes nil
          -recording-dot-repeat nil)))

(defun -record-this-cmd ()
  ;; For the first command, also record its prefix keys
  (when (not -current-dot-repeat)
    (setq -current-dot-repeat -current-prefix-keys))
  (push (this-command-keys-vector) -current-dot-repeat))

(add-hook 'post-command-hook #'-dot-repeat-hook)

(let ((last-buffer nil)
      (last-window nil))
  (defun -dot-repeat-hook ()
    (let ((curr-buff (current-buffer))
          (curr-win (selected-window)))
      ;; Treat the minibuffer as the last "actual" buffer.
      (if (minibufferp curr-buff t)
        (with-current-buffer
          last-buffer
          (-dot-repeat-hook-inner nil nil))
        (-dot-repeat-hook-inner
          (not (equal curr-buff last-buffer))
          (not (equal curr-win last-window)))
        (setq last-buffer curr-buff
              last-window curr-win)))))

(defun -dot-repeat-hook-inner (buffer-changed window-changed)
  (when -recording-dot-repeat
    (let ((pt (point)))
      (cond
        ((or (and window-changed (not buffer-changed))
             (and buffer-changed (not (eq -last-local-pt pt))))
         (-abort-recording))
        ((not (or window-changed buffer-changed))
          (-record-this-cmd)
          (setq -last-local-pt pt))))))

(defvar-local -last-local-pt nil
              "Wile recording, location of last point so we can abort when commands that switched buffers (which we ignore) changed point as a side effect.")


(defun -detect-buffer-changes (&rest args)
  (when -recording-dot-repeat
    (setq -current-dot-repeat-had-changes t)))

(add-hook 'after-change-functions #'-detect-buffer-changes)


;; High-level interface for starting, ending and replaying recording dot repeats.

(defvar -executing-dot-repeat nil)

(defvar-local -dot-repeat-history (il-create 8))

(defun -push-dot-repeat (macro)
  (il-push macro -dot-repeat-history))

;; 'this, 'last, or 'nil
(defvar-local -last-command-was-replay nil)

(defun -track-if-last-command-was-replay ()
  (setq -last-command-was-replay
        (pcase -last-command-was-replay
          ('this 'last)
          ('last nil)
          ('nil nil))))

(add-hook 'post-command-hook #'-track-if-last-command-was-replay)


(defun prev-dot-repeat (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (il-move -dot-repeat-history n)
    (when (equal -last-command-was-replay 'last)
      (undo))
    (undo-boundary)
    (with-undo-amalgamate
      (let ((macro (il-get -dot-repeat-history))
            (-executing-dot-repeat t)
            (this-command this-command))
        (execute-kbd-macro macro)))
    (setq -last-command-was-replay 'this)))

(defun next-dot-repeat (&optional n)
  (interactive "p")
  (prev-dot-repeat (- (or n 1))))

(defun dot-repeat (&optional n)
  (interactive "p")
  (with-undo-amalgamate
    (let ((macro (il-get -dot-repeat-history))
          (-executing-dot-repeat t))
      (execute-kbd-macro macro n)))
  (setq -last-command-was-replay 'this))

(defun dot-repeat-to-kmacro ()
  (interactive)
  (kmacro-push-ring)
  (setq last-kbd-macro (il-get -dot-repeat-history)))

(defun start-dot-repeat ()
  (when (and (not -recording-dot-repeat)
             (not -executing-dot-repeat))
    (setq -last-local-pt (point))
    (-start-recording)))

(defun end-dot-repeat (&optional skip-this-cmd)
  (when -recording-dot-repeat
    (unless skip-this-cmd
      (-record-this-cmd))
    (setq -last-local-pt nil)
    (when (-stop-recording)
      (-push-dot-repeat -last-dot-repeat))))

(provide 'dot-repeat)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "dot-repeat--"))
;; End:
