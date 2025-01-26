;; -*- lexical-binding: t; -*-

(require 'ring)
(require 'utils)


;; Track prefix key events so we can retroactively record them for the command starting a dot-repeat.

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
(defvar-local -current-dot-repeat-had-changes nil)

(defvar-local -recording-dot-repeat nil)

(defun -abort-dot-repeat ()
  (setq -current-dot-repeat '()
        -recording-dot-repeat nil))

(defun -start-dot-repeat ()
  (setq -recording-dot-repeat t))

(defun -end-dot-repeat ()
  (prog2
    (when -current-dot-repeat-had-changes
      (setq -last-dot-repeat (apply #'vconcat (reverse -current-dot-repeat))))
    -current-dot-repeat-had-changes
    (setq -current-dot-repeat nil
          -current-dot-repeat-had-changes nil
          -recording-dot-repeat nil)))

(defun -record-this-cmd ()
  ;; For the first command, also record its prefix keys
  (when (not -current-dot-repeat)
    (setq -current-dot-repeat -current-prefix-keys))
  (push (this-command-keys-vector) -current-dot-repeat))

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

(defvar-local -last-local-pt nil
              "Wile recording, location of last point so we can abort when commands that switched buffers (which we ignore) changed point as a side effect.")

(defun -dot-repeat-hook-inner (buffer-changed window-changed)
  (when -recording-dot-repeat
    (let ((pt (point)))
      (cond
        ((or (and window-changed (not buffer-changed))
             (and buffer-changed (not (eq -last-local-pt pt))))
         (-abort-dot-repeat))
        ((not (or window-changed buffer-changed))
          (-record-this-cmd)
          (setq -last-local-pt pt))))))

(add-hook 'post-command-hook #'-dot-repeat-hook)

(defun -detect-buffer-changes (&rest _)
  (when -current-dot-repeat
    (setq -current-dot-repeat-had-changes t)))

(add-hook 'after-change-functions #'-detect-buffer-changes)


;; High-level interface for starting, ending and replaying recording dot repeats.

(defvar -executing-dot-repeat nil)

(defvar -dot-repeat-history '())
(defvar -dot-repeat-history-cursor (create-cursor -dot-repeat-history))

(defvar-local -local-dot-repeat-history '())
(defvar-local -local-dot-repeat-history-cursor (create-cursor -local-dot-repeat-history))

(defun -push-dot-repeat (macro)
  (push macro -dot-repeat-history)
  (push macro -local-dot-repeat-history)
  (setq -dot-repeat-history-cursor (create-cursor -dot-repeat-history))
  (setq -local-dot-repeat-history-cursor (create-cursor -local-dot-repeat-history)))

(defvar -last-dot-repeat-command nil)

(defun prev-dot-repeat (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (cursor-move n -dot-repeat-history-cursor)
    (let ((macro (cursor-get -dot-repeat-history-cursor)))
      (when (equal last-command -last-dot-repeat-command) (undo))
      (setq -last-dot-repeat-command this-command)
      (let ((-executing-dot-repeat t)
            (this-command this-command))
        (with-undo-amalgamate
          (execute-kbd-macro macro))))))

(defun next-dot-repeat (&optional n)
  (interactive "p")
  (prev-dot-repeat (- (or n 1))))

(defun dot-repeat (&optional n)
  (interactive "p")
  (setq -last-dot-repeat-command this-command)
  (let ((macro (cursor-get -dot-repeat-history-cursor))
        (-executing-dot-repeat t))
    (execute-kbd-macro macro n)))

(defun start-dot-repeat ()
  (when (and (not -recording-dot-repeat)
             (not -executing-dot-repeat))
    (setq -last-local-pt (point))
    (-start-dot-repeat)))

(defun end-dot-repeat (&optional skip-this-cmd)
  (when -recording-dot-repeat
    (unless skip-this-cmd
      (-record-this-cmd))
    (setq -last-local-pt nil)
    (when (-end-dot-repeat)
      (-push-dot-repeat -last-dot-repeat))))


(provide 'dot-repeat)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "dot-repeat--"))
;; End:
