;; -*- lexical-binding: t; -*-

(require 'utils)
(require 'color-utils)

;<thing>[space]<thing>[space]<thing><thing><thing>
;<thing>[space]<thing>||<thing><thing>

;; (defun -beginning-of-thing (thing)
;;   (if-let ((bop (get thing 'beginning-op)))
;;       (funcall bop)
;;     (funcall (get thing 'forward-op) +1)))

(defun -bounds-of-thing-ending-after-point (thing)
  "Bounds of the next thing ending strictly after point. Might also start after point and can be of length zero."
  (when (not (eobp))
    (save-excursion
      (ignore-errors
        (forward-thing thing 1)
        (let ((bounds (bounds-of-thing-at-point thing))
              (end (point)))
          ;; Due to the forward-thing, we are at the end of a thing ending strictly after point. Things
          ;; of non-zero length are found by simply going backwards with forward-thing. To catch "empty"
          ;; things however, we have to use bounds-of-thing-at-bound. But as the latter could also yield
          ;; the next thing (if there is nothing in between this and the next thing, as e.g. for lines)
          ;; this is not sufficient on its own.
          (if (and bounds (= end (cdr bounds))) bounds
            (forward-thing thing -1)
            (cons (point) end)))))))

(defun -bounds-of-thing-starting-before-point (thing)
  "Bounds of the next thing starting strictly before point. Might also end before point and can be of length zero."
  (when (not (bobp))
    (save-excursion
      (ignore-errors
        (forward-thing thing -1)
        (let ((bounds (bounds-of-thing-at-point thing))
              (start (point)))
          (if (and bounds (= start (car bounds))) bounds
            (forward-thing thing 1)
            (cons start (point))))))))

;; FIXME: Assumes bounds-of-thing-at-point prefers the next to the previous thing, which holds for things only having forward-op due to `'bounds-of-thing-at-point's implementation but could fail if 'bounds-of-thing-at-point does not adhere to this. Only relevant for things that can have non-empty space between them, though, which are currently only lines and characters.
(defun thing-nav-bounds-of-thing-at-point (thing &optional prefer-previous-thing)
  (cl-flet ((-previous-bounds ()
              (forward-thing thing -1)
              (bounds-of-thing-at-point thing)))
  (if-let ((pt (point))
           (bounds (bounds-of-thing-at-point thing)))
      (if (and prefer-previous-thing
               (eq pt (car bounds)))
          (-previous-bounds)
        bounds)
    (-previous-bounds)
    )))

; prefer-backward is relevant for things that have empty space between them,
; while the non forward-thing based fallback is needed for things that are empty
; themself.
(defun thing-nav-bounds-of-thing-at-point2 (thing &optional prefer-backward)
  "Get bounds of THING at point. In case there is both a THING ending and starting at point, take the ending one iff PREFER-BACKWARD."
  (cl-flet* ((bounds nil)
             (is-in-bounds ()
                           (and bounds
                             (let ((pt (point)))
                               (not (or (< pt (car bounds))
                                        (> pt (cdr bounds)))))))
             (try-forward ()
                          (setq bounds (-bounds-of-thing-ending-after-point thing))
                          (is-in-bounds))
             (try-backward ()
                           (setq bounds (-bounds-of-thing-starting-before-point thing))
                           (is-in-bounds))
             ;; Needed to find things at point that are of length zero as the
             ;; above only find things starting strictly before or ending
             ;; strictly after point.
             (try-with-op ()
                          (setq bounds (bounds-of-thing-at-point thing))))
            (when (if prefer-backward
                    (or (try-backward) (try-forward) (try-with-op))
                    (or (try-forward) (try-backward) (try-with-op)))
              bounds)))


(defun -eotp (thing &optional prefer-previous-thing backwards)
  (if backwards
      (-botp thing prefer-previous-thing nil)
    (when-let ((bounds (thing-nav-bounds-of-thing-at-point thing prefer-previous-thing)))
      (eq (point) (cdr bounds)))))

(defun -botp (thing &optional prefer-previous-thing backwards)
  (if backwards
      (-eotp thing prefer-previous-thing nil)
    (when-let ((bounds (thing-nav-bounds-of-thing-at-point thing prefer-previous-thing)))
      (eq (point) (car bounds)))))

;; TODO: needed?, yes for insert cmds
(defun bounds-of-nearest-thing-at-point (thing n &optional prefer-previous-thing)
  "Find the bounds of the Nth nearest thing around point. Will search
forward if N is positive and backwards for negative N. This can
include the current thing at point."
  (let* ((forward (> n 0))
         (n (abs n))
         (curr-bounds (thing-nav-bounds-of-thing-at-point
                       thing (xor (not forward) prefer-previous-thing))))
    ;; When we are at the end of a thing at point (or beginning if
    ;; moving backwards), we need to call forward-thing one time less
    ;; as we already count this one.
    (when (and curr-bounds
               (eq (point) (-bounds-end curr-bounds (not forward))))
      (setq n (1- n)))
    (when (> n 0)
      (save-excursion
        (forward-thing thing (if forward n (- n)))
        (setq curr-bounds (thing-nav-bounds-of-thing-at-point thing forward))))
    curr-bounds))


(defun -bounds-end (bounds &optional backward)
  (if backward (car bounds)
    (cdr bounds)))

(defun -bounds-start (bounds &optional backward)
  (if backward (cdr bounds)
    (car bounds)))


(defun bounds-of-next-thing (thing n &optional prefer-previous-thing)
  "Get the bounds of the Nth next thing, not counting the current thing at point (if existent). What is considered the current thing if between two things is determined by PREFER-PREVIOUS-THING."
  (let* ((forward (> n 0))
         (n (abs n))
         (bounds (thing-nav-bounds-of-thing-at-point
                  thing (xor (not forward) prefer-previous-thing))))
    ;; General idea here is to make N into the number of times to forward-thing to get to the thing we want. This means to add one if we are inside a thing but not at the end.
    (when (and bounds (not (eq (point) (-bounds-end bounds (not forward)))))
      (setq n (1+ n)))
    (save-excursion
      (forward-thing thing (if forward n (- n)))
      (thing-nav-bounds-of-thing-at-point thing forward))))


(defun forward-to-nearest-thing-at-point (thing n) 
  (when-let ((bounds (bounds-of-nearest-thing-at-point thing n)))
          (goto-char (if (> n 0) (cdr bounds) (car bounds)))))


(defun bounds-of-nearest-things-at-point (thing n &optional prefer-previous-thing)
  (let* ((forward (> n 0))
         (n (abs n))
         (curr-bounds (thing-nav-bounds-of-thing-at-point
                       thing (xor (not forward) prefer-previous-thing)))
         (res '()))
    ;; Same logic here as in bounds-of-nearest-thing-at-point above
    (when (and curr-bounds
               (eq (point) (-bounds-end curr-bounds (not forward))))
      (push curr-bounds res)
      (setq n (1- n)))
    (save-excursion
      (dotimes (_ n)
        (forward-thing thing (if forward +1 -1))
        (let ((bounds (thing-nav-bounds-of-thing-at-point thing forward)))
          ;; FIXME: defensive, can we assume that forward-thing errors if there is no next thing?
          (when (and res
                     (eq (cdar res) (cdr bounds)))
            (error "No next thing"))
          (push bounds res))))
    (if forward (nreverse res) res)))


(defun thing-nav-beginning-of-thing (thing &optional prefer-backward)
  (when-let ((bounds (thing-nav-bounds-of-thing-at-point thing prefer-backward)))
            (goto-char (car bounds))))


(defun thing-nav-end-of-thing (thing &optional prefer-backward)
  (when-let ((bounds (thing-nav-bounds-of-thing-at-point thing prefer-backward)))
            (goto-char (cdr bounds))))

(defun tapp (thing)
  "Is there a [T]HING [a]t [p]oint?"
  (boolify (thing-nav-bounds-of-thing-at-point thing)))

(defun eotp (thing &optional prefer-backward)
  "Is point at the [e]nd [o]f a [T]HING?"
  (when-let ((bounds (thing-nav-bounds-of-thing-at-point thing prefer-backward)))
            (= (cdr bounds) (point))))

(defun botp (thing &optional prefer-backward)
  "Is point at the [b]eginning [o]f a [T]HING?"
  (when-let ((bounds (thing-nav-bounds-of-thing-at-point thing prefer-backward)))
            (= (car bounds) (point))))


(defun -range-len (range)
  (- (cdr range) (car range)))


(defun swap-things (thing &optional backward)
  (let* ((bounds-curr (thing-nav-bounds-of-thing-at-point thing nil))
         (bounds-other (if backward
                         (save-excursion
                           (goto-char (car bounds-curr))
                           (-bounds-of-thing-starting-before-point thing))
                         (save-excursion
                           (goto-char (cdr bounds-curr))
                           (-bounds-of-thing-ending-after-point thing))))
         (len-diff (- (-range-len bounds-other) (-range-len bounds-curr))))
    (swap-ranges bounds-other bounds-curr)
    (if backward
      (goto-char (car bounds-other))
      (goto-char (+ (car bounds-other) len-diff)))))

(defun isearch-current-thing-at-point (thing &optional n)
  "Search for nearest `current-thing' at point"
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (if-let* ((n (or n 1))
            (bounds (bounds-of-nearest-thing-at-point thing (/ n (abs n)))))
      (progn
        (isearch-forward nil n)
        (isearch-yank-string
         (buffer-substring-no-properties (car bounds) (cdr bounds))))))


(defun goto-nearest-beginning-of-thing (thing &optional backwards)
  (if backwards
      (goto-nearest-end-of-thing thing nil)
    (unless (-botp thing nil)
      (forward-thing thing -1))))


(defun goto-nearest-end-of-thing (thing &optional backwards)
  (if backwards
      (goto-nearest-beginning-of-thing thing nil)
    (unless (-eotp thing nil)
      (forward-thing thing +1))))


(defun join-things (thing n)
  (interactive (list
                current-thing
                (prefix-numeric-value current-prefix-arg)))
  (join-things-at thing n))


(defun join-things-at (thing n &optional sep)
  (interactive (list
                current-thing
                (prefix-numeric-value current-prefix-arg)
                (read-string "Separator: " nil nil " ")))
  (goto-nearest-beginning-of-thing thing t)
  (when-let* ((bounds (nreverse (bounds-of-nearest-things-at-point thing (inc-abs n))))
              (last (car bounds))
              (rest (cdr bounds)))
    (save-excursion
     (dolist (curr rest)
       (let ((start (cdr curr))
             (end (car last)))
         (delete-region start end)
         (when sep
           (goto-char start)
           (insert sep))
         (setq last curr))))))

;; ;; TODO: all at once or individually?
;; (defun comment-nearest-thing-at-point (thing n)
;;   (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
;;   (dolist (bounds (bounds-of-nearest-things-at-point thing n))
;;     (comment-or-uncomment-region (car bounds) (cdr bounds))))


(defun act-on-tap (cmd)
  (lambda (thing n)
    (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
    (let* ((bounds (bounds-of-nearest-things-at-point thing n))
           (start (caar (last bounds)))
           (end (cdar bounds)))
      (funcall cmd start end))))


(defun -act-on-tap-inner (cb)
  (lambda (thing n)
    (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
    (funcall cb (bounds-of-nearest-things-at-point thing n))
    ))

(defun act-on-tap-alt (cmd)
  (lambda (thing n)
    (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
    (if (> n 0)
        (when (eotp thing t)
          (setq n (1+ n)))
      (when (botp thing nil)
        (setq n (1- n))))
    (let* ((bounds (bounds-of-nearest-things-at-point thing n))
           (start (caar bounds))
           (end (cdar (last bounds))))
      (if (> n 0)
          (funcall cmd (point) (cdar (last bounds)))
        (funcall cmd (caar bounds) (point))))))

(defun act-on-tap-alt2 (cmd)
  (lambda (thing n)
    (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
    (let ((pt (point))
          (other (save-excursion
                   (forward-thing thing n)
                   (point))))
      (funcall cmd pt other))))

(defun act-on-tap-region (cmd)
  (-act-on-tap-inner (lambda (bounds)
                       (let ((start (caar bounds))
                             (end (cdar (last bounds))))
                         (funcall cmd start end)))))

(defun delete-bounds (thing &optional n)
  (let (ustart uend lstart lend outer-thing)
    (save-excursion
      ; Upper bound
      (setq outer-thing (forward-up-thing thing)
            uend (point))
      (backward-down-thing outer-thing)
      (setq ustart (point))
      ; Lower bound
      (backward-up-thing thing)
      (setq lstart (point))
      (forward-down-thing outer-thing)
      (setq lend (point)))
    (delete-region ustart uend)
    (delete-region lstart lend)))



;; Command API for current thing

;; TODO: per-mode default?
(defvar-local current-thing 'inner-line)

(defun set-current-thing (thing)
  (interactive (list (read-thing)))
  ; (when (call-interactively-p)
  ;   (message (symbol-name thing)))
  (setq current-thing thing))


(defun setting-current-thing (thing cmd)
  (adviced cmd
           :what (concat "Sets `current-thing` to " (symbol-name cmd) " after execution.")
           :after (lambda (&rest _)
                    (set-current-thing thing))))


;; ;; TODO: like tmp-thing, should then also make above superflous
;; (defun with-new-thing (cmd &optional no-repeat)
;;   "Asks for new current-thing before executing CMD. If repeated, don't ask again."
;;   (adviced cmd
;;            :what "Asks for new `current-thing` before execution. If repeated, don't ask again."
;;            :around (lambda (func _current-thing &rest r)
;;                      (unless (and (eq this-command last-command) (not no-repeat))
;;                        (set-current-thing (read-thing)))
;;                      (apply func `(,current-thing ,@r)))))

;; TODO: documentation of lambda
(defun with-new-thing (cmd &optional thing no-repeat)
  (lambda ()
    (interactive)
    (unless (and (eq this-command last-command) (not no-repeat))
      (set-current-thing (or thing (read-thing))))
    (call-interactively cmd)))

(defvar-local -last-tmp-thing nil)

(defun with-tmp-thing (cmd &optional thing no-repeat)
  (eval `(with-let ((current-thing (or ',thing
                                       (if (and (eq this-command last-command)
                                                (not ',no-repeat))
                                           -last-tmp-thing
                                         (setq -last-tmp-thing (read-thing))))))
                   ,cmd
                   :what "With temporarily set `current-thing'.")))

;; (defun with-tmp-thing2 (cmd &optional thing no-repeat)
;;   "Set new current-thing just for CMD. If repeated, reuse that thing. Asks for thing if THING is nil."
;;   (adviced cmd
;;            :what "Temporarily changes `current-thing`."
;;            :around (lambda (func _current-thing &rest r)
;;                      (let ((current-thing (or thing
;;                                               (if (and (eq this-command last-command)
;;                                                        (not no-repeat))
;;                                                 -last-tmp-thing
;;                                                 (setq -last-tmp-thing (read-thing))))))
;;                      (apply func `(,current-thing ,@r))))))


(defun beginning-of-thing-cmd (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (let ((n (or n 1)))
    (cond
      ((< n 0) (end-of-thing-cmd (- n)))
      ((> n 0) (forward-thing thing (- n))))))


(defun end-of-thing-cmd (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (let ((n (or n 1)))
    (cond
      ((< n 0) (beginning-of-thing-cmd (- n)))
      ((> n 0) (forward-thing thing n)))))


;; (defun next-thing (thing &optional n)
;;   (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
;;   (let ((n (or n 1)))
;;     (if (< n 0) (previous-thing thing (- n))
;;       ;; Check if we are inside but not at the end of a thing, in which case we have to move one extra thing forward
;;       (let ((bounds (thing-nav-bounds-of-thing-at-point thing nil)))
;;         (when (and bounds (/= (point) (cdr bounds)))
;;           (setq n (1+ n))))
;;       (forward-thing thing n)
;;       (thing-nav-beginning-of-thing thing t))))

(defun next-thing (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (let* ((n (or n 1))
         (bounds (bounds-of-next-thing thing n nil)))
    (goto-char (-bounds-start bounds (< n 0)))))

(defun previous-thing (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (let ((n (or n 1)))
    (if (< n 0) (next-thing thing (- n))
      (let ((bounds (thing-nav-bounds-of-thing-at-point thing t)))
	(when (and bounds (/= (point) (car bounds)))
          (setq n (1+ n))))
      (forward-thing thing (- n))
      (thing-nav-end-of-thing thing nil))))

(defun mark-thing (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (unless (= n 0)
    (let* ((n (or n 1))
           (forward (> n 0))
           (direction (if forward 1 -1))
           (nr-ext (1- (abs n)))
           (bounds (bounds-of-thing-at-point current-thing)))
      (if (not bounds) (error (concat "No " (symbol-name current-thing) " at point"))
        (let ((mk (if forward (car bounds) (cdr bounds)))
              (pt (if forward (cdr bounds) (car bounds))))
          (push-mark mk t t)
          (goto-char pt))
        (when (> nr-ext 0)
          (forward-thing current-thing (* direction nr-ext)))))))


(defun down-thing-cmd (thing &optional n)
  "Go down things."
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (set-current-thing
    (forward-down-thing current-thing n)))


(defun up-thing-cmd (thing &optional n)
  "Go up things."
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (set-current-thing
    (forward-up-thing current-thing n)))


(defmacro save-current-thing (&rest body)
  (let ((tmp (gensym "saved-current-thing-")))
    `(let ((,tmp current-thing))
       (unwind-protect
         (progn ,@body)
         (setq current-thing ,tmp)))))


(defun end-of-inner (thing &optional backward)
  (interactive (list current-thing (not (pos-prefix-arg-p))))
  (let ((dir (if backward -1 1)))
    (up-thing-cmd thing dir)
    (down-thing-cmd thing (- dir))))


(defun end-of-outer (thing &optional backward)
  (interactive (list current-thing (not (pos-prefix-arg-p))))
  (let ((dir (if backward -1 1)))
    (down-thing-cmd thing dir)
    (up-thing-cmd thing dir)))


; NEXT: mark inner of outer (n being up-steps) + include tracing

(defun down-until-point-moves (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (let ((direction (if (> n 0) 1 -1))
        (n (abs n))
        (pt (point)))
    (while (and (= pt (point)))
           (set-current-thing
             (forward-down-thing current-thing direction)))
    (when (> n 1)
      (next-thing (* direction (1- n))))))


; (defun mark-outer (thing &optional n)
;   (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
;   (let ((direction (if (> n 0) 1 -1)))
;     (up-thing-cmd n)
;     (save-excursion
;       (down-thing-cmd (- direction))
;       (up-thing-cmd (- direction))
;       (push-mark (point) t (not (region-active-p))))))


; (defun mark-inner (thing &optional n)
;   (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
;   (let* ((n (or n 1))
;          (direction (if (> n 0) 1 -1)))
;     (up-thing-cmd n)
;     (down-thing-cmd (- direction))
;     (save-excursion
;       (up-thing-cmd (- direction))
;       (down-thing-cmd direction)
;       (push-mark (point) t (not (region-active-p))))))


(defun forward-swap-thing (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (let* ((n (or n 1))
         (backward (< n 0))
         (n (abs n)))
    (dotimes (_ n)
      (swap-things current-thing backward))))

(defun backward-swap-thing (thing &optional n)
  (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
  (forward-swap-thing (- (or n 1))))


(defvar-local -thing-hints '())

(defface thing-hint
         `((t :inherit region
              :foreground ,(base16-get :bg0)
              :weight bold
              :background ,(base16-get :blue)))
         "Hints for current-thing")


(defun -hint-range (range)
  (let ((overlay (make-overlay (car range) (cdr range))))
    ; TODO: other face?
    (overlay-put overlay 'face 'thing-hint)
    (push overlay -thing-hints)))

; TODO: Only hint first/last?
(defun hint-thing (thing)
  (interactive (list current-thing))
  (clear-thing-hints)
  ; Note: This won't visit a thing at point of length zero, but there is nothing to hint in that case anyway.
  (dolist (forward '(t nil))
    (save-excursion
      (ignore-errors
        (let* ((pos (point))
               (direction (if forward 1 -1))
               (bound (if forward (window-end) (window-start)))
               (is-in-bounds (lambda ()
                               (if forward (< pos bound) (> pos bound)))))
          (while (funcall is-in-bounds)
                 (end-of-thing-cmd current-thing direction)
                 (let ((pt (point)))
                   (unless (/= pt pos) (error "no next thing"))
                   (setq pos pt))
                 (-hint-range
                   (thing-nav-bounds-of-thing-at-point current-thing forward))))))))

(defun toggle-thing-hints ()
  (interactive)
  (if -thing-hints
    (call-interactively #'clear-thing-hints)
    (call-interactively #'hint-thing)))

(defun clear-thing-hints ()
  (interactive)
  (mapcar (lambda (overlay)
            (delete-overlay overlay))
          -thing-hints)
  (setq -thing-hints '()))

; (defun acting-on-nearest-thing (cmd &optional thing)
;     ; (bounds-of-nearest-thing-at-point (or thing current-thing) )
;   (adviced cmd
;            :what (if thing (concat "Calls command on nearest" (symbol-name thing) ".")
;                    "Asks for thing and calls command on nearest instance.")
;            :interactive
;            :around (lambda (func &rest _)
;                      (unless (and (eq this-command last-command) (not no-repeat))
;                        (set-current-thing (or thing (read-thing)))))))

;; Inner and outer TODO: put into separate module and simplifiy this one

(provide 'thing-nav)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "thing-nav--"))
;; End:
