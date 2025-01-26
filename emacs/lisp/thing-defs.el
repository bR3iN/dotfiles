;; -*- lexical-binding: t; -*-

;; Defining things
;;
;; Note: 'forward-op is sufficient to define a thing as long as the thing can't have length zero; in the latter case also defining 'bounds-of-thing-at-point is enough.

(require 'cl-lib)

(defun def-ts-thing (thing pattern)
  (put thing 'beginning-op
       (apply-partially #'treesit-beginning-of-thing pattern))
  (put thing 'end-op
       (apply-partially #'treesit-end-of-thing pattern))
  (put thing 'forward-op
       (lambda (n)
         (interactive "p")
         (if (> n 0) (treesit-end-of-thing pattern n)
           (treesit-beginning-of-thing pattern (- n)))))
  (put thing 'thing-at-point
       (apply-partially #'treesit-thing-at-point pattern 'nested))
  (put thing 'bounds-of-thing-at-point
       (lambda ()
         (when-let ((node (treesit-thing-at-point pattern 'nested)))
                   (cons
                     (treesit-node-start node)
                     (treesit-node-end node))))))

(def-ts-thing 'ts-fun "function_item")
(def-ts-thing 'ts-block "block")


(defvar-local -find-char nil)

(defun -init-find ()
  (setq -find-char (string (read-char "Char to search: ")))
  'find)

(defun find-char (&optional n)
  (interactive "p")
  (let ((thing (-init-find)))
    (set-current-thing thing)
    (next-thing thing n)))

(cl-labels ((forward-search-thing (f-search b-search target n)
                                  ;; f-search and b-search will be search-forward[-regexp] and search-backward[-regexp], respectively.
              (if (< n 0) (backward-search-thing f-search b-search target (- n))
                (let ((next-end (save-excursion
                                  ;; search-forward does only find ends of matches also starting after point, so we have to go back first to handle the case where we are inside a match.
                                  (if (funcall b-search target nil t)
                                    ;; First search moves to the end of the previous match which must end before point for a previous match to be found, the second one must then be after point.
                                    (funcall f-search target nil t 2)
                                    ;; No previous match means the first match in the buffer (if any) must end after point
                                    (beginning-of-buffer)
                                    (funcall f-search target nil t)))))
                  (unless next-end
                    (error "No next match"))
                  (goto-char next-end)
                  (when (> n 1)
                    (funcall f-search target nil nil (1- n))))))
            ;; Same as above in reverse
            (backward-search-thing (f-search b-search target n)
              (if (< n 0) (forward-search-thing f-search b-search target (- n))
                (let ((prev-start (save-excursion
                                    (if (funcall f-search target nil t)
                                      (funcall b-search target nil t 2)
                                      (end-of-buffer)
                                      (funcall b-search target nil t)))))
                  (unless prev-start
                    (error "No next match"))
                  (goto-char prev-start)
                  (when (> n 1)
                    (funcall b-search target nil nil (1- n)))))))

           (put 'find 'forward-op
                (lambda (&optional n)
                  (let ((isearch-wrap-pause nil)
                        (case-fold-search nil))
                    (forward-search-thing
                      #'search-forward #'search-backward
                      -find-char (or n 1)))))

           (put 'search 'forward-op
                (lambda (&optional n)
                  (forward-search-thing
                    #'search-forward #'search-backward
                    (or (car search-ring)
			(error "No last search"))
		    (or n 1))))

           (put 'regexp-search 'forward-op
                (lambda (&optional n)
                  (forward-search-thing
                    #'search-forward-regexp #'search-backward-regexp
                    (or (car regexp-search-ring)
			(error "No last regexp search"))
		    (or n 1)))))




; (defvar-local -current-ts-node nil)
; (defun -init-treesit ()
;   (print (setq -current-ts-node (treesit-node-on (point) nil)))
;   'treesit)

; (defun bounds-of-treesit-at-point ()
;   (when -current-ts-node
;     (let ((start (treesit-node-start -current-ts-node))
;           (end (treesit-node-end -current-ts-node))
;           (pt (point)))
;       (when (not (or (< pt start)
;                      (> pt end)))
;         `(,start . ,end)))))

; (put 'treesit 'bounds-of-thing-at-point #'bounds-of-treesit-at-point)

; (defun forward-treesit (&optional n)
;   (interactive "p")
;   (let ((n (or n 1)))
;     (if (< n 0) (backward-treesit (- n))
;       (when-let* ((bounds (bounds-of-treesit-at-point))
;                   (_ (= (point) (cdr bounds))))
;                  (setq n (1+ n)))
;       (dotimes (i (abs (1- n)))
;         (if-let ((node (treesit-node-next-sibling -current-ts-node t)))
;                 (setq -current-ts-node node)
;                 (print "No next sibling node")
;                 (setq i n) ; early exit
;                 ))
;       (goto-char (treesit-node-end -current-ts-node)))))

; (defun backward-treesit (&optional n)
;   (interactive "p")
;   (let ((n (or n 1)))
;     (if (< n 0) (forward-treesit (- n))
;       (when-let* ((bounds (bounds-of-treesit-at-point))
;                   (_ (= (point) (car bounds))))
;                  (setq n (1+ n)))
;       (dotimes (i (abs (1- n)))
;         (if-let ((node (treesit-node-prev-sibling -current-ts-node t)))
;                   (setq -current-ts-node node)
;                   (print "No previous sibling node")
;                   (setq i n) ; early exit
;                   ))
;       (goto-char (treesit-node-start -current-ts-node)))))

; (put 'treesit 'forward-op #'forward-treesit)

; (put 'treesit 'down-op
;      (lambda (forward)
;        (let* ((node -current-ts-node)
;               (children (treesit-node-children node t)))
;          (while (length= children 0)
;                 (setq node (if forward
;                              (treesit-node-next-sibling node t)
;                              (treesit-node-prev-sibling node t)))
;                 (unless node (error "No children found"))
;                 (setq children (treesit-node-children node t)))
;          ; (print children)
;          ; (print forward)
;          (setq -current-ts-node (treesit-node-child node (if forward 0 -1) t))
;          ; (print -current-ts-node)
;          (goto-char (if forward (treesit-node-start -current-ts-node)
;                       (treesit-node-end -current-ts-node)))
;          'treesit)))

; (put 'treesit 'up-op (lambda (forward)
;                        (if-let* ((node -current-ts-node)
;                                  (parent (treesit-parent-until
;                                            node
;                                            (lambda (node)
;                                              (treesit-node-check node 'named)))))
;                                (progn
;                                  (setq -current-ts-node parent)
;                                  (goto-char (if forward (treesit-node-end parent)
;                                               (treesit-node-start parent))))
;                                (error "No parent node"))
;                        'treesit))

;; Add forward-op for buffer, simply skipping to the start or end of the current buffer.
(put 'buffer 'forward-op (lambda (&optional n)
                           (goto-char (buffer-end (or n 1)))))

;; Define 'inner-line thing, representing the area from (beginning-of-line-text) to (end-of-line).
(defun forward-inner-line (&optional n)
  (let ((pt (point))
        (bounds (bounds-of-inner-line-at-point)))
    (cond
      ((> n 0)
       (if (and bounds (= pt (cdr bounds)))
         (end-of-line (+ n 1))
         (end-of-line (+ n 0))))
      ((< n 0)
       (if (or (not bounds)
               (= pt (car bounds)))
         (beginning-of-line-text (+ n 1))
         (beginning-of-line-text (+ n 2)))))))

(put 'inner-line 'forward-op
     #'forward-inner-line)

;; Bounds function is neccessary as inner-line can be of length zero
(defun bounds-of-inner-line-at-point ()
  (let ((beg (save-excursion
               (beginning-of-line-text)
               (point)))
        (end (save-excursion
               (end-of-line)
               (point)))
        (pt (point)))
    (when (not (< pt beg))
      `(,beg . ,end))))

(put 'inner-line 'bounds-of-thing-at-point
     #'bounds-of-inner-line-at-point)

; Fix forward-line, which by default doesn't move to the beginning of the
; current line with -1 as argument, but to the start of the last line instead.
(defun fixed-forward-line (&optional n)
  (let ((n (or n 1)))
    (when (and (< n 0) (not (bolp)))
      (setq n (+ n 1)))
    (forward-line n)))
(put 'line 'forward-op #'fixed-forward-line)
(put 'line 'beginning-op (lambda (&optional n)
                           (fixed-forward-line (- (or n 1)))))

; Have paragraphs don't include the previous blank line; we have -next-thing and -previous-thing to include space between things.
(defun fixed-backward-paragraph (&optional n)
  (interactive "p")
  (if (and n (< n 0)) (forward-paragraph (- n))
    (when (and (bolp) (not (bobp)))
      (previous-line))
    (backward-paragraph n)
    (when (not (bobp))
      (next-line))))

(defun fixed-forward-paragraph (&optional n)
  (interactive "p")
  (if (and n (< n 0)) (fixed-backward-paragraph (- n))
    (forward-paragraph n)))

(put 'paragraph 'forward-op #'fixed-forward-paragraph)

;; ;; TODO: breaks things as we can't find things with forward-backward movement anymore
;; ;; FIXME: instead of two forwards, go with (beg op or forward -1) and (end-op or forward) from point (as in orig bound-of-thing-at-point). This should fix it
;; NO, nested things break a lot of stuff, use sp movements only for M- binds
;; (with-eval-after-load 'smartparens
;;   (put 'sexp 'forward-op #'sp-forward-sexp))

;; Thing extensions

; (defun forward-down-thing (thing &optional n)
;   (let* ((n (or n 1))
;          (forward (> n 0))
;          (n (abs n)))
;     (while (> n 0)
;            (if-let ((down-op (get thing 'down-op)))
;                    (setq thing (funcall down-op forward)
;                          n (1- n))
;                    (error (concat "Don't know how to go down a " (symbol-name thing)))))
;     thing))


; (defun backward-down-thing (thing &optional n)
;   (forward-down-thing thing (- (or n 1))))


; (defun forward-up-thing (thing &optional n)
;   (let* ((n (or n 1))
;          (forward (> n 0))
;          (n (abs n)))
;     (while (> n 0)
;            (if-let ((up-op (get thing 'up-op)))
;                    (setq thing (funcall up-op forward)
;                          n (1- n))
;                    (error (concat "Don't know how to go up a " (symbol-name thing)))))
;     thing))


; (defun backward-up-thing (thing &optional n)
;   (forward-up-thing thing (- (or n 1))))

; (defun -extend-thing (thing up-op down-op)
;   (put thing 'up-op up-op)
;   (put thing 'down-op down-op))

; (defun -direction (forward)
;   (if forward 1 -1))

; (put 'sexp 'up-op
;   (lambda (forward)
;     (up-list (-direction forward) t t)
;     'sexp))

; (put 'sexp 'down-op
;   (lambda (forward)
;     (down-list (-direction forward) t)
;     'sexp))


; (defun -contains (upper-thing inner-thing)
;   (cl-labels ((fdown (forward)
;                      (if (not forward) (bdown t)
;                        (unless (tapp upper-thing)
;                          (error (concat "No " (symbol-name upper-thing) " at point")))
;                        (thing-nav-beginning-of-thing upper-thing nil)
;                        (unless (botp inner-thing nil)
;                          (forward-thing inner-thing 1)
;                          (thing-nav-beginning-of-thing inner-thing t))
;                        inner-thing))
;               (bdown (backward)
;                      (if (not backward) (fdown t)
;                        (unless (tapp upper-thing)
;                          (error (concat "No " (symbol-name upper-thing) " at point")))
;                        (thing-nav-end-of-thing upper-thing t)
;                        (unless (eotp inner-thing t)
;                          (forward-thing inner-thing -1)
;                          (thing-nav-end-of-thing inner-thing nil))
;                        inner-thing)))
;              (put upper-thing 'down-op #'fdown)))

; (defun -is-contained-in (inner-thing upper-thing)
;   (cl-labels ((fup (forward)
;                    (if (not forward) (bup t)
;                      (unless (eotp upper-thing t)
;                        (thing-nav-end-of-thing upper-thing t))
;                      upper-thing))
;               (bup (backward)
;                    (if (not backward) (fup t)
;                      (unless (botp upper-thing nil)
;                        (thing-nav-beginning-of-thing upper-thing nil))
;                      upper-thing)))
;              (put inner-thing 'up-op #'fup)))

; (-contains 'line 'inner-line)
; (-is-contained-in 'inner-line 'line)
; (-contains 'paragraph 'line)
; (-is-contained-in 'line 'paragraph)
; (-contains 'inner-line 'symbol)
; (-is-contained-in 'symbol 'inner-line)

; ; Working
; ; (-contains 'paragraph 'line)
; ; (-is-contained-in 'line 'paragraph)
; ; (-contains 'line 'symbol)
; ; (-is-contained-in 'symbol 'line)

; (-contains 'buffer 'paragraph)
; (-is-contained-in 'paragraph 'buffer)


; (-contains 'symbol 'word)
; (-is-contained-in 'word 'symbol)


(defun forward-flymake-diagnostic (&optional n)
  (interactive "p")
  (let ((n (or n 1)))
    (cond ((< n 0) (flymake-goto-prev-error (- n) nil t))
          ((> n 0) (when-let ((bounds (-flymake-overlay-in (point) (1+ (point)))))
                             (goto-char (cdr bounds))
                             (setq n (1- n)))
                   (flymake-goto-next-error n nil t)
                   (goto-char (cdr (-flymake-overlay-in (point) (1+ (point)))))))))

(defun -flymake-overlay-in (beg end)
  (cl-some (lambda (ov)
             (if (overlay-get ov 'flymake-diagnostic)
               (cons (overlay-start ov) (overlay-end ov))
               nil))
           (overlays-in beg end)))

(put 'flymake 'forward-op #'forward-flymake-diagnostic)

(defvar char-to-thing
  `(
    (?f . ts-fun)
    ; (?f . filename)
    (?e . sexp)
    (?W . symbol)
    (?w . word)
    (?B . ts-block)
    (?b . buffer)
    (?c . -init-find)
    (?C . char)
    (?S . regexp-search)
    (?s . search)
    (?n . number)
    (?, . ,(lambda () current-thing))
    (?p . paragraph)
    (?l . line)
    (?v . visual-line)
    (?t . -init-treesit)
    (?o . list)
    (?i . inner-line)
    ;; (32 . whitespace) ; space
    (32 . sexp) ; space
    (?d . defun)
    (?D . flymake)
    (?. . sentence)))


(defun read-thing ()
  (let* ((char (read-char "Select thing"))
         (thing-assoc (assoc char char-to-thing)))
    (when (not thing-assoc)
      (error (concat "Character not associated with thing: " (string char))))
    (let ((thing (cdr thing-assoc)))
      (if (functionp thing) (funcall thing) thing))))


(provide 'thing-defs)
;; Local Variables:
;; read-symbol-shorthands: (("-" . "thing-defs--"))
;; End:
