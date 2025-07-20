;; -*- lexical-binding: t; -*-

(require 'common)
(require 'il)

;; Utils

(defun ms--get-pulse-face ()
  (let* ((sym (make-symbol "ms--pulse-face"))
         (normal-bg (face-attribute 'multi-selection :background nil t))
         (lighter-bg (color-lighten-name normal-bg 20)))
    (custom-declare-face sym `((t :inherit multi-selection
                                  :background ,lighter-bg))
                         "Multi selection pulse face")
    sym))


;; Core API

(defface multi-selection
    `((t :inherit region
         :background ,(colored-region :yellow 0.15 15)))
  "Multi selection")

(defvar-local ms--selections '()
  "Currently active selections

List of non-overlapping bounds in the form of ordered marker-valued cons cells, sorted highest to lowest.")

(defun active-multi-selection (&optional no-error)
  (if ms--selections
      (copy-sequence ms--selections)
    (unless no-error
      (user-error "No active multi selection"))))

(defvar-local ms--selection-history (il-create 100))

(defvar-local ms--selection-overlays '()
  "Overlays of currently rendered selections")

(defun ms--delete-selection-overlays ()
  (dolist (overlay ms--selection-overlays)
    (delete-overlay overlay))
  (setq ms--selection-overlays '()))

(defun multi-select-redisplay ()
  (ms--delete-selection-overlays)
  (pcase-dolist (`(,beg . ,end) ms--selections)
    (let ((overlay (make-overlay (marker-position beg) (marker-position end))))
      (overlay-put overlay 'face 'multi-selection)
      (overlay-put overlay 'priority '(nil . 1))
      (push overlay ms--selection-overlays))))

(defun ms--mark-bounds (bounds)
  (mark-bounds bounds nil t))

(defun ms--push-sorted-bounds (bounds-list)
  "Insert BOUNDS into `ms--selections', preserving the order and mergin it with existing overlapping bounds."
  (let ((last-higher-cell nil)
        (next-cell ms--selections))
    (dolist (bounds (mapcar #'ms--mark-bounds bounds-list))
      ;; We traverse existing selections highest to lowest; we first skip all strictly higher ones.
      (while (and next-cell
                  (<= (cdr bounds) (caar next-cell)))
        (setq last-higher-cell next-cell
              next-cell (cdr next-cell)))
      ;; After this, we merge into BOUNDS all further bounds that overlap it.
      (while (and next-cell
                  (> (cdar next-cell) (car bounds)))
        (setq bounds (ms--mark-bounds (bounds-union bounds (car next-cell)))
              next-cell (cdr next-cell)))
      ;; Now `last-higher-cell' is either nil or has as car the last strictly higher bound and `next-cell'
      ;; is the  tail of strictly lower bounds and `bounds' was merged with all overlapping existing bounds.
      ;; What only remains is reconstructing the new list from this.
      (if last-higher-cell
          (setq next-cell
                (setcdr last-higher-cell (cons bounds next-cell)))
        (setq next-cell
              (setq ms--selections (cons bounds next-cell)))))))

(defun multi-select (beg end &optional no-push-history)
  (let ((beg (if (markerp beg) beg
               (marker-with-type beg nil)))
        (end (if (markerp end) end
               (marker-with-type end t))))
    (ms--add-sorted (list (cons beg end)) no-push-history)))

(defun ms--add-sorted (bounds-list &optional no-push-history)
  (unless no-push-history
    (il-push (copy-sequence ms--selections) ms--selection-history))
  (ms--push-sorted-bounds bounds-list)
  (multi-select-redisplay))

(defun multi-select-pop ()
  (interactive)
  ;; Pop while the last selection is the current one.
  (while-let ((last (il-last ms--selection-history))
              (_ (equal last ms--selections)))
    (il-pop ms--selection-history))
  (setq ms--selections (il-pop ms--selection-history))
  (multi-select-redisplay))

(defun has-multi-selection-p ()
  (when ms--selections t))

(defun multi-select-clear (&optional no-push-history)
  (interactive)
  (when ms--selections
    (unless no-push-history
      (il-push (copy-sequence ms--selections) ms--selection-history))
    (setq ms--selections '())
    (multi-select-redisplay)))

(defun multi-select-visit (visitor)
  (pcase-dolist (`(,beg . ,end) ms--selections)
    (funcall visitor beg end)))


;; Higher-level API

;; TODO: copy selections to point command?
(defun multi-select-into-list (&optional consume)
  (let ((res '()))
    (multi-select-visit (lambda (beg end)
                          (push (cons beg end) res)))
    (when consume
      (multi-select-clear))
    res))

(defun multi-select-region ()
  (interactive)
  (unless (region-active-p)
    (user-error "Region not active"))
  (let ((mk (mark))
        (pt (point)))
    (if (> mk pt)
        (multi-select pt mk)
      (multi-select mk pt)))
  (deactivate-mark))

(defun multi-select-cmd-each (visitor)
  (lambda ()
    (interactive)
    (unless (has-multi-selection-p)
      (user-error "No active multi selection"))
    (multi-select-visit visitor)
    (multi-select-clear)))

(defun multi-select-cmd-everything (visitor)
  (lambda ()
    (interactive)
    (unless (has-multi-selection-p)
      (user-error "No active multi selection"))
    (let ((bounds (multi-select--total-bounds)))
      (funcal visitor (car bounds) (cdr bounds)))
    (multi-select-clear)))

(defun multi-select-merge-selections ()
  (interactive)
  (if-let ((bounds (multi-select--total-bounds)))
      (progn
        (multi-select-clear)
        (multi-select (car bounds) (cdr bounds) t))
    (user-error "No active multi selection")))

(defun multi-select-visit-as-regions (cmd &optional non-interactively)
  (save-mark-and-excursion
    (multi-select-visit (lambda (beg end)
                          (push-mark beg t t)
                          (goto-char end)
                          (if non-interactively
                              (funcall cmd)
                            (call-interactively cmd))))))


(defun multi-select--total-bounds ()
  (unless (has-multi-selection-p)
    (user-error "No active multi selection"))
  (let ((min-pos nil)
        (max-pos nil))
    (multi-select-visit (lambda (beg end)
                          (when (or (not min-pos)
                                    (< beg min-pos))
                            (setq min-pos beg))
                          (when (or (not max-pos)
                                    (> end max-pos))
                            (setq max-pos end))))
    (when (and min-pos max-pos)
      (cons min-pos max-pos))))


(defun multi-select-into-region (&optional n)
  (interactive "p")
  (let* ((n (or n 1))
         (bounds (multi-select--total-bounds))
         (mk (car bounds))
         (pt (cdr bounds)))
    (multi-select-clear)
    (when (< n 0)
      (swap pt mk))
    (push-mark mk nil t)
    (goto-char pt)))

(use-package multiple-cursors)

(with-eval-after-load 'multi-insert
  
  (defun multi-select-insert ()
    (interactive)
    (multi-select-visit (lambda (beg end)
                          (multi-insert-create-cursor beg)))
    (multi-select-clear)
    (pop-into-multi-insert))

  (defun multi-select-append ()
    (interactive)
    (multi-select-visit (lambda (beg end)
                          (multi-insert-create-cursor end)))
    (multi-select-clear)
    (pop-into-multi-insert))

  (defun multi-select-change ()
    (interactive)
    (multi-select-visit (lambda (beg end)
                          (delete-region beg end)
                          (multi-insert-create-cursor beg)))
    (multi-select-clear)
    (pop-into-multi-insert)))

(with-eval-after-load 'thing-nav
  (defvar-local ms--last-thing nil)
  
  (defun multi-select-things-at-point (thing &optional n)
    (interactive (list current-thing (prefix-numeric-value current-prefix-arg)))
    (setq ms--last-thing thing)
    (dolist (bound (
                    bounds-of-nearest-things-at-point
                    ;; bounds-of-things-at-point
                    thing n))
      (multi-select (car bound) (cdr bound))))

  (defun multi-select-extend-by-thing (thing &optional n)
    (interactive (list
                  (or ms--last-thing (error "No previous thing selection to extend"))
                  (prefix-numeric-value current-prefix-arg)))
    (let* ((n (or n 1))
           (forward (> n 0))
           (n (abs n))
           (new-bounds '()))
      (save-excursion
        (goto-char (if-let ((bounds (multi-select--total-bounds)))
                       (if forward (cdr bounds)
                         (car bounds))
                     (user-error "No active multi selection")))
        (catch 'stop
          (dotimes (_ n)
            (if-let ((bounds (bounds-of-next-thing thing (if forward +1 -1) t)))
                (progn
                  (push bounds new-bounds)
                  (goto-char (if forward (cdr bounds)
                               (car bounds))))
              (warn "No next thing")
              (throw 'stop nil)))))
      (dolist (bounds new-bounds)
        (multi-select (car bounds) (cdr bounds)))))

  (defun multi-select-extend-by-thing-reverse (thing &optional n)
    (interactive (list
                  (or ms--last-thing (error "No previous thing selection to extend"))
                  (prefix-numeric-value current-prefix-arg)))
    (multi-select-extend-by-thing thing (- (or n 1))))

  (defun multi-select-things-in-region (thing beg end)
    (interactive (list current-thing (region-beginning) (region-end)))
    (ms--add-sorted (bounds-of-things-in-bounds thing (cons beg end)))
    (setq ms--last-thing thing)
    (deactivate-mark))

  (defun multi-select-refine-by-thing (thing)
    (interactive (list current-thing))
    (let ((bounds-list (apply #'append
                              (mapcar (apply-partially #'bounds-of-things-in-bounds thing)
                                      ms--selections))))
      (multi-select-clear)
      (ms--add-sorted (mapcar #'ms--mark-bounds bounds-list) t)))
  
  )

(defun multi-wrap ()
  (interactive)
  (when-let ((pair (ask-pair)))
    (save-mark-and-excursion
      (multi-select-visit-as-regions
       (lambda ()
         (sp-wrap-with-pair (plist-get pair :open)))
       t))
    (multi-select-clear)))


(defun ms--forward (&optional n)
  (interactive "p")
  (let* ((prev nil)
         (n (or n 1))
         (forward (> n 0))
         (n (abs n))
         (pt (point))
         (next (reverse ms--selections)))
    ;; Move along NEXT until either all selections ending strictly before point were skipped if FORWARD
    ;; or all selections beginning strictly before point were pushed onto PREV otherwise.
    (while (and next
                (> pt (if forward
                          (cdar next)
                        (caar next))))
      (push (car next) prev)
      (setq next (cdr next)))
    (let ((to-traverse (if forward next prev)))
      (dotimes (_ n)
        (unless to-traverse
          (user-error "No %s multi-selection" (if forward "next" "previous")))
        (goto-char (if forward (cdar to-traverse)
                     (caar to-traverse)))
        (setq to-traverse (cdr to-traverse))))))

(put 'multi-select 'forward-op #'ms--forward)


(provide 'multi-select)
