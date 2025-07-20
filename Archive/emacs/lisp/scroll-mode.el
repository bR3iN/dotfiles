;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup smooth-scroll nil
  "Smooth, animated scrolling akin to neoscroll.nvim."
  :group 'convenience)

(defcustom smooth-scroll-easing 'linear
  "Easing function for smooth-scroll.
One of: linear, quadratic, cubic, quartic, quintic, sine, circular."
  :type '(choice (const linear)
          (const quadratic)
          (const cubic)
          (const quartic)
          (const quintic)
          (const sine)
          (const circular))
  :group 'smooth-scroll)

(defcustom smooth-scroll-duration 0.1
  "Default total time in seconds for smooth-scroll animations."
  :type 'number
  :group 'smooth-scroll)
(defun smooth-scroll--get-delay-fn ()
  "Return a one-argument easing function for `smooth-scroll-easing`.
Maps progress [0,1] to eased value [0,1], errors if unrecognized."
  (pcase smooth-scroll-easing
    ('linear    (lambda (x) x))
    ('quadratic (lambda (x) (* x x)))
    ('cubic     (lambda (x) (* x x x)))
    ('quartic   (lambda (x) (expt x 4)))
    ('quintic   (lambda (x) (expt x 5)))
    ('sine      (lambda (x) (- 1 (cos (* x (/ float-pi 2))))))
    ('circular  (lambda (x) (- 1 (sqrt (- 1 (* x x))))))
    (_ (error "smooth-scroll: Unknown easing: %S" smooth-scroll-easing))))

;; (defun smooth-scroll--get-delay-fn ()
;;   "Return a two-argument delay function based on the current easing.
;; The returned function takes STEP and TOTAL and returns the fraction of TOTAL-TIME."
;;   (let ((ef (smooth-scroll--get-easing-fn)))
;;     (lambda (step total)
;;       (let* ((x1 (/ (float step) total))
;;              (x2 (/ (float (1+ step)) total)))
;;         (- (funcall ef x2)
;;            (funcall ef x1))))))

(defun smooth-scroll--half-window-lines (multiplier)
  "Compute number of visual lines as half the window height times MULTIPLIER."
  (max 1 (floor (* multiplier (/ (window-body-height) 2.0)))))

(defmacro smooth-scroll--overwrite (symbol value)
  (let ((old-val (make-symbol "old-val")))
    `(let ((,old-val ,symbol))
       (setq ,symbol ,value)
       (lambda ()
         (setq ,symbol ,old-val)))))

(defun smooth-scroll--animate (lines scroll-time delay-fn)
  "Animate scrolling of LINES over SCROLL-TIME seconds.
DELAY-FN is a function (STEP TOTAL) -> fraction of SCROLL-TIME.
Positive LINES scroll down; negative scroll up.
Stops at buffer boundaries. During intermediate steps, cursor is in col 0; final step restores column."
  (let* ((forward (> lines 0))
         (direction (if forward 1 -1))
         (lines (abs lines))
         (start-time (float-time))
         (next-time (float-time))
         (lines-moved 0)
         (perc-moved 0)
         ;; (temporary-goal-column (cons))
         (cbs '())
         (count 0)
         )
    (cl-labels
        ((move-line (arg)
           (let ((scroll-preserve-screen-position 'always)
                 ;; Required for line-move-visual to preserve the column
                 (last-command this-command))
             (setq lines-moved (+ lines-moved (abs arg)))
             (condition-case err
                 (scroll-up arg)
               (beginning-of-buffer (ignore-errors (line-move-visual arg)))
               (end-of-buffer (ignore-errors (line-move-visual arg)))
               (:success t))))
         (update ()
           (cl-incf count)
           (let* ((perc (min 1 (/ (- (float-time) start-time)
                                  scroll-time)))
                  (target-line-relative (round (* lines (funcall delay-fn perc))))
                  (lines-to-move (- target-line-relative lines-moved)))
             (move-line (if forward lines-to-move
                          (- lines-to-move)))
             (redisplay t)
             perc)))
      (push (smooth-scroll--overwrite mode-line-format (format-mode-line mode-line-format))
            cbs)
      (push (smooth-scroll--overwrite inhibit-compacting-font-caches t)
            cbs)
      (while (< perc-moved 1)
        (setq perc-moved (update))
        (sleep-for 0.001))
      ;; (message "%d" count)
      (dolist (cb cbs)
        (funcall cb)))))


(defun smooth-scroll-half-window-down (&optional multiplier)
  "Smoothly scroll DOWN by half a screen times MULTIPLIER visual lines.
PREFIX arg MULTIPLIER defaults to 1. Uses configured easing and duration."
  (interactive "p")
  (let* ((lines      (smooth-scroll--half-window-lines multiplier))
         (total-time smooth-scroll-duration)
         (delay-fn   (smooth-scroll--get-delay-fn)))
    (smooth-scroll--animate lines total-time delay-fn)))

(defun smooth-scroll-half-window-up (&optional multiplier)
  "Smoothly scroll UP by half a screen times MULTIPLIER visual lines.
PREFIX arg MULTIPLIER defaults to 1. Uses configured easing and duration."
  (interactive "p")
  (let* ((lines      (smooth-scroll--half-window-lines multiplier))
         (total-time smooth-scroll-duration)
         (delay-fn   (smooth-scroll--get-delay-fn)))
    (smooth-scroll--animate (- lines) total-time delay-fn)))




(setq scroll-preserve-screen-position 'always)



(require 'utils)

; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)

(setq scroll-margin 2)
(setq scroll-conservatively 999)
; (setq scroll-step 1)
 
; (setq scroll-preserve-screen-position t)

;; Smoother scrolling
(pixel-scroll-mode 1)
;; Allow builtin scroll commands to move not only line-by-line
(pixel-scroll-precision-mode 1)
;; (setq mwheel-coalesce-scroll-events nil)
(setq pixel-scroll-precision-interpolate-page t
      pixel-scroll-precision-interpolate-mouse t
                                        ; pixel-scroll-precision-interpolation-total-time 0.5
      )

(defmacro mk-scroll-cmd (name expr)
  `(defun ,name (n)
     (interactive "p")
     (pixel-scroll-precision-interpolate ,expr nil 1)))

(mk-scroll-cmd sscroll-page-up (window-text-height nil t))
(mk-scroll-cmd sscroll-page-down (- (window-text-height nil t)))
(mk-scroll-cmd sscroll-hp-down (- (/ (window-text-height nil t) 2)))
(mk-scroll-cmd sscroll-hp-up (/ (window-text-height nil t) 2))

(defun sscroll-line-down (n)
  (interactive "p")
  (pixel-scroll-up n))

(defun sscroll-line-up (n)
  (interactive "p")
  (pixel-scroll-down n))

(defmacro smooth-scroll-down (expr)
  `(lambda (n)
     (interactive "p")
     (pixel-scroll-precision-interpolate (- ,expr) nil 1)))

(defmacro smooth-scroll-up (expr)
  `(lambda (n)
     (interactive "p")
     (pixel-scroll-precision-interpolate ,expr nil 1)))


(defun my-split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun my-split-window-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun start-scroll-mode ()
  (interactive)
  (save-win-pos)
  (hydra-scroll-mode/body))

(defun switch-to-error-buffer ()
  (interactive)
  (unless next-error-buffer (error "No next-error buffer"))
  (if-let ((win (get-buffer-window next-error-buffer)))
          (select-window win)
          (switch-to-buffer next-error-buffer)))

(let ((hydra-heads
       `(;; Move focus
         ("h" windmove-left)
         ("j" windmove-down)
         ("k" windmove-up)
         ("l" windmove-right)
         ("<left>" windmove-left)
         ("<down>" windmove-down)
         ("<up>" windmove-up)
         ("<down>" windmove-right)
         ;; Move windows
         ("H" windmove-swap--left)
         ("J" windmove-swap-states-down)
         ("K" windmove-swap-states-up)
         ("L" windmove-swap-states-right)
         ("S-<left>" windmove-swap-states-left)
         ("S-<down>" windmove-swap-states-down)
         ("S-<up>" windmove-swap-states-up)
         ("S-<down>" windmove-swap-states-right)
         ;; Split windows
         ("s" my-split-window-below :exit t)
         ("v" my-split-window-right :exit t)
         
         ("n" sscroll-hp-down)
         ("p" sscroll-hp-up)
         ;; ("n" sscroll-page-down)
         ;; ("p" sscroll-page-up)
         ("j" sscroll-line-down)
         ("k" sscroll-line-up)

         ("K" beginning-of-buffer)
         ("J" end-of-buffer)

         ;; ("c" (lambda ()
         ;;        (interactive)
         ;;        (recenter)))
         ;; ("z" (lambda ()
         ;;        (interactive)
         ;;        (recenter)))
         ;; ("b" (lambda ()
         ;;        (interactive)
         ;;        (recenter-top-bottom -1)))
         ;; ("t" (lambda ()
         ;;        (interactive)
         ;;        (recenter-top-bottom 0)))
         
         ("q" (restore-win-pos) :exit t :pre nil)
         ("w" nil :pre nil)
         ("<escape>" nil :pre nil))))
  
  (with-eval-after-load 'hydra
    (mapply #'defhydra
            `(hydra-scroll-mode
              (:foreign-keys warn :hint nil)
              "Scroll window"
              ,@hydra-heads)))

  ;; FIXME: apply more flexible than mapply
  (defvar-keymap scroll-keys :suppress t)

  (dolist (head hydra-heads)
    (when-let ((key (car head))
               (cmd (cadr head)))
      (keymap-set scroll-keys key cmd))))



(defmacro profiled (&rest body)
  `(progn
     (profiler-start 'cpu+mem)
     (prog1
         (progn ,@body)
       (profiler-stop)
       ;; (profiler-report)
       )))


(provide 'scroll-mode)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "scroll-mode--"))
;; End:
