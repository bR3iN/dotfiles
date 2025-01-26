;; -*- lexical-binding: t; -*-

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
(setq pixel-scroll-precion-interpolate-page t
      pixel-scroll-precion-interpolate-mouse t
      ; pixel-scroll-precion-interpolation-total-time 0.5
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
       `(("d" sscroll-hp-down)
         ("u" sscroll-hp-up)
         ("n" sscroll-page-down)
         ("p" sscroll-page-up)
         ("j" sscroll-line-down)
         ("k" sscroll-line-up)

         ("g" beginning-of-buffer)
         ("G" end-of-buffer)

         ("c" (lambda ()
                (interactive)
                (recenter)))
         ("z" (lambda ()
                (interactive)
                (recenter)))
         ("b" (lambda ()
                (interactive)
                (recenter-top-bottom -1)))
         ("t" (lambda ()
                (interactive)
                (recenter-top-bottom 0)))
         
         ("q" (restore-win-pos) :exit t :pre nil)
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




(provide 'scroll-mode)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "scroll-mode--"))
;; End:
