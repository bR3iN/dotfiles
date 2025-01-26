;; -*- lexical-binding: t; -*-

(require 'utils)

(fringe-mode 0)

(use-package base16-theme
  :config
  (setq base16-theme-256-color-source 'colors)
  (require 'base16-colors)
  (deftheme base16)
  (base16-theme-define 'base16 base16-colors)
  (enable-theme 'base16)

  (require 'color-utils)
  (set-face-attribute 'cursor nil :background (base16-get :green))
  (set-face-attribute 'region nil :background (colored-region :blue 0.15))
  (set-face-attribute 'secondary-selection nil :background (colored-region :magenta 0.15))
  (set-face-attribute 'match nil :weight 'bold)
  (set-face-attribute 'mode-line-inactive nil :background (color-darken-name (base16-get :bg2) 10))
  )

(with-eval-after-load 'eat
  (let ((term16-colors
         '(:bg0
           :dark_red
           :dark_green
           :dark_yellow
           :dark_blue
           :dark_magenta
           :dark_cyan
           :fg1

           :bg3
           :red
           :green
           :yellow
           :blue
           :magenta
           :cyan
           :fg3))
        (count 0))
    (dolist (sym term16-colors)
      (set-face-attribute
       (intern (format "eat-term-color-%i" count))
       nil :foreground (base16-get sym))
      (setq count (1+ count))))
  )

; TODO: check out add-to-ordered-list
(defun sinsert (el ls &optional get-key)
  (if (null ls) (list el)
    (let* ((get-key (or get-key #'car))
	   (head (car ls))
	   (tail (cdr ls))
	   (head-key (funcall get-key head))
	   (el-key (funcall get-key el)))
      (cond ((< el-key head-key) (cons el ls))
	    ((> el-key head-key) (cons head (sinsert el tail get-key)))
	    ((= el-key head-key) (cons el tail))))))


;; Insert the retina font style, dropping the last style for that (as we can't assign a new vector due to the symbol being constant). This is idempotent due to sinsert replacing an existing entry.
(let ((patched-font-weight-table
       (vconcat
	(sinsert [95 retina] (append font-weight-table nil)
		 (lambda (el) (aref el 0))))))
  (dotimes (i (length font-weight-table))
    (aset font-weight-table i (aref patched-font-weight-table i))))

(set-face-attribute
 'default nil
 :family "Fira Code"
 :height 120
 :weight 'retina)

(set-fontset-font t 'unicode "Symbols Nerd Font" nil 'prepend)

(provide 'appearance)
