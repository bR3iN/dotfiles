;; -*- lexical-binding: t; -*-

(require 'utils)

(fringe-mode 0)

;; FIXME: A `my/'-prefix yields a syntax error when compiling a treesit-query later
(defface font-lock-namespace-face
  '((t :inherit font-lock-type-face))
  "Face for namespaces")

(defface current-thing-face
  '()
  "Face for current thing in mode line")

(use-package base16-theme
  :config
  (setq base16-theme-256-color-source 'colors)
  (require 'base16-colors)
  (deftheme base16)
  (base16-theme-define 'base16 base16-colors)
  (enable-theme 'base16)

  (require 'color-utils)
  (let ((faces `((cursor :background ,(base16-get :green))
                 ;;(mode-line-inactive :background ,(color-darken-name (base16-get :bg2) 10))
                 (flymake-error :background unspecified :underline ,(base16-get :dark_red))
                 (flymake-warning :background unspecified)

                 ;; Misc. selections
                 (region :background ,(colored-region :blue 0.15))
                 (secondary-selection :background ,(colored-region :magenta 0.15))
                 (match :weight bold)
                 (shadow :foreground ,(base16-get :base04))
                 (current-thing-face :foreground ,(base16-get :cyan))
                 
                 (font-lock-builtin-face              :foreground ,(base16-get :base0D))
                 (font-lock-comment-delimiter-face    :foreground ,(base16-get :base03))
                 (font-lock-comment-face              :foreground ,(base16-get :base03))
                 (font-lock-constant-face             :foreground ,(base16-get :magenta))
                 (font-lock-number-face               :foreground ,(base16-get :magenta))
                 (font-lock-doc-face                  :foreground ,(base16-get :base04))
                 ;;(font-lock-doc-string-face           :foreground ,(base16-get :base03))
                 (font-lock-function-name-face        :foreground ,(base16-get :dark_cyan) :weight normal)
                 (font-lock-function-call-face        :foreground ,(base16-get :cyan) :weight medium)
                 (font-lock-keyword-face              :foreground ,(base16-get :yellow) :weight normal)
                 (font-lock-negation-char-face        :foreground ,(base16-get :base09))
                 (font-lock-preprocessor-face         :foreground ,(base16-get :base0D))
                 (font-lock-regexp-grouping-backslash :foreground ,(base16-get :brown))
                 (font-lock-regexp-grouping-construct :foreground ,(base16-get :base0E))
                 (font-lock-string-face               :foreground ,(base16-get :magenta))
                 (font-lock-type-face                 :foreground ,(base16-get :base0B) :weight medium)
                 (font-lock-variable-name-face        :foreground ,(base16-get :dark_red) :weight normal)
                 (font-lock-variable-use-face         :foreground ,(base16-get :red) :weight medium)
                 (font-lock-property-name-face        :foreground ,(base16-get :dark_orange) :weight normal)
                 (font-lock-property-use-face         :foreground ,(base16-get :orange) :weight medium)
                 (font-lock-warning-face              :foreground ,(base16-get :base08))
                 (font-lock-delimiter-face            :foreground ,(base16-get :brown))
                 (font-lock-operator-face             :foreground ,(base16-get :fg1) :weight medium)
                 (font-lock-bracket-face              :foreground ,(base16-get :brown))
                 ;; Custom face
                 (font-lock-namespace-face            :foreground ,(base16-get :dark_green) :weight normal)
                 )))
    (dolist (face-config faces)
      (apply #'set-face-attribute (car face-config) nil (cdr face-config))))
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
