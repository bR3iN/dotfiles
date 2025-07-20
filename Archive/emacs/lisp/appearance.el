;; -*- lexical-binding: t; -*-

(require 'common)

(fringe-mode 0)

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

(defun setup-frame-appearance ()
  (set-face-attribute
   'default nil
   :family "FiraCode Nerd Font Mono"
   :height 120
   :weight 'retina)
  
  (enable-theme 'base16)

  (let ((faces`((default :background ,(color-darken-name (base16-get :bg0) 50))
                ;; (internal-border :background ,(base16-get :bg0))
                ;; (cursor :background ,(base16-get :green))
                ;; (mode-line-inactive :background ,(color-darken-name (base16-get :bg2) 10))
                (flymake-error :background unspecified :underline ,(base16-get :dark_red))
                (flymake-warning :background unspecified)
                (flymake-note-echo-at-eol    :box nil :background ,(base16-get :bg3) :foreground ,(base16-get :dark_green))
                (flymake-warning-echo-at-eol :box nil :background ,(base16-get :bg3) :foreground ,(base16-get :dark_orange))
                (flymake-error-echo-at-eol   :box nil :background ,(base16-get :bg3) :foreground ,(base16-get :dark_red))

                ;; Misc. selections
                (region :background ,(colored-region :cyan 0.15 25))
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
                (font-lock-function-name-face        :foreground ,(lightened :dark_cyan 0.2) :weight medium)
                (font-lock-function-call-face        :foreground ,(lightened :cyan 0.5) :weight medium)
                (font-lock-keyword-face              :foreground ,(base16-get :yellow) :weight normal)
                (font-lock-negation-char-face        :foreground ,(base16-get :base09))
                (font-lock-preprocessor-face         :foreground ,(base16-get :base0D))
                (font-lock-regexp-grouping-backslash :foreground ,(base16-get :brown))
                (font-lock-regexp-grouping-construct :foreground ,(base16-get :base0E))
                (font-lock-string-face               :foreground ,(base16-get :magenta))
                (font-lock-type-face                 :foreground ,(base16-get :base0B) :weight medium)
                (font-lock-variable-name-face        :foreground ,(lightened :dark_red 0.2) :weight medium)
                (font-lock-variable-use-face         :foreground ,(lightened :red 0.5) :weight medium)
                (font-lock-property-name-face        :foreground ,(lightened :dark_orange 0.2) :weight medium)
                (font-lock-property-use-face         :foreground ,(lightened :orange 0.5) :weight medium)
                (font-lock-warning-face              :foreground ,(base16-get :base08))
                (font-lock-delimiter-face            :foreground ,(base16-get :brown))
                (font-lock-operator-face             :foreground ,(base16-get :fg1) :weight medium)
                (font-lock-bracket-face              :foreground ,(base16-get :brown))
                ;; Custom face
                (font-lock-namespace-face            :foreground ,(base16-get :dark_green) :weight normal)

                (fringe                   :background unspecified)
                (line-number              :background unspecified)
                (line-number-current-line :background unspecified) ;; ihnerits fringe
                (mode-line-active         :background ,(color-darken-name (base16-get :bg2) 0)
                                          )
                (mode-line-inactive       :background ,(color-darken-name (base16-get :bg0) 0) :foreground ,(base16-get :fg0)
                                          )
                )))

    (dolist (face-config faces)
      (apply #'set-face-attribute (car face-config) nil (cdr face-config)))))

(add-hook 'after-make-frame-functions #'setup-frame-appearance 91)

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

  (require 'color-utils)

  )

(with-eval-after-load 'init
  (setup-frame-appearance))

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


;; FIXME: doesn't work, cursor not affected by face-remap-add-relative? Is frame-global, why does meow have functions for that then?
(with-eval-after-load 'meow
  (defun -color-modeline (color-id &optional weight &rest extra)
    ;; (apply #'face-remap-add-relative 'mode-line-active :box `(:line-width -1 :color ,(base16-get color-id)) extra)
    ;; (let ((color (rgb-mix (base16-get :bg3) (base16-get color-id) (or weight 0.2))))
    ;; (apply #'face-remap-add-relative 'mode-line-active   :box `(:color ,color :line-width -1) extra)
    ;; (apply #'face-remap-add-relative 'mode-line-inactive :background color extra)
    ;; )
    )
  
  (defun -adapt-modeline-color (new-state)
    (pcase new-state
      ('normal (-color-modeline :green))
      ('insert (-color-modeline :yellow))
      ('motion (-color-modeline :bg1 0.0 :box `(:color ,(base16-get :bg2) :line-width -1)))
      ('visual (-color-modeline :cyan))))
  

  (add-hook 'meow-switch-state-hook #'-adapt-modeline-color)

  (defface meow-visual-cursor
      `((t :inherit cursor))
    "Cursor in visual mode")
  
  (custom-set-faces
   `(meow-insert-cursor ((t (:background ,(base16-get :yellow)))))
   `(meow-visual-cursor ((t (:background ,(base16-get :cyan)))))
   `(meow-motion-cursor ((t (:background ,(base16-get :brown)))))
   `(meow-normal-cursor ((t (:background ,(base16-get :green)))))))

;; (use-package dimmer
;;   :config
;;   (setq dimmer-fraction 0.2)
;;   (dimmer-mode +1))

(use-package anzu
    :config
  (global-anzu-mode))

;; (setf (alist-get 'alpha-background default-frame-alist) 0.90)
(set-frame-parameter nil 'alpha-background 0.95)



(use-package ligature
    :config
  (ligature-set-ligatures 't '("\\\\" "\\\\\\" "//" "///"))

  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (global-ligature-mode 't))

;; (set-fontset-font t 'unicode "Symbols Nerd Font" nil 'prepend)

(provide 'appearance)
