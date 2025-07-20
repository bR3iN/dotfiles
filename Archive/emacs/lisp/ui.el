;; -*- lexical-binding: t; -*-

(require 'common)

(setopt use-short-answers t)


;; Echo pressed keys instantly (value of 0.0 disables this functionality) and don't display
;; the "press C-h for help" suffix.
(setq echo-keystrokes 0.001
      echo-keystrokes-help nil)

(setq compilation-context-lines 3
      vc-follow-symlinks t)

;; Never prompt for identifier, use symbol at point (universal-argument should be able to override this).
(setq xref-prompt-for-identifier nil)


(use-package tab-bar
    :custom
  (tab-bar-close-button-show nil)
  (tab-bar-auto-width nil)
  (tab-bar-show 1)
  (tab-bar-separator " ")
  (tab-bar-format '(tab-bar-format-tabs))
  (tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-hints
                                       tab-bar-tab-name-format-close-button
                                       tab-bar--tab-margins
                                       tab-bar-tab-name-format-face))
  :custom-face
  (tab-bar              ((t (
                             :foreground ,(base16-get :fg0)
                             ;; "Transparent" background
                             :background ,(base16-get :bg0)
                             :height 0.9
                             ))))
  (tab-bar-tab          ((t (
                             :foreground ,(base16-get :green)
                             :background ,(base16-get :bg3)
                             ;; :bold t
                             :box (:line-width 1 :color ,(base16-get :bg2))))))
  (tab-bar-tab-inactive ((t (
                             :foreground ,(base16-get :fg1)
                             :background ,(base16-get :bg2)
                             :box (:line-width 1 :color ,(base16-get :bg1))))))
  :straight nil
  :config
  (defun tab-bar--tab-margins (name &rest _)
    (format " %s " name))
  ;; Auto enabled on tab creation
  ;; (tab-bar-mode)
  )


(use-package mood-line
    :config
  (defun mood-line-segment-state ()
    (pcase-let ((`(,indicator . ,color-id) 
                  (alist-get meow--current-state
                             `((normal "Normal" . :fg0)
                               (insert "Insert" . :yellow)
                               (visual "Visual" . :cyan)
                               (visrec "VisRec" . :cyan)
                               (keypad "Keypad" . :green)
                               (motion "Motion" . :brown))
                             `("?" . :magenta))
                  ))
      (propertize (format "%s" indicator)
                  'face `(
                          ;; :background
                          ;; ,(base16-get :bg1)
                          :foreground
                          ,(if (mode-line-window-selected-p)
                               (base16-get color-id)
                             (rgb-mix
                              (base16-get color-id)
                              (base16-get :bg3)
                              0.5)))))
    
    )
  
  (defun mood-line-segment-current-thing ()
    (when (boundp 'current-thing)
      (let ((thing-color (if (memq meow--current-state '(normal visual visrec))
                             (base16-get :dark_cyan)
                           (base16-get :dark_cyan)))
            (thing (symbol-name current-thing)))
        (propertize (symbol-name current-thing) 'face `(:foreground ,thing-color)))))

  (defun mood-line-segment-ace-window ()
    (when-let ((ace-path (window-parameter (selected-window) 'ace-window-path)))
      (propertize (format "%s" ace-path)
                  'face `(
                          :foreground ,(base16-get :dark_red)
                          :weight bold
                          ;; :background ,(base16-get :bg1)
                          ))))

  (defun dirvish-ace-ml ()
    (if-let ((seg (mood-line-segment-ace-window)))
        (concat " " seg)
      ""))

  (with-eval-after-load 'dirvish
    (let ((segs (plist-get dirvish-mode-line-format :left)))
      (unless (memq 'ace segs)
        (setf (plist-get dirvish-mode-line-format :left) (cons 'ace segs)))
      ))
  
  (setq mode-line-sep (propertize "  " 'face `(:foreground ,(base16-get :bg1))))
  (mood-line-mode)

  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  (mood-line-format
   '((" "
      (mood-line-segment-ace-window)
      " "
      (mood-line-segment-state)
      mode-line-sep
      (mood-line-segment-buffer-name)
      mode-line-sep
      (mood-line-segment-buffer-status)
      mode-line-sep
      (mood-line-segment-anzu)
      mode-line-sep
      (mood-line-segment-multiple-cursors)
      mode-line-sep
      (mood-line-segment-cursor-position)
      mode-line-sep
      (mood-line-segment-scroll)
      nil)
     ((mood-line-segment-current-thing)
      mode-line-sep
      (mood-line-segment-vc)
      mode-line-sep
      (mood-line-segment-major-mode)
      mode-line-sep
      (mood-line-segment-misc-info)
      mode-line-sep
      (mood-line-segment-checker)
      mode-line-sep
      (mood-line-segment-process)
      mode-line-sep
      ))))

(setq window-sides-vertical t)

(add-hook 'imenu-after-jump-hook (nlambda recenter-top-after-jump ()
                                   (recenter-top-bottom 0)))

;; Display buffer behavior achieving the following:
;; - `display-to-buffer' prefers the mru window (possibly the current one, if not in a side window) when the buffer is not yet visible somewhere.
;; - In side windows, `switch-to-buffer' is forced to respect the window-dedication (which is not "strong" so we can't use `switch-to-buffer-in-dedicated-window').
;; - We can still manually `switch-to-buffer' to a buffer usually displayed in a side window in a non-side window (as we only set `switch-to-buffer-obey-display-actions' locally).

(setq display-buffer-base-action '((display-buffer-reuse-window
                                    ;; display-buffer-same-window
                                    display-buffer-use-some-window
                                    org-display-buffer-split
                                    )
                                   (reusable-frames . nil) (some-window . mru)))

(defun display-buffer-in-side-window-wrapped (buffer alist)
  "`display-buffer-in-side-window' but also sets `switch-to-buffer-obey-display-actions' locally to force buffer switches into another window."
  (when-let ((res (display-buffer-in-side-window buffer alist)))
    (with-current-buffer buffer
      (setq-local switch-to-buffer-obey-display-actions t))
    res))



;; TODO: could add inhibit-same-window window-parameter, to allow displaying side windows manually in normal buffers
(let ((left-width 35)
      (open '((display-buffer-reuse-window display-buffer-in-side-window-wrapped))))
  ;; Do NOT add (dedicated . t) here as display-buffer-in-side-window will set it to 'side by itself which yields the wanted behavior; having 't instead will cause new buffers displayed in the same side window to not be dedicated.
  (dolist (el
            ;; Right side
            `(
              ;; ("\\*info\\*" ,@open
              ;;  (side . right) (slot . -3))
              ("\\*Help\\*" 
               ,@open
               (side . right) (slot . -2))
              ("\\*info\\*" 
               ,@open
               (side . right) (slot . -2))
              ("\\*Embark Actions\\*" 
               ,@open
               (side . right) (slot . -2))
              ("\\*eldoc" 
               ,@open
               (side . right) (slot . -1))

              ("\\*compilation\\*" 
               ,@open
               (side . right) (slot . 1))
              ("\\*Flymake.*\\*" 
               ,@open
               (side . right) (slot . 1))
              ("\\*xref\\*" 
               ,@open
               (side . right) (slot . 1))
              ("\\*Embark Export.*" 
               ,@open
               (side . right) (slot . 1))
              
              ;; Bottom
              ("\\*Messages\\*" 
               ,@open
               (side . bottom) (slot . 1))
              ("\\*\\(.*-\\)\\(log\\|Log\\)\\*" 
               ,@open
               (side . bottom) (slot . 1))
              ("\\*\\(.*-\\)?eat" 
               ,@open
               (side . bottom) (slot . 0))
              ("\\*vterm\\*" 
               ,@open
               (side . bottom) (slot . 0))
              ("\\*Inferior Haskell\\*" 
               ,@open
               (side . bottom) (slot . 0))
              ((category . comint) 
               ,@open
               (side . bottom) (slot . 0))

              ("\\*EGLOT LSP Hierarchy\\*" 
               ,@open
               (side . left) (slot . 0)  (window-width . ,left-width))))
    (add-to-list 'display-buffer-alist el))
  (setq dirvish-side-width left-width))

;; (use-package ultra-scroll
;;     :straight (:type git
;;                      :host github
;;                      :repo "jdtsmith/ultra-scroll")
;;     :init
;;     (setq scroll-conservatively 101 ; important!
;;           scroll-margin 2) 
;;     :config
;;     (ultra-scroll-mode -1))

(provide 'ui)
