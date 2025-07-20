;; Temporarily disable garbage collection for faster startup
(setq gc-cons-threshold most-positive-fixnum)

(with-eval-after-load 'init
  (run-with-idle-timer 1.2 t 'garbage-collect)
  (setq gc-cons-threshold 2000000000))

;; ;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Make Emacs Native-compile .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq byte-compile-warnings nil)


;; Tell emacs where to find out code
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; We use straight instead
(setq package-enable-at-startup nil)

;; Never load UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(modify-all-frames-parameters
 '((left-fringe . 0)
   (right-fringe . 0)
   ;; (left-margin-width . 1)
   ;; (right-margin-width . 1)
   (internal-border-width . 10)
   (alpha-background . 100)
   ))
