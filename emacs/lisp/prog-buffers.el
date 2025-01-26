;; -*- lexical-binding: t; -*-

(require 'utils)
(require 'cl-lib)
(require 'dedicated-windows)

; (setq display-buffer-alist
;       '(("\\*Help\\*" . ((display-buffer-pop-up-window display-buffer-reuse-window display-buffer-at-bottom) . nil))))


; (defun -display-in-sym (name-lit)
;   (intern (concat "-display-buffer-in-" (symbol-name name-lit))))

; (defun -win-var (name-lit)
;   (intern (concat "-" (symbol-name name-lit) "-window")))

; (defmacro -def-reusable-window (name-lit)
;   (let* ((name (symbol-name name-lit))
;          (win-var (intern (concat "-" name "-window")))
;          (display-in (intern (concat "-display-buffer-in-" name))))
;     `(progn
;        (defvar ,win-var nil)
;        (defun ,display-in (buffer _alist)
;          (when ,win-var
;            (select-window ,win-var)
;            (set-window-buffer ,win-var buffer)
;            (set-window-dedicated-p ,win-var t))))))

; (-def-reusable-window docs )
; (-def-reusable-window interact )

;; For reuse: display in buffer (does shadow inside of display-buffer-alist)
;; global list of associations, api for setting and unsetting dedicated window
;; If set, associations are made (save exact els, then symbol not needed?)

(defvar prog-buffers-windows `((docs . (lambda ()
                                         (let ((display-buffer-overriding-action `((display-buffer-same-window) . nil)))
                                           (describe-mode))))
                               (interact . (lambda ()
                                             (if (project-current) (project-eshell)
                                               (eshell))))))

(defvar prog-buffers-display-buffer-alist `(("\\*Help\\*" . docs)
                                            ("\\*eldoc" . docs)
                                            ("\\*compilation\\*" . interact)
                                            ("\\*eshell\\*" . interact)
                                            ))

(define-minor-mode prog-buffers-mode nil
                   :lighter " pb"
                   :global t
                   (if prog-buffers-mode
                     (-init-mode)
                     (-deinit-mode)))

(defvar -disable-mode-callbacks '())

(defun -deinit-mode ()
  (dolist (cb -disable-mode-callbacks)
    (funcall cb))
  (setq -disable-mode-callbacks '()))

(defun -init-mode ()
  (-associate-buffer)
  (-setup-dedicated-windows)
  (-setup-eldoc-display))

(defun -setup-eldoc-display ()
  (let ((old eldoc-display-functions))
    (setq eldoc-display-functions '(eldoc-display-in-buffer -eldoc-display))
    (push (lambda ()
            (setq eldoc-display-functions old))
          -disable-mode-callbacks)))

(defvar prog-buffers-eldoc-win-id 'docs)

;; TODO register appending and don't call eldoc-display-in-buffer
(defun -eldoc-display (&rest args)
  (eldoc-doc-buffer t))


; ;; TODO register appending and don't call eldoc-display-in-buffer
; (defun -eldoc-display (&rest args)
;   (when prog-buffers-eldoc-win-id
;     (when-let ((win (alist-get prog-buffers-eldoc-win-id -windows)))
;       (apply #'eldoc-display-in-buffer args)
;       (-with-win-unlocked win
;         (set-window-buffer win (eldoc-doc-buffer))))))


(defun -setup-dedicated-windows ()
  "Creates initial layout from prog-buffers-windows."
  (unless (seq-empty-p prog-buffers-windows)
    (let ((last-win nil))
      (alist-map
        (lambda (id-sym init)
          ;; TODO: splitting at root will always balance to 50%
          (let ((win (if (not last-win) (split-root-window-right)
                       (split-window-below nil last-win))))
            ;; Initialize window with registered callback (before setting it as dedicated)
            (with-selected-window win (ignore-errors (funcall init)))
            (dedicated-windows-set id-sym win)
            ;; Register callback closing window
            (push (lambda ()
                    (dedicated-windows-unset id-sym)
                    (delete-window win))
                  -disable-mode-callbacks)
            (setq last-win win)))
        prog-buffers-windows))))


; (defun -close-windows ()
;   "Close dedicated windows we created."
;   (dolist (id-sym '(docs interact)))
;   (alist-map (lambda (_id win)
;                (unprotect-window win)
;                (delete-window win)) -windows))


; (defvar -active-display-buffer-assocs '())

(defun -associate-buffer ()
  "Modifies dedicated-windows-alist according to prog-buffers-display-buffer-alist."
  (dolist (el (reverse prog-buffers-display-buffer-alist))
    (push el dedicated-windows-alist)
    (push (lambda ()
            (setq dedicated-windows-alist (remove el dedicated-windows-alist)))
          -disable-mode-callbacks)))

; (defun -deassociate-buffer ()
;   (dolist (el -active-display-buffer-assocs)
;     )
;   (setq -active-display-buffer-assocs '()))


; (defvar -eldoc-display-alt nil
;   "Saves the value of eldoc-display-functions while mode is active.")

; (defvar -windows '()
;   "Alist mapping identifying symbols to the window they represent while mode is active.")

; (defun -def-display-buffer-in (id-sym)
;   ;; Return interned symbol instead of lambda so we can later find the element for deletion.
;   (let ((sym (intern (concat "prog-buffers-display-buffer-in-" (symbol-name id-sym)))))
;     (fset sym (lambda (buffer &optional _alist)
;                 (print buffer)
;                 (when-let ((win (alist-get id-sym -windows)))
;                   (-with-win-unlocked win
;                     (set-window-buffer win buffer)))))
;     sym))

; (defun -mk-display-buffer-alist-el (regexp id-sym)
;   `(,regexp . (,(-def-display-buffer-in id-sym) . nil)))

; (defun -deassociate-buffer ()
;   "Remove associations made above."
;   (alist-map
;     (lambda (regexp id-sym)
;       (let ((el (-mk-display-buffer-alist-el regexp id-sym)))
;         (setq display-buffer-alist (remove el display-buffer-alist))))
;     prog-buffers-alist))


(provide 'prog-buffers)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "prog-buffers--"))
;; End:
