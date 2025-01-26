;; -*- lexical-binding: t; -*-

(require 'utils)
(require 'cl-lib)
(require 'protect-windows)

(defvar dedicated-windows-alist '()
  "Alist of associations of the form (REGEXP . ID-SYM) where REGEXP is a regexp as in display-buffer-alist and ID-SYM an identifying symbol.")

(defvar -windows '()
  "Alist mapping identifying symbol to cons cell of currently set window callback called when unset.")

(defun dedicated-windows-set (id-sym win)
  "Display buffers associated with ID-SYM (as specified in dedicated-windows-alist) in WIN."
  (let ((display-buffer-assocs (alist-map
                                 (lambda (regexp id-sym)
                                   (let ((fun-display-name (concat "<display-buffer-in-dedicated-window-with-id-" (symbol-name id-sym) ">"))
                                         (fun (lambda (buffer &optional _alist)
                                                (with-window-non-dedicated win
                                                                           (set-window-buffer win buffer)))))
                                     `(,regexp . (,(named fun-display-name fun) . nil))))
                                 (seq-filter (lambda (el)
                                               (eq (cdr el) id-sym)) dedicated-windows-alist))))
    ;; Add associations
    (dolist (el display-buffer-assocs)
      (add-to-list 'display-buffer-alist el))
    ;; Track window, registering a callback removing the assocations made above
    (setf (alist-get id-sym -windows) `(,win . ,(lambda ()
                                                  (dolist (el display-buffer-assocs)
                                                    (setq display-buffer-alist (remove el display-buffer-alist))))))
    ;; Prevent emacs from displaying other buffers in window or killing it
    (protect-window win)
    (set-window-dedicated-p win t)))

(defun dedicated-windows-unset (id-sym)
  "No longer display buffers associated with ID-SYM (as specified in dedicated-windows-alist) the previously set window. Returns window if successfull."
  (when-let ((el (alist-get id-sym -windows)))
    (let ((win (car el))
          (cb (cdr el)))
      (unprotect-window win)
      (set-window-dedicated-p win nil)
      (funcall cb)
      (setf (alist-get id-sym -windows nil t) nil)
      win)))

; (defun call-with-dedicated-windows-selected (id-sym func)
;   "Call FUNC with "
;   (setf (alist-get id-sym -windows nil t) nil))

(provide 'dedicated-windows)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "dedicated-windows--"))
;; End:
