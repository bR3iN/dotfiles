;; -*- lexical-binding: t; -*-

(use-package smartparens
  :config

  (require 'smartparens-config)
  (smartparens-global-strict-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

  (defmacro wrap-with (pair)
    `(lambda ()
      (interactive)
      (sp-wrap-with-pair ,pair)))

  (defvar pair-alias-alist `(("p" . "(")
                             ("d" . nil)
                             ("b" . "{")
                             ("s" . "[")))

  (defun ask-pair ()
    (if-let* ((pair (char-to-string (read-char "Pair: ")))
        (pair (if (equal pair 127) nil pair))   
           (pair (or (alist-get pair pair-alias-alist nil nil #'equal) pair)))
      (sp-get-pair pair)))

  (defun wrap (n)
    (interactive "p")
    (if-let ((pair (ask-pair)))
      (if (> n 1) (progn
                    (sp-up-sexp (- (dec-abs n)) t)
                    (let ((current-prefix-arg nil))
                      (sp-wrap-with-pair (plist-get pair :open))))
        (sp-wrap-with-pair (plist-get pair :open)))))

  (defun rewrap (n)
    (interactive "p")
    (let ((pair (ask-pair))
          (n (dec-abs n)))
      (unless (= n 0)
        (sp-up-sexp (- n) t))
      (if pair
          (sp-rewrap-sexp `(,(plist-get pair :open) . ,(plist-get pair :close)) nil)
        (sp-up-sexp -1 t)
        (sp-unwrap-sexp))
      ))

  (defun -beg-bounds (sp-sexp backwards)
    (if backwards
        (cons (sp-get sp-sexp :end-in) (sp-get sp-sexp :end))
      (cons (sp-get sp-sexp :beg) (sp-get sp-sexp :beg-in))))

  (defun -get-nearest-sexp (backwards)
    (let* ((pt (point))
           (direction (if backwards -1 +1))
           (prev (save-excursion
                   (if backwards (progn
                                   
                                   )
                       (sp-backward-up-sexp)
                       (sp-get-sexp)
                       )
                   ))
           (prev-open-bounds (-beg-bounds prev backwards)))
      (if (and prev
               (< (car prev-open-bounds) pt)
               (< pt (cdr prev-open-bounds)))
          prev
        (sp-get-sexp backwards))))

  

  ;; (defun test ()
  ;;   (a (b c) (d
  ;;             e))
  ;;   ("\\(\\)")
  ;;   (" \\( ( ) bc ( ) \\) "))

  
  (defun -next-sexp-after-point (&optional backwards)
    "Find the next balanced expression starting strictly after point."
    (when-let* ((pt (point))
                (direction (if backwards -1 +1))
                (next (sp-get-sexp backwards))
                (next-beg (-sexp-beg next backwards)))
      ;; If the next sexp found by smarparens starts before point, it is
      ;; the surrounding one and there are no balanced expression after
      ;; point inside the current balanced expression; if it starts at
      ;; point, we are directly at the beginning of a balanced expression.
      (cond
       ((> next-beg pt) next)
       ((< next-beg pt)
        ;; Skip over the outer sexp and take the next one.
        (save-excursion
          (goto-char (-sexp-end next backwards))
          (sp-get-sexp backwards)))
       ((= next-beg pt)
        ;; Take the next inner balanced expression if existent and fall back to
        ;; the next outer one if not.
        (save-excursion
          (goto-char (-sexp-beg-in next backwards))
          (let ((next-inner (sp-get-sexp backwards)))
            (if (= (-sexp-beg next-inner backwards) next-beg)
                ;; No inner balanced expression, sp-get-sexp returned outer one.
                (progn
                  (goto-char (-sexp-end next backwards))
                  (sp-get-sexp backwards))
              next-inner)))))))

  (defun -sexp-end (sexp backwards)
    (if backwards (sp-get sexp :beg)
      (sp-get sexp :end)))

  (defun -sexp-beg-in (sexp backwards)
    (if backwards (sp-get sexp :end-in)
      (sp-get sexp :beg-in)))

  (defun -sexp-beg (sexp backwards)
    (-sexp-end sexp (not backwards)))

  (defun -sexp-end-in (sexp backwards)
    (-sexp-beg-in sexp (not backwards)))

  (defun kill-unwrap-region (beg end)
    "Kill region and delete delimiters outside of region that become unbalanced this way."
    (interactive `(,(region-beginning) ,(region-end)))
    (goto-char beg)
    (save-excursion
      (let* (;; First keep only track of what to delete so we don't incorrectly
             ;; match delimiters if there are unmatched ones around.
             (to-delete '())
             ;; The closest balanced expression ahead of us
             (next (sp-get-sexp)))
        (while (and next (< (sp-get next :beg) end))
          (push (cons
                 ;; Only delete text after end so we don't delete too
                 ;; much when killing the actual region.
                 (max end (sp-get next :end-in))
                 (max end (sp-get next :end)))
                to-delete)
          (goto-char (sp-get next :beg))
          (setq next (-next-sexp-after-point)))))
    ;; Delete everything last to first order so the bounds we tracked don't
    ;; become invalid before they are deleted.
    (dolist (bounds (nreverse to-delete))
      (delete-region (car bounds) (cdr bounds)))
    (kill-region beg end))

  (defun -scan-region-forward (beg end backwards)
    (if backwards (progn)
      (let* ((pt (point))
             (direction (if backwards -1 +1))
             (bounds-found '())
             (is-outside-region (lambda (bounds)
                                  (if backwards
                                      (< (cdr bounds) beg)
                                    (> (car bounds) end))))
             (next (-get-nearest-sexp backwards)))
        (while (and next (not (funcall is-outside-region (-beg-bounds next backwards))))
          (let ((cl-bounds (-beg-bounds next (not backwards))))
            (when (funcall is-outside-region cl-bounds)
              (push cl-bounds bounds-found)))
          (goto-char (-sexp-end next backwards))
          (setq next (-next-sexp-after-point backwards)))
        (if backwards bounds-found
          (nreverse bounds-found)))))

  (defun unwrap (n)
    (interactive "p")
    (sp-up-sexp n t)
    (sp-unwrap-sexp (if (> n 0) -1 +1)))
  )

(add-hook 'minibuffer-setup-hook 'electric-pair-local-mode)

(provide 'up-and-down)

;; Local Variables:
;; read-symbol-shorthands: (("-" . "up-and-down--"))
;; End:

