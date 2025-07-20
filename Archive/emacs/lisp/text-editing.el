;; -*- lexical-binding: t; -*-

(require 'common)

(setq org-directory "~/Notes")

(use-package markdown-mode
    :config
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-wiki-link-alias-first nil
        markdown-fontify-code-blocks-natively t)

  (defun markdown--find-wiki-link-at-point ()
    "Ensure there's a wiki link on the current line ending at point.
Sets match data for the found link, or signals an error."
    (unless
        (and
         (save-excursion
           (re-search-backward markdown-regex-wiki-link (line-beginning-position) t))
         (equal (point) (match-end 6)))
      (user-error "No wiki link ending at point")))

  (defun markdown--modify-wiki-link-at-point (fn)
    "Modify the wiki link ending at point by applying FN to its link text.
FN is called with the current link text (or nil) and should return the
new text (or nil to remove alias)."
    (save-excursion
      (markdown--find-wiki-link-at-point)
      (let* ((open        (match-string-no-properties 2))
             (comp1       (match-string-no-properties 3))
             (sep         (match-string-no-properties 4))
             (comp2       (match-string-no-properties 5))
             (close       (match-string-no-properties 6))
             ;; Interpret groups
             (alias-first markdown-wiki-link-alias-first)
             (target      (if sep
                              (if alias-first comp2 comp1)
                            comp1))
             (alias       (and sep
                               (if alias-first comp1 comp2)))
             ;; Create new groups
             (new-text    (funcall fn alias))
             (new-first   (if new-text
                              (if alias-first new-text target)
                            target))
             (new-second  (and new-text
                               (if alias-first target new-text))))
        ;; Replace the original link with updated text
        (replace-match (concat open
                               new-first
                               ;; FIXME: Need the separater hardcoded here if there was none previously; can we get it from the regexp instead?
                               (when new-second (concat (or sep "|") new-second))
                               close)
                       t t nil 1))))

  (defun markdown-change-wiki-link-text-at-point ()
    "Interactively change the link text of the wiki link ending at point."
    (interactive)
    (markdown--modify-wiki-link-at-point
     (lambda (old)
       (let ((read (read-string
                    (format "New link text (current: %S): " old)
                    old)))
         (if (equal read "") nil
           read))))))

(defvar zk-dir "~/Zettelkasten")

(use-package zk-emacs
    :straight nil
    :config
    (zk-register zk-dir)
    (zk-auto-mode)
    (keymap-set zk-mode-map "<mouse-1>" #'markdown-follow-wiki-link-at-point)
    (keymap-set zk-mode-map "C-]" #'markdown-follow-wiki-link-at-point)
    (keymap-set zk-mode-map "C-i" #'zk-insert-link)
    (keymap-set zk-mode-map "M-h" #'markdown-change-wiki-link-text-at-point)
    ;; (defun zk-screenshot ()
    ;;   (interactive)
    ;;   (let ((basename)
    ;;         (path (concat "~/Zettelkasten/screenshots/" filename ".png")))
    ;;     (shell-command (concat "grim -g $(slurp) " path))
    ;;     ))
    )

(use-package emanote-live
    :straight nil
    :config
    (emanote-live-configure zk-dir))


(provide 'text-editing)
