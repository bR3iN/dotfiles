;; -*- lexical-binding: t; -*-

(require 'common)

;; Visualize undo history
(use-package vundo)

;; Persist undo history
(use-package undo-fu-session
    :config
  (undo-fu-session-global-mode)
  (let ((undo-hist-dir "~/.local/share/emacsundo"))
    (unless (file-directory-p undo-hist-dir)
      (make-directory undo-hist-dir t))
    (setq undo-fu-session-directory undo-hist-dir)))

(use-package goto-last-change)

;; (use-package undo-tree
;;     :config
;;   ;; undo-tree uses this as a heuristic to detect incompatible major modes which of course makes no sense if we remap C-/ manually
;;   ;; TODO: overwrites C-/ in visual mode?
;;   (advice-add 'undo-tree-overridden-undo-bindings-p :override #'ignore)
;;   (global-undo-tree-mode 1)

;;   (setq undo-tree-visualizer-diff t)
;;   ;; Persist undo history
;;   (let ((undo-hist-dir "~/.local/share/emacsundo"))
;;     (unless (file-directory-p undo-hist-dir)
;;       (make-directory undo-hist-dir t))
;;     (setq undo-tree-auto-save-history t
;;           undo-tree-history-directory-alist `(("." . ,undo-hist-dir)))))

;; (defun smart-undo (&optional n)
;;   "undo-tree-undo for positive numeric prefixes, undo-tree-redo for negative ones."
;;   (interactive "*p")
;;   (let ((n (or n 1)))
;;     (if (< n 0) (undo-tree-redo (- n))
;;       (undo-tree-undo n))))


(provide 'undo)
