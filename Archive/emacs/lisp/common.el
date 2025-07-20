;; -*- lexical-binding: t; -*-

(require 'bootstrap/straight)

(straight-use-package 'use-package)

(setq
 ;; Have `:straight t' by default in `use-package'
 straight-use-package-by-default t
 ;; Workaround for https://github.com/radian-software/straight.el/issues/1022#issuecomment-2614489883
 straight-cache-autoloads nil
 ;; Also load packages if they have e.g. :hook declarations
 use-package-always-demand t)

;; Builtin stuff
(require 'use-package)
(require 'cl-lib)

;; Lokal code
(require 'utils)
(require 'color-utils)

(provide 'common)
