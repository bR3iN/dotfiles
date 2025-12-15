(local {: buf-opts!} (require :utils))

;; Note: backup and writebackup are global options and cannot be set buffer-locally
;; (buf-opts! {:backup false :writebackup false})
(buf-opts! {:iskeyword+ "\\"})

(vim.cmd.abbreviate ["<buffer>" "\\bf" "\\mathbf"])
(vim.cmd.abbreviate ["<buffer>" "\\rm" "\\mathrm"])
(vim.cmd.abbreviate ["<buffer>" "\\cal" "\\mathcal"])
(vim.cmd.abbreviate ["<buffer>" "\\bb" "\\mathbb"])
(vim.cmd.abbreviate ["<buffer>" "\\frak" "\\mathfrak"])
(vim.cmd.abbreviate ["<buffer>" "iff" "if and only if"])
