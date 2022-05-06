(local {: add!} (require :pkg))
(import-macros {: let!} :utils.macros)

; set snipped directory
(let! vsnip_snippet_dir (.. (vim.fn.stdpath :config) :/vsnip))

(add! "rafamadriz/friendly-snippets")
(add! "hrsh7th/vim-vsnip-integ")
