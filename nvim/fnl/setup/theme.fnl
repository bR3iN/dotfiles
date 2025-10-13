(import-macros {: set! : setl! : setl+ : setl- : setg! : set+ : let! : with-cb}
               :utils.macros)

(local {: hl!
        : use!
        : setup
        : keymaps!} (require :utils))

(local {: get-named} (require :utils.colors))
(local {: name} (require :base16-colors))
(local colors (get-named))

;; Custom color setup; load colorscheme description (name + base16 colors)
(use! [;;:bR3iN/base16.nvim
       :folke/tokyonight.nvim
       :shaunsingh/nord.nvim
       :ellisonleao/gruvbox.nvim
       :rebelot/kanagawa.nvim
       :catppuccin/nvim]
      {:autocmds #{:ColorScheme {:callback (let [cb #(do
                                                       ;; Highlights overrides and groups for local plugins
                                                       ;; (hl-ext! :LineNrAbove {:bg colors.base01}) 
                                                       ;; (hl-ext! :LineNrBelow {:bg colors.base01}) 
                                                       ;; (hl-ext! :CursorLineNr {:bg (darken colors.base02 0.1)})
                                                       (hl! :Comment
                                                            {:extend true
                                                             :fg colors.base03})
                                                       (hl! :CommentHighlighted
                                                            {:extend true
                                                             :fg colors.base0F})
                                                       (hl! :GitSignsCurrentLineBlame
                                                            {:link :CommentHighlighted})
                                                       (hl! :TrailingWhitespace
                                                            {:extend true
                                                             :fg colors.base03
                                                             :bg colors.base03})
                                                       (hl! :LspInlayHint
                                                            {:extend true
                                                             :bg :NONE})
                                                       ;; Toggle the color of comments TODO: For some reason doesn't work with external nord colorscheme
                                                       (hl! :BqfPreviewTitle
                                                            {:fg colors.green
                                                             :bg colors.base02})
                                                       ;; Load after setting `CommentHighlighted` above
                                                       ;; Use <C-o><C-h> instead 
                                                       ;; (imap! :<C-h> "<C-o>:ToggleComments<CR>")
                                                       (setup :plugin.toggle-comments)
                                                       (keymaps! {:n {:<C-h> ":ToggleComments<CR>"}}))]
                                             (cb)
                                             cb)}}
       :init #(let! nord_disable_background true)
       :setup {:tokyonight {:transparent true}
               :gruvbox {:transparent_mode true}
               :kanagawa {:transparent true}}
       :config #(let []
                  ;; Decide if we use an external colorscheme or our own base16-based one
                  (case name
                    "Tokyonight Moon"
                    (do
                      (vim.cmd.colorscheme :tokyonight-moon))
                    "Nord"
                    (vim.cmd.colorscheme :nord)
                    "Gruvbox"
                    (use! :ellisonleao/gruvbox.nvim
                          {:config #(do
                                      (setup :gruvbox {:transparent_mode true})
                                      (vim.cmd.colorscheme :gruvbox))})
                    "Kanagawa"
                    (vim.cmd.colorscheme :kanagawa)
                    "Catppuccin"
                    #(do
                       (vim.cmd.colorscheme :catppuccin-mocha)
                       (hl! :Normal {:fg colors.base05 :bg :None})
                       (hl! :NormalNC {:fg colors.base05 :bg :None}))
                    ;; Fallback; derives colorscheme from base16 colors
                    _
                    (vim.cmd.colorscheme :base16)))})
