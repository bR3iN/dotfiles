(local {: hl! : use! : reload : opts! : setup : keymaps!} (require :utils))

(local c (require :config))
(local {: get-named : darken} (require :utils.colors))

(local {: name} (require :base16-colors))
(local colors (get-named))

(opts! {:winborder :solid
        :signcolumn :yes
        :termguicolors true
        ;; Remove redundant mode prompt in insert area
        :showmode true
        :cmdheight 1})

(opts! {;; Always display tab line
        :showtabline 1
        ;; Show statusline on all windows
        :laststatus 3
        :fillchars "vert:│,wbr: "
        :scrolloff 2
        :linebreak true})

(reload :appearance.colorscheme)

(use! [:rebelot/heirline.nvim :SmiteshP/nvim-navic :b0o/incline.nvim]
      {:setup {:nvim-navic {:highlight true
                            :separator c.navic-sep
                            :lsp {:auto_attach true}}}
       :reload :appearance.statusline})

;; Custom color setup; load colorscheme description (name + base16 colors)
(use! [;;:bR3iN/base16.nvim
       ;; :folke/tokyonight.nvim
       ;; :shaunsingh/nord.nvim
       ;; :ellisonleao/gruvbox.nvim
       ;; :rebelot/kanagawa.nvim
       ;; :catppuccin/nvim
       ] {:autocmds #{:ColorScheme {:callback (let [cb #(do
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
                                                                (keymaps! {:n {:<C-h> {:desc "Toggle Comment Highlighting"
                                                                                       :callback ":ToggleComments<CR>"}
                                                                               :<Plug>ui#toggle-comments ":ToggleComments<CR>"}}))]
                                                      (cb)
                                                      cb)}}
                :init #(set vim.g.nord_disable_background true)
                ;; :setup {:tokyonight {:transparent true}
                ;;         :gruvbox {:transparent_mode true}
                ;;         :kanagawa {:transparent true}}
                :config #(let []
                           ;; Decide if we use an external colorscheme or our own base16-based one
                           (case name
                             ;; "Tokyonight Moon"
                             ;; (vim.cmd.colorscheme :tokyonight-moon)
                             ;; "Nord"
                             ;; (vim.cmd.colorscheme :nord)
                             ;; "Gruvbox"
                             ;; (use! :ellisonleao/gruvbox.nvim
                             ;;       {:config #(do
                             ;;                   (setup :gruvbox {:transparent_mode true})
                             ;;                   (vim.cmd.colorscheme :gruvbox))})
                             ;; "Kanagawa"
                             ;; (vim.cmd.colorscheme :kanagawa)
                             ;; "Catppuccin"
                             ;; #(do
                             ;;    (vim.cmd.colorscheme :catppuccin-mocha)
                             ;;    (hl! :Normal {:fg colors.base05 :bg :None})
                             ;;    (hl! :NormalNC {:fg colors.base05 :bg :None}))
                             ;; Fallback; derives colorscheme from base16 colors
                             _
                             (vim.cmd.colorscheme :base16)))})

(use! :folke/which-key.nvim
      {:hl {:WhichKeyDesc {:fg colors.base05}
            :WhichKeySeparator {:fg (darken colors.base04 0.2)}}
       :setup {:which-key {:delay 300
                           :win {:width {:min 30 :max 60}
                                 :height {:min 4 :max 0.75}
                                 :padding [0 1]
                                 :col -1
                                 :row -1
                                 :border :rounded
                                 :title true
                                 :title_pos :left}
                           :layout {:width {:min 30}}
                           :icons {:mappings false
                                   :rules false
                                   :keys (collect [_ key (ipairs [:Up
                                                                  :Down
                                                                  :Left
                                                                  :Right
                                                                  :C
                                                                  :M
                                                                  :D
                                                                  :S
                                                                  :CR
                                                                  :Esc
                                                                  :ScrollWheelDown
                                                                  :ScrollWheelUp
                                                                  :NL
                                                                  :BS
                                                                  :Space
                                                                  :Tab
                                                                  :F1
                                                                  :F2
                                                                  :F3
                                                                  :F4
                                                                  :F5
                                                                  :F6
                                                                  :F7
                                                                  :F8
                                                                  :F9
                                                                  :F10
                                                                  :F11
                                                                  :F12])]
                                           (values key (.. "[" key "]")))}}}})
