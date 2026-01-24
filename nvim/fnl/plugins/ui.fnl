(local {: use! : reload : keymaps!} (require :utils))
(local {: get-named} (require :utils.colors))
(local colors (get-named))

;; Theme and statusline
(reload :plugins.ui.theme)

(use! [:rebelot/heirline.nvim :SmiteshP/nvim-navic :b0o/incline.nvim]
      {:reload :plugins.ui.statusline})

; (require :plugin.highlight-trailing-whitespace)

;; Highlights hex color codes with their color
(use! :NvChad/nvim-colorizer.lua
      {:setup {:colorizer {:user_default_options {:names false}}}
       :keymaps {:n {:<Plug>ui#toggle-colorizer vim.cmd.ColorizerToggle}}})

(use! :lukas-reineke/headlines.nvim
      {:setup {:headlines {:markdown {:fat_headlines false}}}})

;; Floating preview in quickfix window
(use! :kevinhwang91/nvim-bqf
      {:hl {:BqfPreviewBorder {:bg :NONE :fg colors.mid}}
       :setup {:bqf {:func_map {:fzffilter "" :open "<C-]>"}
                     :preview {:winblend 0}}}})

(use! :lukas-reineke/indent-blankline.nvim
      {:hl {:IblScope {:fg colors.dark_green :bold true}
            :IblIndent {:fg colors.bg2 :bold true}}
       :setup {:ibl {:enabled false}}
       :keymaps #{:n {:<Plug>ui#toggle-indent-lines vim.cmd.IBLToggle}}})

(local {: darken} (require :utils.colors))

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
