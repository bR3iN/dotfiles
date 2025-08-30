(local {: use!} (require :utils))
(local {: colors} (require :base16-colors))
(local {: darken} (require :utils.colors))

(use! :folke/which-key.nvim
      {:hl {:WhichKeyDesc {:fg colors.base05}
            :WhichKeySeparator {:fg (darken colors.base04 0.2)}}
       :setup {:which-key {:delay 1000
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

