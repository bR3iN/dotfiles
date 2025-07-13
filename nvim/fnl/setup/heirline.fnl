(local {: colors} (require :base16-colors))
(local {: empty?} (require :utils))

(local {:setup setup-heirline} (require :heirline))
(local {:setup setup-navic} (require :nvim-navic))
(local {: lsp_attached : is_git_repo} (require :heirline.conditions))
(local {:get_location navic_get_location :is_available navic_available}
       (require :nvim-navic))

(local navic-sep "  ")
(local navic-sep-hl {:fg colors.fg0})

(local navic-hls {:NavicIconsArray {:fg colors.yellow}
                  :NavicIconsBoolean {:fg colors.cyan :bold true}
                  :NavicIconsClass {:fg colors.cyan}
                  :NavicIconsConstant {:fg colors.yellow}
                  :NavicIconsConstructor {:fg colors.cyan}
                  :NavicIconsEnum {:fg colors.cyan}
                  :NavicIconsEnumMember {:fg colors.fg0}
                  :NavicIconsEvent {:fg colors.fg0}
                  :NavicIconsField {:fg colors.fg0 :italic true}
                  :NavicIconsFile {:fg colors.green}
                  :NavicIconsFunction {:fg colors.blue :italic true}
                  :NavicIconsInterface {:fg colors.cyan}
                  :NavicIconsKey {:fg colors.cyan}
                  :NavicIconsMethod {:fg colors.blue :italic true}
                  :NavicIconsModule {:fg colors.fg0 :italic true}
                  :NavicIconsNamespace {:fg colors.fg0 :italic true}
                  :NavicIconsNull {:fg colors.cyan}
                  :NavicIconsNumber {:fg colors.magenta}
                  :NavicIconsObject {:fg colors.cyan}
                  :NavicIconsOperator {:fg colors.cyan}
                  :NavicIconsPackage {:fg colors.fg0 :italic true}
                  :NavicIconsProperty {:fg colors.fg0 :italic true}
                  :NavicIconsString {:fg colors.green :italic true}
                  :NavicIconsStruct {:fg colors.cyan}
                  :NavicIconsTypeParameter {:fg colors.blue}
                  :NavicIconsVariable {:fg colors.fg0 :bold true}
                  :NavicText {:fg colors.fg1}
                  :NavicSeparator navic-sep-hl})

(local hl--sep {:provider "│" :hl {:fg colors.bg0 :bold true}})
(local hl--space {:provider " "})

(local hl-mode-map
       {:NORMAL {:color colors.dark_green :modes [:n :niI :niR :niV]}
        :OP {:color colors.dark_green :modes [:no :nov :noV "no\022"]}
        :VISUAL {:color colors.base0D :modes [:v :vs]}
        :LINES {:color colors.base0D :modes [:V :Vs]}
        :BLOCK {:color colors.base0D :modes ["\022" "\022s" "\019"]}
        :SELECT {:color colors.base09 :modes [:s :S]}
        :INSERT {:color colors.dark_yellow :modes [:i :ic :ix]}
        :REPLACE {:color colors.base0E :modes [:R :Rc :Rx]}
        :V-REPLACE {:color colors.base0E :modes [:Rv :Rvc :Rvx]}
        :COMMAND {:color colors.dark_green :modes [:c :cv :ce]}
        :ENTER {:color colors.base0C :modes [:r]}
        :MORE {:color colors.base0C :modes [:rm]}
        :CONFIRM {:color colors.base09 :modes [:r?]}
        :SHELL {:color colors.dark_green :modes [" !"]}
        :TERM {:color colors.dark_green :modes [:nt :t]}
        :NONE {:color colors.base0A :modes [:null]}})

(local hl--mode (let [get-color #(let [{: mode_colors : mode} $1]
                                   (. mode_colors mode))]
                  {:provider #(.. " " (. $1 :mode) " ")
                   :init #(let [{: mode_names} $1]
                            (->> (vim.fn.mode 1)
                                 (. mode_names)
                                 (tset $1 :mode)))
                   :hl #{:reverse true :bold true :fg (get-color $1)}
                   :update {1 :RecordingEnter 2 :RecordingLeave 3 :ModeChanged}
                   :static (do
                             (local res {:mode_names {} :mode_colors {}})
                             (each [name {: color : modes} (pairs hl-mode-map)]
                               (tset res :mode_colors name color)
                               (each [_ mode (ipairs modes)]
                                 (tset res :mode_names mode name)))
                             res)}))

(local hl--lsps
       {:hl {:fg colors.dark_yellow}
        1 {:provider ""}
        2 {:flexible 2
           1 [{:provider " ["}
              {:provider #(table.concat (icollect [_ {: name} (pairs (vim.lsp.get_clients {:bufnr 0}))]
                                          name)
                                        " ")}
              {:provider "]"}]
           2 [{:provider " "} {:provider #(length (vim.lsp.get_clients))}]}})

(local hl--navic
       (let [navic-get #(let [loc (navic_get_location $...)]
                          (if (not (empty? loc))
                              (.. navic-sep loc)
                              loc))]
         {:hl navic-sep-hl
          :update :CursorMoved
          1 {:flexible 5
             1 {:provider #(navic-get)}
             2 {:provider #(navic-get {:depth_limit 1})}}}))

(local hl--ruler
       [{:provider " %l" :hl {:fg colors.dark_blue}}
        {:provider ":"}
        {:provider "%c " :hl {:fg colors.dark_blue}}])

(local hl--git {:condition is_git_repo
                :init #(set $1.status_dict vim.b.gitsigns_status_dict)
                1 [{:hl {:fg colors.dark_orange :bold true}
                    1 {:provider "󰘬"}
                    2 {:flexible 4
                       1 {:provider (fn [self]
                                      (.. " " self.status_dict.head))}
                       2 {:provider ""}}}
                   {:flexible 1
                    1 [{:provider " "}
                       {:provider (fn [self]
                                    (.. "+" (or self.status_dict.added 0)))
                        :hl {:fg colors.dark_green}}
                       {:provider (fn [self]
                                    (.. "-" (or self.status_dict.removed 0)))
                        :hl {:fg colors.dark_red}}
                       {:provider (fn [self]
                                    (.. "~" (or self.status_dict.changed 0)))
                        :hl {:fg colors.dark_yellow}}]
                    2 {:provider ""}}
                   {:provider " "}]})

(local hl--file {:hl {:fg colors.fg0 :bold true}
                 :flexible 3
                 1 {:provider "%f"}
                 2 {:provider #(let [name (-> vim.g.actual_curbuf
                                              (tonumber)
                                              (vim.api.nvim_buf_get_name)
                                              (vim.fn.fnamemodify ":t"))]
                                 (if (not= "" name)
                                     name
                                     "[No Name]"))}})

(local hl--line-count {:provider " %L "})
(local hl--perc {:hl {:fg colors.fg0} 1 {:provider " %p%% "}})

(local statusline {:hl {:fg colors.fg0 :bg colors.bg2 :reverse false}
                   ;; left side
                   1 [hl--mode
                      hl--space
                      {:condition lsp_attached 1 [hl--lsps hl--space]}]
                   2 {:provider "%="}
                   ;; middle
                   3 [hl--space
                      hl--file
                      {:condition #(navic_available) 1 hl--navic}
                      hl--space]
                   4 {:provider "%="}
                   ;; right side
                   5 [hl--space
                      hl--git
                      hl--sep
                      hl--line-count
                      hl--sep
                      hl--ruler
                      hl--sep
                      ; Percentage in file
                      hl--perc]})

(fn hl! [name hl]
   (vim.api.nvim_set_hl 0 name hl))

{:setup (fn [_]
          ;; Setup navic highlight groups
          (each [hl-name hl-opt (pairs navic-hls)]
            (set hl-opt.bg colors.base02)
            (hl! hl-name hl-opt))
          (hl! :StatusLine {:reverse false :bg colors.bg2})
          (hl! :StatusLineNC {:reverse false :bg colors.bg2})
          (setup-navic {:highlight true
                        :separator navic-sep
                        :lsp {:auto_attach true}})

          ;; Setup heirline
          (setup-heirline {: statusline}))}
