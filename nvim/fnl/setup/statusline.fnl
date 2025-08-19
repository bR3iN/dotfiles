(local {: darken : get-named : mix : lighten} (require :utils.colors))
(local {: empty? : hl! : starts-with} (require :utils))

(local {:setup setup-heirline} (require :heirline))
(local {:setup setup-navic} (require :nvim-navic))
(local {: lsp_attached
        : is_git_repo
        : has_diagnostics
        : buffer_matches
        : is_active} (require :heirline.conditions))

(local {:get_location navic_get_location :is_available navic_available}
       (require :nvim-navic))

(local named (get-named))

(fn with-rounded-bg [bg comp]
  {:hl {: bg}
   1 {:provider "" :hl {:fg bg :bg :NONE}}
   2 comp
   3 {:provider "" :hl {:fg bg :bg :NONE}}})

(local navic-sep "  ")
(local navic-sep-hl {:fg (darken named.fg0 0.2)})

(local navic-hls {:NavicIconsArray {:fg named.yellow}
                  :NavicIconsBoolean {:fg named.cyan :bold true}
                  :NavicIconsClass {:fg named.cyan}
                  :NavicIconsConstant {:fg named.yellow}
                  :NavicIconsConstructor {:fg named.cyan}
                  :NavicIconsEnum {:fg named.cyan}
                  :NavicIconsEnumMember {:fg named.dark_cyan}
                  :NavicIconsEvent {:fg named.fg0}
                  :NavicIconsField {:fg named.dark_orange :italic true}
                  :NavicIconsFile {:fg named.green}
                  :NavicIconsFunction {:fg named.blue :italic true}
                  :NavicIconsInterface {:fg named.cyan}
                  :NavicIconsKey {:fg named.cyan}
                  :NavicIconsMethod {:fg named.blue :italic true}
                  :NavicIconsModule {:fg named.dark_green :italic true}
                  :NavicIconsNamespace {:fg named.fg0 :italic true}
                  :NavicIconsNull {:fg named.cyan}
                  :NavicIconsNumber {:fg named.magenta}
                  :NavicIconsObject {:fg named.cyan}
                  :NavicIconsOperator {:fg named.cyan}
                  :NavicIconsPackage {:fg named.fg0 :italic true}
                  :NavicIconsProperty {:fg named.dark_orange :italic true}
                  :NavicIconsString {:fg named.green :italic true}
                  :NavicIconsStruct {:fg named.cyan}
                  :NavicIconsTypeParameter {:fg named.blue}
                  :NavicIconsVariable {:fg named.dark_yellow :bold true}
                  :NavicText {:fg named.fg1}
                  :NavicSeparator navic-sep-hl})

(local hl--sep {:provider "│" :hl {:fg named.bg0 :bold true}})
(local hl--space {:provider " "})

(local hl--mode-map
       {:NORMAL {:color named.dark_green :modes [:n :niI :niR :niV]}
        :OP {:color named.dark_green :modes [:no :nov :noV "no\022"]}
        :VISUAL {:color named.base0D :modes [:v :vs]}
        :LINES {:color named.base0D :modes [:V :Vs]}
        :BLOCK {:color named.base0D :modes ["\022" "\022s" "\019"]}
        :SELECT {:color named.base09 :modes [:s :S]}
        :INSERT {:color named.dark_yellow :modes [:i :ic :ix]}
        :REPLACE {:color named.base0E :modes [:R :Rc :Rx]}
        :V-REPLACE {:color named.base0E :modes [:Rv :Rvc :Rvx]}
        :COMMAND {:color named.dark_green :modes [:c :cv :ce]}
        :ENTER {:color named.base0C :modes [:r]}
        :MORE {:color named.base0C :modes [:rm]}
        :CONFIRM {:color named.base09 :modes [:r?]}
        :SHELL {:color named.dark_green :modes [" !"]}
        :TERM {:color named.dark_green :modes [:nt :t]}
        :NONE {:color named.base0A :modes [:null]}})

(local hl--mode-lookup (let [res {}]
                         (each [name {: color : modes} (pairs hl--mode-map)]
                           (each [_ mode (ipairs modes)]
                             (tset res mode {: name : color})))
                         res))

(local hl--mode {:provider #(.. " " (. hl--mode-lookup $1.mode :name) " ")
                 :init #(set $1.mode (vim.fn.mode 1))
                 :hl #{:reverse true
                       :bold true
                       :fg (. hl--mode-lookup $1.mode :color)}
                 :update {1 :RecordingEnter 2 :RecordingLeave 3 :ModeChanged}
                 :static {}})

(local hl--lsp {:icon "" :color named.dark_yellow})

(local hl--lsp-list
       {:hl {:fg hl--lsp.color}
        1 {:provider hl--lsp.icon}
        2 {:flexible 2
           1 [{:provider " "}
              {:provider #(table.concat (icollect [_ {: name} (pairs (vim.lsp.get_clients {:bufnr 0}))]
                                          name)
                                        " ")}
              {:provider " "}]
           2 [{:provider " "} {:provider #(length (vim.lsp.get_clients))}]}})

(local hl--lsp-icon {:hl {:fg hl--lsp.color} :provider hl--lsp.icon})

(local hl--navic
       (let [navic-get #(let [loc (navic_get_location $...)]
                          ;; (if (not (empty? loc))
                          ;;     (.. navic-sep loc)
                          ;;     loc)
                          loc)]
         {:hl navic-sep-hl
          :update :CursorMoved
          1 {:flexible 5
             1 {:provider #(navic-get)}
             2 {:provider #(navic-get {:depth_limit 1})}}}))

;; 1 [{:provider " "} hl--sep {:provider " "}]
;; [hl--sep {:provider " "}]
(local hl--diag
       (let []
         {:update [:DiagnosticChanged :BufEnter]
          1 {:provider " "}
          2 (icollect [_ diag (ipairs [:hint :info :warn :error])]
              (let [severity (. vim.diagnostic.severity (string.upper diag))
                    {:signs {:text {severity icon}}} (vim.diagnostic.config)]
                ;; Component for a single diagnostic type
                {:init #(set $1.count
                             (length (vim.diagnostic.get 0 {: severity})))
                 :hl {:fg (. named diag)}
                 1 {:condition #(> $1.count 0)
                    :provider #(.. "" $1.count "  ")}}))}))

(local hl--git-shortstat
       {:init #(set $1.status_dict vim.b.gitsigns_status_dict)
        1 [{:provider (fn [self]
                        (.. "+" (or self.status_dict.added 0)))
            :hl {:fg named.dark_green}}
           {:provider (fn [self]
                        (.. "-" (or self.status_dict.removed 0)))
            :hl {:fg named.dark_red}}
           {:provider (fn [self]
                        (.. "~" (or self.status_dict.changed 0)))
            :hl {:fg named.dark_yellow}}]})

(local hl--git {:init #(set $1.status_dict vim.b.gitsigns_status_dict)
                1 [{:hl {:fg named.dark_orange :bold true}
                    1 {:provider "󰘬"}
                    2 {:flexible 4
                       1 [{:provider " "}
                          ;; Branchname
                          {:provider #$1.status_dict.head}]
                       2 {:provider ""}}}
                   ;; Current diff shortstat
                   {:provider " "}]})

(local hl--ruler [{:provider " %l" :hl {:fg named.dark_blue}}
                  {:provider ":"}
                  {:provider "%c " :hl {:fg named.dark_blue}}])

(local hl--file {:hl {:bold nil}
                 :flexible 3
                 1 {:provider "%f"}
                 2 {:provider #(let [name (-> vim.g.actual_curbuf
                                              (tonumber)
                                              (vim.api.nvim_buf_get_name)
                                              (vim.fn.fnamemodify ":t"))]
                                 (if (not= "" name)
                                     name
                                     "[No Name]"))}})

(local hl--line-count {:provider " %L " :hl {:fg nil}})
(local hl--perc {:hl {:fg nil} 1 {:provider " %p%% "}})

(local hl--win-nr {:provider #(tostring (vim.api.nvim_win_get_number 0))
                   :hl {:fg named.dark_red}})

;; Top-level statusline definitions

(local statusline {:hl {:fg named.fg0}
                   1 [;; left side
                      hl--mode
                      hl--space
                      {:condition is_git_repo 1 [hl--git hl--space]}
                      {:provider "%="}
                      ;; ;; middle
                      ;; hl--space
                      ;; hl--file
                      {:condition #(navic_available) 1 hl--navic}
                      ;; hl--space
                      {:provider "%="}
                      ;; right side
                      {:condition lsp_attached 1 [hl--space hl--lsp-list]}
                      hl--sep
                      hl--line-count
                      hl--sep
                      hl--ruler
                      hl--sep
                      ; Percentage in file
                      hl--perc]})

(local winbar-icons [{:condition (fn [] vim.bo.readonly)
                      :provider " "
                      :hl {:fg named.green}}
                     {:condition (fn [] vim.bo.modified)
                      :provider " "
                      :hl {:fg named.orange}}])

(local winbar {:hl {:fg (lighten named.fg0 0.2) :bg (darken named.bg3 0.2)}
               ;; left side
               1 [{1 [hl--space
                      hl--win-nr
                      hl--space
                      {:hl #{:bold (is_active)} 1 hl--file}
                      hl--space
                      winbar-icons]}
                  ;; {:provider "%="}
                  ;; ;; middle
                  ;; [hl--space
                  ;;    ; {:condition #(navic_available) 1 hl--navic}
                  ;;    hl--space]
                  {:provider "%="}
                  ;; right side
                  hl--space
                  ;; LSP diagnostics
                  {:condition has_diagnostics 1 [hl--sep hl--diag]}
                  ;; Git Shortstats
                  {:condition is_git_repo
                   1 [hl--sep hl--space hl--git-shortstat hl--space]}
                  ;; [hl--sep hl--space hl--win-nr hl--space]
                  ;; {:condition lsp_attached
                  ;;  1 [hl--sep hl--space hl--lsp-icon hl--space]}
                  ]})

;; Setup

;; Navic highlight groups
(each [hl-name hl-opt (pairs navic-hls)] ; (set hl-opt.bg named.bg2)
  (hl! hl-name hl-opt))

(hl! :StatusLine {:extend true :bold false})
(hl! :StatusLineNC {:extend true :bold false})
(hl! :WinBar {:fg named.fg1})
(hl! :WinBarNC {:fg named.fg1})
(setup-navic {:highlight true :separator navic-sep :lsp {:auto_attach true}})

;; Setup heirline
(setup-heirline {: statusline
                 : winbar
                 :opts {:disable_winbar_cb (fn [{: buf}]
                                             (let [is-floating (not= (. (vim.api.nvim_win_get_config 0)
                                                                        :relative)
                                                                     "")
                                                   buftype []
                                                   filetype [:TelescopeResults]]
                                               (or is-floating
                                                   (buffer_matches {: buftype
                                                                    : filetype}
                                                                   buf))))}})
