(import-macros {: set! : setl! : setl+ : setl- : setg! : set+ : let! : with-cb}
               :utils.macros)

(local {: darken : get-named : mix : lighten} (require :utils.colors))
(local {: empty? : hl! : starts-with : autocmd!} (require :utils))

(local {:setup setup-heirline} (require :heirline))
(local {: make_tablist} (require :heirline.utils))
(local {:setup setup-navic} (require :nvim-navic))
(local {:setup setup-incline} (require :incline))
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
(local hl--sep-left-aligned {:provider "⎸" :hl {:fg named.bg0 :bold true}})
(local hl--sep-right-aligned {:provider "⎹" :hl {:fg named.bg0 :bold true}})
(local hl--space {:provider " "})

(local hl--mode-map
       {:NORMAL {:color named.dark_green :modes [:n :niI :niR :niV :nt]}
        :OP {:color named.dark_green :modes [:no :nov :noV "no\022"]}
        :VISUAL {:color named.base0D :modes [:v :vs]}
        :LINES {:color named.base0D :modes [:V :Vs]}
        :BLOCK {:color named.base0D :modes ["\022" "\022s" "\019"]}
        :SELECT {:color named.base09 :modes [:s :S]}
        :INSERT {:color named.yellow :modes [:i :ic :ix :t]}
        :REPLACE {:color named.base0E :modes [:R :Rc :Rx]}
        :V-REPLACE {:color named.base0E :modes [:Rv :Rvc :Rvx]}
        :COMMAND {:color named.dark_magenta :modes [:c :cv :ce]}
        :ENTER {:color named.base0C :modes [:r]}
        :MORE {:color named.base0C :modes [:rm]}
        :CONFIRM {:color named.base09 :modes [:r?]}
        :SHELL {:color named.dark_green :modes [" !"]}
        :NONE {:color named.base0A :modes [:null]}})

(local hl--mode-lookup (let [res {}]
                         (each [name {: color : modes} (pairs hl--mode-map)]
                           (each [_ mode (ipairs modes)]
                             (tset res mode {: name : color})))
                         res))

(fn get-mode []
  (. (vim.api.nvim_get_mode) :mode))

(local hl--mode-short {:provider " "
                       :init #(set $1.color
                                   (. hl--mode-lookup (get-mode) :color))
                       :hl #{:bg (. hl--mode-lookup (get-mode) :color)}
                       :static {}})

(local hl--mode {:provider #(.. " " (. hl--mode-lookup $1.mode :name) " ")
                 :init #(set $1.mode (get-mode))
                 :hl #{:reverse true
                       :bold true
                       :fg (. hl--mode-lookup $1.mode :color)}
                 ;; :update {1 :RecordingEnter 2 :RecordingLeave 3 :ModeChanged}
                 :static {}})

(local hl--lsp {:icon "" :color (darken named.dark_yellow 0.1)})

(local hl--lsp-list
       {:hl {:fg hl--lsp.color}
        1 [{:provider hl--lsp.icon}
           {:provider " "}
           {:flexible 2
            1 [{:provider #(table.concat (icollect [_ {: name} (pairs (vim.lsp.get_clients {:bufnr 0}))]
                                           name)
                                         " ")}]
            2 [{:provider " "} {:provider #(length (vim.lsp.get_clients))}]}]})

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

(fn try-around [arr el]
  (when (< 0 (length arr))
    (table.insert arr el)
    (table.insert arr 1 el))
  arr)

(fn nonempty [arr]
  (icollect [_ subarr (ipairs arr)]
    (when (< 0 (length subarr))
      subarr)))

(fn sep-with [arr sep]
  (let [res []]
    (each [n el (ipairs (nonempty arr))]
      (when (> n 1)
        (table.insert res sep))
      (table.insert res el))
    res))

(fn inc--icons [{: buf}]
  (let [res []
        add #(table.insert res $1)]
    (when (. vim.bo buf :readonly)
      (add {1 "" :guifg named.green}))
    (when (. vim.bo buf :modified)
      (add {1 "" :guifg named.yellow}))
    (sep-with res " ")))

(fn inc--diag [{: buf}]
  (sep-with (icollect [_ diag (ipairs [:hint :info :warn :error])]
              (let [severity (. vim.diagnostic.severity (string.upper diag))
                    {:signs {:text {severity icon}}} (vim.diagnostic.config)
                    count (length (vim.diagnostic.get buf {: severity}))]
                ;; Component for a single diagnostic type
                (when (> count 0)
                  {:guifg (. named diag) 1 (.. count " ")})))
            " "))

(fn inc--win-nr [{: win}]
  {1 (tostring (vim.api.nvim_win_get_number win))
   :guifg named.dark_cyan
   :gui "bold"})

(hl! :InclineNormal {:link :WinBar})
(hl! :InclineNormalNC {:link :InclineNormal})

(setup-incline {:window {:margin {:horizontal 0} :padding 0}
                :hide {:cursorline true}
                :render (fn [props]
                          [(try-around (sep-with [(inc--icons props)
                                                  (inc--diag props)
                                                  ;; (inc--win-nr props)
                                                  ]
                                                 {1 " ⎸" :guifg named.bg0 :gui :bold})
                                       " ")])})

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
                       2 {:provider ""}}}]})

(local hl--ruler [{:provider "%l" :hl {:fg named.dark_blue}}
                  {:provider ":" :hl {:fg (darken named.fg0 0.2)}}
                  {:provider "%c" :hl {:fg named.dark_blue}}])

(fn get-buf-name [bufnr]
  (let [name (vim.api.nvim_buf_get_name bufnr)]
    (if (= "" name) "[No Name]" ;; For terminal buffers, strip away noise
        (= (. vim.bo bufnr :buftype) :terminal) (string.gsub name "([^:]+:)" "")
        (vim.fn.fnamemodify name ":t"))))

(local hl--file {:hl #{:fg (if (is_active) named.fg0 named.fg0)
                       :bold (is_active)}
                 :flexible 3
                 1 {:provider "%f"}
                 2 {:provider #(-> vim.g.actual_curbuf
                                   (tonumber)
                                   (get-buf-name))}})

(local hl--line-count {:provider "%L" :hl {:fg nil}})
(local hl--perc {:hl {:fg nil} 1 {:provider "%p%%"}})

(local hl--win-nr {:provider #(tostring (vim.api.nvim_win_get_number 0))
                   :hl {:fg named.dark_red :bold true}})

(fn is-not-incline-win [win]
  (and (vim.api.nvim_win_is_valid win)
       (let [buf (vim.api.nvim_win_get_buf win)]
         (not= (. vim.bo buf :filetype) :incline))))

(local hl--tablist {;;:condition #(< 1 (length (vim.api.nvim_list_tabpages)))
                    1 (make_tablist {:hl #(if $1.is_active "TabLineSel")
                                     ;; :condition #(vim.api.nvim_tabpage_is_valid $1.tabnr)
                                     1 [{:provider #(.. "%" $1.tabnr "T")}
                                        hl--space
                                        ;; Tab nr
                                        {:hl {:fg named.dark_yellow}
                                         :provider #$1.tabnr}
                                        ;; Separator
                                        {:hl {:fg named.fg0} :provider ":"}
                                        ;; Selected window
                                        {:provider #(-> $.tabpage
                                                        (vim.api.nvim_tabpage_get_win)
                                                        (vim.api.nvim_win_get_buf)
                                                        (get-buf-name))}
                                        {:hl {:fg named.fg0} :provider "/"}
                                        ;; Number of windows
                                        ;; FIXME: wrong, likely counts
                                        {:hl {:fg named.fg0}
                                         :provider #(->> $1.tabpage
                                                         (vim.api.nvim_tabpage_list_wins)
                                                         (vim.tbl_filter is-not-incline-win)
                                                         (length))}
                                        hl--space
                                        {:provider "%T"}]})})

;; Top-level statusline definitions

(local hl--user-and-host
       [{:hl {:fg named.green} :provider (. (vim.uv.os_get_passwd) :username)}
        {:provider "@"}
        {:provider (vim.uv.os_gethostname)}])

(local tabline [;; left side
                hl--mode-short
                hl--tablist
                {:provider "%="}
                ;; right side
                {:condition is_git_repo 1 [hl--space hl--git]}
                hl--space
                {:hl {:bg named.bg2} 1 [hl--space hl--user-and-host hl--space]}
                ])

(autocmd! {:event [:ModeChanged :InsertLeave :InsertEnter]
           :callback #(vim.cmd.redrawtabline)})

(local winbar-icons [{:condition (fn [] vim.bo.readonly)
                      :provider " "
                      :hl {:fg named.green}}
                     {:condition (fn [] vim.bo.modified)
                      :provider " "
                      :hl {:fg named.orange}}])

(local statusline [;; left side
               hl--space
               hl--win-nr
               hl--space
               hl--sep-left-aligned
               hl--file
               hl--space
               ;; Modifer icons
               {:provider "%="}
               ;; middle
               ;; [hl--space
               ;;  {:condition #(navic_available) 1 hl--navic}
               ;;  hl--space]
               {:provider "%="}
               {:condition lsp_attached 1 hl--lsp-list}
               hl--sep-right-aligned
               ;; Line Count
               hl--space
               {:hl {:fg named.dark_magenta} 1 hl--line-count}
               hl--sep-right-aligned
               ;; Ruler
               hl--space
               hl--ruler
               hl--sep-right-aligned
               ; Percentage in file
               hl--space
               {:hl {:fg named.dark_green} 1 hl--perc}
               hl--space
               ])

;; Setup

;; Navic highlight groups
(each [hl-name hl-opt (pairs navic-hls)] ; (set hl-opt.bg named.bg2)
  (hl! hl-name hl-opt))

(hl! :StatusLine {:fg named.fg1 :bg named.bg3})
(hl! :StatusLineNC {:fg named.fg1 :bg (darken named.bg1 0.0)})
;; (hl! :StatusLine {:bg named.base02 :fg named.base03 :bold false})
;; (hl! :StatusLineNC {:bg named.base02 :fg named.base03 :bold false})
;; :hl {:fg (lighten named.fg0 0.2) :bg (darken named.bg3 0.2)}
(hl! :WinBar {:fg named.fg0 :bg named.bg1})
(hl! :WinBarNC {:link :WinBar})
(hl! :TabLine {:fg named.fg0 :bg named.bg1 :bold false})
(hl! :TabLineSel {:fg named.green :bg named.bg3 :bold false})

;; (hl! :Normal {:extend true :bg (darken named.bg0 0.1)})
;; (hl! :NormalNC {:extend true :bg (darken named.bg0 0.5)})
(setup-navic {:highlight true :separator navic-sep :lsp {:auto_attach true}})

;; Setup heirline
(setup-heirline {: statusline
                 ;; : winbar
                 : tabline
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

;; Always display tab line
(set! showtabline 2)

;; Show statusline on all windows
(set! laststatus 2)
