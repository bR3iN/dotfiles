(local {: darken : get-named : mix : lighten} (require :utils.colors))
(local {: empty?
        : hl!
        : remove-prefix
        : remove-opt-prefix
        : autocmd!
        : starts-with
        : unique-bufname} (require :utils))

(local {:setup setup-heirline} (require :heirline))
(local {: make_tablist : is_active} (require :heirline.utils))
(local {:setup setup-navic} (require :nvim-navic))
;; (local {:setup setup-incline} (require :incline))
(local {: lsp_attached
        : is_git_repo
        : has_diagnostics
        : buffer_matches
        : is_active} (require :heirline.conditions))

(local {:get_location navic_get_location :is_available navic_available}
       (require :nvim-navic))

(local named (get-named))

(local winbar-active-bg (mix named.blue named.fg0 0.85))

(fn get-hl [name field]
  (. (vim.api.nvim_get_hl 0 {: name :link false}) field))

(fn with-rounded-bg [bg comp]
  {:hl {: bg}
   1 {:provider "" :hl {:fg bg :bg :NONE}}
   2 comp
   3 {:provider "" :hl {:fg bg :bg :NONE}}})

(fn parse-term-bufname [bufname]
  (case (string.match bufname "^term://(.+)//(%d+):(.+)$")
    (path pid command) {: path :pid (tonumber pid) : command}
    _ bufname))

(fn fmt-term-bufname [bufname shorten]
  (if shorten "[term]"
      (let [{: path : command} (parse-term-bufname bufname)]
        (string.format "[%s]%s" path command))))

(fn is-terminal? [bufnr]
  (= (. vim.bo bufnr :buftype) :terminal))

(local tabline-color named.transparent)
(local winbar-secondary named.bg1)
(local winbar-color named.bg3)
(local winbar-active-color named.bg3)
(local normal-color named.transparent)

(local HL-ALIGN-MARK {:provider "%="})

;; Is kinda finicky here with transparent backgrounds, so we can't implicitly inherit the current winbar color set by WinBar[NC]
(macro forward-transition [left-color ?right-color]
  `{:provider "" :hl #{:bg ,left-color :fg ,?right-color}})

(macro backward-transition [left-color ?right-color]
  `{:provider "" :hl #{:fg ,left-color :bg ,?right-color}})

(local empty-bufname "Unnamed")

(fn get-bufname [bufnr]
  (let [name (vim.api.nvim_buf_get_name bufnr)]
    (if ;; Terminal buffers
        (= (. vim.bo bufnr :buftype) :terminal) (case (parse-term-bufname name)
                                                  {: command}
                                                  (string.format "term:%s"
                                                                 command)
                                                  ;; Fallback if name was custom-formatted
                                                  nil
                                                  name
                                                  _
                                                  name)
        ;; Oil buffers
        (= (. vim.bo bufnr :filetype) :oil) (let [name (remove-opt-prefix name
                                                                          "oil://")
                                                  name (if (starts-with name
                                                                        vim.env.HOME)
                                                           (.. "~/"
                                                               (-> name
                                                                   (remove-prefix vim.env.HOME)
                                                                   ;; TODO: Can $HOME even have a trailing slash?
                                                                   (remove-opt-prefix "/")))
                                                           name)]
                                              name)
        ;; 
        (if (= "" name)
            empty-bufname
            (unique-bufname name)))))

(fn get-bufname-full [bufnr]
  (let [bufname (vim.fn.bufname bufnr)]
    ;; (case (. vim.bo bufnr :buftype)
    ;;   :terminal (let [(parse-term-bufname bufname)])
    ;;   )
    (case (parse-term-bufname bufname)
      {: path : command : pid} (string.format "%s:%s:%s" (tostring pid) command
                                              path)
      "" empty-bufname
      name (unique-bufname name)))
  ;; (if (is-terminal? bufnr)
  ;;     (let [bufname (vim.api.nvim_buf_get_name bufnr)
  ;;           {: path : command : pid} (parse-term-bufname bufname)]
  ;;       (string.format "%s:%s:%s" (tostring pid) command path))
  ;;     (case (vim.fn.bufname bufnr)
  ;;       "" empty-bufname
  ;;       name name))
  )

(fn get-bufname-shorter [bufnr]
  (let [bufname (vim.api.nvim_buf_get_name bufnr)]
    (case (parse-term-bufname bufname)
      {: command} (string.format "term:%s" command)
      "" empty-bufname
      name (unique-bufname name))))

(fn truncated [name]
  (let [size (length name)
        shown 7
        sep "..."
        max-size (+ (* 2 shown) (length sep))]
    (if (<= size max-size) name
        (.. (string.sub name 0 shown) sep (string.sub name (- size shown))))))

(fn get-bufname-shortest [bufnr]
  (-> bufnr
      (get-bufname-shorter)
      (truncated)))

(local hl-sep {:provider "│" :hl {:fg named.dark_bg0}})
(local hl-sep-left-aligned {:provider "⎸" :hl {:fg named.dark_bg0}})
(local hl-sep-right-aligned {:provider "⎹" :hl {:fg named.dark_bg0}})
(local hl-space {:provider " "})
(local hl-wide-sep [;; hl-space
                    {:provider "▕▏" :hl {:fg named.dark_bg0}}
                    ;; hl-sep hl-space
                    ])

(local hl-mode-map
       {:NORMAL {:color named.dark_green :modes [:n :niI :niR :niV :nt]}
        :OP {:color named.dark_green :modes [:no :nov :noV "no\022"]}
        :VISUAL {:color named.cyan :modes [:v :vs]}
        :LINES {:color named.blue :modes [:V :Vs]}
        :BLOCK {:color named.blue :modes ["\022" "\022s" "\019"]}
        :SELECT {:color named.dark_orange :modes [:s :S]}
        :INSERT {:color named.dark_yellow :modes [:i :ic :ix :t]}
        :REPLACE {:color named.magenta :modes [:R :Rc :Rx]}
        :V-REPLACE {:color named.magenta :modes [:Rv :Rvc :Rvx]}
        :COMMAND {:color named.dark_magenta :modes [:c :cv :ce]}
        :ENTER {:color named.cyan :modes [:r]}
        :MORE {:color named.cyan :modes [:rm]}
        :CONFIRM {:color named.orange :modes [:r?]}
        :SHELL {:color named.dark_green :modes [" !"]}
        :NONE {:color named.yellow :modes [:null]}})

(local hl-mode-lookup (let [res {}]
                        (each [name {: color : modes} (pairs hl-mode-map)]
                          (each [_ mode (ipairs modes)]
                            (tset res mode {: name : color})))
                        res))

(fn get-mode []
  (. (vim.api.nvim_get_mode) :mode))

(local hl-mode-short {:provider "█"
                      :init #(set $1.color (. hl-mode-lookup (get-mode) :color))
                      :hl #{:fg (. hl-mode-lookup (get-mode) :color)}
                      :static {}})

(local hl-mode {:provider #(.. " " (. hl-mode-lookup $1.mode :name) " ")
                :init #(set $1.mode (get-mode))
                :hl #{:reverse true
                      :bold true
                      :fg (. hl-mode-lookup $1.mode :color)}
                ;; :update {1 :RecordingEnter 2 :RecordingLeave 3 :ModeChanged}
                :static {}})

(local hl-lsp {:icon "" :color (darken named.dark_yellow 0.1)})

(local hl-lsp-list
       {:hl {:fg hl-lsp.color}
        1 [{:provider hl-lsp.icon}
           {:provider " "}
           {:flexible 2
            1 [{:provider #(table.concat (icollect [_ {: name} (pairs (vim.lsp.get_clients {:bufnr 0}))]
                                           name)
                                         " ")}]
            2 [{:provider " "} {:provider #(length (vim.lsp.get_clients))}]}]})

(local hl-lsp-icon {:hl {:fg hl-lsp.color} :provider hl-lsp.icon})

(local hl-navic
       (let [navic-get #(let [loc (navic_get_location $...)]
                          ;; (if (not (empty? loc))
                          ;;     (.. navic-sep loc)
                          ;;     loc)
                          loc)]
         {:hl {:link :NavicSeparator}
          ;; navic-sep-hl
          :update :CursorMoved
          1 {:flexible 5
             1 {:provider #(navic-get)}
             2 {:provider #(navic-get {:depth_limit 1})}}}))

;; 1 [{:provider " "} hl-sep {:provider " "}]
;; [hl-sep {:provider " "}]
(local hl-diag2 {:update [:DiagnosticChanged :BufEnter]
                 1 [;;{:provider ""}
                    (icollect [_ diag (ipairs [:hint :info :warn :error])]
                      (let [severity (. vim.diagnostic.severity
                                        (string.upper diag))
                            icon (. (vim.diagnostic.config) :signs :text
                                    severity)]
                        ;; Component for a single diagnostic type
                        {:init #(set $1.count
                                     (length (vim.diagnostic.get 0 {: severity})))
                         :hl {:fg (. named diag) :bg named.bg1}
                         1 {:condition #(> $1.count 0)
                            :provider #(.. " " $1.count " ")}}))
                    {:provider " " :hl {:bg named.bg1}}]})

;; Lowercase here as we use them to index the map of named colors (`named`)
(local diag-kinds-lc [:hint :info :warn :error])

(fn diag-static-state []
  (collect [_ diag (ipairs diag-kinds-lc)]
    (let [severity (. vim.diagnostic.severity (string.upper diag))
          icon (. (vim.diagnostic.config) :signs :text severity)]
      (values diag {: icon :color (. named diag) : severity}))))

(fn diag-counts [static]
  (collect [_ diag (ipairs diag-kinds-lc)]
    (let [severity (. static diag :severity)
          count (->> {: severity}
                     (vim.diagnostic.get 0)
                     (length))]
      (values diag count))))

(fn has-diags []
  (< 0 (length (vim.diagnostic.get 0))))

(local hl-diag {:update [:DiagnosticChanged :BufEnter :CursorHold]
                :init (fn [self]
                        (when (not self.static)
                          (set self.static (diag-static-state)))
                        (set self.counts (diag-counts self.static)))
                ;; :condition #(< 0 (accumulate [sum 0
                ;;                               _ count (pairs $1.counts)]
                ;;                    (+ sum n)))
                1 [;;{:provider ""}
                   (icollect [_ diag (ipairs diag-kinds-lc)]
                     (let [severity (. vim.diagnostic.severity
                                       (string.upper diag))
                           icon (. (vim.diagnostic.config) :signs :text
                                   severity)]
                       ;; Component for a single diagnostic type
                       {;; :init #(set $1.count
                        ;;           (length (vim.diagnostic.get 0 {: severity})))
                        :hl #{:fg (. $1.static diag :color) :bg named.bg1}
                        :condition #(> (. $1.counts diag) 0)
                        :provider #(.. " " (. $1.counts diag) " ")}))
                   {:provider " " :hl {:bg named.bg1}}]})

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
                    icon (. (vim.diagnostic.config) :signs :text severity)
                    count (length (vim.diagnostic.get buf {: severity}))]
                ;; Component for a single diagnostic type
                (when (> count 0)
                  {:guifg (. named diag) 1 (.. count " ")})))
            " "))

(fn inc--win-nr [{: win}]
  {1 (tostring (vim.api.nvim_win_get_number win))
   :guifg named.dark_cyan
   :gui "bold"})

;; (hl! :InclineNormal {:link :WinBar})
;; (hl! :InclineNormalNC {:link :InclineNormal})

;; (setup-incline {:window {
;;                          ;; :overlap {:borders false}
;;                          :margin {:horizontal 0}
;;                          :padding 0
;;                          }
;;                 :hide {;;:cursorline true
;;                        :focused_win true}
;;                 :render (fn [props]
;;                           [ " "
;;                            {1 (get-bufname-shorter props.buf)
;;                             :guifg named.fg3}
;;                            " "
;;                            (inc--win-nr props)
;;                            " "
;;                            ;; (try-around (sep-with [(inc--icons props)
;;                            ;;                        (inc--diag props)
;;                            ;;                        ;; (inc--win-nr props)
;;                            ;;                        ]
;;                            ;;                       {1 " ⎸"
;;                            ;;                        :guifg named.bg0
;;                            ;;                        :gui :bold})
;;                            ;;             " ")
;;                            ])})

(local hl-git-shortstat
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

(local hl-git {:init #(set $1.status_dict vim.b.gitsigns_status_dict)
               1 [{:hl {:fg named.dark_orange :bold true}
                   1 {:provider "󰘬"}
                   2 {:flexible 4
                      1 [{:provider " "}
                         ;; Branchname
                         {:provider #$1.status_dict.head}]
                      2 {:provider ""}}}]})

(local hl-ruler [{:provider "%l" :hl {:fg named.dark_blue}}
                 {:provider ":" :hl {:fg (darken named.fg0 0.2)}}
                 {:provider "%c" :hl {:fg named.dark_blue}}])

(local hl-file {:hl {:bold true}
                :flexible 3
                ;; 1 {:provider #(-> (vim.api.nvim_get_current_buf)
                ;;                                    (get-bufname-full))}
                1 {:provider #(-> (vim.api.nvim_get_current_buf)
                                  (get-bufname))}
                2 {:provider #(-> (vim.api.nvim_get_current_buf)
                                  (get-bufname)
                                  (truncated))}})

(local hl-line-count {:provider "%L" :hl {:fg nil}})
(local hl-perc {:hl {:fg nil} 1 {:provider "%p%%"}})

(local hl-win-nr {:provider #(tostring (vim.api.nvim_win_get_number 0))
                  :hl {:fg named.dark_red :bold true}})

(fn is-not-incline-win [win]
  (and (vim.api.nvim_win_is_valid win)
       (let [buf (vim.api.nvim_win_get_buf win)]
         (not= (. vim.bo buf :filetype) :incline))))

(local hl-tablist {;;:condition #(< 1 (length (vim.api.nvim_list_tabpages)))
                   1 (make_tablist {:static {:fg-dim (darken named.dark_fg0 0.5)
                                             :bg named.dark_bg1
                                             :bg-selected named.bg2}
                                    :hl #{:bg (if $1.is_active $1.bg-selected
                                                  $1.bg)
                                          :fg (if $1.is_active
                                                  named.dark_green
                                                  named.dark_fg0)}
                                    :init (fn [self]
                                            ;; :init is evaluated after :condition but seemingly before
                                            ;; the :init of children, so we just keep it here to check
                                            ;; it below.
                                            (set self.nr-wins
                                                 (->> self.tabpage
                                                      (vim.api.nvim_tabpage_list_wins)
                                                      (vim.tbl_filter is-not-incline-win)
                                                      (length))))
                                    ;; :condition #(vim.api.nvim_tabpage_is_valid $1.tabnr)
                                    1 [;; Start Clickable Area
                                       {:provider #(.. "%" $1.tabnr "T")}
                                       {:provider "▏"
                                        :hl {:fg named.transparent}}
                                       ;; Tab nr
                                       {:hl {:fg named.dark_yellow}
                                        :provider #$1.tabnr}
                                       ;; Separator
                                       {:hl #{:fg $1.fg-dim} :provider ":"}
                                       ;; Selected window
                                       {:provider #(-> $1.tabpage
                                                       (vim.api.nvim_tabpage_get_win)
                                                       (vim.api.nvim_win_get_buf)
                                                       (get-bufname-shortest)
                                                       (truncated))}
                                       ;; Number of windows
                                       {:hl #{:fg $1.fg-dim}
                                        :condition #(> $1.nr-wins 1)
                                        1 {:provider #(string.format "+%s"
                                                                     (- $1.nr-wins
                                                                        1))}}
                                       {:provider "▕"
                                        :hl {:fg named.transparent}}
                                       ;; End Clickable Area
                                       {:provider "%T"}]})})

;; Top-level statusline definitions

(local hl-user-and-host
       [{:hl {:fg named.green} :provider (. (vim.uv.os_get_passwd) :username)}
        {:provider "@"}
        {:provider (vim.uv.os_gethostname)}])

(local tabline [;; left side
                ;; hl-mode-short
                hl-tablist
                HL-ALIGN-MARK
                ;; right side
                ;; {:condition is_git_repo 1 [hl-space hl-git]}
                ;; hl-space
                ;; {:hl {:bg named.bg2} 1 [hl-space hl-user-and-host hl-space]}
                ])

(autocmd! {:event [:ModeChanged :InsertLeave :InsertEnter]
           ;; :callback #(vim.cmd.redrawstatus)
           :callback #(vim.cmd.redrawtabline)})

(fn padded [?left-pad ?inner ?right-pad]
  (if ?inner
      (.. (or ?left-pad "") ?inner (or ?right-pad ""))
      ""))

(fn get-buftype-icon []
  (case vim.bo.buftype
    ""
    {:icon "󰈔" :fg (if vim.bo.modified named.cyan named.dark_brown)}
    :terminal
    {:icon "" :fg named.yellow}
    :help
    {:icon "" :fg named.dark_green}
    ;; Oil buffers
    (where :acwrite (= vim.bo.filetype :oil))
    {:icon "" :fg (if vim.bo.modified named.cyan named.dark_brown)}))

(local hl-buftype-icon {:init #(set $1.state (get-buftype-icon))
                        :provider #(padded " " (?. $1 :state :icon))
                        :hl #{:fg (?. $1 :state :fg)}})

(local hl-buftype-icon2 [{:condition #(= vim.bo.buftype "")
                          :provider " 󰈔"
                          :hl #{:fg named.fg0}}
                         {:condition #(= vim.bo.buftype "terminal")
                          :provider " "
                          :hl {:fg named.brown}}])

(local hl-file-icons [{:condition (fn [] vim.bo.readonly)
                       :provider " "
                       :hl {:fg named.green}}
                      {:condition (fn [] vim.bo.modified)
                       :provider " "
                       :hl {:fg named.orange}}
                      {:condition #(not (or vim.bo.modified vim.bo.readonly))
                       :provider "  "
                       :hl {:fg named.orange}}])

(local statusline [hl-mode-short
                   hl-space
                   {:hl {:bold true :fg named.fg0} :provider "%f"}
                   HL-ALIGN-MARK
                   hl-space
                   ;; Right side
                   ;; Attached language server
                   {:condition lsp_attached 1 [hl-lsp-list hl-wide-sep]}
                   {:condition is_git_repo
                    1 [;; Current branch
                       hl-git
                       hl-space
                       ;; Shortstats
                       hl-git-shortstat
                       hl-wide-sep]}
                   ;; Line Count
                   {:hl {:fg named.dark_magenta} 1 hl-line-count}
                   hl-wide-sep
                   ;; Ruler
                   hl-ruler
                   hl-wide-sep
                   ; Percentage in file
                   {:hl {:fg named.dark_green} 1 hl-perc}
                   hl-space])

(fn curr-winbar-color []
  (if (is_active) winbar-active-color winbar-color))

(local winbar
       [;; left side
        {:hl {:bg normal-color}
         1 [hl-space
            hl-win-nr
            hl-space
            {:hl #(if (is_active)
                      {:bg winbar-active-bg
                       ;; :bg (. hl-mode-lookup (get-mode) :color)
                       :fg named.transparent
                       ;; High contrast but might not match the colorscheme
                       :fg "#000000"}
                      {;; :bg (mix named.fg0 named.bg3 0.7)
                       :bg named.mid
                       :fg "#000000"})
             1 [(backward-transition normal-color)
                hl-space
                hl-file
                hl-space
                (forward-transition nil (curr-winbar-color))]}]}
        hl-space
        {:condition (fn [] vim.bo.readonly)
         :provider " "
         :hl {:fg named.green}}
        ;; hl-file-icons
        ;; hl-space
        ;; TODO: See if aerial.nvim (<leader>n + <leader>to) is enough
        ;; {:condition #(navic_available) 1 hl-navic}
        HL-ALIGN-MARK
        hl-space
        {;; Intermediate background transition if diagnostics are present
         :fallthrough false
         1 {:condition has-diags
            :hl #{:bg winbar-secondary}
            1 [(forward-transition (curr-winbar-color) winbar-secondary)
               hl-diag
               (forward-transition nil normal-color)]}
         2 (forward-transition (curr-winbar-color) normal-color)}
        {:hl {:bg normal-color} 1 [hl-buftype-icon hl-space]}])

;; Setup

;; (hl! :StatusLine {:fg named.fg1 :bg named.bg2})
;; (hl! :WinBarNC {:fg named.fg0 :bg winbar-color})
;; (hl! :WinBar {:fg named.fg0 :bg winbar-active-color})
;; (hl! :TabLine {:fg named.fg0 :bg tabline-color :bold false})
;; (hl! :TabLineSel {:fg named.green :bg named.bg3 :bold false})

;; (hl! :NormalNC {:extend true :bg (darken named.bg0 0.5)})
;; (setup-navic {:highlight true :separator navic-sep :lsp {:auto_attach true}})

;; (autocmd! {:event :TermOpen
;;            :pattern :*
;;            :command "setlocal winhighlight=Normal:TerminalBackground"})

;; (hl! :TerminalBackground {:bg named.bg0})
;; (hl! :WinSeparator {:fg named.transparent})

;; --- Signcolumn ---

;; Signs

(fn sign->category [{: sign_hl_group}]
  (if (vim.startswith sign_hl_group "GitSigns") :git
      ;; (and (= vim.v.relnum 0) (vim.startswith sign_hl_group :DiagnosticSign)) nil
      ;; Other
      :other))

;; Mostly usefule due to `(set loc val)` not returning the new value.
(macro get_with_default [loc default]
  "Gets LOC or sets it to DEFAULT (the latter lazily evaluated), returns LOC's 
  current value in any case. LOC shouldn't have side effects as it might be evaluted
  multiple times."
  `(or ,loc (do
              (set ,loc ,default)
              ,loc)))

(fn trim-sign [{: sign_text &as sign}]
  (set sign.sign_text (vim.trim sign_text))
  sign)

(fn sort-signs [signs]
  "Also trims"
  ;; TODO: buf correct?
  (let [res {}
        get-curr (fn [row cat]
                   (let [entry (get_with_default (. res row) {})
                         curr (get_with_default (. entry cat)
                                                {:priority -1 :sign nil})]
                     curr))]
    (each [_ [_id row _col {: priority &as details}] (ipairs signs)]
      (let [cat (sign->category details)]
        (when cat
          (let [curr (get-curr row cat)]
            (when (> priority curr.priority)
              (set curr.priority priority)
              (set curr.sign details))))))
    ;; Remove info needed only for aggregating.
    (collect [row entry (pairs res)]
      (values row (collect [cat {: sign} (pairs entry)]
                    (values cat sign))))))

(fn buffer-signs [?buf ?ns ?fst ?lst]
  "Defaults to whole current buffer and all namespaces."
  (vim.api.nvim_buf_get_extmarks (or ?buf 0) (or ?ns -1) [(or ?fst 0) 0]
                                 [(or ?lst -1) -1] {:details true :type :sign}))

;; (vim.print (buffer-signs))

(fn line-signs [lnum]
  (buffer-signs nil nil lnum lnum))

;; (let [signs (buffer-signs)]
;;   (each [_ [_id _row _col details] (ipairs signs)]
;;     (trim-sign details))
;;   (vim.print (sort-signs signs)))

;; Line Numbers

(fn line-count [win]
  (-> win
      (vim.api.nvim_win_get_buf)
      (vim.api.nvim_buf_line_count)))

(fn buf-lnum-width [win]
  (-> win
      (line-count)
      (tostring)
      (length)
      (math.max 3)))

(local NO-BREAK-SPC " ")

(fn resolve-lnum [win]
  "Decides which number to show for the current line, respecting options like
  `number` and `relativenumber`."
  (let [num? (. vim.wo win :number)
        relnum? (. vim.wo win :relativenumber)]
    (if (and (or num? relnum?) (< 0 vim.v.virtnum))
        ;; NOTE: Space or empty string might be truncated by nvim.
        ;; NO-BREAK-SPC
        ""
        (and relnum? (not (and num? (= 0 vim.v.relnum))))
        (tostring vim.v.relnum)
        num?
        (tostring vim.v.lnum)
        nil)))

(fn pad [str size]
  (let [len (length str)
        diff (- size len)]
    ;; Regular space might be realigned by neovim
    (.. (string.rep NO-BREAK-SPC diff) str)))

(fn line-nr []
  (let [win (vim.api.nvim_get_current_win)
        lnum (resolve-lnum win)]
    (pad lnum (buf-lnum-width win))
    ;; lnum
    ))

;; (fn sign->component [{: sign_text : sign_hl_group}]
;;   {:provider sign_text :hl sign_hl_group})

(fn sign-comp [cat ?opts]
  (let [{: fallback : trim} (or ?opts {})]
    {:provider #(let [text (?. $1 :signs cat :sign_text)
                      text (if (and trim text) (vim.trim text) text)]
                  (or text fallback ""))
     :hl #(?. $1 :signs cat :sign_hl_group)}))

(fn hl-line-nr [{: pad-left : pad-right : pad-lnum : hl}]
  (let [;; Inject default options
        pad-lnum (or pad-lnum true)
        init (fn [self]
               (let [win (vim.api.nvim_get_current_win)
                     lnum (resolve-lnum win)]
                 (set self.lnum (if (and pad-lnum lnum)
                                    (pad lnum (buf-lnum-width win))
                                    lnum))))]
    {: init
     : hl
     1 [{:provider #(when (and pad-left $1.lnum) :▌)
         :hl {:fg named.transparent}}
        {:provider #$1.lnum}
        {:provider #(when (and pad-right $1.lnum) :▐)
         :hl {:fg named.transparent}}]}))

;; (local hl-line-nr (let []
;;                      {:init (fn [self]
;;                               (let [win (vim.api.nvim_get_current_win)
;;                                     lnum (resolve-lnum win)
;;                                     lnum (-?> lnum
;;                                               (pad (buf-lnum-width win)))]
;;                                 (set self.lnum lnum)))
;;                       1 [{:provider #(when $1.lnum :▌)
;;                           :hl {:fg named.transparent}}
;;                          {:provider #$1.lnum}
;;                          {:provider #(when $1.lnum :▐)
;;                           :hl {:fg named.transparent}}]}))

(local cfg {:git_as_left_lnum_padding true})

(local statuscolumn {:condition #(not= :no vim.wo.signcolumn)
                     :init (fn [self]
                             (let [;; nvim api uses 0-based line counts here
                                   lnum (- vim.v.lnum 1)]
                               (set self.signs
                                    (-> lnum
                                        (line-signs)
                                        (sort-signs)
                                        (. lnum)))))
                     1 (let [nr-bg nil]
                         [;; Only visible when `foldcolumn` is set.
                          {:provider :%C}
                          (sign-comp :other {:trim true :fallback NO-BREAK-SPC})
                          ;; NOTE: This causes neovim to pad until some
                          ;; internal minimum width, which can be too high if
                          ;; not using `signcolumn=number`. We also pad
                          ;; with non-breaking spaces above for this.
                          HL-ALIGN-MARK
                          (if cfg.git_as_left_lnum_padding
                              [;; Replace left-padding of line-nr with gitsigns
                               (sign-comp :git {:trim true})
                               (hl-line-nr {:pad-right true
                                            :pad-lnum false
                                            :hl {:bg nr-bg}})]
                              [(sign-comp :git {:trim true})
                               (hl-line-nr {:pad-left true
                                            :pad-right true
                                            :pad-lnum true
                                            :hl {:bg nr-bg}})])
                          ;; remove our padding.
                          ;; {:provider " "}
                          ;; {:provider "0"}
                          ;; {:provider " " :hl {:bg nr-bg}}
                          ;; { :hl {:bg nil}
                          ;;  1 (sign-comp :git)}
                          ])})

;; --- Setup ---

;; Setup heirline
(setup-heirline {: statusline
                 : winbar
                 : tabline
                 ;; Needs `signcolumn=number` to not have neovim enforce its miminum width.
                 : statuscolumn
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
