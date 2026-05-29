(local {: buf-opts!
        : put!
        : command!
        : get-cwd-override
        : reload
        : opts!
        : use!
        : keymaps!
        : autocmd!
        : feed!} (require :utils))

(local {: spawn-capture-output : spawn} (require :utils.async))

(local {: mk-op!} (require :utils.operator))
(local {: get-named : mix} (require :utils.colors))
(import-macros {: with-saved : with-cleanup : input!} :utils.macros)

(local colors (get-named))

(local hotpot (let [api (require :hotpot.api)]
                (api.context (vim.fn.stdpath :config))))

;; General Options and Keymaps

(opts! {;; Used for CursorHold autocmd
        :updatetime 1000
        ;; Enable dialogs
        :confirm true
        ;; Needed for hl-CursorLineNr
        :cursorline true
        ;; Ignore case for lowercase searches
        :ignorecase true
        ;; Search case-sensitive as soon as mixed casing is present; requires `ignorecase`
        :smartcase true
        ;; Live preview in split of :s/[...] invocations
        :inccommand :split
        ;; Enable mouse support
        :mouse :a
        ;; Show line number of current line, relative line numbers for the rest
        :number true
        :relativenumber true
        ;; Show cursor coordinates in status bar
        :ruler true
        ;; Set split behaviour
        :splitbelow true
        :splitright true
        ;; Ignore case for wildcards expansion
        :wildignorecase true
        ;; First complete longest substring and open wildmenu, then cycle through matches
        :wildmode "longest:full,full"
        ;; Undo behaviour
        :undofile true
        :undodir (.. (vim.fn.stdpath :data) :/undo)
        ;; Wrap behaviour
        :breakindent true
        :wrap true
        ;; Default tab behaviour
        :shiftwidth 4
        :tabstop 4
        :expandtab true
        :conceallevel 3
        ;; Start with folds expanded
        :foldlevelstart 99})

(set vim.g.editorconfig true)

;; set leader keys
;; TODO: <space>x as LL and try <space>c for buffer local operations? Or not useful?
(local L " ")
(local LL " c")
(set vim.g.mapleader L)
(set vim.g.maplocalleader LL)

;; Aliases
;; (keymaps! {:n {"<CR>" "<C-]>"}} {:remap true})
(keymaps! {:n {L {:w {:desc "Window Commands" :callback "<C-w>" :remap true}}}})

;; Insert mode keymaps
(keymaps! {:i {;; FIXME: doesn't work inside tmux
               :<C-CR> {:desc "Break line after cursor"
                        :callback "<C-o>mz<CR><ESC>`za"}
               ;; Correctly indent when pasting multiple lines in insert mode
               :<C-r> {:desc "Paste (auto-indent)" :callback "<C-r><C-o>"}
               ;; Capitalize word in front of cursor
               :<C-u> {:desc "Capitalize Word" :callback "<Esc>viwUea"}}})

;; Select until end of line (like `C`, `D` and `Y`)
(keymaps! {:n {L {:v {:desc "Select to End of Line" :callback :vg_}}}})

;; [O]pen things
(keymaps! {:n {L {:o {:a {:desc "Open Alternate File" :callback "<C-^>"}
                      :q {:desc "Open Quickfix List" :callback vim.cmd.copen}
                      :l {:desc "Open Location List" :callback vim.cmd.lopen}
                      ;; <leader>k more consistent with dap symbol info?
                      ;; :i {:desc "Open Symbol Info"
                      ;;     :callback #(vim.lsp.buf.hover)}
                      :d {:desc "Open Diagnostic"
                          :callback #(vim.diagnostic.open_float)}
                      ;; Global targets under <C-*> prefix
                      :<C-v> {:desc "Open Vim Config File"
                              :callback #(vim.cmd.edit (.. vim.env.HOME
                                                           "/.config/nvim/fnl/main.fnl"))}
                      :<C-l> {:desc "Open Lsp Logs"
                              :callback #(vim.cmd.edit (vim.lsp.log.get_filename))}
                      :<C-f> {:desc "Open Fennel Cache"
                              :callback #(let [{: destination} (hotpot.metadata)]
                                           (vim.cmd.edit destination))}
                      :<C-r> {:desc "Open Vim Runtime"
                              :callback #(vim.cmd.edit vim.env.VIMRUNTIME)}
                      :<C-p> {:desc "Open Plugin Files"
                              :callback #(vim.cmd.edit (.. vim.env.HOME
                                                           "/.local/share/nvim/site/pack"))}
                      :<C-n> {:desc "Open Notes"
                              :callback #(let [notes-dir (.. vim.env.HOME
                                                             :/Notes)]
                                           (vim.cmd.cd notes-dir)
                                           (vim.cmd.edit :index.md))}}}}})

;; [Q]uit things
(keymaps! {:n {L {:q {:V {:desc "Quit NeoVim" :callback ":<C-u>qall<CR>"}
                      :w {:desc "Close Window" :callback ":q<CR>"}
                      :q {:desc "Close Quickfix List" :callback vim.cmd.cclose}
                      :l {:desc "Close Location List" :callback vim.cmd.lclose}
                      :p {:desc "Close Preview" :callback "<C-w>z"}}}}})

;; File Operations
;; Not under leader to not accidentally trigger the tmux leader <C-space> when doing e.g. `<C-]><space>s`
(keymaps! {:n {:<C-s> {:desc "Save All Files" :callback ":<C-u>wall<CR>"}
               :g<C-s> {:desc "Save File" :callback ":<C-u>update<CR>"}}})

;; `sudo`-write trick with pkexec
(command! :WriteAsRoot "write !pkexec tee % >/dev/null")

(keymaps! {:n {;; :o {:desc "Open URL/file under cursor"
               ;;     :callback #(vim.cmd (.. "!xdg-open "
               ;;                             (vim.fn.shellescape (vim.fn.expand "<cfile>"))))}
               L {:R {:desc "Reload Config"
                      :callback #(do
                                   (vim.print "Reloading config")
                                   ;; TODO: Conflicts with vim.loader? Check docs
                                   (dofile vim.env.MYVIMRC))}}}
           [:v :n] {L {:y {:desc "Yank to Clipboard" :callback "\"+y"}
                       :p {:desc "Paste from Clipboard" :callback "\"+p"}
                       :P {:desc "Paste from Clipboard before cursor"
                           :callback "\"+P"}}}})

;; Navigation

;; Leap with s/gs
(use! :https://codeberg.org/andyg/leap.nvim
      {:setup {:leap {:safe_labels {}}}
       :keymaps {[:n :v] {:s {:desc "Jump in Buffer" :callback "<Plug>(leap)"}}
                 :n {:S {:desc "Jump to Other Buffer"
                         :callback "<Plug>(leap-from-window)"}}}})

(use! [:nvim-telescope/telescope.nvim
       :nvim-telescope/telescope-fzf-native.nvim
       :nvim-telescope/telescope-ui-select.nvim
       :nvim-lua/plenary.nvim]
      {:setup {:telescope #(let [actions (require :telescope.actions)]
                             {:defaults {:sorting_strategy :ascending
                                         :scroll_strategy :limit
                                         :layout_config {:prompt_position :top}
                                         :layout_strategy :flex
                                         :mappings {:i {"<C-j>" actions.preview_scrolling_down
                                                        "<C-k>" actions.preview_scrolling_up
                                                        "<C-l>" actions.preview_scrolling_right
                                                        "<C-h>" actions.preview_scrolling_left
                                                        "<C-q>" (fn [bufnr]
                                                                  (actions.smart_send_to_qflist bufnr)
                                                                  (vim.cmd.cfirst))
                                                        "<C-]>" actions.select_default
                                                        "<C-x>" actions.drop_all
                                                        "<C-a>" actions.select_all
                                                        "<C-d>" actions.results_scrolling_down
                                                        "<C-u>" actions.results_scrolling_up
                                                        "<C-s>" actions.select_horizontal
                                                        "<esc>" actions.close}}}
                              :extensions {:fzf {:fuzzy false
                                                 :override_generic_sorter true
                                                 :override_file_sorter true
                                                 :case_mode :smart_case}}
                              :pickers {:diagnostics {:sort_by :severity}
                                        :buffers {:mappings {:n {:<C-x> :delete_buffer}
                                                             :i {:<C-x> :delete_buffer}}}}})}
       :config #(let [{: load_extension} (require :telescope)
                      {: pick-term : ensure-fzf-native} (require :utils.telescope)
                      builtin (require :telescope.builtin)]
                  (load_extension :ui-select)
                  ;; fzf-native needs `make` to be run, vim.pack.add currently does not support this
                  (ensure-fzf-native)
                  (keymaps! {:n {L {:f {:desc "Open File"
                                        :callback #(builtin.find_files {:follow true
                                                                        :cwd (get-cwd-override)})}
                                    ";" {:desc "Resume Last Picker"
                                         :callback #(builtin.resume)}
                                    "/" {:desc "Grep Workspace"
                                         :callback #(builtin.live_grep {:cwd (get-cwd-override)})}
                                    :? {:desc "Grep Buffers"
                                        :callback #(builtin.live_grep {:grep_open_files true
                                                                       :cwd (get-cwd-override)})}
                                    :C {:desc "Incoming Calls (LSP)"
                                        :callback #(builtin.lsp_incoming_calls)}
                                    :O {:desc "Outcoming Calls (LSP)"
                                        :callback #(builtin.lsp_outgoing_calls)}
                                    :i {:desc "Document Symbols"
                                        :callback #(builtin.lsp_document_symbols)}
                                    :I {:desc "Workspace Symbols"
                                        :callback #(builtin.lsp_workspace_symbols)}
                                    ;; FIXME: use ]e/[e instead
                                    ;; :e {:desc "Buffer Errors"
                                    ;;     :callback #(builtin.diagnostics
                                    ;;                      {:bufnr 0
                                    ;;                       :severity vim.diagnostic.severity.ERROR
                                    ;;                       :prompt_title "Buffer Errors"})}
                                    ;; :E {:desc "Workspace Errors"
                                    ;;     :callback #(builtin.diagnostics
                                    ;;                      {:severity vim.diagnostic.severity.ERROR
                                    ;;                       :prompt_title "Workspace Errors"})}
                                    :o<C-h> {:desc "Help Tags"
                                             :callback #(builtin.help_tags)}
                                    "<BS>" {:desc "Pick Terminal"
                                            :callback pick-term}
                                    :b {:desc "Find Buffers"
                                        :callback #(builtin.buffers {:select_current true
                                                                     :sort_lastused true
                                                                     :sort_mru true})}
                                    :e {:desc "Buffer Diagnostics"
                                        :callback #(builtin.diagnostics {:bufnr 0})}
                                    :E {:desc "Workspace Diagnostics"
                                        :callback #(builtin.diagnostics)}
                                    :G {:desc "Git Status"
                                        :callback #(builtin.git_status)}}}
                             :v {L {:/ {:desc "Grep Selection in Workspace"
                                        :callback #(builtin.grep_string)}
                                    :? {:desc "Grep Selection in Buffers"
                                        :callback #(builtin.grep_string {:grep_open_files true})}}}}))})

;; Navigate history containing substring
(keymaps! {:c {:<M-p> {:desc "Previous History" :callback #(feed! :<Up>)}
               :<M-n> {:desc "Next History" :callback #(feed! :<Down>)}}})

(keymaps! {:n (let [{: next : prev} (require :utils.goto-lens)]
                {"[a" {:desc "Previous Code Lens" :callback prev}
                 "]a" {:desc "Next Code Lens" :callback next}})})

(keymaps! {:n {;; Buffer navigation
               "]b" {:desc "Next Buffer" :callback ":<C-u>bnext<CR>"}
               "[b" {:desc "Previous Buffer" :callback ":<C-u>bprev<CR>"}
               ;; Quickfix list
               "]q" {:desc "Next Quickfix" :callback ":<C-u>cnext<CR>"}
               "[q" {:desc "Previous Quickfix" :callback ":<C-u>cprev<CR>"}
               "]Q" {:desc "Last Quickfix" :callback ":<C-u>clast<CR>"}
               "[Q" {:desc "First Quickfix" :callback ":<C-u>cfirst<CR>"}
               ;; Location list
               "[l" {:desc "Previous Location" :callback ":<C-u>lprev<CR>"}
               "]l" {:desc "Next Location" :callback ":<C-u>lnext<CR>"}
               "[L" {:desc "First Location" :callback ":<C-u>lfirst<CR>"}
               "]L" {:desc "Last Location" :callback ":<C-u>llast<CR>"}
               ;; Errors
               ;; TODO
               "[e" {:desc "Go to previous error"
                     :callback #(vim.diagnostic.jump {:count -1
                                                      :severity vim.diagnostic.severity.ERROR})}
               "]e" {:desc "Go to next error"
                     :callback #(vim.diagnostic.jump {:count 1
                                                      :severity vim.diagnostic.severity.ERROR})}}})

;; Navigate tabs and windows
(let [keys (let [res {}]
             (for [i 1 9]
               (set (. res (tostring i)) i))
             res)]
  (keymaps! {:n {L {;; <n>: go to window <n>
                    "" (collect [as_str _ (pairs keys)]
                         (values as_str
                                 {:desc (.. "Focus Window " as_str)
                                  :callback (.. as_str "<C-w>w")}))
                    ;; q+<n>: delete window <n>
                    :q (collect [as_str n (pairs keys)]
                         (values as_str
                                 {:desc (.. "Kill Window " as_str)
                                  :callback #(vim.api.nvim_win_close (vim.fn.win_getid n)
                                                                     false)}))}
                 ;; Alt+<n> to go to tab <n>
                 "" (collect [as_str _ (pairs keys)]
                      (values (.. "<M-" as_str ">")
                              {:desc (.. "Focus Tab " as_str)
                               :callback (.. as_str "gt")}))}
             ;; Same in insert mode
             :i (collect [as_str _ (pairs keys)]
                  (values (.. "<M-" as_str ">")
                          {:desc (.. "Focus Tab " as_str)
                           :callback (.. "<esc>" as_str "gt")}))
             ;; Same in terminal mode
             :t (collect [as_str _ (pairs keys)]
                  (values (.. "<M-" as_str ">")
                          {:desc (.. "Focus Tab " as_str)
                           :callback (.. "<C-\\><C-n>" as_str "gt")}))}))

(keymaps! {[:n :v] {"<C-j>" {:desc "Down (Display Line)" :callback "gj"}
                    "<C-k>" {:desc "Up (Display Line)" :callback "gk"}}})

(use! :tpope/vim-repeat)

;; Fixes things like not highlighting parens inside comments.
;; (use! :monkoose/matchparen.nvim
;;       {;; Disable builtin plugin we replace
;;        :init #(set vim.g.loaded_matchparen 1)
;;        :setup {:matchparen {}}})

;; TODO:
(fn selection-forward? []
  (let [[cl cc] (vim.api.nvim_win_get_cursor 0)
        [bl bc] (vim.api.nvim_buf_get_mark 0 "<")]
    (and (= cl bl) (= cc bc))))

(do
  (fn ensure-forward [?forward]
    (let [cursor-is-ahead (selection-forward?)]
      (when (if (= ?forward false)
                cursor-is-ahead
                (not cursor-is-ahead))
        (vim.api.nvim_feedkeys "o" "x" false))))
  (keymaps! {:v {:<Plug> {"(ensure-backward)" #(ensure-forward true)
                          "(ensure-forward)" #(ensure-forward)}}}))

;; In particular replaces builtin matchpair plugin to support match-highlighting in comments.
(use! :andymass/vim-matchup
      {:init (fn []
               ;; Don't override <N>% movement.
               (set vim.g.matchup_motion_override_Npercent 0)
               ;; Don't replace the statusline if matching pair is offscreen.
               (set vim.g.matchup_matchparen_offscreen {}))
       :setup {:match-up {:treesitter {:stopline 500}}}
       :hl {:MatchWord {:link :VisualNOS}}
       :keymaps {:opts {:remap true}
                 :n {;; CTRL-L-default
                     ;; FIXME: good?
                     :<Esc> {:callback "<Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-L><CR>"
                             :remap false}
                     :<M-h> "<Plug>(matchup-[%)v%o"
                     :<M-l> "<Plug>(matchup-]%)v%o"
                     :<M-k> "v<M-k>"
                     :<M-j> "z%<M-k>"
                     :<C-k> "<Plug>(matchup-Z%)"
                     :<C-j> "<Plug>(matchup-z%)"
                     :<C-h> "<Plug>(matchup-[%)"
                     :<C-l> "<Plug>(matchup-]%)"}
                 :x {:<M-l> "a%<Plug>(ensure-forward)"
                     :<M-j> "<Esc><M-j>"
                     :<M-h> "a%<Plug>(ensure-backward)"
                     :<M-k> "i%<Plug>(ensure-backward)"
                     :z% false}}})

(use! :windwp/nvim-autopairs
      {:setup {:nvim-autopairs {:enable_check_bracket_line false
                                ;; First three are the defaults, the last fixes <C-w> in vim.ui.input prompts.
                                :disable_filetype [:TelescopePrompt
                                                   :spectre_panel
                                                   :snacks_picker_input
                                                   :snacks_input]
                                :map_c_h true
                                :map_c_w true}}})

(use! :kylechui/nvim-surround
      {:init #(set vim.g.nvim_surround_no_insert_mappings true)
       ;; TODO: Check if still relevant with autopairs
       :keymaps {:i {:<C-s> "<Plug>(nvim-surround-insert)"
                     :<C-s><C-s> "<Plug>(nvim-surround-insert-line)"}}})

(use! [:mbbill/undotree]
      {:keymaps {:n {L {:tu {:desc "Toggle Undo Tree"
                             :callback (fn []
                                         (vim.cmd.UndotreeToggle)
                                         (vim.cmd.UndotreeFocus))}}}}})

;; Floating preview in quickfix window
(use! :kevinhwang91/nvim-bqf
      {:hl {:BqfPreviewBorder {:bg :NONE :fg colors.mid}}
       :setup {:bqf {:func_map {:fzffilter "" :open "<C-]>"}
                     :preview {:winblend 0 :border :rounded}}}})

;; (keymaps! {:n {l {:m {:k {:desc "Make" :callback ":make!<CR>"}
;;                             :f {:desc "Make Flash"
;;                                 :callback ":make! flash<CR>"}
;;                             :c {:desc "Make Clean"
;;                                 :callback ":make! clean<CR>"}
;;                             :t {:desc "Make Test" :callback ":make! test<CR>"}
;;                             :b {:desc "Make Build"
;;                                 :callback ":make! build<CR>"}}}}})

;; --- Plugin Collections ---

(use! [:nvim-mini/mini.nvim]
      {:setup {:mini.align {}
               :mini.bracketed #(let [base-config ;; Disables everything by default
                                      (collect [target opts (pairs (. (require :mini.bracketed)
                                                                      :config))]
                                        (when opts.suffix
                                          (values target {:suffix ""})))
                                      config {:yank {:suffix :y}
                                              :conflict {:suffix :x}
                                              :oldfile {:suffix :o
                                                        :options {:wrap false}}}]
                                  (vim.tbl_deep_extend :force base-config
                                                       config))
               :mini.files {:options {:use_as_default_file_explorer false}}}
       :keymaps {:n {"_" {:desc "Open Dir (MiniFiles)"
                          :callback #(_G.MiniFiles.open (vim.api.nvim_buf_get_name 0)
                                                        false)}
                     ;; mini.files -> oil.nvim
                     :- {:ft :minifiles
                         :desc "Open Dir in Buffer"
                         :callback #(-?> (get-cwd-override) (vim.cmd.edit))}}}
       :hl {:MiniFilesBorderModified {:extend :MiniFilesBorder
                                      :fg colors.orange}}})

;; Doesn't wrap compared to the default bindings and prints an error on wrap-failure unlike mini.bracketed ones.
(keymaps! {:n (let [jump (fn [count]
                           (vim.diagnostic.jump {: count :wrap false}))]
                {"]d" {:desc "Next Diagnostic" :callback #(jump vim.v.count1)}
                 "[d" {:desc "Previous Diagnostic"
                       :callback #(jump (- vim.v.count1))}
                 "]D" {:desc "Last Diagnostic" :callback #(jump math.huge)}
                 "[D" {:desc "First Diagnostic"
                       :callback #(jump (- math.huge))}})})

(use! [:folke/snacks.nvim]
      {;; Force resetup of snacks.nvim on reload
       :init #(-?> _G
                   (. :package)
                   (. :loaded)
                   (. :snacks)
                   (tset :did_setup false))
       :setup {:snacks {:animate {:fps 120}
                        :bigfile {}
                        :image {;; Rendering of latex inline
                                :math {:enabled false}}
                        :input {:win {:border :rounded
                                      :keys {:i_esc {1 :<esc>
                                                     2 :cancel
                                                     :mode :i}}}}
                        :dim {:animate {:enabled false}}
                        ;; :statuscolumn {:enabled true
                        ;;                ;; :left [:mark]
                        ;;                ;; :left []
                        ;;                ;; :right [:git :fold]
                        ;;                }
                        :indent {:enabled false}
                        ;; :words {}
                        ;; :notifier {}
                        ;; TODO:
                        :toggle {:notify true}
                        :scroll {:animate {:duration {:total 150}}}
                        :zen {:zoom {:show {:tabline false :statusline false}}}}}
       :config (fn []
                 ;; Toggle things
                 (let [{: toggle} (require :snacks)
                       {: lenses : diag-curr : diag-lines} (require :utils.toggle)]
                   (-> {:ui (toggle.indent)
                        :uh (toggle.inlay_hints)
                        ;; :m (toggle.image)
                        :tz (toggle.zen)
                        :t- (toggle.dim)
                        ;; :D (toggle.diagnostics)
                        :ul (toggle.new diag-lines)
                        :uL (toggle.new diag-curr)
                        :uA (toggle.new lenses)
                        :ur (toggle.words)
                        :t+ (toggle.zoom)
                        :ur (toggle.option :relativenumber
                                           {:name "Relative Numbers"})
                        :un (toggle.option :number {:name "Line Numbers"})
                        :uw (toggle.option :wrap {:name "Wrap"})
                        :us (toggle.option :spell {:name "Spellcheck"})}
                       (vim.iter)
                       (: :each #($2:map (.. L $1))))))
       :keymaps #(let [{: scratch : notifier : bufdelete} (require :snacks)]
                   {:n {L {:u {:c #(vim.cmd.ToggleComments)}
                           "qb" {:desc "Kill buffer"
                                 :callback bufdelete.delete}
                           :- {:desc "Toggle scratch buffer"
                               :callback scratch.open}
                           ;; FIXME: borders/contents
                           :o<C-m> {:desc "Notification History"
                                    :callback notifier.show_history}}}})})

;; File Explorer

(use! {:src :stevearc/oil.nvim :version (vim.version.range :v2.*)}
      {:setup {:oil {:columns []
                     :use_default_keymaps false
                     :keymaps {:g? :actions.show_help
                               "<C-]>" :actions.select
                               "<CR>" :actions.select
                               ;; FIXME: <C-s> saves now
                               ;; :<C-s>v :actions.select_vsplit
                               ;; :<C-s>s :actions.select_split
                               "gs" :actions.change_sort
                               :gp :actions.preview
                               :<C-p> :actions.close
                               :gf :actions.refresh
                               :- :actions.parent
                               :g- :actions.open_cwd
                               "`" :actions.cd
                               "~" :actions.tcd
                               :g. :actions.toggle_hidden
                               :<C-c> "<C-^>"}}}
       :keymaps #(let [{: open} (require :oil)]
                   {:n {:- {:desc "Open File Explorer" :callback open}}})})

;; Terminal

(autocmd! {:event :TermOpen
           :callback (fn [{: buf}]
                       ;; (set vim.wo.number false)
                       ;; (set vim.wo.relativenumber false)
                       ;; FIXME: use when having separate picker
                       ;; (set vim.bo.buflisted false)
                       ;; Don't insert here as this doesn't have to be an active buffer
                       (keymaps! {:opts {:buffer buf}
                                  :n {;; Go to shell prompts
                                      "[s" {:desc "Previous Shell Prompt"
                                            :callback "[["}
                                      "]s" {:desc "Next Shell Prompt"
                                            :callback "]]"}
                                      ;; "Forward" some keys directly in terminal mode
                                      "" (let [keys ["<CR>"
                                                     "<C-c>"
                                                     "<C-n>"
                                                     "<C-p>"]]
                                           (collect [_ key (ipairs keys)]
                                             (values key (.. "i" key))))}}))}
          {:event :TermClose
           :callback (fn [{: buf}]
                       (when (vim.api.nvim_buf_is_valid buf)
                         ;; Autoclose terminal buffers on clean exit; `default-autocmd` does something similar for simple shell buffers, but always closes the window in the process.
                         ;; We keep this functionality as things like `:Cargo` rely on the auto-closing behavior (which is why we check for buffer validity) above but reimplement it for other buffers in a more granular way, not necessarily deleting the window.
                         (when (= 0 vim.v.event.status)
                           (case (. vim.b buf :term_autoclose)
                             :window (vim.api.nvim_buf_delete buf {:force true})
                             :buffer (_G.Snacks.bufdelete.delete buf)
                             nil nil
                             other (error (.. "value unknown: " other))))))}
          {:event [:BufWinEnter :BufEnter]
           :callback ;; Avoids triggering wrongly as startinsert will only happen after a sequence of commands and also catches more cases (for some reason).
           (vim.schedule_wrap #(when (and (or (= vim.bo.buftype
                                                           :terminal)
                                                        (= vim.bo.buftype
                                                           :prompt))
                                                    (-?> (vim.api.nvim_get_mode)
                                                         (. :mode)
                                                         (vim.startswith :n))
                                                    ;; NOTE: Don't do that in telescope prompts as this inserts a literal "A" in there
                                                    (not= vim.bo.filetype
                                                          :TelescopePrompt))
                                           (vim.cmd :startinsert!)))})

(fn open-term [{: autoclose}]
  (let [cwd (get-cwd-override)
        buf (vim.api.nvim_create_buf false true)]
    (vim.api.nvim_set_current_buf buf)
    (tset vim.b buf :term_autoclose autoclose)
    (vim.fn.jobstart [:/usr/bin/fish] {:term true : cwd})))

;; Terminal keymaps
(keymaps! {:t {:<Esc> {:desc "Exit Terminal Mode" :callback "<C-\\><C-n>"}
               ["<C-v><C-[>" :<C-v><Esc>] {:desc "Send Escape to Terminal"
                                           :callback :<Esc>}}})

(keymaps! {:t {[:<Esc> "<C-[>"] {:desc "Exit Terminal Mode"
                                 :callback "<C-\\><C-n>"}}
           :n {;; Open terminal
               "<BS>" {:desc "Open Terminal"
                       :callback #(open-term {:autoclose :buffer})}
               "<C-w>" {"V" {:desc "Terminal in Vertical Split"
                             :callback #(do
                                          (vim.cmd.vsplit)
                                          (open-term {:autoclose :window}))}
                        "S" {:desc "Terminal in Horizontal Split"
                             :callback #(do
                                          (vim.cmd.split)
                                          (open-term {:autoclose :window}))}}}})

;; Flattens files opened in terminal into current instance
(use! "willothy/flatten.nvim" {:setup {:flatten {}}})

;; --- Autocompletion & Snippets ---

(use! [{:src :saghen/blink.cmp :version (vim.version.range "v1.*")}
       :rafamadriz/friendly-snippets
       :chrisgrieser/nvim-scissors]
      {:setup {:scissors {:snippetDir (.. (vim.fn.stdpath :config) "/snippets")}
               :blink.cmp {:completion {:menu {:border :none}
                                        :documentation {:auto_show true
                                                        :auto_show_delay_ms 200
                                                        :window {:border :single
                                                                 ;; NOTE: For some reason, using a `solid` border with a scrollbar will render the scrollbar outside the completion window. So as workaround, use a single border and set the border fg to the bg.
                                                                 :scrollbar true}}
                                        ;; Don't preselect first item for easier inserting-without-accepting
                                        :list {:selection {:preselect false}}}
                           :cmdline {:enabled true
                                     :keymap {;;:preset :inherit
                                              ;; TODO:
                                              ;; "<C-n>" false
                                              ;; "<C-p>" false
                                              ;; "<Tab>" [:show_and_insert_or_accept_single :select_next]
                                              ;; "<Tab>" [:select_next :show]
                                              ;; "<S-Tab>" [:select_prev
                                              ;;            :fallback]
                                              }
                                     :completion {:menu {:auto_show true}
                                                  :list {:selection {:preselect false
                                                                     :auto_insert true}}}}
                           :fuzzy {:sorts [:exact :score :sort_text]}
                           :keymap {;; Conflicts with tmux leader
                                    "<C-space>" false
                                    ;; Doesn't remove insertion
                                    "<C-e>" [:hide :fallback]
                                    ;; Does remove insertion
                                    "<C-c>" [:cancel :fallback]
                                    "<C-n>" [:select_next :show]
                                    "<C-p>" [:select_prev :fallback]
                                    "<C-l>" [:select_and_accept :fallback]}
                           ;; Experimental
                           :signature {:enabled true
                                       :trigger {;; Don't autoshow
                                                 :enabled false}
                                       ;; :window {:border :none}
                                       }}}
       :hl {:BlinkCmpLabelMatch {:link ;; Linking to `Normal` doesn't work for whatever reason
                                 :PmenuMatch}
            :BlinkCmpScrollBarThumb {:link :PmenuThumb}
            ;; :BlinkCmpScrollBarGutter {:link :Pmenu}
            :BlinkCmpDoc {:link :PmenuDoc}
            :BlinkCmpDocBorder {:link :PmenuDocBorder}}})

;; --- Git ---

(use! :lewis6991/gitsigns.nvim
      (let [;; NOTE: For right-aligned bars, use "▊" with {:reverse true}
            ;; in the hl-groups.
            ;; thinner than defaults
            text "│"]
        {:setup {:gitsigns {:signs {:add {: text}
                                    :change {: text}
                                    ;; :delete {: text}
                                    ;; :untracked {:text "┆"}
                                    }
                            :signs_staged_enable true
                            :signs_staged {:add {:text "+"}
                                           :change {:text "~"}
                                           :delete {:text "-"}
                                           ;; :untracked {:text "┆"}
                                           }
                            :current_line_blame_opts {:delay 0}
                            :signcolumn false
                            :word_diff false
                            :linehl false}}
         :hl #(let [;; For word_diff
                    for-bg #(mix $1 colors.dark_bg0 0.1)
                    ;; For in-bufer indications
                    for-bg-dim #(mix $1 colors.dark_bg0 0.015)
                    ;; E.g. for signs/numhl
                    for-fg #(mix $1 colors.dark_bg0 0.1)]
                {;; Used in signcolumn
                 :GitSignsAdd {:fg (for-fg colors.green)}
                 :GitSignsChange {:fg (for-fg colors.yellow)}
                 :GitSignsDelete {:fg (for-fg colors.red)}
                 :GitSignsStagedAdd {:link :GitSignsAdd}
                 :GitSignsStagedChange {:link :GitSignsChange}
                 :GitSignsStagedDelete {:link :GitSignsDelete}
                 ;; Used for linehl (FIXME: change in colorscheme instead? or sort out overrides)
                 :DiffAdd {:bg (for-bg-dim colors.green)}
                 :DiffChange {:bg (for-bg-dim colors.yellow)}
                 :DiffDelete {:bg (for-bg-dim colors.red)}
                 ;; Used for word_diff
                 :GitSignsAddInline {:bg (for-bg colors.green)}
                 :GitSignsChangeInline {:bg (for-bg colors.yellow)}
                 :GitSignsDeleteInline {:bg (for-bg colors.red)}})
         :keymaps {[:n :v] {"]g" {:desc "Next Git Hunk"
                                  :callback #(vim.cmd.Gitsigns :nav_hunk :next)}
                            "[g" {:desc "Previous Git Hunk"
                                  :callback #(vim.cmd.Gitsigns :nav_hunk :prev)}}
                   [:o :x] {:ih {:desc "Select Git Hunk"
                                 :callback #(vim.cmd.Gitsigns :select_hunk)}}
                   :n (let [;; Unsure about e.g. "<leader>gu" vs.
                            ;; "<leader>ug" yet, so factored out for easier
                            ;; changing.
                            ui {:s {:desc "Toggle Git Signcolumn"
                                    :callback #(vim.cmd.Gitsigns :toggle_signs)}
                                :n {:desc "Toggle Git NumHL"
                                    :callback #(vim.cmd.Gitsigns :toggle_numhl)}
                                ;; NOTE: No usecase?
                                :l {:desc "Toggle Line Highlights"
                                    :callback #(vim.cmd.Gitsigns :toggle_linehl)}
                                :w {:desc "Toggle Git Word Diff"
                                    :callback #(vim.cmd.Gitsigns :toggle_word_diff)}
                                :b {:desc "Toggle Git Line Blame"
                                    :callback #(vim.cmd.Gitsigns :toggle_current_line_blame)}}
                            open {:B {:desc "Open Git Blame (Buffer)"
                                      :callback #(vim.cmd.Gitsigns :blame)}
                                  :b {:desc "Open Git Blame (Hover)"
                                      :callback #(vim.cmd.Gitsigns :blame_line)}
                                  :h {:desc "Open Hunk Preview"
                                      :callback #(vim.cmd.Gitsigns :preview_hunk)}}]
                        {L {:g {:u ui
                                :o open
                                ;; Operations
                                :a {:desc "Stage Hunk"
                                    :callback #(vim.cmd.Gitsigns :stage_hunk)}
                                :<BS> {:desc "Undo Stage Hunk"
                                       :callback #(vim.cmd.Gitsigns :undo_stage_hunk)}
                                :RRR {:desc "Reset Hunk"
                                      :callback #(vim.cmd.Gitsigns :reset_hunk)}
                                :h {:desc "Preview Hunk Inline"
                                    :callback #(vim.cmd.Gitsigns :preview_hunk_inline)}}}})
                   :v {L {:g {:a {:desc "Stage Hunk"
                                  :callback #(vim.cmd.Gitsigns :stage_hunk)}
                              :RRR {:desc "Reset Hunk"
                                    :callback #(vim.cmd.Gitsigns :reset_hunk)}}}}}}))

;; LSP & Co.

(vim.diagnostic.config {:virtual_lines false
                        :virtual_text {:current_line true}
                        :signs {:text (let [sym "›" ;; sym ""
                                            ]
                                        {vim.diagnostic.severity.ERROR sym
                                         vim.diagnostic.severity.WARN sym
                                         vim.diagnostic.severity.INFO sym
                                         vim.diagnostic.severity.HINT sym})}})

(keymaps! {:n {;; :K #(vim.lsp.buf.hover {:border :none})
               L {:a {:desc "Code Action" :callback :<Plug>lsp#code-action}
                  :A {:desc "Code Lenses" :callback vim.lsp.codelens.run}
                  :r {:n {:desc "Rename Symbol" :callback vim.lsp.buf.rename}
                      :f {:desc "Format File"
                          :callback :<Plug>edit#format-file}}
                  :l {:d {:desc "Goto Definition" :callback "<C-]>"}
                      :D {:desc "Goto Declaration"
                          :callback vim.lsp.buf.declaration}
                      :t {:desc "Goto Type Definition"
                          :callback vim.lsp.buf.type_definition}
                      :i {:desc "Goto Implementation"
                          :callback vim.lsp.buf.implementation}
                      :c {:desc "Incoming Calls"
                          :callback vim.lsp.buf.incoming_calls}
                      :o {:desc "Outgoing Calls"
                          :callback vim.lsp.buf.outgoing_calls}
                      :r {:desc "Goto References"
                          :callback vim.lsp.buf.references}}}
               ;; Some indirection to allow buffer local overrides of certain actions
               :<Plug> {:edit#format-file vim.lsp.buf.format
                        :lsp#code-action vim.lsp.buf.code_action}}})

(use! [:neovim/nvim-lspconfig]
      {:autocmds {:LspAttach {:callback (fn [{: buf}]
                                          ;; Initial displaying
                                          (_G.vim.lsp.codelens.enable true
                                                                      {:bufnr buf}))}}})

(let [ts-setup (fn [buf]
                 (let [{: start} (require :vim.treesitter)
                       {: get_lang : add} (require :vim.treesitter.language)
                       ft (. vim.bo buf :filetype)
                       lang (get_lang ft)]
                   (when (and lang (add lang))
                     (when (not= vim.wo.foldmethod :expr)
                       (set vim.wo.foldmethod :expr)
                       (set vim.wo.foldexpr "v:lua.vim.treesitter.foldexpr()"))
                     ;; (set (. vim.bo buf :indentexpr)
                     ;;      "v:lua.require'nvmi-treesitter'.indentexpr()")
                     ;; Enable treesitter
                     (start buf lang))))]
  (use! [{:src :nvim-treesitter/nvim-treesitter :version :main}
         :nvim-treesitter/nvim-treesitter-textobjects]
        {:autocmds {:FileType {:pattern :* :callback #(ts-setup $.buf)}
                    :BufReadPost {:pattern :* :callback #(ts-setup $.buf)}}
         :setup {:nvim-treesitter-textobjects {;; :select {:enable true
                                               ;;          :keymaps {:if "@function.inner"
                                               ;;                    :af "@function.outer"
                                               ;;                    :ic "@call.inner"
                                               ;;                    :ac "@call.outer"
                                               ;;                    :il "@loop.inner"
                                               ;;                    :al "@loop.outer"
                                               ;;                    :ik "@conditional.inner"
                                               ;;                    :ak "@conditional.outer"}}
                                               ;; :swap {:enable true
                                               ;;        :swap_next {"<leader>." "@parameter.inner"
                                               ;;                    :<Plug>edit#swap-param-next "@parameter.inner"}
                                               ;;        :swap_previous {"<leader>," "@parameter.inner"
                                               ;;                        :<Plug>edit#swap-param-prev "@parameter.inner"}}
                                               }}
         :keymaps #(let [{:select_textobject select_textobject} (require :nvim-treesitter-textobjects.select)
                         select (fn [query ?module]
                                  (select_textobject query
                                                     (or ?module :textobjects)))]
                     ;; TODO: continue
                     {[:x :o] {:af #(select "@function.outer")
                               :if #(select "@function.inner")}})}))

;; (use! [:aaronik/treewalker.nvim]
;;       {:setup {:treewalker {}}
;;        :keymaps {:n (let [base {:h :Left :j :Down :k :Up :l :Right}]
;;                   (collect [key dir (pairs base)]
;;                     (values (.. :<C- key :>) #(vim.cmd.Treewalker dir))))}})

;; Debugging

;; (use! [:mason-org/mason.nvim
;;        :jay-babu/mason-nvim-dap.nvim ]
;;       {:setup {:mason {}
;;                :mason-nvim-dap {}}})

(use! [:mfussenegger/nvim-dap
       ;; :nvim-neotest/nvim-nio
       ; needed by nvim-dap-ui
       {:src :igorlfs/nvim-dap-view :version (vim.version.range "1.*")}
       ;; :rcarriga/nvim-dap-ui
       :theHamsta/nvim-dap-virtual-text
       ;; :mfussenegger/nvim-dap-python
       ]
      {:setup {:nvim-dap-virtual-text {:virt_text_pos :inline}
               :dap-view {:winbar {:sections ["watches"
                                              "scopes"
                                              "exceptions"
                                              "breakpoints"
                                              "threads"
                                              "repl"
                                              "console"]}}
               ;; :dap-python :python
               }
       :reload [:dap-configs]
       :ft {:dap-repl #(vim.cmd "abbreviate <buffer> e -exec")
            :dap-float (fn [{: buf}]
                         (keymaps! {:opts {:buffer buf}
                                    :n {[:q :<ESC>] {:desc "Close Float"
                                                     :callback vim.cmd.quit}}}))}
       :keymaps #(let [dap (require :dap)
                       {: run_last} (require :utils.dap)
                       {: show_view :open open_view} (require :dap-view)
                       widgets (require :dap.ui.widgets)
                       ;; sidebar #(let [sidebar (->> $1
                       ;;                             (. widgets)
                       ;;                             (widgets.sidebar))]
                       ;;            #(sidebar.toggle))
                       ;; cfloat #(->> $1 (. widgets) (widgets.centered_float))
                       interactive-breakpoint #(input! [cond
                                                        {:prompt "Condition: "}
                                                        count
                                                        {:prompt "Count: "}
                                                        log
                                                        {:prompt "Log Message: "}]
                                                       (dap.toggle_breakpoint cond
                                                                              count
                                                                              log))]
                   {:v {L {:d {;; :k {:desc "Hover" :callback widgets.hover}
                               :e {:desc "Eval Expression"
                                   :callback #(-> (vim.fn.getregion (vim.fn.getpos ".")
                                                                    (vim.fn.getpos "v"))
                                                  (widgets.hover {}))}}}}
                    :n {"[f" {:desc "Up Stack Frame" :callback dap.up}
                        "]f" {:desc "Down Stack Frame" :callback dap.down}
                        L {:d {:<CR> {:desc "Run to Cursor"
                                      :callback #(dap.run_to_cursor)}
                               :<Tab> {:desc "Toggle Breakpoint"
                                       :callback dap.toggle_breakpoint}
                               :<S-Tab> {:desc "Toggle Breakpoint (Interactive)"
                                         :callback interactive-breakpoint}
                               :k {:desc "Eval Expression"
                                   :callback #(-> (vim.fn.expand :<cexpr>)
                                                  (widgets.hover {}))}
                               :K {:desc "Eval Expression (Interactive)"
                                   :callback #(input! [expr
                                                       {:prompt "Expr: "
                                                        :default (vim.fn.expand :<cexpr>)}]
                                                      (widgets.hover expr {}))}
                               :q {:desc "Terminate" :callback dap.terminate}
                               :c {:desc "Continue"
                                   :callback dap.continue
                                   :repeatable true}
                               :s {:desc "Step Into"
                                   :callback dap.step_into
                                   :repeatable true}
                               :p {:desc "Step Back"
                                   :callback dap.step_back
                                   :repeatable true}
                               :P {:desc "Reverse Continue"
                                   :callback dap.reverse_continue
                                   :repeatable true}
                               :n {:desc "Step Over"
                                   :callback dap.step_over
                                   :repeatable true}
                               :o {:desc "Step Out"
                                   :callback dap.step_out
                                   :repeatable true}
                               :r {:desc "Restart / Run Last"
                                   :callback #(if (dap.session)
                                                  (dap.restart)
                                                  (do
                                                    (vim.print "Rerunning last DAP session")
                                                    (run_last)))}
                               :<space> {:desc "Focus Frame"
                                         :callback dap.focus_frame}
                               ;; :l {:desc "List Breakpoints"
                               ;;     :callback (fn []
                               ;;                 (dap.list_breakpoints)
                               ;;                 (vim.cmd.copen))}
                               :H {:desc "Toggle Debug Virtual Text"
                                   :callback vim.cmd.DapVirtualTextToggle}
                               :N {:desc "New Session"
                                   :callback vim.cmd.DapNew
                                   :deprecated "handled by dap.continue"}
                               ;; :L {:desc "Dap Logs"
                               ;;     :callback vim.cmd.DapShowLog}
                               :D {:desc "Clear Breakpoints"
                                   :callback dap.clear_breakpoints}
                               "" (collect [key view (pairs {:B :breakpoints
                                                             :E :exceptions
                                                             :W :watches
                                                             :R :repl
                                                             :T :threads
                                                             :C :console
                                                             :S :scopes})]
                                    (values key
                                            {:callback (fn []
                                                         (open_view)
                                                         (show_view view))
                                             :desc (string.format "Open Dap-View: %s"
                                                                  view)
                                             ;; EXPERIMENTAL: should jump to the view after opening it
                                             :repeatable true}))}
                           :t {:d {:desc "Toggle DAP View"
                                   :callback #(vim.cmd :DapViewToggle!)}}}}})
       :signs {:DapBreakpoint {:text :B :texthl :DiagnosticHint}
               :DapBreakpointCondition {:text :C :texthl :DiagnosticWarn}
               :DapLogPoint {:text :L :texthl :DiagnosticInfo}
               :DapStopped {:text :→ :texthl :DiagnosticOk}
               :DapBreakpointRejected {:text :R :texthl :DiagnosticError}}})

;; Plugins for specific filetypes

(use! :elkowar/yuck.vim)

;; Treesitter indentation for fennel is messed up, so use this plugin for this
(use! :jaawerth/fennel.vim)

(use! :lervag/vimtex
      {:config (fn []
                 (set vim.g.vimtex_format_enabled 1)
                 (set vim.g.vimtex_quickfix_mode 0)
                 (set vim.g.vimtex_view_method :zathura)
                 (set vim.g.tex_flavor :latex))})

(use! [{:src :mrcjkb/rustaceanvim :version :524d8be}]
      {:init #(let [lsp-config {:rust-analyzer {:cargo {:buildScripts {:enable true}}
                                                :procMacro {:enable true}
                                                :imports {;; Merge auto imports on the module level
                                                          :granularity {:group :module}
                                                          ;; Prefer relative auto import paths
                                                          :prefix :self}}}
                    codelldb-path (.. vim.env.HOME "/.local/share/codelldb")
                    adapter {:executable {:command (.. codelldb-path
                                                       "/adapter/codelldb")
                                          :args ["--liblldb"
                                                 (.. codelldb-path
                                                     "lldb/lib/liblldb")
                                                 "--port"
                                                 "${port}"]}
                             :host "127.0.0.1"
                             :port "${port}"
                             :type :server}]
                ;; Avoids "Unrecognized option: 'write-mode'" error.
                ;; FIXME: see if it is valid here, was previously top-level
                (set vim.g.rustfmt_detect_version 1)
                (set vim.g.rustaceanvim
                     {:tools {:float_win_config {:border :none}}
                      :server {:default_settings lsp-config}
                      :dap {: adapter}}))
       :keymaps {:opts {:ft :rust}
                 :n {:<Plug> {:lsp#code-action {:callback #(vim.cmd.RustLsp :codeAction)}
                              :debug#start {:callback #(vim.cmd.RustLsp :debuggables)}}
                     LL {:d {:desc "Run Debuggables"
                             :callback #(vim.cmd.RustLsp :debuggables)}
                         :D {:desc "Debug" :callback #(vim.cmd.RustLsp :debug)}
                         :r {:desc "Run" :callback #(vim.cmd.RustLsp :run)}}}}})

(use! [:Civitasv/cmake-tools.nvim]
      {:setup {:cmake-tools {:cmake_regenerate_on_save false}}
       :keymaps {:opts {:ft :cpp}
                 :n {LL {:b {:desc "Build" :callback vim.cmd.CMakeBuild}
                         :B {:desc "Select Build Preset"
                             :callback vim.cmd.CMakeSelectBuildPreset}
                         :C {:desc "Select Configure Preset"
                             :callback vim.cmd.CMakeSelectConfigurePreset}}}}})

;; Forked as plugin doesn't have an API for custom keybindings
(use! :bR3iN/jupynium.nvim {:setup {:jupynium {}}})

(use! :zk-org/zk-nvim
      {:config #(let [zk (require :zk)
                      util (require :zk.util)
                      create-and-insert-link (fn []
                                               (let [loc (util.get_lsp_location_from_caret)
                                                     title (vim.fn.input "Title: ")]
                                                 (when (not= title "")
                                                   (zk.new {: title
                                                            :edit false
                                                            :insertLinkAtLocation loc}))))
                      get-note #(vim.fn.bufname)
                      open-results (fn [results]
                                     (each [_ {: absPath} (ipairs results)]
                                       (vim.cmd (.. "e " absPath))))
                      open-backlinks #(zk.pick_notes {:linkTo [(get-note)]
                                                      :maxDistance vim.v.count1
                                                      :recursive true}
                                                     {} open-results)
                      open-links #(zk.pick_notes {:linkedBy [(get-note)]
                                                  :maxDistance vim.v.count1
                                                  :recursive true}
                                                 {} open-results)
                      create-note #(let [title (vim.fn.input "Title: ")]
                                     (when (not= title "")
                                       (zk.new {: title})))
                      capture-with (fn [f]
                                     ;; Screenshot -> f -> insert text at cursor
                                     (spawn-capture-output :zk-screenshot nil
                                                           (fn [code
                                                                _signal
                                                                stdout
                                                                _stderr]
                                                             (if (= 0 code)
                                                                 (-> stdout f
                                                                     put!)))))
                      insert-screenshot #(capture-with #(.. "![[" $1 "]]"))
                      on_attach (fn [_client bufnr]
                                  (keymaps! {:opts {:buffer bufnr}
                                             :n {LL {:n {:desc "Create new note"
                                                         :callback create-note}
                                                     ;; :N {:desc "Create new note and link it"
                                                     ;;     :callback create-and-insert-link}
                                                     :o {:desc "Edit note"
                                                         :callback #(zk.edit)}
                                                     :b {:desc "Edit open note"
                                                         :callback ":<C-u>ZkBuffers<CR>"}
                                                     :l {:desc "Show links"
                                                         :callback open-links}
                                                     :L {:desc "Show backlinks"
                                                         :callback open-backlinks}}}
                                             :x {LL {:n {:desc "Into note as title"
                                                         :callback ":ZkNewFromTitleSelection<CR>"}
                                                     :N {:desc "Into note as content"
                                                         :callback ":ZkNewFromContentSelection<CR>"}}}
                                             :i {:<C-h> {:desc "Move to start of link"
                                                         :callback :<Esc>hcT|}
                                                 :<C-l> {:desc "Move past link"
                                                         :callback :<Esc>2la}
                                                 "<C-u>" {:desc "Select link text"
                                                          :callback "<Esc>2hvT|uf]2la"}
                                                 ;; :<C-i> {:desc "Insert link"
                                                 ;;         :callback "<C-o>:ZkInsertLink<CR>"}
                                                 :<C-j> {:desc "Create and insert link"
                                                         :callback create-and-insert-link}
                                                 :<C-p> {:desc "Insert screenshot"
                                                         :callback insert-screenshot}}}))]
                  (zk.setup {:picker :telescope :lsp {:config {: on_attach}}}))})

;; Misc Autocmds

(let [pattern (.. vim.env.HOME
                  "/.{dotfiles/config,config}/nvim/*.{vim,lua,fnl}")
      regpat (vim.fn.glob2regpat pattern)
      setup (fn [prefix]
              "Fold at `PREFIX --- [...] ---` lines (presumably comments)"
              (set vim.wo.foldmethod :expr)
              ;; FIXME: seems to need a buffer reload to discover new folds
              (set vim.wo.foldexpr
                   (.. "getline(v:lnum)=~'^" prefix " ---'?'>1':'='")))
      callback (fn [{: buf}]
                 (case (. vim.bo buf :filetype)
                   :fennel (setup ";;")
                   :lua (setup "--")))]
  (autocmd! ;; Autoreload config files on save
            {:event :BufWritePost
             : pattern
             ;; Schedule is needed a otherwise highlight groups become weirdly mixed up.
             :callback #(vim.schedule #(dofile vim.env.MYVIMRC))}
            {:event [:BufReadPost :FileReadPost] : pattern : callback}
            {:event [:FileType]
             :pattern [:vim :lua :fennel]
             :callback (fn [ev]
                         (when (>= (vim.fn.match (vim.fn.expand "%:p") regpat)
                                   0)
                           (callback ev)))}))

(autocmd! ;; Autotrust our own changes to .hotpot.fnl
          {:event :BufWritePre
           :pattern (.. vim.env.HOME
                        "/.{dotfiles/config,config}/nvim/.hotpot.fnl")
           :callback (fn [{:buf bufnr}]
                       (vim.secure.trust {:action :allow : bufnr}))}
          ;; Autoreload tmux config on change
          (when vim.env.TMUX
            {:event :BufWritePost
             :pattern (.. vim.env.HOME
                          "/.{dotfiles/config,config}/tmux/tmux.conf")
             :callback #(let [cmd [:tmux
                                   :source-file
                                   (.. vim.env.HOME "/.config/tmux/tmux.conf")]]
                          (vim.system cmd {:text true}
                                      (fn [res]
                                        (if (not= 0 res.code)
                                            (vim.schedule #(vim.notify (vim.inspect {: cmd
                                                                                     : res})
                                                                       vim.log.levels.ERROR))))))})
          ;; Don't create undofiles for temporary files
          {:event :BufWritePre
           :pattern :/tmp/*
           :callback #(buf-opts! {:undofile false})}
          {:event :BufWritePre
           :pattern "~/.crypt/*"
           :callback #(buf-opts! {:undofile false})}
          ;; Highlight on yank
          {:event :TextYankPost
           :pattern "*"
           :callback #(vim.hl.on_yank {:higroup :IncSearch :timeout 150})})

(reload :appearance)

;; --- Experimental ---

;; (let [{: setup} (reload :plugin.visual-history)]
;;   (setup)
;;   (keymaps! {:x {:z :<Plug>visual-history#pop :q :<Plug>visual-history#abort}
;;              :n {:gv :<Plug>visual-history#gv}}))

;; (let [{: extend-forward : extend-backward} (require :utils.treesitter)]
;;   (keymaps! {:v {"]n" {:desc "Extend selection to next sibling"
;;                        :callback extend-forward}
;;                  "[n" {:desc "Extend selection to prev sibling"
;;                        :callback extend-backward}}}))

(use! [:stevearc/aerial.nvim]
      {:setup #(let [actions (require :aerial.actions)]
                 {:aerial {:keymaps {:o (fn []
                                          (actions.jump.callback)
                                          (actions.close.callback))
                                     :<Tab> :actions.toggle
                                     :<S-Tab> :actions.toggle_recursive}
                           :nav {:keymaps {:q :actions.close :o :actions.jump}}}})
       :keymaps {:n {L {:to {:desc "Toggle Outline"
                             :callback #(vim.cmd.AerialToggle :right)}
                        :n {:desc "Aerial Nav"
                            :callback #(vim.cmd.AerialNavToggle)}}}}})

(use! [:jmacadie/telescope-hierarchy.nvim]
      {:config #(let [{: load_extension} (require :telescope)]
                  (load_extension :hierarchy))
       ;; :keymaps #(let [{: run} (require :callgraph)]
       ;;               {:n {l {:l {:C #(run {:direction :in})
       ;;                                :O #(run {:direction :out})}}}})
       })

;; Open urls externally with xdg-open
;; FIXME: mk-op! is deprecated?
(mk-op! :OpenExternally (let [cmd :xdg-open
                              open #(let [on-exit (fn [exit]
                                                    (if (not= exit 0)
                                                        (error (.. cmd " '" $1
                                                                   "' failed with exit code "
                                                                   exit))))]
                                      (spawn cmd {:args [$1]} on-exit))]
                          (fn [lines]
                            (vim.tbl_map open lines))))

(keymaps! {:n {"<M-o>" {:desc "Forward Jump" :callback "<C-i>"}}})

;; FIXME: for debugging
;; (keymaps! {:n (collect [_ key (ipairs [:<C-CR>
;;                                        :<M-CR>
;;                                        :<C-M-CR>
;;                                        :<Tab>
;;                                        :<C-i>
;;                                        "<C-[>"
;;                                        "<C-M-[>"
;;                                        "<M-[>"
;;                                        :<ESC>
;;                                        :<C-ESC>
;;                                        :<C-Left>
;;                                        :<C-M-Left>
;;                                        :<M-Left>])]
;;                 (values key #(vim.print key)))})

;; TODO:
(keymaps! {:n {L {[:k :oh] {:desc "Hover" :callback "K" :remap true}}}})

;; Expand selection using treesitter
;; TODO
(keymaps! {:opts {:remap true}
           :n {"+" {:desc "Expand Selection" :callback "vin"}}
           :v {"+" {:desc "Expand Selection" :callback "an"}
               "-" {:desc "Contract Selection" :callback "in"}}})

;; Navigate windows with Alt + vim keys
(let [dirs {:h "Left" :j "Down" :k "Up" :l "Right"}
      nmaps (vim.tbl_extend :error
                            (collect [key desc (pairs dirs)]
                              (values (.. "<M-" key ">")
                                      {:desc (.. "Window " desc)
                                       :callback (.. "<C-w>" key)}))
                            {"<M-q>" {:desc "Close Window" :callback "<C-w>q"}
                             "<M-c>" {:desc "New Tab" :callback ":tabnew<CR>"}
                             ;; FIXME:
                             "<M-s>" {:callback "<C-w>s"}
                             "<M-v>" {:callback "<C-w>v"}
                             "<M-n>" {:desc "Next Tab"
                                      :callback ":tabnext<CR>"}
                             "<M-p>" {:desc "Previous Tab"
                                      :callback ":tabprevious<CR>"}})
      imaps (collect [lhs {: callback} (pairs nmaps)]
              (values lhs {:callback (.. "<C-\\><C-n>" callback)}))]
  (keymaps! {[:n :v :s :x] nmaps [:i :t] imaps}))

;; TODOs
;; - matchup/treesitter binds
