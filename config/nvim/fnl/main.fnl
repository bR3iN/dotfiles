(local {: buf-opts!
        : ft!
        : put!
        : get-cwd-override
        : opts!
        : use!
        : keymaps!
        : lsps!
        : autocmd!
        : reload} (require :utils))

(local {: spawn-capture-output : spawn} (require :utils.async))
(local {: mk-op!} (require :utils.operator))
(local {: get-named : mix} (require :utils.colors))

(local colors (get-named))

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
        :undodir (.. (vim.fn.stdpath :data) :undo)
        ;; Wrap behaviour
        :breakindent true
        :wrap true
        ;; Default tab behaviour
        :shiftwidth 4
        :tabstop 4
        :expandtab true
        ;; Start with folds expanded
        :foldlevelstart 99})

(set vim.g.editorconfig true)

;; set leader keys
(set vim.g.mapleader " ")
(set vim.g.maplocalleader " c")

;; Open urls externally with xdg-open
(mk-op! :OpenExternally (let [cmd :xdg-open
                              open #(let [on-exit (fn [exit]
                                                    (if (not= exit 0)
                                                        (error (.. cmd " '" $1
                                                                   "' failed with exit code "
                                                                   exit))))]
                                      (spawn cmd {:args [$1]} on-exit))]
                          (fn [lines]
                            (vim.tbl_map open lines))))

;; Allows ftplugins to override keybinds buffer-locally.
(keymaps! {:n {:<Plug>lsp#code-action vim.lsp.buf.code_action
               :<Plug>edit#format-file vim.lsp.buf.format}})

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

;; Use <F13> (emitted by caps lock) as alternative localleader and as <C-o> in normal mode
;; (keymaps! {[:n :x] {"<F13>" {:desc "Alternative localleader"
;;                              :callback "<localleader>"}
;;                     :<localleader> {:<ESC> {:desc "Cancel prefix"
;;                                             :callback (fn [])}}}
;;            :i {"<F13>" {:desc "Nested Normal Mode" :callback "<C-o>"}}})

;; Aliases
(keymaps! {:n {"<CR>" "<C-]>" "<M-o>" "<C-i>"}} {:remap true})

;; Insert mode keymaps
(keymaps! {:i {;; Correctly indent when pasting multiple lines in insert mode
               :<C-r> {:desc "Paste (auto-indent)" :callback :<C-r><C-o>}
               ;; Capitalize word in front of cursor
               :<C-u> {:desc "Capitalize Word" :callback :<Esc>viwUea}}})

;; Clear highlight search
;; (keymaps! {:n {:<ESC> {:desc "Clear Search Highlight"
;;                        :callback ":<C-u>nohlsearch<CR><C-l>"}}})

;; Select until end of line (like `C`, `D` and `Y`)
;; (keymaps! {:n {:<leader>v {:desc "Select to End of Line" :callback :vg_}}})

;; Open/Close things
(keymaps! {:n {:<leader>a {:desc "Open Alternate File" :callback "<C-^>"}
               :<leader>o {:v {:desc "Open Vim Config Files"
                               :callback #(vim.cmd.edit (.. vim.env.HOME
                                                            "/.config/nvim/fnl/main.fnl"))}
                           :V {:desc "Open Vim Runtime"
                               :callback #(vim.cmd.edit vim.env.VIMRUNTIME)}
                           :p {:desc "Open Plugin Files"
                               :callback #(vim.cmd.edit (.. vim.env.HOME
                                                            "/.local/share/nvim/site/pack"))}
                           :q {:desc "Open Quickfix"
                               :callback ":<C-u>copen<CR>"}
                           :l {:desc "Open Location List"
                               :callback ":<C-u>lopen<CR>"}
                           :n {:desc "Open Notes"
                               :callback #(let [notes-dir (.. vim.env.HOME
                                                              :/Notes)]
                                            (vim.cmd.cd notes-dir)
                                            (vim.cmd.edit :index.md))}
                           :d vim.diagnostic.open_float
                           :c ":e ~/.config/<CR>"}
               :<leader>q {:V {:desc "Quit NeoVim" :callback ":<C-u>qall<CR>"}
                           :w {:desc "Quit Window" :callback ":q<CR>"}
                           :q {:desc "Close Quickfix"
                               :callback ":<C-u>cclose<CR>"}
                           :l {:desc "Close Location List"
                               :callback ":<C-u>lclose<CR>"}}}})

;; Expand selection using LSP
(keymaps! {:n {"+" "vin"} :v {"+" "an" "-" "in"}} {:remap true})

;; File Operations
;; Not under leader to not accidentally trigger the tmux leader <C-space> when doing e.g. `<C-]><space>s`
(keymaps! {:n {"<leader>x" {:S {:desc "Save File" :callback ":<C-u>update<CR>"}
                            :w {:desc "Save File (Force)"
                                :ocallback ":<C-u>write<CR>"}
                            :l {:desc "Reload File from Disk"
                                :callback ":<C-u>e!<CR>"}
                            :s {:desc "Save All Files"
                                :callback ":<C-u>wall<CR>"}
                            :W {:desc "Write as Root"
                                :callback ":<C-u>w !pkexec tee % >/dev/null<CR>"}}}})

(keymaps! {:n {;; :o {:desc "Open URL/file under cursor"
               ;;     :callback #(vim.cmd (.. "!xdg-open "
               ;;                             (vim.fn.shellescape (vim.fn.expand "<cfile>"))))}
               :<leader> {:R {:desc "Reload Config"
                              :callback #(let [{: clear-cache} (require :hotpot.api.cache)]
                                           (vim.print "Clearing cache and reloading config")
                                           (clear-cache {:silent true})
                                           ;; Conflicts with vim.loader? Check docs
                                           (dofile vim.env.MYVIMRC))}}}
           [:v :n] {:<leader> {:y {:desc "Yank to Clipboard" :callback "\"+y"}
                               :p {:desc "Paste from Clipboard"
                                   :callback "\"+p"}
                               :P {:desc "Paste from Clipboard before cursor"
                                   :callback "\"+P"}}}})

(keymaps! {:n {:<leader> {:v {:desc "Select to End of Line" :callback :vg_}
                          :w {:desc "Window Commands"
                              :callback "<C-w>"
                              :remap true}}}})

;; Navigation

(keymaps! {[:n :v] {;; FIXME: useful? C.f. terminal [s/]s binds
                    "]]" "<C-i>"
                    "[[" "<C-o>"
                    "]e" {:desc "Go to next error"
                          :callback #(vim.diagnostic.jump {:count 1
                                                           :severity vim.diagnostic.severity.ERROR})}
                    "[e" {:desc "Go to previous error"
                          :callback #(vim.diagnostic.jump {:count -1
                                                           :severity vim.diagnostic.severity.ERROR})}}})

;; Leap with s/gs
(use! :ggandor/leap.nvim
      {:setup {:leap {:safe_labels {}}}
       :keymaps {[:n :v] {:s {:desc "Jump in Buffer" :callback "<Plug>(leap)"}
                          :gs {:desc "Jump to Other Buffer"
                               :callback "<Plug>(leap-from-window)"}}}})

(use! [:nvim-telescope/telescope.nvim
       :nvim-telescope/telescope-fzf-native.nvim
       :nvim-lua/plenary.nvim]
      {:config (fn []
                 ;; fzf-native needs `make` to be run, vim.pack.add currently does not support this
                 (let [path (-> [:telescope-fzf-native.nvim]
                                (vim.pack.get)
                                (. 1)
                                (. :path))]
                   (spawn-capture-output :make {:cwd path}
                                         (fn [code]
                                           (when (not= 0 code)
                                             (vim.print "Failed to compile fzf-native")))))
                 (local {: setup : load_extension} (require :telescope))
                 (local builtins (require :telescope.builtin))
                 (local actions (require :telescope.actions))
                 (local pickers (require :telescope.pickers))
                 (local finders (require :telescope.finders))
                 (local make_entry (require :telescope.make_entry))
                 (local conf (. (require :telescope.config) :values))
                 (local smart_qf_and_open
                        (fn [bufnr]
                          (actions.smart_send_to_qflist bufnr)
                          (vim.cmd.cfirst)))
                 (local pick (fn [action ?opts]
                               (let [picker (. builtins action)
                                     opts (or ?opts {})]
                                 (picker opts))))
                 (local keymaps
                        {:picker {:i {"<C-j>" actions.preview_scrolling_down
                                      "<C-k>" actions.preview_scrolling_up
                                      "<C-l>" actions.preview_scrolling_right
                                      "<C-h>" actions.preview_scrolling_left
                                      "<C-q>" smart_qf_and_open
                                      "<C-]>" actions.select_default
                                      "<C-x>" actions.drop_all
                                      "<C-a>" actions.select_all
                                      "<C-d>" actions.results_scrolling_down
                                      "<C-u>" actions.results_scrolling_up
                                      "<C-s>" actions.select_horizontal
                                      "<esc>" actions.close}}})
                 (setup {:defaults {:sorting_strategy :ascending
                                    :scroll_strategy :limit
                                    :layout_config {:prompt_position :top}
                                    :layout_strategy :flex
                                    :mappings keymaps.picker}
                         :extensions {:fzf {:fuzzy false
                                            :override_generic_sorter true
                                            :override_file_sorter true
                                            :case_mode :smart_case}}
                         :pickers {:diagnostics {:sort_by :severity}
                                   :buffers {:mappings {:n {:<C-x> :delete_buffer}
                                                        :i {:<C-x> :delete_buffer}}}}})
                 (load_extension :fzf)

                 (fn bufnr->entry [bufnr]
                   ;; Format expected by make_entry.gen_from_buffer
                   (let [flag ""
                         info (. (vim.fn.getbufinfo bufnr) 1)]
                     {: bufnr : info : flag}))

                 (fn pick-bufs [bufnrs ?opts]
                   (let [opts (or ?opts {})
                         _ (set (. opts :bufnr_width)
                                (-> bufnrs (_G.unpack) (math.max) (tostring)
                                    (length)))
                         entry_maker (or opts.entry_maker
                                         (make_entry.gen_from_buffer opts))
                         picker (pickers.new opts
                                             {:prompt_title "terminal buffer"
                                              :finder (finders.new_table {:results (vim.tbl_map bufnr->entry
                                                                                                bufnrs)
                                                                          : entry_maker})
                                              :sorter (conf.generic_sorter opts)
                                              :previewer (conf.grep_previewer opts)})]
                     (picker:find)))

                 (fn term-buffers []
                   (let [show-buffer? (fn [bufnr]
                                        (and (vim.api.nvim_buf_is_loaded bufnr)
                                             (= (. vim.bo bufnr :buftype)
                                                :terminal)))]
                     (->> (vim.api.nvim_list_bufs)
                          (vim.tbl_filter show-buffer?))))

                 (keymaps! {:n {"<leader>f" {:desc "Open File"
                                             :callback #(pick :find_files
                                                              {:follow true
                                                               :cwd (get-cwd-override)})}
                                :<leader> {";" #(pick :resume)
                                           "/" {:desc "Grep Workspace"
                                                :callback #(pick :live_grep
                                                                 {:cwd (get-cwd-override)})}
                                           :? {:desc "Grep Buffers"
                                               :callback #(pick :live_grep
                                                                {:grep_open_files true
                                                                 :cwd (get-cwd-override)})}
                                           :i {:desc "Document Symbols"
                                               :callback #(pick :lsp_document_symbols)}
                                           :I {:desc "Workspace Symbols"
                                               :callback #(pick :lsp_workspace_symbols)}
                                           ;; FIXME: use ]e/[e instead
                                           ;; :e {:desc "Buffer Errors"
                                           ;;     :callback #(pick :diagnostics
                                           ;;                      {:bufnr 0
                                           ;;                       :severity vim.diagnostic.severity.ERROR
                                           ;;                       :prompt_title "Buffer Errors"})}
                                           ;; :E {:desc "Workspace Errors"
                                           ;;     :callback #(pick :diagnostics
                                           ;;                      {:severity vim.diagnostic.severity.ERROR
                                           ;;                       :prompt_title "Workspace Errors"})}
                                           :H {:desc "Help Tags"
                                               :callback #(pick :help_tags)}
                                           "_" {:desc "Pick Terminal"
                                                ;; FIXME: error less hard when no are available
                                                :callback #(let [bufs (term-buffers)]
                                                             (if (= (length bufs)
                                                                    0)
                                                                 (vim.notify "No terminal buffers")
                                                                 (pick-bufs bufs)))}
                                           :b {:desc "Find Buffers"
                                               :callback #(pick :buffers
                                                                {:select_current true
                                                                 :sort_lastused true
                                                                 :sort_mru true})}
                                           :d {:desc "Buffer Diagnostics"
                                               :callback #(pick :diagnostics
                                                                {:bufnr 0})}
                                           :D {:desc "Workspace Diagnostics"
                                               :callback #(pick :diagnostics)}
                                           :G {:desc "Git Status"
                                               :callback #(pick :git_status)}}}
                            :v {:<leader> {"/" {:desc "Grep Selection in Workspace"
                                                :callback #(pick :grep_string
                                                                 {})}
                                           :? {:desc "Grep Selection in Buffers"
                                               :callback #(pick :grep_string
                                                                {:grep_open_files true})}}}}))})

(fn feed [keys]
  (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes keys true true true)
                         :n false))

;; Navigate history containing substring
(keymaps! {:c {:<M-p> {:desc "Previous History" :callback #(feed :<Up>)}
               :<M-n> {:desc "Next History" :callback #(feed :<Down>)}}})

;; Navigate windows with Alt + vim keys
(let [nav-maps (collect [_ key (ipairs [:h :j :k :l])]
                 (values (.. "<M-" key ">") (.. "<C-w>" key)))
      nmaps (vim.tbl_extend :error nav-maps
                            {"<M-x>" "<C-w>q"
                             "<M-c>" ":tabnew<CR>"
                             "<M-n>" ":tabnext<CR>"
                             "<M-p>" ":tabprevious<CR>"})
      imaps (collect [lhs rhs (pairs nmaps)]
              (values lhs {:callback (.. "<C-\\><C-n>" rhs)}))]
  (keymaps! {[:n :v :s :x] nmaps [:i :t] imaps}))

;; Buffer and list navigation
(keymaps! {:n {;; Buffer navigation
               "]b" {:desc "Next Buffer" :callback ":<C-u>bnext<CR>"}
               "[b" {:desc "Previous Buffer" :callback ":<C-u>bprev<CR>"}
               "]q" {:desc "Next Quickfix" :callback ":<C-u>cnext<CR>"}
               "[q" {:desc "Previous Quickfix" :callback ":<C-u>cprev<CR>"}
               "]Q" {:desc "Last Quickfix" :callback ":<C-u>clast<CR>"}
               "[Q" {:desc "First Quickfix" :callback ":<C-u>cfirst<CR>"}
               "]l" {:desc "Next Location" :callback ":<C-u>lnext<CR>"}
               "[l" {:desc "Previous Location" :callback ":<C-u>lprev<CR>"}
               "]L" {:desc "Last Location" :callback ":<C-u>llast<CR>"}
               "[L" {:desc "First Location" :callback ":<C-u>lfirst<CR>"}}})


(let [keys (let [res {}]
             (for [i 1 9]
               (set (. res (tostring i)) i))
             res)]
  (keymaps! {:n {:<leader> {;; <n>: go to window <n>
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
             :i (collect [as_str _ (pairs keys)]
                  (values (.. "<M-" as_str ">")
                          {:desc (.. "Focus Tab " as_str)
                           :callback (.. "<esc>" as_str "gt")}))
             :t (collect [as_str _ (pairs keys)]
                  (values (.. "<M-" as_str ">")
                          {:desc (.. "Focus Tab " as_str)
                           :callback (.. "<C-\\><C-n>" as_str "gt")}))}))

(keymaps! {[:n :v] {"<C-j>" "gj" "<C-k>" "gk"}})

(use! :tpope/vim-repeat)

(use! :windwp/nvim-autopairs
      {:setup {:nvim-autopairs {:enable_check_bracket_line false
                                :map_c_h true
                                :map_c_w true}}})

(use! :kylechui/nvim-surround
      {:setup {:nvim-surround {:keymaps {:insert :<C-s>
                                         :insert_line :<C-s><C-s>}}}})

(use! [:mbbill/undotree]
      {:keymaps {:n {:<leader>tu {:callback (fn []
                                              (vim.cmd.UndotreeToggle)
                                              (vim.cmd.UndotreeFocus))}}}})

;; Highlights hex color codes in their color
(use! :NvChad/nvim-colorizer.lua
      {:setup {:colorizer {:user_default_options {:names false}}}
       :keymaps {:n {:<leader>t {:c {:desc "Toggle Colorizer"
                                     :callback vim.cmd.ColorizerToggle}}}}})

;; Floating preview in quickfix window
(use! :kevinhwang91/nvim-bqf
      {:hl {:BqfPreviewBorder {:bg :NONE :fg colors.mid}}
       :setup {:bqf {:func_map {:fzffilter "" :open "<C-]>"}
                     :preview {:winblend 0}}}})


(keymaps! {:n {"<leader>m" {:k {:desc "Make" :callback ":make!<CR>"}
                            :f {:desc "Make Flash"
                                :callback ":make! flash<CR>"}
                            :c {:desc "Make Clean"
                                :callback ":make! clean<CR>"}
                            :t {:desc "Make Test" :callback ":make! test<CR>"}
                            :b {:desc "Make Build"
                                :callback ":make! build<CR>"}}}})


(use! [:nvim-mini/mini.nvim] {:setup {:mini.align {}
                                      :mini.bracketed {:diagnostic {:suffix ""}}
                                      ;; :mini.jump {}
                                      }})

(use! [:folke/snacks.nvim]
      {;; Force resetup of snacks.nvim on reload
       :init #(-?> _G
                   (. :package)
                   (. :loaded)
                   (. :snacks)
                   (tset :did_setup false))
       :setup {:snacks {:animate {:fps 120}
                        :bigfile {}
                        :image {}
                        :dim {:animate {:enabled false}}
                        :indent {:enabled false}
                        ;; :words {}
                        ;; :notifier {}
                        ;; TODO:
                        :toggle {:notify true}
                        :scroll {:animate {:duration {:total 150}}}
                        :zen {:zoom {:show {:tabline false :statusline false}}}}}
       :config (fn []
                 ;; Toggle
                 (let [{: toggle} (require :snacks)
                       diag-lines {:name "LSP Lines"
                                   :get #(-> (vim.diagnostic.config)
                                             (. :virtual_lines))
                                   :set #(vim.diagnostic.config {:virtual_lines (if $1
                                                                                    {:current_line true}
                                                                                    false)
                                                                 :virtual_text (if $1
                                                                                   false
                                                                                   {:current_line true})})}
                       diag-curr {:name "Diag Cursorline Only"
                                  :get #(case (vim.diagnostic.config)
                                          {:virtual_text {:current_line true}} true
                                          {:virtual_lines {:current_line true}} true
                                          _ false)
                                  :set #(vim.diagnostic.config (case (vim.diagnostic.config)
                                                                 {:virtual_lines false} {:virtual_text (if $1
                                                                                                           {:current_line true}
                                                                                                           true)}
                                                                 {:virtual_text false} {:virtual_lines (if $1
                                                                                                           {:current_line true}
                                                                                                           true)}
                                                                 _ (error "virtual lines and text are both enabled")))}
                       keys {:i (toggle.indent)
                             :h (toggle.inlay_hints)
                             :z (toggle.zen)
                             :- (toggle.dim)
                             :dd (toggle.diagnostics)
                             :dv (toggle.new diag-lines)
                             :dl (toggle.new diag-curr)
                             :r (toggle.words)
                             :+ (toggle.zoom)
                             :lr (toggle.option :relativenumber
                                                {:name "Relative Numbers"})
                             :ln (toggle.option :number {:name "Line Numbers"})
                             :w (toggle.option :wrap {:name "Wrap"})
                             :s (toggle.option :spell {:name "Spellcheck"})}]
                   (each [key bind (pairs keys)]
                     (bind:map (.. :<leader>t key)))))
       :keymaps #(let [{: scratch : notifier : bufdelete} (require :snacks)]
                   {:n {:<leader> {"qb" {:desc "Kill buffer"
                                         :callback bufdelete.delete}
                                   :- {:desc "Toggle scratch buffer"
                                       :callback scratch.open}
                                   :oN notifier.show_history
                                   ;; :fe {:callback explorer}
                                   }}})})

;; File Explorer

(use! :stevearc/oil.nvim
      {:setup {:oil {:columns []
                     :use_default_keymaps false
                     :keymaps {:g? :actions.show_help
                               "<C-]>" :actions.select
                               "<CR>" :actions.select
                               :<C-s>v :actions.select_vsplit
                               :<C-s>s :actions.select_split
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
               "<C-w>" {"V" {:callback #(do
                                          (vim.cmd.vsplit)
                                          (vim.cmd.terminal {:args [:fish]}))}
                        "S" {:callback #(do
                                          (vim.cmd.split)
                                          (vim.cmd.terminal {:args [:fish]}))}}}})



;; Flattens files opened in terminal into current instance
(use! "willothy/flatten.nvim" {:setup {:flatten {}}})

;; Clear autocmds from `:h default-autocmd` that auto-close shell terminals on exit
(vim.api.nvim_clear_autocmds {:group :nvim.terminal :event :TermClose})

;; Terminal setup
(autocmd! {:event :TermOpen
           :callback (fn [{: buf}]
                       ;; (set vim.wo.number false)
                       ;; (set vim.wo.relativenumber false)
                       ;; FIXME: use when having separate picker
                       ;; (set vim.bo.buflisted false)
                       ;; Don't insert here as this doesn't have to be an active buffer
                       (keymaps! {:n {;; Go to shell prompts
                                      "[s" "[["
                                      "]s" "]]"
                                      ;; "Forward" some keys directly in terminal mode
                                      "" (let [keys ["<CR>"
                                                     "<C-c>"
                                                     "<C-n>"
                                                     "<C-p>"
                                                     "q"]]
                                           (collect [_ key (ipairs keys)]
                                             (values key (.. "i" key))))}}
                                 {:buffer buf}))}
          {:event :TermClose
           :callback (fn [{: buf}]
                       ;; Don't alter window layout when closing with <CR>, but allow doing so with <BS>
                       (keymaps! {[:t :n] {:<CR> {:desc "Delete Buffer"
                                                  :callback #(_G.Snacks.bufdelete.delete buf)}
                                           :<BS> {:desc "Kill Buffer and Window"
                                                  :callback #(vim.api.nvim_buf_delete buf
                                                                                      {})}}}
                                 {:buffer buf})
                       ;; Autoclose terminal buffers on clean exit; `default-autocmd` does something similar for simple shell buffers, but always closes the window in the process.
                       (when (= 0 vim.v.event.status)
                         (case (. vim.b buf :term_autoclose)
                           :window (vim.api.nvim_buf_delete buf {:force true})
                           :buffer (_G.Snacks.bufdelete.delete buf)
                           nil nil
                           other (error (.. "value unknown: " other)))
                         (_G.Snacks.bufdelete.delete buf))
                       )}
          {:event [:BufWinEnter :BufEnter]
           :callback ;; Avoids triggering wrongly as startinsert will only happen after a sequence of commands and also catches more cases (for some reason).
           (vim.schedule_wrap #(when (= vim.bo.buftype :terminal)
                                           (vim.cmd.startinsert)))})

;; (use! "akinsho/toggleterm.nvim"
;;       {:setup {:toggleterm {:highlights {:StatusLine {:guibg colors.statusline}}
;;                             :direction :float
;;                             :shade_terminals false
;;                             :open_mapping "<c-j>"}}})


;; Autocompletion & Snippets

;; (use! [:L3MON4D3/LuaSnip :rafamadriz/friendly-snippets]
;;       ;; Setup custom snippets in separate module to not duplicate them when ; reloading the config
;;       ;; Find snippets defined by plugins
;;       {:config #(let [{:lazy_load load_from_vscode} (require :luasnip.loaders.from_vscode)
;;                       ; {:lazy_load load_from_lua} (require :luasnip.loaders.from_lua)
;;                       ]
;;                   (load_from_vscode {:exclude [:norg]})
;;                   ;; (setup :snippets)
;;                   ;; (load_from_lua)
;;                   )})

(use! [{:src :saghen/blink.cmp :version (vim.version.range "v1.*")}
       :rafamadriz/friendly-snippets]
      {:setup {:blink.cmp {:completion {:menu {:border :none}
                                        :documentation {:auto_show true
                                                        :auto_show_delay_ms 200
                                                        :window {:border :single
                                                                 ;; NOTE: For some reason, using a `solid` border with a scrollbar will render the scrollbar outside the completion window. So as workaround, use a single border and set the border fg to the bg.
                                                                 :scrollbar true}}
                                        ;; Don't preselect first item for easier inserting-without-accepting
                                        :list {:selection {:preselect false}}}
                           :cmdline {:keymap {:preset :inherit
                                              "<C-n>" false
                                              "<C-p>" false
                                              "<Tab>" [:select_next :show]
                                              "<S-Tab>" [:select_prev
                                                         :fallback]}
                                     :completion {:menu {:auto_show true}
                                                  :list {:selection {:preselect false}}}}
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
                           ;; :signature {:enabled true}
                           }}
       :hl {:BlinkCmpLabelMatch {:link ;; Linking to `Normal` doesn't work for whatever reason
                                 :PmenuMatch}
            :BlinkCmpScrollBarThumb {:link :PmenuThumb}
            ;; :BlinkCmpScrollBarGutter {:link :Pmenu}
            :BlinkCmpDoc {:link :PmenuDoc}
            :BlinkCmpDocBorder {:link :PmenuDocBorder}}})

;; Version control

(use! :lewis6991/gitsigns.nvim
      (let [_signs {:add {:text "┃"}
                    :change {:text "┃"}
                    :delete {:text "┃"}
                    :untracked {:text "┆"}}
            signs_staged {:add {:text "+"}
                          :change {:text "~"}
                          :delete {:text "-"}
                          :untracked {:text "┆"}}]
        {:setup {:gitsigns {:signcolumn false
                            ;; :numhl true
                            ;; : signs
                            : signs_staged
                            :linehl false
                            :word_diff false
                            :sign_priority 0}}
         :hl #(let [for-bg #(mix colors.bg0 $1 0.9)
                    for-inline #(mix colors.bg0 $1 0.7)]
                {;; Used in signcolumn
                 :Added {:fg colors.green}
                 :Changed {:fg colors.cyan}
                 :Removed {:fg colors.red}
                 ;; Used for linehl
                 :DiffAdd {:bg (for-bg colors.green)}
                 :DiffChange {:bg (for-bg colors.cyan)}
                 :DiffDelete {:bg (for-bg colors.red)}
                 ;; Used for word_diff
                 :GitSignsDeleteInline {:bg (for-inline colors.red)}
                 :GitSignsAddInline {:bg (for-inline colors.green)}
                 :GitSignsChangeInline {:bg (for-inline colors.cyan)}})
         :keymaps {[:n :v] {"]g" {:desc "Next Git Hunk"
                                  :callback #(vim.cmd.Gitsigns :nav_hunk :next)}
                            "[g" {:desc "Previous Git Hunk"
                                  :callback #(vim.cmd.Gitsigns :nav_hunk :prev)}}
                   :n {:<leader>g {:a {:desc "Stage Hunk"
                                       :callback #(vim.cmd.Gitsigns :stage_hunk)}
                                   :RRR {:desc "Reset Hunk"
                                         :callback #(vim.cmd.Gitsigns :reset_hunk)}
                                   :h {:desc "Preview Hunk"
                                       :callback #(vim.cmd.Gitsigns :preview_hunk_inline)}}
                       :<leader>u {:g {:desc "Toggle Git Diff"
                                       :callback #(vim.cmd.Gitsigns :toggle_word_diff)}}
                       :<leader>o {:g {:b {:desc "Open Git Blame"
                                           :callback #(vim.cmd.Gitsigns :blame)}}}}}}))

;; LSP & Co.

(keymaps! {:n {:<leader>l {:d {:desc "Goto Definition" :callback "<C-]>"}
                           :D {:desc "Goto Declaration"
                               :callback vim.lsp.buf.declaration}
                           :t {:desc "Goto Type Definition"
                               :callback vim.lsp.buf.type_definition}
                           :i {:desc "Goto Implementation"
                               :callback vim.lsp.buf.implementation}
                           :r {:desc "Goto References"
                               :callback vim.lsp.buf.references}
                           :h {:desc "Hover"
                               ;; :callback #(vim.lsp.buf.hover {:border _G.border-type})
                               :callback #(vim.lsp.buf.hover {:silent true})}
                           :n {:desc "Rename Symbol"
                               :callback vim.lsp.buf.rename}
                           :a {:desc "Code Action"
                               :callback :<Plug>lsp#code-action}
                           :l {:desc "Code Lenses"
                               :callback vim.lsp.codelens.run}
                           :f {:desc "Format File"
                               :callback :<Plug>edit#format-file}}}})


(use! [:neovim/nvim-lspconfig]
      {:config (fn []
                 (autocmd! {:event :LspAttach
                            :pattern :*
                            :callback (fn [{: buf}]
                                        ;; Initial displaying
                                        (vim.lsp.codelens.refresh {:bufnr buf})
                                        ;; Updating lenses
                                        (vim.api.nvim_create_autocmd [:BufEnter
                                                                      :CursorHold
                                                                      :InsertLeave]
                                                                     {:callback #(vim.lsp.codelens.refresh {:bufnr 0})
                                                                      :buffer buf}))})
                 (lsps! {:clangd {:on_attach (fn [_client bufnr]
                                               (keymaps! {:n {:<C-c> {:desc "Switch source/header"
                                                                      :callback #(vim.cmd.LspClangdSwitchSourceHeader)}}}
                                                         {:buffer bufnr}))}
                         ;; :bashls {}
                         ;; :vimls {}
                         :fennel_ls {}
                         :cmake {}
                         ;; :rust_analyzer {}  ; Configured by rustaceans instead
                         ;; :marksman {}
                         ;; :racket_langserver {}
                         })
                 (case (pcall reload :lsp-local)
                   (true config) (lsps! config)
                   (false _) nil))})

(use! [:nvim-treesitter/nvim-treesitter
       :nvim-treesitter/nvim-treesitter-textobjects]
      ;; "nvim-treesitter/playground"
      {:setup {:nvim-treesitter.configs {:highlight {:enable true}
                                         ;; :indent {:enable true}
                                         ;; :additional_vim_regex_highlighting [:fennel]
                                         ;; :incremental_selection {:enable true
                                         ;;                         :keymaps {:node_incremental "V"
                                         ;;                                   :scope_incremental "v"}}
                                         :textobjects {:select {:enable true
                                                                :keymaps {:if "@function.inner"
                                                                          :af "@function.outer"
                                                                          :ic "@call.inner"
                                                                          :ac "@call.outer"
                                                                          :il "@loop.inner"
                                                                          :al "@loop.outer"
                                                                          :ik "@conditional.inner"
                                                                          :ak "@conditional.outer"}}
                                                       :swap {:enable true
                                                              :swap_next {"<leader>." "@parameter.inner"
                                                                          :<Plug>edit#swap-param-next "@parameter.inner"}
                                                              :swap_previous {"<leader>," "@parameter.inner"
                                                                              :<Plug>edit#swap-param-prev "@parameter.inner"}}}}}
       :config (fn []
                 ;; Use treesitter-based folds
                 (opts! {:foldmethod :expr
                         :foldexpr "nvim_treesitter#foldexpr()"}))})

;; FIXME:
(vim.diagnostic.config {;;:float {:border _G.border-type}
                        :virtual_lines false
                        :virtual_text {:current_line true}
                        :signs {:text {vim.diagnostic.severity.ERROR ""
                                       ; ""
                                       vim.diagnostic.severity.WARN ""
                                       ; ""
                                       vim.diagnostic.severity.INFO ""
                                       ; ""
                                       vim.diagnostic.severity.HINT ""
                                       ; "󰌵"
                                       }}})

;; Debugging

(ft! {:dap-repl #(vim.cmd "abbreviate <buffer> e -exec")
      :dap-float #(keymaps! {:n {[:q :<ESC>] vim.cmd.quit}} {:buffer true})})

;; (use! [:mason-org/mason.nvim
;;        :jay-babu/mason-nvim-dap.nvim ]
;;       {:setup {:mason {}
;;                :mason-nvim-dap {}}})

(use! [:mfussenegger/nvim-dap
       :nvim-neotest/nvim-nio
       ; needed by nvim-dap-ui
       :igorlfs/nvim-dap-view
       :rcarriga/nvim-dap-ui
       :theHamsta/nvim-dap-virtual-text
       ;; :mfussenegger/nvim-dap-python
       ]
      {:keymaps #(let [dap (require :dap)
                       widgets (require :dap.ui.widgets)
                       dapui (require :dapui)]
                   {:n {:<leader> {"t" {;; "d" {:desc "Toggle Debug REPL"
                                        ;;      :callback dap.repl.toggle}
                                        "dr" {:desc "Toggle Debug REPL"
                                              :callback vim.cmd.DapViewToggle}
                                        "du" {:desc "Toggle Debug UI"
                                              :callback dapui.toggle}}
                                   "<CR>" {:desc "Run to Cursor"
                                           :callback dap.run_to_cursor}
                                   "<C-CR>" {:desc "Run at Cursor"
                                             :callback :<Plug>debug#at-cursor}
                                   "<TAB>" {:desc "Toggle Breakpoint"
                                            :callback dap.toggle_breakpoint}
                                   ;; FIXME: hydra
                                   "g" {"b" {:desc "Toggle Breakpoint"
                                             :callback dap.toggle_breakpoint}
                                        "l" {:desc "List Breakpoints"
                                             :callback dap.list_breakpoints}
                                        "e" {:desc "Eval Expression"
                                             :callback #(dapui.eval nil
                                                                    {:enter true})}
                                        "s" {:desc "Step Into"
                                             :callback dap.step_into}
                                        "n" {:desc "Step Over"
                                             :callback dap.step_over}
                                        "o" {:desc "Step Out"
                                             :callback dap.step_out}
                                        "N" {:desc "DapNew"
                                             :callback vim.cmd.DapNew}
                                        "R" {:desc "Run Last"
                                             :callback dap.run_last}
                                        "T" {:desc "Terminate"
                                             :callback dap.terminate}
                                        "c" {:desc "Continue"
                                             :callback dap.continue}
                                        "r" {:desc "Restart"
                                             :callback dap.restart}
                                        "h" {:desc "Hover"
                                             :callback widgets.hover}
                                        "p" {:desc "Preview"
                                             :callback widgets.preview}
                                        "d" {:desc "Clear Breakpoints"
                                             :callback dap.clear_breakpoints}}}}})
       :setup {:nvim-dap-virtual-text {:virt_text_pos :inline}
               :dap-view {}
               :dapui {}
               ;; :dap-python :python
               }
       :config (let [adapter-configs {:gdb {:type :executable
                                            :command :gdb
                                            :args [:--interpreter=dap
                                                   :--eval-command
                                                   "set print pretty on"]}
                                      :codelldb {:type :executable
                                                 :id :codelldb
                                                 :command :codelldb}
                                      ;; :lldb {:type :executable
                                      ;;        :id :lldb
                                      ;;        :command :/usr/bin/lldb-dap}
                                      }
                     ask-exe #(vim.fn.input "Path to executable: "
                                            (.. (vim.fn.getcwd) "/") :file)
                     filetype-configs {:cpp [{:name "Launch lldb"
                                              :type :lldb
                                              :request :launch
                                              :program ask-exe
                                              :stopOnEntry false
                                              :runInTerminal false
                                              :cwd "${workspaceFolder}"}
                                             {:name "Launch gdb"
                                              :type :gdb
                                              :request :launch
                                              :program ask-exe
                                              :stopOnEntry false
                                              :runInTerminal false
                                              :cwd "${workspaceFolder}"}]}]
                 #(let [{: adapters : configurations} (require :dap)]
                    (each [name config (pairs adapter-configs)]
                      (set (. adapters name) config))
                    (each [name config (pairs filetype-configs)]
                      (set (. configurations name) config))))})

;; Plugins for specific filetypes

(use! :elkowar/yuck.vim)

;; Treesitter indentation for fennel is messed up, so use this plugin for this
(use! :jaawerth/fennel.vim)
;; FIXME: find place for `curl -o $HOME/.local/share/fennel-ls/docsets/nvim.lua https://git.sr.ht/~micampe/fennel-ls-nvim-docs/blob/main/nvim.lua`

(use! :lervag/vimtex
      {:config (fn []
                 (set vim.g.vimtex_format_enabled 1)
                 (set vim.g.vimtex_quickfix_mode 0)
                 (set vim.g.vimtex_view_method :zathura)
                 (set vim.g.tex_flavor :latex))})

(use! [:mrcjkb/rustaceanvim]
      {:init #(let [lsp-config {:rust-analyzer {:cargo {:buildScripts {:enable true}}
                                                :procMacro {:enable true}
                                                :imports {;; Merge auto imports on the module level
                                                          :granularity {:group :module}
                                                          ;; Prefer relative auto import paths
                                                          :prefix :self}}}]
                ;; Avoids "Unrecognized option: 'write-mode'" error.
                ;; FIXME: see if it is valid here, was previously top-level
                (set vim.g.rustfmt_detect_version 1)
                (set vim.g.rustaceanvim
                     {:server {:default_settings lsp-config}}))})

;; Forked as plugin doesn't have an API for custom keybindings
(use! :bR3iN/jupynium.nvim {:setup {:jupynium {}}})

(use! [:MeanderingProgrammer/render-markdown.nvim]
      {:setup {:render-markdown {:completions {:lsp {:enabled true}}
                                 :code {;;:width :block
                                        :language_name false}}}})

(use! :iamcco/markdown-preview.nvim
      {:init #(set vim.g.mkdp_combine_preview true)
       :config #((. vim.fn "mkdp#util#install"))
       :ft! {:markdown (fn [{: buf}]
                         (keymaps! {:<localleader> {:c {:desc "Preview in Browser"
                                                        :callback vim.cmd.MarkdownPreview}}}
                                   {:buffer buf}))}})

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
                      insert-screenshot #(spawn-capture-output :zk-screenshot
                                                               nil
                                                               (fn [code
                                                                    _signal
                                                                    stdout
                                                                    _stderr]
                                                                 (if (= 0 code)
                                                                     (put! (.. "![["
                                                                               stdout
                                                                               "]]")))))
                      on_attach (fn [_client bufnr]
                                  (keymaps! {:n {"<localleader>" {:n {:desc "Create new note"
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
                                             :x {"<localleader>" {:n {:desc "Into note as title"
                                                                      :callback ":ZkNewFromTitleSelection<CR>"}
                                                                  :N {:desc "Into note as content"
                                                                      :callback ":ZkNewFromContentSelection<CR>"}}}
                                             :i {:<C-h> {:desc "Move to start of link"
                                                         :callback :<Esc>hcT|}
                                                 :<C-l> {:desc "Move past link"
                                                         :callback :<Esc>2la}
                                                 :<C-y> {:desc "Select link text"
                                                         :callback "<Esc>2hvT|uf]2la"}
                                                 :<C-i> {:desc "Insert link"
                                                         :callback "<C-o>:ZkInsertLink<CR>"}
                                                 :<C-j> {:desc "Create and insert link"
                                                         :callback create-and-insert-link}
                                                 :<C-p> {:desc "Insert screenshot"
                                                         :callback insert-screenshot}}}
                                            {:buffer bufnr}))]
                  (zk.setup {:picker :telescope :lsp {:config {: on_attach}}}))})

;; Misc Autocmds

(autocmd! ;; Autoreload config files on save
          {:event :BufWritePost
           :pattern (.. vim.env.HOME
                        "/.{dotfiles/config,config}/nvim/*.{vim,lua,fnl}")
           :callback #(dofile vim.env.MYVIMRC)}
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
           :callback #(vim.highlight.on_yank {:higroup :IncSearch :timeout 150})})


;; Discover and safely run .nvim.{.lua,fnl} files
(opts! {:exrc true})
(reload :utils/exrc)
