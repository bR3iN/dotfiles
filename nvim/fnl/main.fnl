(import-macros {: set! : setl! : setl+ : setl- : setg! : set+ : let! : with-cb}
               :utils.macros)

(local {: hl!
        : reload
        : keymaps!
        : autocmd!
        : use!
        : ft-autocmd!
        : starts-with} (require :utils))

(local {: spawn} (require :utils.async))
(local {: mk-op!} (require :utils.operator))
(local {: get-named} (require :utils.colors))

(local {: name} (require :base16-colors))
(local colors (get-named))

(fn feed [keys]
  (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes keys true true true)
                         :m true))

(fn setup [mod ?opts] ; Loads module and calls its `setup` function
  (let [{: setup} (require mod)]
    (setup (or ?opts {}))))

;; General Options and Keymaps

(set! exrc)

;; set leader keys
(let! mapleader " ")
(let! maplocalleader " c")

;; Enable dialogs
(set! confirm)

;; Needed for hl-CursorLineNr
(set! cursorline)

;; Ignore case for lowercase searches
(set! ignorecase)
(set! smartcase)

;requires `ignorecase`

(set! inccommand :split)

;; Enable mouse support
(set! mouse :a)

;; Show line number of current line, relative line numbers for the rest
(set! number)
(set! relativenumber)

;; Show cursor coordinates in status bar
(set! ruler)

;; Set split behaviour
(set! splitbelow)
(set! splitright)

;; Ignore case for wildcards expansion
(set! wildignorecase)
;; First complete longest substring and open wildmenu, then cycle through matches
(set! wildmode ["longest:full" :full])

;; Undo behaviour
(set! undofile)
(set! undodir (.. (vim.fn.stdpath :data) :undo))

;; Wrap behaviour
(set! breakindent)
(set! wrap)

;; Default tab behaviour
(set! shiftwidth 4)
(set! tabstop 4)
(set! expandtab)

;; Start with folds expanded
(set! foldlevelstart 99)

;; Correctly indent when pasting multiple lines in insert mode
(keymaps! {:i {:<C-r> {:desc "Paste (auto-indent)" :callback :<C-r><C-o>}}})

;; Clear highlight search
(keymaps! {:n {:<C-l> {:desc "Clear Search Highlight"
                       :callback ":<C-u>nohlsearch<CR><C-l>"}}})

;; Capitalize word in front of cursor
(keymaps! {:i {:<C-u> {:desc "Capitalize Word" :callback :<Esc>viwUea}}})

;; Select until end of line (like `C`, `D` and `Y`)
(keymaps! {:n {:<leader>v {:desc "Select to End of Line" :callback :vg_}}})

;; (keymaps! {:v {:<C-y> "\"+y"}})
(keymaps! {:i {:<C-p> {:desc "Paste from Clipboard" :callback :<C-r>+}}})

(use! :tpope/vim-repeat)
;; (use! :tpope/vim-commentary)

(use! :windwp/nvim-autopairs
      {:setup {:nvim-autopairs {:enable_check_bracket_line false
                                :map_c_h true
                                :map_c_w true}}})

(use! :kylechui/nvim-surround
      {:setup {:nvim-surround {:keymaps {:insert :<C-s>
                                         :insert_line :<C-s><C-s>}}}})

;; (vim.keymap.del :i "<C-s>")
;; (add! "tpope/vim-surround"
;;       (fn []
;;         ; Find numbers with `:echo char2nr("B")`
;;         ; "B"
;;         (let! surround_66  "{\r}\1\1")))

(autocmd! ;; Autoreload config files on save
          {:event :BufWritePost
           :pattern (.. vim.env.HOME "/.{dotfiles,config}/nvim/*.{vim,lua,fnl}")
           :callback #(dofile vim.env.MYVIMRC)}
          ;; Don't create undofiles for temporary files
          {:event :BufWritePre :pattern :/tmp/* :callback #(setl! noundofile)}
          {:event :BufWritePre
           :pattern "~/.crypt/*"
           :callback #(setl! noundofile)} ;; Highlight on yank
          {:event :TextYankPost
           :pattern "*"
           :callback #(vim.highlight.on_yank {:higroup :IncSearch :timeout 150})})

;; Package management

;; (nmap! :<leader>pu :<Plug>PkgUpdate)
;; (nmap! :<leader>pc :<Plug>PkgClean)
;; (nmap! :<leader>pl :<Plug>PkgList)
;; (nmap! :<leader>op ":edit ~/.local/share/nvim/site/pack/pkgs/start/<CR>")

;; Appearance

(set _G.border-type :single)

(set vim.o.signcolumn :yes)
(set! termguicolors)

;; Remove redundant mode prompt in insert area
(set! noshowmode)

(set! cmdheight 1)

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

;; after schem
(use! [:rebelot/heirline.nvim :SmiteshP/nvim-navic :b0o/incline.nvim]
      {:reload [:setup/statusline]})

(set! fillchars {:vert "│" :wbr " "})
(set! conceallevel 2)
;; (set! cmdheight 2)
(set! scrolloff 1)
(set! linebreak)

; (require :plugin.highlight-trailing-whitespace)

;; Highlights hex color codes with their color
(use! :NvChad/nvim-colorizer.lua
      {:setup {:colorizer {:user_default_options {:names false}}}})

(use! :lukas-reineke/headlines.nvim
      {:setup {:headlines {:markdown {:fat_headlines false}}}})

;; Navigation

;; Use <Tab> as gE
(keymaps! {[:n :v :o] {:<Tab> {:desc "Goto End of Word (backward)"
                               :callback :ge}
                       :<S-Tab> {:desc "Goto End of WORD (backward)"
                                 :callback :gE}}
           ;; Avoid conflicts with C-i bindings
           :n {:<C-i> {:desc "Jump Forward" :callback :<C-i>}}})

;; Sane `<Esc>` behaviour in terminal mode
(keymaps! {:t {:<Esc> {:desc "Exit Terminal Mode" :callback "<C-\\><C-n>"}
               :<C-v><Esc> {:desc "Send Escape to Terminal" :callback :<Esc>}}})

;; Open main.fnl
(keymaps! {:n {:<leader>ov {:desc "Open Vim Config"
                            :callback ":<C-u>edit ~/.config/nvim/fnl/main.fnl<CR>"}}})

;; Goto alternative/[p]revious file
;; (keymaps! {:n {:<C-p> :<C-^>}})

(keymaps! {:n {;; Move between diagnostics
               ;; "[d" vim.diagnostic.goto_prev
               ;; "]d" vim.diagnostic.goto_next
               ;; Buffer navigation
               "]b" {:desc "Next Buffer" :callback ":<C-u>bnext<CR>"}
               "[b" {:desc "Previous Buffer" :callback ":<C-u>bprev<CR>"}
               "]q" {:desc "Next Quickfix" :callback ":<C-u>cnext<CR>"}
               "[q" {:desc "Previous Quickfix" :callback ":<C-u>cprev<CR>"}
               "]Q" {:desc "Last Quickfix" :callback ":<C-u>clast<CR>"}
               "[Q" {:desc "First Quickfix" :callback ":<C-u>cfirst<CR>"}
               ;; Navigate quickfix and location lists
               :<leader>oq {:desc "Open Quickfix" :callback ":<C-u>copen<CR>"}
               :<leader>qq {:desc "Close Quickfix"
                            :callback ":<C-u>cclose<CR>"}
               "]l" {:desc "Next Location" :callback ":<C-u>lnext<CR>"}
               "[l" {:desc "Previous Location" :callback ":<C-u>lprev<CR>"}
               "]L" {:desc "Last Location" :callback ":<C-u>llast<CR>"}
               "[L" {:desc "First Location" :callback ":<C-u>lfirst<CR>"}
               :<leader>ol {:desc "Open Location List"
                            :callback ":<C-u>lopen<CR>"}
               :<leader>ql {:desc "Close Location List"
                            :callback ":<C-u>lclose<CR>"}}})

;; Better folds
;; (add! [:kevinhwang91/nvim-ufo :kevinhwang91/promise-async]
;;       #(setup :ufo {:provider_selector #[:treesitter :indent]}))

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

(keymaps! {[:n :v] {:go {:desc "Open URL Externally"
                         :callback :<Plug>OpenExternally}}})

;; Write and quit
(keymaps! {:n {:<leader> {:w {:desc "Write File" :callback ":<C-u>w<cr>"}
                          :qV {:desc "Quit All" :callback ":<C-u>qall<CR>"}
                          :qb {:desc "Delete Buffer" :callback ":bd<CR>"}
                          :qw {:desc "Quit Window" :callback ":q<CR>"}
                          ;; "sudo write"-trick via polkit agent
                          :W {:desc "Write as Root"
                              :callback ":<C-u>w !pkexec tee % >/dev/null<CR>"}}}})

;; Navigate history containing substring
;; FIXME: debug
(keymaps! {:c {:<M-p> {:desc "Previous History" :callback #(feed :<Up>)}
               :<M-n> {:desc "Next History" :callback #(feed :<Down>)}}})

;; FIXME: fzf-native needs `make` to be run, vim.pack.add currently does not support this
(use! [:nvim-telescope/telescope.nvim
       :nvim-telescope/telescope-fzf-native.nvim
       :nvim-lua/plenary.nvim] {:reload :setup/telescope})

;; Leap with s
(use! :ggandor/leap.nvim
      {:setup {:leap {:safe_labels {}}}
       :config #(let [{: set_default_keymaps} (require :leap)]
                  (set_default_keymaps))})

;; Smooth scrolling
(use! :karb94/neoscroll.nvim
      {;; Disable default mappings
       :setup {:neoscroll {:mappings {}}}
       :map #(let [{: scroll : zt : zz : zb} (require :neoscroll)
                   get-height #(vim.api.nvim_win_get_height 0)]
               {:n {:<C-u> #(scroll (- vim.wo.scroll) {:duration 100})
                    :<C-d> #(scroll vim.wo.scroll {:duration 100})
                    :<C-b> #(scroll (- (get-height)) {:duration 250})
                    :<C-f> #(scroll (get-height) {:duration 250})
                    :zt #(zt {:half_win_duration 100})
                    :zz #(zz {:half_win_duration 100})
                    :zb #(zb {:half_win_duration 100})}})})

;; Split file explorer
(use! :stevearc/oil.nvim
      {:setup {:oil {:columns []
                     :use_default_keymaps false
                     :keymaps {:g? :actions.show_help
                               "<C-]>" :actions.select
                               "CR" :actions.select
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
                               :g. :actions.toggle_hidden}}}
       :map #(let [{: open} (require :oil)]
               {:n {:- {:desc "Open File Explorer" :callback open}}})})

;; TODO: Use fork until https://github.com/numToStr/Navigator.nvim/pull/35 is merged
;; (use! :Vinh-CHUC/Navigator.nvim
;;       {:setup {:Navigator nil}
;;        :map {[:n :t] (let [mk-lhs #(.. :<M- $1 :>)
;;                            mk-rhs #(. vim.cmd (.. :Navigator $1))]
;;                        (collect [dir keys (pairs {:Left [:h :left]
;;                                                   :Right [:l :right]
;;                                                   :Up [:k :up]
;;                                                   :Down [:j :down]})]
;;                          (values (vim.tbl_map mk-lhs keys) (mk-rhs dir))))}})

;; Navigate windows with Alt + vim keys
(keymaps! {[:n :t :i :v :s :x] (collect [_ key (ipairs [:h :j :k :l])]
                                 (values (.. "<M-" key ">") (.. "<C-w>" key)))})

;; Filetype plugins
(use! :elkowar/yuck.vim)

;; Treesitter indentation for fennel is messed up, so use this plugin for this
(use! :jaawerth/fennel.vim)

(use! :lervag/vimtex {:config (fn []
                                (let! vimtex_format_enabled 1)
                                (let! vimtex_quickfix_mode 0)
                                (let! vimtex_view_method :zathura)
                                (let! tex_flavor :latex))})

(keymaps! {:n {:<leader>on {:desc "Open Notes"
                            :callback #(let [notes-dir (.. vim.env.HOME :/Notes)]
                                         (vim.cmd.cd notes-dir)
                                         (vim.cmd.edit :index.md))}}})

;; Autocompletion and snippets

(use! [:L3MON4D3/LuaSnip :rafamadriz/friendly-snippets]
      ;; Setup custom snippets in separate module to not duplicate them when ; reloading the config
      ;; Find snippets defined by plugins
      {:config #(let [{:lazy_load load_from_vscode} (require :luasnip.loaders.from_vscode)
                      ; {:lazy_load load_from_lua} (require :luasnip.loaders.from_lua)
                      ]
                  (load_from_vscode {:exclude [:norg]})
                  ;; (setup :snippets)
                  ;; (load_from_lua)
                  )})

(use! [:hrsh7th/nvim-cmp
       :hrsh7th/cmp-nvim-lsp
       :hrsh7th/cmp-nvim-lua
       :kdheepak/cmp-latex-symbols
       :hrsh7th/cmp-path
       :hrsh7th/cmp-omni
       :hrsh7th/cmp-buffer
       ;; :rcarriga/cmp-dap
       :saadparwaiz1/cmp_luasnip] {:reload :setup/cmp})

;; Coding related stuff

;; Run current file, if applicable
;; (let [call #($1)
;;       ft-to-runner {:fennel #(vim.cmd.Fnlfile "%")
;;                     :python #(vim.api.nvim_command "w !python3")
;;                     :sh #(vim.api.nvim_command "w !bash")
;;                     :fish #(vim.api.nvim_command "w !fish")
;;                     :lua #(vim.api.nvim_command "lua dofile(vim.fn.expand('%'))")}]
;;   (nmap! :<leader>rr #(-?>> vim.o.filetype
;;                             (. ft-to-runner)
;;                             (call))))

(keymaps! {:n {"<leader>m" {:k {:desc "Make" :callback ":make!<CR>"}
                            :f {:desc "Make Flash"
                                :callback ":make! flash<CR>"}
                            :c {:desc "Make Clean"
                                :callback ":make! clean<CR>"}
                            :t {:desc "Make Test" :callback ":make! test<CR>"}
                            :b {:desc "Make Build"
                                :callback ":make! build<CR>"}}}})

;; (use! :neomake/neomake
;;       {:init (do
;;                ;; Appearance
;;                (let! neomake_error_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakeErrorSign"
;;                       })
;;                (let! neomake_warning_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakeWarningSign"
;;                       })
;;                (let! neomake_message_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakemessageSign"
;;                       })
;;                (let! neomake_info_sign
;;                      {:text "➤"
;;                       ; :texthl "NeomakeInfoSign"
;;                       }))
;;        :map #{:n {:<leader>n {:m {:desc "Neomake Run"
;;                                   :callback #(vim.cmd.Neomake)}
;;                               :c {:desc "Neomake Clean"
;;                                   :callback #(vim.cmd.NeomakeClean)}}}}
;;        :hl {:NeomakeErrorSign {:link :DiagnosticSignError}
;;             :NeomakeWarningSign {:link :DiagnosticSignWarn}
;;             :NeomakeMessageSign {:link :DiagnosticSignHint}
;;             :NeomakeInfoSign {:link :DiagnosticSignInfo}
;;             :NeomakeVirtualtextError {:link :DiagnosticError}
;;             :NeomakeVirtualtextWarning {:link :DiagnosticWarn}
;;             :NeomakeVirtualtextMessage {:link :DiagnosticHint}
;;             :NeomakeVirtualtextInfo {:link :DiagnosticInfo}}})

(use! [:nvim-treesitter/nvim-treesitter
       :nvim-treesitter/nvim-treesitter-textobjects]
      ;; "nvim-treesitter/playground"
      {:setup {:nvim-treesitter.configs {:highlight {:enable true}
                                         ;; :indent {:enable true}
                                         ;; :additional_vim_regex_highlighting [:fennel] 
                                         ;; Use with indent=true
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
                                                              :swap_next {:<leader>. "@parameter.inner"}
                                                              :swap_previous {"<leader>," "@parameter.inner"}}}}}
       :config (fn []
                 ;; FIXME: replace needs with :bundle or something? Could be done outside of internal.fnl
                 (set! foldmethod :expr)
                 ;; Use treesitter-based folds
                 (set! foldexpr "nvim_treesitter#foldexpr()"))})

(vim.diagnostic.config {:signs {:text {vim.diagnostic.severity.ERROR ""
                                       ; ""
                                       vim.diagnostic.severity.WARN ""
                                       ; ""
                                       vim.diagnostic.severity.INFO ""
                                       ; ""
                                       vim.diagnostic.severity.HINT ""
                                       ; "󰌵"
                                       }}})

;; Avoids "Unrecognized option: 'write-mode'" error.
(let! rustfmt_detect_version 1)

(use! [:neovim/nvim-lspconfig :zk-org/zk-nvim :mrcjkb/rustaceanvim]
      {:reload :setup/lspconfig})

(let [cap-to-handler {:textDocument/hover vim.lsp.handlers.hover
                      :textDocument/signatureHelp vim.lsp.handlers.signature_help}]
  (each [cap handler (pairs cap-to-handler)]
    (tset vim.lsp.handlers cap (vim.lsp.with handler {:border _G.border-type}))))

(use! "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
      {:setup {:lsp_lines nil}
       :map #(let [{: toggle} (require :lsp_lines)
                   toggle-with-inline #(let [lines-on (toggle)]
                                         (vim.diagnostic.config {:virtual_text (not lines-on)}))]
               {:n {:<leader>tl {:desc "Toggle LSP Lines"
                                 :callback toggle-with-inline}}})})

(use! [:mfussenegger/nvim-dap
       :nvim-neotest/nvim-nio
       ; needed by nvim-dap-ui
       :rcarriga/nvim-dap-ui
       :theHamsta/nvim-dap-virtual-text
       ;; :mfussenegger/nvim-dap-python
       ]
      {:map #(let [dap (require :dap)
                   dapui (require :dapui)]
               {:n {:<leader> {"t" {"d" {:desc "Toggle Debug REPL"
                                         :callback dap.repl.toggle}
                                    "D" {:desc "Toggle Debug UI"
                                         :callback dapui.toggle}}
                               "<CR>" {:desc "Run to Cursor"
                                       :callback dap.run_to_cursor}
                               "g" {:b {:desc "Toggle Breakpoint"
                                        :callback dap.toggle_breakpoint}
                                    :l {:desc "List Breakpoints"
                                        :callback dap.list_breakpoints}
                                    :e {:desc "Eval Expression"
                                        :callback #(dapui.eval nil
                                                               {:enter true})}
                                    :s {:desc "Step Into"
                                        :callback dap.step_into}
                                    :n {:desc "Step Over"
                                        :callback dap.step_over}
                                    :o {:desc "Step Out"
                                        :callback dap.step_out}
                                    :r {:desc "Restart" :callback dap.restart}
                                    :R {:desc "Run Last"
                                        :callback dap.run_last}
                                    :c {:desc "Continue"
                                        :callback dap.continue}
                                    :t {:desc "Terminate"
                                        :callback dap.terminate}
                                    :D {:desc "Clear Breakpoints"
                                        :callback dap.clear_breakpoints}}}}})
       :setup {:nvim-dap-virtual-text {:virt_text_pos :inline}
               :dapui {}
               ;; :dap-python :python
               }
       :config (let [adapter-configs {:gdb {:type :executable
                                            :command :gdb
                                            :args [:--interpreter=dap
                                                   :--eval-command
                                                   "set print pretty on"]}
                                      :lldb {:type :executable
                                             :name :lldb
                                             :command :/usr/bin/lldb-dap}}
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

(use! :bR3iN/emanote.nvim
      {:command #(let [{: start : stop} (require :emanote-live)
                       emanote_url "http://localhost:8080"]
                   {:EmanoteConnect (fn []
                                      (start {:port 8000 : emanote_url})
                                      (spawn :qutebrowser
                                             {:args [:--target
                                                     :window
                                                     emanote_url]}))
                    :EmanoteDisconnect (partial stop 500)})})

;; Forked as plugin doesn't have an API for custom keybindings
(use! :bR3iN/jupynium.nvim {:setup {:jupynium {}}})

(use! :folke/trouble.nvim
      {:setup {:trouble {:icons {:indent {:last "╰╴"}}}}})

(keymaps! {:n {:<leader>ot {:desc "Open Trouble Diagnostics"
                            :callback ":<C-u>Trouble diagnostics focus<CR>"}
               :<leader>qt {:desc "Close Trouble Diagnostics"
                            :callback ":<C-u>Trouble diagnostics close<CR>"}}})

(use! :lewis6991/gitsigns.nvim {:setup {:gitsigns {:signcolumn false}}})

(autocmd! {:event [:BufEnter]
           :pattern "*"
           :callback (vim.schedule_wrap #(when (starts-with vim.bo.filetype
                                                            "dap")
                                           (case vim.bo.buftype
                                             :prompt (vim.schedule #(vim.cmd.startinsert)))))})

(ft-autocmd! {:fennel (fn []
                        (keymaps! {:n {:gqq {:desc "Format fennel file"
                                             :callback (.. ":<C-u>w<CR>:"
                                                           "! fnlfmt --fix %<CR><CR>")}}
                                   :x {:gq {:desc "Format fennel selection"
                                            :callback (.. ":'<,'>! fnlfmt -<CR>")}}}
                                  {:buffer true})
                        (setl! commentstring ";; %s")
                        (let [{: find_files} (require :telescope.builtin)
                              {: cache-prefix} (require :hotpot.api.cache)]
                          (setl- iskeyword ".") ; Search in cache ; (keymaps! {:n {"<leader>" {"f" {"c" #(find_files {:cwd (cache-prefix) :hidden true})}}}} {:buffer true :silent true})
                          ))
              :c #(setl! shiftwidth 2)
              :cpp #(setl! shiftwidth 2)
              :dap-repl #(vim.cmd "abbreviate <buffer> e -exec")
              :markdown (fn []
                          (setl! nobackup)
                          (setl! nowritebackup)
                          (setl+ iskeyword "\\")
                          (vim.cmd "abbreviate <buffer> \\bf \\mathbf
                                                  abbreviate <buffer> \\rm \\mathrm
                                                  abbreviate <buffer> \\cal \\mathcal
                                                  abbreviate <buffer> \\bb \\mathbb
                                                  abbreviate <buffer> \\frak \\mathfrak
                                                  abbreviate <buffer> iff if and only if"))
              :org (fn []
                     (let [{: action} (require :orgmode)]
                       (keymaps! {:i {:<C-CR> {:desc "Org meta return"
                                               :callback #(action :org_mappings.meta_return)}}}
                                 {:buffer true}))
                     (vim.cmd "abbreviate -- - [ ]"))
              :qf #(keymaps! {:n {:q {:desc "Close quickfix"
                                      :callback :<Cmd>cclose<CR>}}}
                             {:buffer true})
              ;; FIXME: looks weird
              :rust #(keymaps! {:n {"<leader>" {"c" {"r" {:desc "Cargo run"
                                                          :callback ":<C-u>Cargo run<CR>"}
                                                     "b" {:desc "Cargo build"
                                                          :callback ":<C-u>Cargo build<CR>"}
                                                     "t" {:desc "Cargo test"
                                                          :callback ":<C-u>Cargo test<CR>"}
                                                     "l" {:desc "Cargo clippy"
                                                          :callback ":<C-u>Cargo clippy<CR>"}}
                                                "r" {"f" {:desc "Format file"
                                                          :callback ":<C-u>RustFmt<CR>"}}}}
                                :v {"<leader>" {"r" {"f" {:desc "Format range"
                                                          :callback ":RustFmtRange<CR>"}}}}}
                               {:buffer true})
              :sh #(setl! shiftwidth 4)
              :zsh #(setl! shiftwidth 4)})

;; Floating preview in quickfix window
(use! :kevinhwang91/nvim-bqf
      {:hl {:BqfPreviewBorder {:bg :NONE :fg colors.mid}}
       :setup {:bqf {:func_map {:fzffilter "" :open "<C-]>"}
                     :preview {:winblend 0}}}})

(use! :lukas-reineke/indent-blankline.nvim
      {:hl {:IblScope {:fg colors.dark_green :bold true}
            :IblIndent {:fg colors.bg2 :bold true}}
       :setup {:ibl {:enabled false}}
       :map #{:n {:<leader>ti {:desc "Toggle Indent Lines"
                               :callback vim.cmd.IBLToggle}}}})

(reload :setup/which-key)

(vim.diagnostic.config {:float {:border _G.border-type}})

(keymaps! {:n {:<leader> {"k" #(vim.lsp.buf.hover {:border _G.border-type})
                          "t" {"h" #(vim.lsp.inlay_hint.enable (not (vim.lsp.inlay_hint.is_enabled)))}}}})

;; Helix-like bindings
(keymaps! {:v {"<leader>" {"y" {:desc "Yank to Clipboard" :callback "\"+y"}
                           "p" {:desc "Paste from Clipboard" :callback "\"+p"}
                           "P" {:desc "Paste from Clipboard before cursor"
                                :callback "\"+P"}}}
           :n {"g" {"d" {:desc "Goto Definition" :callback "<C-]>"}
                    "D" {:desc "Goto Declaration"
                         :callback vim.lsp.buf.declaration}
                    "t" {:desc "Goto Type Definition"
                         :callback vim.lsp.buf.type_definition}
                    "i" {:desc "Goto Implementation"
                         :callback vim.lsp.buf.implementation}
                    "r" {:desc "Goto References"
                         :callback vim.lsp.buf.references}
                    "a" {:desc "Goto Alternate File" :callback "<C-^>"}}
               "<leader>" {"rn" {:desc "Rename Symbol"
                                 :callback vim.lsp.buf.rename}
                           "w" {:desc "Window Commands" :callback "<C-w>"}
                           "s" {:desc "Save File" :callback ":update<CR>"}
                           "S" {:desc "Save All Files" :callback ":wall<CR>"}
                           "y" {:desc "Yank to Clipboard" :callback "\"+y"}
                           "p" {:desc "Paste from Clipboard" :callback "\"+p"}
                           "P" {:desc "Paste from Clipboard before Cursor"
                                :callback "\"+P"}
                           "a" {:desc "Code Action"
                                :callback vim.lsp.buf.code_action}
                           "o" {"d" vim.diagnostic.open_float}
                           "x" {"R" {:desc "Reload Config"
                                     :callback #(do
                                                  (vim.print "Reloading config")
                                                  (dofile vim.env.MYVIMRC))}}}}})

;; Window
;; FIXME: cleanup
(let [keys (let [res {}]
             (for [i 1 9]
               (set (. res (tostring i)) i))
             res)]
  (keymaps! {:n {"<leader>" {"" (collect [as_str _ (pairs keys)]
                                  (values as_str
                                          {:desc (.. "Focus Window " as_str)
                                           :callback (.. as_str "<C-w>w")}))
                             "t" (collect [as_str _ (pairs keys)]
                                   (values as_str
                                           {:desc (.. "Focus Tab " as_str)
                                            :callback (.. as_str "gt")}))
                             "q" (collect [as_str n (pairs keys)]
                                   (values as_str
                                           {:desc (.. "Kill Window " as_str)
                                            :callback #(vim.api.nvim_win_close (vim.fn.win_getid n)
                                                                               false)}))}
                 "<M-n>" {:desc "Go to next tab" :callback vim.cmd.tabnext}
                 "<M-p>" {:desc "Go to previous tab"
                          :callback vim.cmd.tabprevious}
                 "<M-c>" {:desc "Create New Tab" :callback vim.cmd.tabnew}
                 "" (collect [as_str _ (pairs keys)]
                      (values (.. "<M-" as_str ">")
                              {:desc (.. "Focus Tab " as_str)
                               :callback (.. as_str "gt")}))}}))

;; Always move go to marked column
(keymaps! {:n {"'" "`"
               "<leader>oC" ":e ~/.config/<CR>"}})

(reload :setup/terminal)
