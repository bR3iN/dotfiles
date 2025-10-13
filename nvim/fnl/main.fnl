(import-macros {: opt! : setl! : setl+ : setl- : let! : with-cb} :utils.macros)

(local {: hl!
        : reload
        : use!
        : setup
        : keymaps!
        : autocmd!
        : UNBIND
        : dispatchables!
        : dispatch!
        : ft!
        : lsps!
        : starts-with} (require :utils))

;; Export for easier use, especially in .nvim.fnl
(set _G.use! use!)
(set _G.keymaps! keymaps!)
(set _G.autocmd! autocmd!)
(set _G.lsps! lsps!)

(local {: spawn} (require :utils.async))
(local {: mk-op!} (require :utils.operator))
(local {: mix : get-named} (require :utils.colors))

(local {: name} (require :base16-colors))
(local colors (get-named))

(fn feed [keys]
  (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes keys true true true)
                         :m false))

(dispatchables! :g {:code_action vim.lsp.buf.code_action
                    :fmt_range #(feed :gq)})

(keymaps! {:n {:gqq #(dispatch! :fmt_file)} :x {:gq #(dispatch! :fmt_range)}})

;; General Options and Keymaps

;; Used for CursorHold autocmd
(opt! updatetime 1000)

;; set leader keys
(let! mapleader " ")
(let! maplocalleader " c")

;; Enable dialogs
(opt! confirm true)

;; Needed for hl-CursorLineNr
(opt! cursorline true)

;; Ignore case for lowercase searches
(opt! ignorecase true)
(opt! smartcase true)

;requires `ignorecase`

;; Live preview in split of :s/[...] invocations
(opt! inccommand :split)

;; Enable mouse support
(opt! mouse :a)

;; Show line number of current line, relative line numbers for the rest
(opt! number true)
(opt! relativenumber true)

;; Show cursor coordinates in status bar
(opt! ruler true)

;; Set split behaviour
(opt! splitbelow true)
(opt! splitright true)

;; Ignore case for wildcards expansion
(opt! wildignorecase true)
;; First complete longest substring and open wildmenu, then cycle through matches
(opt! wildmode "longest:full,full")

;; Undo behaviour
(opt! undofile true)
(opt! undodir (.. (vim.fn.stdpath :data) :undo))

;; Wrap behaviour
(opt! breakindent true)
(opt! wrap true)

;; Default tab behaviour
(opt! shiftwidth 4)
(opt! tabstop 4)
(opt! expandtab true)

;; Start with folds expanded
(opt! foldlevelstart 99)

(keymaps! {:i {;; Correctly indent when pasting multiple lines in insert mode
               :<C-r> {:desc "Paste (auto-indent)" :callback :<C-r><C-o>}
               ;; Capitalize word in front of cursor
               :<C-u> {:desc "Capitalize Word" :callback :<Esc>viwUea}}})

;; Clear highlight search
;; (keymaps! {:n {:<C-l> {:desc "Clear Search Highlight"
;;                        :callback ":<C-u>nohlsearch<CR><C-l>"}}})
(keymaps! {:n {:<ESC> {:desc "Clear Search Highlight"
                       :callback ":<C-u>nohlsearch<CR><C-l>"}}})

;; Select until end of line (like `C`, `D` and `Y`)
(keymaps! {:n {:<leader>v {:desc "Select to End of Line" :callback :vg_}}})

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

(opt! signcolumn :yes)
(opt! termguicolors true)

;; Remove redundant mode prompt in insert area
(opt! showmode true)

(opt! cmdheight 1)

(reload :setup/theme)

;; Always display tab line
(opt! showtabline 1)
;; Show statusline on all windows
(opt! laststatus 3)

;; after loading theme
(use! [:rebelot/heirline.nvim :SmiteshP/nvim-navic :b0o/incline.nvim]
      {:reload [:setup/statusline]})

(opt! fillchars "vert:│,wbr: ")
(opt! conceallevel 2)
(opt! scrolloff 2)
(opt! linebreak true)

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
               ["<C-v><C-[>" :<C-v><Esc>] {:desc "Send Escape to Terminal" :callback :<Esc>}}})

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
                            :callback ":<C-u>lclose<CR>"}
               ;; TODO: use leader+]/[ instead of +o/t?
               "<leader>" {"]w" #(set vim.wo.wrap true)
                           "[w" #(set vim.wo.wrap false)}}})

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
       :keymaps {[:n :v] {:s "<Plug>(leap-anywhere)"}}})

;; Smooth scrolling
(use! :karb94/neoscroll.nvim
      {;; Disable default mappings
       :setup {:neoscroll {:mappings {}}}
       :keymaps #(let [{: scroll : zt : zz : zb} (require :neoscroll)
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
       :keymaps #(let [{: open} (require :oil)]
                   {:n {:- {:desc "Open File Explorer" :callback open}}})})

;; TODO: Use fork until https://github.com/numToStr/Navigator.nvim/pull/35 is merged
;; (use! :Vinh-CHUC/Navigator.nvim
;;       {:setup {:Navigator nil}
;;        :keymaps {[:n :t] (let [mk-lhs #(.. :<M- $1 :>)
;;                            mk-rhs #(. vim.cmd (.. :Navigator $1))]
;;                        (collect [dir keys (pairs {:Left [:h :left]
;;                                                   :Right [:l :right]
;;                                                   :Up [:k :up]
;;                                                   :Down [:j :down]})]
;;                          (values (vim.tbl_map mk-lhs keys) (mk-rhs dir))))}})

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
;;        :keymaps #{:n {:<leader>n {:m {:desc "Neomake Run"
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
                 (opt! foldmethod :expr)
                 ;; Use treesitter-based folds
                 (opt! foldexpr "nvim_treesitter#foldexpr()"))})

(vim.diagnostic.config {:signs {:text {vim.diagnostic.severity.ERROR ""
                                       ; ""
                                       vim.diagnostic.severity.WARN ""
                                       ; ""
                                       vim.diagnostic.severity.INFO ""
                                       ; ""
                                       vim.diagnostic.severity.HINT ""
                                       ; "󰌵"
                                       }}})

(reload :setup/dap)

(use! [:neovim/nvim-lspconfig :zk-org/zk-nvim] {:reload :setup/lspconfig})
(let [cap-to-handler {:textDocument/hover vim.lsp.handlers.hover
                      :textDocument/signatureHelp vim.lsp.handlers.signature_help}]
  (each [cap handler (pairs cap-to-handler)]
    (tset vim.lsp.handlers cap (vim.lsp.with handler {:border _G.border-type}))))

;; Avoids "Unrecognized option: 'write-mode'" error.
(let! rustfmt_detect_version 1)

(use! [:mrcjkb/rustaceanvim]
      {:init #(let [lsp-config {:rust-analyzer {:cargo {:buildScripts {:enable true}}
                                                :procMacro {:enable true}
                                                :imports {;; Merge auto imports on the module level
                                                          :granularity {:group :module}
                                                          ;; Prefer relative auto import paths
                                                          :prefix :self}}}]
                (set vim.g.rustaceanvim
                     {:server {:default_settings lsp-config}}))})

;; (use! "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
;;       {:setup {:lsp_lines {}}
;;        :keymaps #(let [{: toggle} (require :lsp_lines)
;;                        toggle-with-inline #(let [lines-on (toggle)]
;;                                              (vim.diagnostic.config {:virtual_text (not lines-on)}))]
;;                    {:n {:<leader>tl {:desc "Toggle LSP Lines"
;;                                      :callback toggle-with-inline}}})})

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

; (set (. vim.lsp.handlers :textDocument/publishDiagnostics) (vim.lsp.with vim.lsp.diagnostic.on_publish_diagnostics {:update_in_insert true}))

(use! :lewis6991/gitsigns.nvim
      (let [signs {:add {:text "┃"}
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
         :keymaps {[:n :v] {"]g" #(vim.cmd.Gitsigns :nav_hunk :next)
                            "[g" #(vim.cmd.Gitsigns :nav_hunk :prev)}
                   :n {:<leader> {:g {:a #(vim.cmd.Gitsigns :stage_hunk)
                                      :RRR #(vim.cmd.Gitsigns :reset_hunk)
                                      :h #(vim.cmd.Gitsigns :preview_hunk_inline)}
                                  :tg {:l #(do
                                             ;; (vim.cmd.Gitsigns :toggle_linehl)
                                             (vim.cmd.Gitsigns :toggle_word_diff))}
                                  :og {:b #(vim.cmd.Gitsigns :blame)}}}}}))

;; (autocmd! {:event [:BufEnter]
;;            :pattern "*"
;;            :callback (vim.schedule_wrap #(when (starts-with vim.bo.filetype
;;                                                             "dap")
;;                                            (case vim.bo.buftype
;;                                              :prompt (do
;;                                                          (vim.print "")
;;                                                          (vim.cmd.startinsert)))))})

(ft! {:fennel (fn []
                (dispatchables! :b
                                {:fmt_range #(feed ":'<,'>! fnlfmt -<CR>")
                                 :fmt_file #(feed ":<C-u>w<CR>:! fnlfmt --fix %<CR><CR>")})
                (keymaps! {; :n {:gqq {:desc "Format fennel file"
                           ;           :callback (.. ":<C-u>w<CR>:"
                           ;                         "! fnlfmt --fix %<CR><CR>")}}
                           ; :x {:gq {:desc "Format fennel selection"
                           ;          :callback (.. ":'<,'>! fnlfmt -<CR>")}}
                           } {:buffer true})
                (setl! commentstring ";; %s")
                (let [{: find_files} (require :telescope.builtin)
                      {: cache-prefix} (require :hotpot.api.cache)]
                  (setl- iskeyword ".") ; Search in cache ; (keymaps! {:n {"<leader>" {"f" {"c" #(find_files {:cwd (cache-prefix) :hidden true})}}}} {:buffer true :silent true})
                  ))
      :c #(setl! shiftwidth 2)
      :cpp #(setl! shiftwidth 2)
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
      :oil (fn []
             (keymaps! {:n {"<ESC>" "<C-^>"}} {:buffer true}))
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
      :rust #(let [cmd (fn [action]
                         #(vim.cmd.RustLsp action))]
               (dispatchables! :b
                               {:fmt_file vim.cmd.RustFmt
                                :code_action (cmd :codeAction)
                                :start_debugger (cmd :debuggables)
                                :debug_at_cursor (cmd :debug)})
               (keymaps! {:n {"<C-CR>" (cmd :run)
                              "<leader>" {"c" {"r" {:desc "Cargo run"
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
                         {:buffer true}))
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
       :keymaps #{:n {:<leader>ti {:desc "Toggle Indent Lines"
                                   :callback vim.cmd.IBLToggle}}}})

(reload :setup/which-key)

(vim.diagnostic.config {:float {:border _G.border-type}
                        :virtual_lines false
                        :virtual_text {:current_line true}})

(do
  (var current-state-idx 1)
  (let [toggle-states [;; Only virtual text in current lines
                       {:virtual_lines false
                        :virtual_text {:current_line true}}
                       ;; Only lsp-lines in current lines
                       {:virtual_lines {:current_line true}
                        :virtual_text false}
                       ;; Lsp lines in all lines
                       {:virtual_lines true :virtual_text false}]
        ;; Cycle states
        next-state (fn []
                     (if (= current-state-idx (length toggle-states))
                         1
                         (+ current-state-idx 1)))
        set-state (fn [idx]
                    (set current-state-idx idx)
                    (vim.diagnostic.config (. toggle-states idx)))]
    ;; Set initial state
    (set-state 1)
    (keymaps! {:n {:<leader>t {;; Keybind to cycle states
                               :l {:callback #(set-state (next-state))}
                               ;; Go back to first state
                               :L {:callback #(set-state 1)}}}})))

(keymaps! {:n {:<leader> {"k" #(vim.lsp.buf.hover {:border _G.border-type})
                          "t" {"h" #(vim.lsp.inlay_hint.enable (not (vim.lsp.inlay_hint.is_enabled)))}}}})

;; Easier clipboard access
(keymaps! {[:n :x] {"<leader>" {"y" {:desc "Yank to Clipboard"
                                     :callback "\"+y"}
                                "p" {:desc "Paste from Clipboard"
                                     :callback "\"+p"}
                                "P" {:desc "Paste from Clipboard before cursor"
                                     :callback "\"+P"}}}
           :i {:<C-p> {:desc "Paste from Clipboard" :callback :<C-r>+}}})

;; Helix-like bindings
(keymaps! {:n {"g" {"d" {:desc "Goto Definition" :callback "<C-]>"}
                    "D" {:desc "Goto Declaration"
                         :callback vim.lsp.buf.declaration}
                    "t" {:desc "Goto Type Definition"
                         :callback vim.lsp.buf.type_definition}
                    "y" {:desc "Goto Type Definition"
                         :callback vim.lsp.buf.type_definition}
                    "i" {:desc "Goto Implementat[ion"
                         :callback vim.lsp.buf.implementation}
                    "r" {:desc "Goto References"
                         :callback vim.lsp.buf.references}
                    "a" {:desc "Goto Alternate File" :callback "<C-^>"}}
               "<C-w>" {"V" {:callback #(do
                                          (vim.cmd.vsplit)
                                          (vim.cmd.terminal {:args [:fish]}))}
                        "S" {:callback #(do
                                          (vim.cmd.split)
                                          (vim.cmd.terminal {:args [:fish]}))}}
               ["<leader>w"] {:desc "Window Commands"
                              :callback "<C-w>"
                              :remap true}
               "<leader>" {"rn" {:desc "Rename Symbol"
                                 :callback vim.lsp.buf.rename}
                           "s" {:desc "Save File" :callback ":update<CR>"}
                           "S" {:desc "Save All Files" :callback ":wall<CR>"}
                           "a" {:desc "Code Action"
                                :callback #(dispatch! :code_action)}
                           "A" {:desc "Code Lenses"
                                :callback vim.lsp.codelens.run}
                           "o" {"d" vim.diagnostic.open_float}
                           "x" {"R" {:desc "Reload Config"
                                     :callback #(do
                                                  (vim.print "Reloading config")
                                                  (dofile vim.env.MYVIMRC))}}}}})

;; Window and tab management
(let [keys (let [res {}]
             (for [i 1 9]
               (set (. res (tostring i)) i))
             res)]
  (keymaps! {:n {"<leader>" {;; <n>: go to window <n>
                             "" (collect [as_str _ (pairs keys)]
                                  (values as_str
                                          {:desc (.. "Focus Window " as_str)
                                           :callback (.. as_str "<C-w>w")}))
                             ;; t+<n>: go to tab <n>
                             "t" (collect [as_str _ (pairs keys)]
                                   (values as_str
                                           {:desc (.. "Focus Tab " as_str)
                                            :callback (.. as_str "gt")}))
                             ;; q+<n>: delete window <n>
                             "q" (collect [as_str n (pairs keys)]
                                   (values as_str
                                           {:desc (.. "Kill Window " as_str)
                                            :callback #(vim.api.nvim_win_close (vim.fn.win_getid n)
                                                                               false)}))}
                 "" (collect [as_str _ (pairs keys)]
                       (values (.. "<M-" as_str ">")
                               {:desc (.. "Focus Tab " as_str)
                                :callback (.. as_str "gt")}))
                 ;; "<M-c>" {:desc "Create New Tab" :callback vim.cmd.tabnew}
                 }
             ;; TODO: experimental: Alt+<n> to go to tab <n>
             ;; FIXME: deduplicate, also see above
             :i (collect [as_str _ (pairs keys)]
                       (values (.. "<M-" as_str ">")
                               {:desc (.. "Focus Tab " as_str)
                                :callback (.. "<esc>" as_str "gt")}))
             :t (collect [as_str _ (pairs keys)]
                       (values (.. "<M-" as_str ">")
                               {:desc (.. "Focus Tab " as_str)
                                :callback (.. "<C-\\><C-n>" as_str "gt")}))}))

(keymaps! {:n {;; Always move go to marked column
               ;; "'" "`"
               ;; Open configuration
               "<leader>oC" ":e ~/.config/<CR>"}})

;; Swap j <-> gj, k <-> gk
(keymaps! {[:n :v] {"<C-j>" "gj" "<C-k>" "gk"}})

(reload :setup/terminal)

(opt! exrc true)
;; Discover and safely run .nvim.fnl files
(reload :setup/exrc)

(fn readchar []
  (-> (vim.fn.getchar)
      (vim.fn.nr2char)))

;; Sets an uppcase/global mark by reading a lowercase one
(fn set-global-mark []
  (let [char (-> (readchar)
                 (string.upper))]
    (feed (.. :m char))))

;; Go to an uppcase/global mark by reading a lowercase one
(fn goto-global-mark [in-column]
  (let [char (-> (readchar)
                 (string.upper))]
    (feed (.. (if in-column "`" "'") char))))

(keymaps! {[:n :v] {:<leader> {:m set-global-mark
                               "'" (partial goto-global-mark false)
                               "`" (partial goto-global-mark true)}}})
