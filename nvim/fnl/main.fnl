(import-macros {: set! : setl! : setl+ : setl- : setg! : set+ : let! : with-cb}
               :utils.macros)

(local {: hl! : reload : keymaps! : autocmd! : use!} (require :utils))
(local {: spawn} (require :utils.async))
(local {: mk-op!} (require :utils.operator))

(local {: colors : name} (require :base16-colors))

(fn feed [keys]
  (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes keys true true true)
                         :m true))

(fn setup [mod ?opts] ; Loads module and calls its `setup` function
  (let [{: setup} (require mod)]
    (setup (or ?opts {}))))

;; General Options and Keymaps

(set! exrc)

; set leader keys
(let! mapleader " ")
(let! maplocalleader " ")

; Enable dialogs
(set! confirm)

; Needed for hl-CursorLineNr
(set! cursorline)

; Ignore case for lowercase searches
(set! ignorecase)
(set! smartcase)

;requires `ignorecase`

(set! inccommand :split)

; Enable mouse support
(set! mouse :a)

; Show line number of current line, relative line numbers for the rest
(set! number)
(set! relativenumber)

; Show cursor coordinates in status bar
(set! ruler)

; Set split behaviour
(set! splitbelow)
(set! splitright)

; Ignore case for wildcards expansion
(set! wildignorecase)
; First complete longest substring and open wildmenu, then cycle through matches
(set! wildmode ["longest:full" :full])

; Undo behaviour
(set! undofile)
(set! undodir (.. (vim.fn.stdpath :data) :undo))

; Wrap behaviour
(set! breakindent)
(set! wrap)

; Default tab behaviour
(set! shiftwidth 4)
(set! tabstop 4)
(set! expandtab)

; Start with folds expanded
(set! foldlevelstart 99)

; Correctly indent when pasting multiple lines in insert mode
(keymaps! {:i {:<C-r> :<C-r><C-o>}})

; Break insert mode; similar to <C-g>u but also "resets" <C-a>
; TODO: experimental
(keymaps! {:i {:<C-q> :<Esc>a}})

; Clear highlight search
(keymaps! {:n {:<C-l> ":<C-u>nohlsearch<CR><C-l>"}})

; Capitalize word in front of cursor
(keymaps! {:i {:<C-u> :<Esc>viwUea}})

; Select until end of line (like `C`, `D` and `Y`)
(keymaps! {:n {:<leader>v :vg_}})

(keymaps! {:v {:<C-y> "\"+y"}})
(keymaps! {:i {:<C-p> :<C-r>+}})

(use! :tpope/vim-repeat)
(use! :tpope/vim-commentary)

(use! :windwp/nvim-autopairs
      {:setup {:nvim-autopairs {:enable_check_bracket_line false
                                :map_c_h true
                                :map_c_w true}}})

(use! :kylechui/nvim-surround
      {:setup {:nvim-surround {:keymaps {:insert :<C-s>
                                         :insert_line :<C-s><C-s>}}}})

; (vim.keymap.del :i "<C-s>")
; (add! "tpope/vim-surround"
;       (fn []
;         ; Find numbers with `:echo char2nr("B")`
;         ; "B"
;         (let! surround_66  "{\r}\1\1")))

(autocmd! :init_fnl
          [; Autoreload config files on save
           {:event :BufWritePost
            :pattern (.. vim.env.HOME
                         "/.{dotfiles,config}/nvim/*.{vim,lua,fnl}")
            :callback #(dofile vim.env.MYVIMRC)}
           ; Don't create undofiles for temporary files
           {:event :BufWritePre :pattern :/tmp/* :callback #(setl! noundofile)}
           {:event :BufWritePre
            :pattern "~/.crypt/*"
            :callback #(setl! noundofile)}
           ; Highlight on yank
           {:event :TextYankPost
            :pattern "*"
            :callback #(vim.highlight.on_yank {:higroup :IncSearch
                                               :timeout 150})}])

;; Package management

; (nmap! :<leader>pu :<Plug>PkgUpdate)
; (nmap! :<leader>pc :<Plug>PkgClean)
; (nmap! :<leader>pl :<Plug>PkgList)
; (nmap! :<leader>op ":edit ~/.local/share/nvim/site/pack/pkgs/start/<CR>")

;; Appearance

(set! termguicolors)

; Show statusline on all windows

(set! cmdheight 1)
(set! laststatus 2)

; Custom color setup; load colorscheme description (name + base16 colors)
(use! [:bR3iN/base16.nvim
       :folke/tokyonight.nvim
       :shaunsingh/nord.nvim
       :ellisonleao/gruvbox.nvim
       :rebelot/kanagawa.nvim
       :catppuccin/nvim]
      {:autocmds #{:ColorScheme {:callback (let [{: darken} (require :base16.utils)
                                                 cb #(do
                                                       ;; Highlights overrides and groups for local plugins
                                                       (hl! :FloatBorder
                                                            {:extend true
                                                             :fg colors.base03})
                                                       (hl! :WinSeparator
                                                            {:extend true
                                                             :fg colors.base02})
                                                       (hl! :SignColumn
                                                            {:extend true
                                                             :bg nil})
                                                       ;; (hl-ext! :LineNrAbove {:bg colors.base01}) 
                                                       ;; (hl-ext! :LineNrBelow {:bg colors.base01}) 
                                                       ;; (hl-ext! :CursorLineNr {:bg (darken colors.base02 0.1)})
                                                       (hl! :CursorLine
                                                            {:extend true
                                                             :bg (darken colors.base02
                                                                         0.1)})
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
       :init #(do
                (let! base16_colors_lua :base16-colors)
                (let! nord_disable_background true))
       :setup {:tokyonight {:transparent true}
               :gruvbox {:transparent_mode true}
               :kanagawa {:transparent true}}
       :config #(let []
                  ;; Decide if we use an external colorscheme or our own base16-based one
                  (case name
                    "Tokyonight Moon"
                    (vim.cmd.colorscheme :tokyonight-moon)
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

;; after scheme
(use! [:rebelot/heirline.nvim :SmiteshP/nvim-navic]
      {:setup {:setup/heirline {}}})

(set! fillchars {:vert "│"})
(set! conceallevel 2)
; (set! cmdheight 2)
(set! scrolloff 1)
(set! linebreak)

(require :plugin.highlight-trailing-whitespace)

; Highlights hex color codes with their color
(use! :NvChad/nvim-colorizer.lua
      {:setup {:colorizer {:user_default_options {:names false}}}})

(use! :lukas-reineke/headlines.nvim
      {:setup {:headlines {:markdown {:fat_headlines false}}}})

;; Navigation

;; Use <Tab> as gE
(keymaps! {[:n :v :o] {:<Tab> :ge :<S-Tab> :gE}
           ;; Avoid conflicts with C-i bindings
           :n {:<C-i> :<C-i>}})

; Sane `<Esc>` behaviour in terminal mode
(keymaps! {:t {:<Esc> "<C-\\><C-n>" :<C-v><Esc> :<Esc>}})

; Open main.fnl
(keymaps! {:n {:<leader>ov ":<C-u>edit ~/.config/nvim/fnl/main.fnl<CR>"}})

; Goto alternative/[p]revious file
(keymaps! {:n {:<C-p> :<C-^>}})

(keymaps! {:n {; Buffer navigation
               "]b" ":<C-u>bnext<CR>"
               "[b" ":<C-u>bprev<CR>"
               "]q" ":<C-u>cnext<CR>"
               "[q" ":<C-u>cprev<CR>"
               "]Q" ":<C-u>clast<CR>"
               "[Q" ":<C-u>cfirst<CR>"
               ; Navigate quickfix and location lists
               :<leader>oq ":<C-u>copen<CR>"
               :<leader>qq ":<C-u>cclose<CR>"
               "]l" ":<C-u>lnext<CR>"
               "[l" ":<C-u>lprev<CR>"
               "]L" ":<C-u>llast<CR>"
               "[L" ":<C-u>lfirst<CR>"
               :<leader>ol ":<C-u>lopen<CR>"
               :<leader>ql ":<C-u>lclose<CR>"}})

; Better folds
; (add! [:kevinhwang91/nvim-ufo :kevinhwang91/promise-async]
;       #(setup :ufo {:provider_selector #[:treesitter :indent]}))

; Open urls externally with xdg-open
(mk-op! :OpenExternally (let [cmd :xdg-open
                              open #(let [on-exit (fn [exit]
                                                    (if (not= exit 0)
                                                        (error (.. cmd " '" $1
                                                                   "' failed with exit code "
                                                                   exit))))]
                                      (spawn cmd {:args [$1]} on-exit))]
                          (fn [lines]
                            (vim.tbl_map open lines))))

(keymaps! {[:n :v] {:go :<Plug>OpenExternally :go :<Plug>OpenExternally}})

;; Write and quit
(keymaps! {:n {:<leader> {:w ":<C-u>w<cr>"
                          :qV ":<C-u>qall<CR>"
                          :qb ":bd<CR>"
                          :qw ":q<CR>"
                          ;; "sudo write"-trick via polkit agent
                          :W ":<C-u>w !pkexec tee % >/dev/null<CR>"}}})

;; Navigate history containing substring
;; FIXME: debug
(keymaps! {:c {:<M-p> #(feed :<Up>) :<M-n> #(feed :<Down>)}})

(use! [:nvim-telescope/telescope.nvim :nvim-lua/plenary.nvim]
      {:config #(require :setup/telescope)})

; Leap with s
(use! :ggandor/leap.nvim
      {:setup {:leap {:safe_labels {}}}
       :config #(let [{: set_default_keymaps} (require :leap)]
                  (set_default_keymaps))})

; Smooth scrolling
(use! :karb94/neoscroll.nvim
      {;; Disable default mappings
       :setup {:neoscroll {:mappings {}}}
       :map #(let [{: scroll : zt : zz : zb} (require :neoscroll)
                   get-height #(vim.api.nvim_win_get_height 0)]
               {:n {:<C-u> #(scroll (- vim.wo.scroll) {:duration 150})
                    :<C-d> #(scroll vim.wo.scroll {:duration 150})
                    :<C-b> #(scroll (- (get-height)) {:duration 350})
                    :<C-f> #(scroll (get-height) {:duration 350})
                    :zt #(zt {:half_win_duration 125})
                    :zz #(zz {:half_win_duration 125})
                    :zb #(zb {:half_win_duration 125})}})})

; Split file explorer
(use! :stevearc/oil.nvim {:setup {:oil {:columns []
                                        :use_default_keymaps false
                                        :keymaps {:g? :actions.show_help
                                                  "<C-]>" :actions.select
                                                  "CR" :actions.select
                                                  :<C-s>v :actions.select_vsplit
                                                  :<C-s>s :actions.select_split
                                                  :gp :actions.preview
                                                  :<C-p> :actions.close
                                                  :gf :actions.refresh
                                                  :- :actions.parent
                                                  :_ :actions.open_cwd
                                                  "`" :actions.cd
                                                  "~" :actions.tcd
                                                  :g. :actions.toggle_hidden}}}
                          :map #(let [{: open} (require :oil)]
                                  {:n {:- open}})})

; TODO: Use fork until https://github.com/numToStr/Navigator.nvim/pull/35 is merged
(use! :Vinh-CHUC/Navigator.nvim
      {:setup {:Navigator nil}
       :map {[:n :t] (let [mk-lhs #(.. :<M- $1 :>)
                           mk-rhs #(. vim.cmd (.. :Navigator $1))]
                       (collect [dir keys (pairs {:Left [:h :left]
                                                  :Right [:l :right]
                                                  :Up [:k :up]
                                                  :Down [:j :down]})]
                         (values (vim.tbl_map mk-lhs keys) (mk-rhs dir))))}})

;; Filetype plugins
(use! :elkowar/yuck.vim)

; Treesitter indentation for fennel is messed up, so use this plugin for this
(use! :jaawerth/fennel.vim)

(use! :lervag/vimtex {:config (fn []
                                (let! vimtex_format_enabled 1)
                                (let! vimtex_quickfix_mode 0)
                                (let! vimtex_view_method :zathura)
                                (let! tex_flavor :latex))})

(keymaps! {:n {:<leader>on #(let [notes-dir (.. vim.env.HOME :/Notes)]
                              (vim.cmd.cd notes-dir)
                              (vim.cmd.edit :index.md))}})

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
       :saadparwaiz1/cmp_luasnip] {:reload :setup/cmp})

;; Coding related stuff

; Run current file, if applicable
; (let [call #($1)
;       ft-to-runner {:fennel #(vim.cmd.Fnlfile "%")
;                     :python #(vim.api.nvim_command "w !python3")
;                     :sh #(vim.api.nvim_command "w !bash")
;                     :fish #(vim.api.nvim_command "w !fish")
;                     :lua #(vim.api.nvim_command "lua dofile(vim.fn.expand('%'))")}]
;   (nmap! :<leader>rr #(-?>> vim.o.filetype
;                             (. ft-to-runner)
;                             (call))))

(keymaps! {:n {"<leader>m" {:k ":make!<CR>"
                            :f ":make! flash<CR>"
                            :c ":make! clean<CR>"
                            :t ":make! test<CR>"
                            :b ":make! build<CR>"}}})

(use! :neomake/neomake
      {:init (do
               ;; Appearance
               (let! neomake_error_sign
                     {:text "➤"
                      ; :texthl "NeomakeErrorSign"
                      })
               (let! neomake_warning_sign
                     {:text "➤"
                      ; :texthl "NeomakeWarningSign"
                      })
               (let! neomake_message_sign
                     {:text "➤"
                      ; :texthl "NeomakemessageSign"
                      })
               (let! neomake_info_sign
                     {:text "➤"
                      ; :texthl "NeomakeInfoSign"
                      }))
       :map #{:n {:<leader>n {:m #(vim.cmd.Neomake) :c #(vim.cmd.NeomakeClean)}}}
       :hl {:NeomakeErrorSign {:link :DiagnosticSignError}
            :NeomakeWarningSign {:link :DiagnosticSignWarn}
            :NeomakeMessageSign {:link :DiagnosticSignHint}
            :NeomakeInfoSign {:link :DiagnosticSignInfo}
            :NeomakeVirtualtextError {:link :DiagnosticError}
            :NeomakeVirtualtextWarning {:link :DiagnosticWarn}
            :NeomakeVirtualtextMessage {:link :DiagnosticHint}
            :NeomakeVirtualtextInfo {:link :DiagnosticInfo}}})

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

(vim.diagnostic.config {:signs {:text {vim.diagnostic.severity.ERROR ""
                                       vim.diagnostic.severity.WARN ""
                                       vim.diagnostic.severity.INFO ""
                                       vim.diagnostic.severity.HINT "󰌵"}}})

(use! [:neovim/nvim-lspconfig :mickael-menu/zk-nvim :mrcjkb/rustaceanvim]
      {:reload :setup/lspconfig})

(use! "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
      {:setup {:lsp_lines nil}
       :map #(let [{: toggle} (require :lsp_lines)
                   toggle-with-inline #(let [lines-on (toggle)]
                                         (vim.diagnostic.config {:virtual_text (not lines-on)}))]
               {:n {:<leader>tl toggle-with-inline}})})

(use! [:mfussenegger/nvim-dap
       :nvim-neotest/nvim-nio
       :rcarriga/nvim-dap-ui
       :theHamsta/nvim-dap-virtual-text
       :mfussenegger/nvim-dap-python]
      {:map #(let [dap (require :dap)]
               {:n {:<leader>d {:b dap.toggle_breakpoint
                                :l dap.list_breakpoints
                                :s dap.step_into
                                :n dap.step_over
                                :r dap.continue
                                :t dap.terminate
                                :c dap.clear_breakpoints
                                :o dap.repl.open}}})
       :setup {:nvim-dap-virtual-text nil
               ;; :dapui nil
               :dap-python :python}
       :config (let [adapter-configs {:gdb {:type :executable
                                            :command :gdb
                                            :args [:--interpreter=dap
                                                   :--eval-command
                                                   "set print pretty on"]}}
                     filetype-configs {:cpp [{:name "Launch C++"
                                              :type :gdb
                                              :request :launch
                                              :program :build/main}]}]
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

; Forked as plugin doesn't have an API for custom keybindings
(use! :bR3iN/jupynium.nvim {:setup {:jupynium {}}})

(use! :folke/trouble.nvim
      {:setup {:trouble {:icons {:indent {:last "╰╴"}}}}})

(keymaps! {:n {:<leader>ot ":<C-u>Trouble diagnostics focus<CR>"
               :<leader>qt ":<C-u>Trouble diagnostics close<CR>"}})

(use! :lewis6991/gitsigns.nvim {:setup {:gitsigns {:signcolumn false}}})

; Keep ftplugin logic inline here to not spread the config too much
(let [ftplugins (reload :ftplugins)]
  (autocmd! :ftplugins
            (icollect [filetype callback (pairs ftplugins)]
              {:event :FileType :pattern filetype : callback})))

; Floating preview in quickfix window
(use! :kevinhwang91/nvim-bqf
      {:setup {:bqf {:func_map {:fzffilter "" :open "<C-]>"}
                     :preview {:winblend 0}}}})

(use! :lukas-reineke/indent-blankline.nvim
      {:hl {:IblScope {:fg colors.dark_green :bold true}
            :IblIndent {:fg colors.bg2 :bold true}}
       :setup {:ibl {:enabled false}}
       :map #{:n {:<leader>ti vim.cmd.IBLToggle}}})

(use! :folke/which-key.nvim
      {:setup {:which-key {:delay 1000 :preset :helix}}})
