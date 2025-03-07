(import-macros {: set! : setl! : setl+ : setl- : setg! : set+ : let! : with-cb}
               :utils.macros)


(local {: add!} (require :pkg))
; TODO: remove augroup!
(local {: nmap!
        : vmap!
        : tmap!
        : cmap!
        : imap!
        : xmap!
        : smap!
        : omap!
        : command!
        : augroup!
        : autocmd!
        : put!} (require :utils.nvim))

(local {: nil? : empty?} (require :utils))
(local {: spawn : spawn-capture-output} (require :utils.async))
(local {: mk-op!} (require :utils.operator))

(fn feed [keys]
  (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes keys true true true)
                         :m true))

(fn setup [mod ?opts] ; Loads module and calls its `setup` function
  (let [{: setup} (require mod)]
    (setup (or ?opts {}))))

; Manage bootstrapped packages
(add! :rktjmp/hotpot.nvim)
(add! :bR3iN/pkg.nvim)

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
(imap! :<C-r> :<C-r><C-o>)

; Break insert mode; similar to <C-g>u but also "resets" <C-a>
; TODO: experimental
(imap! :<C-q> :<Esc>a)

; Clear highlight search
(nmap! :<C-l> ":<C-u>nohlsearch<CR><C-l>")

; Capitalize word in front of cursor
(imap! :<C-u> :<Esc>viwUea)

; Select until end of line (like `C`, `D` and `Y`)
(nmap! :<leader>v :vg_)

(vmap! :<C-y> "\"+y")
(imap! :<C-p> :<C-r>+)

(add! :tpope/vim-repeat)
(add! :tpope/vim-commentary)
(add! :kylechui/nvim-surround
      #(setup :nvim-surround
              {:keymaps {:insert :<C-s> :insert_line :<C-s><C-s>}}))

; (add! "echasnovski/mini.surround"
;       #(setup :mini.surround {:mappings {:add "ys"
;                                          :delete "ds"
;                                          :find ""
;                                          :find_left ""
;                                          :highlight ""
;                                          :replace "cs"
;                                          :update_n_lines ""
;                                          :suffix_last "p"
;                                          :suffix_next "n"}
;                               :search_method "cover_or_next"}))

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

(nmap! :<leader>pu :<Plug>PkgUpdate)
(nmap! :<leader>pc :<Plug>PkgClean)
(nmap! :<leader>pl :<Plug>PkgList)
(nmap! :<leader>op ":edit ~/.local/share/nvim/site/pack/pkgs/start/<CR>")

;; Appearance

(set! termguicolors)

; Custom color setup; load colorscheme description (name + base16 colors)
(add! :bR3iN/base16.nvim
      #(let [hl-ext! (fn [name hl]
                       (->> {: name}
                            (vim.api.nvim_get_hl 0)
                            (vim.tbl_extend :keep hl)
                            (vim.api.nvim_set_hl 0 name)))
            hl! (fn [name hl]
                  (vim.api.nvim_set_hl 0 name hl))
             {: colors : name} (require :base16-colors)
             {: darken} (require :base16.utils)] ; Decide if we use an external colorscheme or our own base16-based one
         (match name
           "Tokyonight Moon"
           (add! :folke/tokyonight.nvim
                 #(do
                    (setup :tokyonight {:transparent true})
                    (vim.cmd.colorscheme :tokyonight-moon)))
           :Nord
           (add! :shaunsingh/nord.nvim
                 #(do
                    (let! nord_disable_background true)
                    (vim.cmd.colorscheme :nord)))
           :Gruvbox
           (add! :ellisonleao/gruvbox.nvim
                 #(do
                    (setup :gruvbox {:transparent_mode true})
                    (vim.cmd.colorscheme :gruvbox)))
           :Kanagawa
           (add! :rebelot/kanagawa.nvim
                 #(do
                    (setup :kanagawa {:transparent true})
                    (vim.cmd.colorscheme :kanagawa)))
           :Catppuccin
           (add! :catppuccin/nvim
                 #(do
                    (vim.cmd.colorscheme :catppuccin-mocha)
                    (vim.api.nvim_set_hl 0 :Normal
                                         {:fg colors.base05 :bg :None})
                    (vim.api.nvim_set_hl 0 :NormalNC
                                         {:fg colors.base05 :bg :None})))
           ; Fallback; derives colorscheme from base16 colors
           _
           (do
             (let! base16_colors_lua :base16-colors)
             (vim.cmd.colorscheme :base16))) ; Highlights overrides and groups for local plugins
         (hl-ext! :FloatBorder {:fg colors.base03})
         (hl-ext! :WinSeparator {:fg colors.base02}) ; (hl-ext! :SignColumn {:bg colors.base02}) ; (hl-ext! :LineNrAbove {:bg colors.base01}) ; (hl-ext! :LineNrBelow {:bg colors.base01}) ; (hl-ext! :CursorLineNr {:bg (darken colors.base02 0.1)})
         (hl-ext! :CursorLine {:bg (darken colors.base02 0.1)})
         (hl-ext! :Comment {:fg colors.base03})
         (hl-ext! :CommentHighlighted {:fg colors.base0F})
         (vim.api.nvim_set_hl 0 :GitSignsCurrentLineBlame
                              {:link :CommentHighlighted})
         (hl-ext! :TrailingWhitespace {:fg colors.base03 :bg colors.base03}) ; Toggle the color of comments TODO: For some reason doesn't work with external nord colorscheme
         (hl! :BqfPreviewTitle {:fg colors.green :bg colors.base02})
         ; Load after setting `CommentHighlighted` above
         (setup :plugin.toggle-comments)
         (nmap! :<C-h> ":ToggleComments<CR>")
         (imap! :<C-h> "<C-o>:ToggleComments<CR>")))

(set! fillchars {:vert "│"})

(set! conceallevel 2)
; (set! cmdheight 2)
(set! scrolloff 1)
(set! linebreak)

(require :plugin.highlight-trailing-whitespace)

; Highlights hex color codes with their color
(add! :NvChad/nvim-colorizer.lua
      #(setup :colorizer {:user_default_options {:names false}}))

(add! :lukas-reineke/headlines.nvim
      #(setup :headlines {:markdown {:fat_headlines false}}))

; (add!
;   "nvim-orgmode/orgmode"
;   (fn []
;     (setup
;      :orgmode
;      {:org_agenda_files ["~/neorg/**/*.org"]
;       :win_split_mode :edit
;       :org_default_notes_files "~/neorg/notes.org"
;       :mappings {:prefix "<leader>a"
;                  :org {:org_open_at_point "<C-]>"}
;                  :org_return "<C-CR>"}})
;     ))

;; Navigation

(nmap! :<Tab> :ge)
(vmap! :<Tab> :ge)
(omap! :<Tab> :ge)
(nmap! :<S-Tab> :gE)
(vmap! :<S-Tab> :gE)
(omap! :<S-Tab> :gE)
(nmap! :<C-i> :<C-i>)

; Sane `<Esc>` behaviour in terminal mode
(tmap! :<Esc> "<C-\\><C-n>")
(tmap! :<C-v><Esc> :<Esc>)

; Open main.fnl
(nmap! :<leader>ov ":<C-u>edit ~/.config/nvim/fnl/main.fnl<CR>")

; Goto alternative/[p]revious file
(nmap! :<C-p> :<C-^>)

; Buffer navigation
(nmap! "]b" ":<C-u>bnext<CR>")
(nmap! "[b" ":<C-u>bprev<CR>")

; Navigate quickfix and location lists
(nmap! "]q" ":<C-u>cnext<CR>")
(nmap! "[q" ":<C-u>cprev<CR>")
(nmap! "]Q" ":<C-u>clast<CR>")
(nmap! "[Q" ":<C-u>cfirst<CR>")
(nmap! :<leader>oq ":<C-u>copen<CR>")
(nmap! :<leader>qq ":<C-u>cclose<CR>")

(nmap! "]l" ":<C-u>lnext<CR>")
(nmap! "[l" ":<C-u>lprev<CR>")
(nmap! "]L" ":<C-u>llast<CR>")
(nmap! "[L" ":<C-u>lfirst<CR>")
(nmap! :<leader>ol ":<C-u>lopen<CR>")
(nmap! :<leader>ql ":<C-u>lclose<CR>")

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

(nmap! :go :<Plug>OpenExternally)
(vmap! :go :<Plug>OpenExternally)

; Write and quit
(nmap! :<leader>w ":<C-u>w<cr>")
(nmap! :<leader>qV ":<C-u>qall<CR>")
(nmap! :<leader>qb ":bd<CR>")
(nmap! :<leader>qw ":q<CR>")
; "sudo write"-trick via polkit agent
(nmap! :<leader>W ":<C-u>w !pkexec tee % >/dev/null<CR>")

; Navigate history containing substring
(cmap! :<M-p> #(feed :<Up>))
(cmap! :<M-n> #(feed :<Down>))

(add! [:nvim-telescope/telescope.nvim :nvim-lua/plenary.nvim]
      (fn []
        (let [builtins (require :telescope.builtin)
              actions (require :telescope.actions)
              {: buffer_dir} (require :telescope.utils)
              smart_qf_and_open (fn [bufnr]
                                  (actions.smart_send_to_qflist bufnr) ; (actions.open_qflist bufnr)
                                  (vim.cmd.cfirst))]
          ;Setup plugin
          (setup :telescope
                 {:defaults {:sorting_strategy :ascending
                             :scroll_strategy :limit
                             :layout_config {:prompt_position :top}
                             :layout_strategy :flex
                             :mappings {:i {:<C-j> actions.preview_scrolling_down
                                            :<C-k> actions.preview_scrolling_up
                                            :<C-l> actions.preview_scrolling_right
                                            :<C-h> actions.preview_scrolling_left
                                            :<C-q> smart_qf_and_open
                                            "<C-]>" actions.select_default
                                            :<C-x> actions.drop_all
                                            :<C-a> actions.select_all
                                            :<C-d> actions.results_scrolling_down
                                            :<C-u> actions.results_scrolling_up
                                            :<C-s> actions.select_horizontal}
                                        :n {:<C-j> actions.preview_scrolling_down
                                            :<C-k> actions.preview_scrolling_up
                                            :<C-l> actions.preview_scrolling_right
                                            :<C-h> actions.preview_scrolling_left
                                            :<C-q> smart_qf_and_open
                                            "<C-]>" actions.select_default
                                            :<C-x> actions.drop_all
                                            :<C-a> actions.select_all
                                            :<C-d> actions.results_scrolling_down
                                            :<C-u> actions.results_scrolling_up
                                            :<C-s> actions.select_horizontal}}}
                  :pickers {:buffers {:mappings {:n {:<C-x> :delete_buffer}
                                                 :i {:<C-x> :delete_buffer}}}}})
          ; Setup keymaps; if in an oil buffer, use the buffer's directory as `cwd`
          (let [try-get-cwd #(if (= vim.o.filetype :oil)
                                 (let [{: get_current_dir} (require :oil)]
                                   (get_current_dir))
                                 nil)
                pick (fn [action ?opts]
                       (let [picker (. builtins action)
                             opts (vim.tbl_extend :force {:cwd (try-get-cwd)}
                                                  (or ?opts {}))]
                         (picker opts)))]
            (each [[mode lhs] rhs (pairs {[:n :<leader>ff] #(pick :find_files
                                                                  {:follow true})
                                          ; Resumes previous picker
                                          [:n :<leader>f.] #(pick :resume)
                                          [:n :<leader>b] #(pick :buffers
                                                                 {:sort_lastused true
                                                                  :sort_mru true})
                                          [:n :<leader>fd] #(pick :diagnostics)
                                          [:n :<leader>fg] #(pick :live_grep)
                                          [:n :<leader>fw] #(pick :grep_string)
                                          [:v :<leader>fg] #(pick :grep_string)
                                          [:n :<leader>fl] #(pick :live_grep
                                                                  {:grep_open_files true})
                                          [:n :<leader>fL] #(pick :lsp_workspace_symbols)})]
              (vim.keymap.set mode lhs rhs))))))

; Leap with s
(add! :ggandor/leap.nvim
      (fn []
        (let [{: set_default_keymaps : setup} (require :leap)]
          (set_default_keymaps)
          (setup {:safe_labels {}}))))

; Smooth scrolling
(add! :karb94/neoscroll.nvim
      (fn []
        (let [{: setup : scroll : zt : zz : zb} (require :neoscroll)
              get-height #(vim.api.nvim_win_get_height 0)] ; Disable default mappings
          (setup {:mappings {}}) ; Add custom mappings
          (nmap! :<C-u> #(scroll (- vim.wo.scroll) {:duration 150}))
          (nmap! :<C-d> #(scroll vim.wo.scroll {:duration 150}))
          (nmap! :<C-b> #(scroll (- (get-height)) {:duration 350}))
          (nmap! :<C-f> #(scroll (get-height) {:duration 350}))
          (nmap! :zt #(zt {:half_win_duration 125}))
          (nmap! :zz #(zz {:half_win_duration 125}))
          (nmap! :zb #(zb {:half_win_duration 125})))))

; Split file explorer
(add! :stevearc/oil.nvim #(let [{: setup : open} (require :oil)]
                            (setup {:columns []
                                    ; disables icons
                                    :use_default_keymaps false
                                    :keymaps {:g? :actions.show_help
                                              "<C-]>" :actions.select
                                              :<C-s>v :actions.select_vsplit
                                              :<C-s>s :actions.select_split
                                              :gp :actions.preview
                                              :<C-p> :actions.close
                                              :gf :actions.refresh
                                              :- :actions.parent
                                              :_ :actions.open_cwd
                                              "`" :actions.cd
                                              "~" :actions.tcd
                                              :g. :actions.toggle_hidden}})
                            (nmap! "-" open)))

; TODO: Use fork until https://github.com/numToStr/Navigator.nvim/pull/35 is merged
(add! :Vinh-CHUC/Navigator.nvim
      (fn []
        (setup :Navigator)
        (let [mk-lhs #(.. :<M- $1 ">")]
          (each [direction keys (pairs {:Left [:h :left]
                                        :Right [:l :right]
                                        :Up [:k :up]
                                        :Down [:j :down]})]
            (each [_ key (ipairs keys)]
              (let [lhs (mk-lhs key)
                    rhs (. vim.cmd (.. :Navigator direction))]
                (each [_ mk-map (ipairs [nmap! tmap!])]
                  (mk-map lhs rhs))))))))

; Tmux interop
; (add! "christoomey/vim-tmux-navigator"
;       (fn []
;         ; Disable default mappings
;         (let! tmux_navigator_no_mappings 1)
;         ; Set custom mappings
;         (let [mk-lhs #(.. :<M- $1 :>)]
;           (each [direction keys (pairs
;                                   {:Left  [:h :left]
;                                    :Right [:l :right]
;                                    :Up    [:k :up]
;                                    :Down  [:j :down]})]
;             (each [_ key (ipairs keys)]
;               (let [lhs (mk-lhs key)
;                     rhs #(vim.cmd (.. :TmuxNavigate direction))]
;                 (each [_ mk-map (ipairs [nmap! tmap!])]
;                   (mk-map lhs rhs))))))))

;; Filetype plugins

(add! :elkowar/yuck.vim)

; Treesitter indentation for fennel is messed up, so use this plugin for this
(add! :m15a/vim-fennel-syntax)

(add! :lervag/vimtex (fn []
                       (let! vimtex_format_enabled 1)
                       (let! vimtex_quickfix_mode 0)
                       (let! vimtex_view_method :zathura)
                       (let! tex_flavor :latex)))

(nmap! :<leader>on #(let [notes-dir (.. vim.env.HOME :/Notes)]
                      (vim.cmd.cd notes-dir)
                      (vim.cmd.edit :index.md)))

;; Autocompletion and snippets

(add! [:hrsh7th/nvim-cmp
       :hrsh7th/cmp-nvim-lsp
       :hrsh7th/cmp-nvim-lua
       :kdheepak/cmp-latex-symbols
       :hrsh7th/cmp-path
       :hrsh7th/cmp-omni
       :hrsh7th/cmp-buffer
       :L3MON4D3/LuaSnip
       :saadparwaiz1/cmp_luasnip
       :rafamadriz/friendly-snippets]
      (fn []
        (let [cmp (require :cmp)
              selected? #(not= (cmp.get_selected_entry) nil)
              {: locally_jumpable : jump : lsp_expand : expandable : expand} (require :luasnip)
              feed #(-> $1
                        (vim.api.nvim_replace_termcodes true true true)
                        (vim.fn.feedkeys))
              config {:mapping (let [next-or-complete (cmp.mapping (fn [fallback]
                                                                     (if (cmp.visible)
                                                                         (cmp.select_next_item {:behavior cmp.SelectBehavior.Insert})
                                                                         (cmp.complete)))
                                                                   [:i :s])
                                     prev (cmp.mapping (fn [fallback]
                                                         (if (cmp.visible)
                                                             (cmp.select_prev_item {:behavior cmp.SelectBehavior.Insert})
                                                             (fallback)))
                                                       [:i :s])
                                     confirm (cmp.mapping (fn [fallback]
                                                            (if (cmp.visible)
                                                                (cmp.confirm {:select true})
                                                                (fallback)))
                                                          [:i :s])
                                     snippet-next (cmp.mapping (fn [fallback]
                                                                 (if (locally_jumpable 1)
                                                                     (jump 1)
                                                                     (fallback)))
                                                               [:i :s])
                                     snippet-prev (cmp.mapping (fn [fallback]
                                                                 (if (locally_jumpable -1)
                                                                     (jump -1)
                                                                     (fallback)))
                                                               [:i :s])
                                     snippet-expand (cmp.mapping (fn [fallback]
                                                                   (if (expandable)
                                                                       (expand)
                                                                       (fallback)))
                                                                 [:i :s])
                                     abort (cmp.mapping (fn [fallback]
                                                          (if (cmp.visible)
                                                              (cmp.abort)
                                                              (fallback)))
                                                        [:i :s])
                                     docs-down (cmp.mapping (fn [fallback]
                                                              (if (cmp.visible)
                                                                  (cmp.scroll_docs 5)
                                                                  (fallback)))
                                                            [:i :s])
                                     docs-up (cmp.mapping (fn [fallback]
                                                            (if (cmp.visible)
                                                                (cmp.scroll_docs -5)
                                                                (fallback)))
                                                          [:i :s])]
                                 {:<C-n> next-or-complete
                                  :<Down> next-or-complete
                                  :<C-p> prev
                                  :<Up> prev
                                  :<C-l> confirm
                                  :<Right> confirm
                                  :<Tab> snippet-next
                                  :<S-Tab> snippet-prev
                                  :<C-k> snippet-expand
                                  :<C-e> abort
                                  :<C-d> docs-down
                                  :<C-u> docs-up})
                      :completion {:completeopt "menu,menuone"}
                      :snippet {:expand (fn [{: body}]
                                          (lsp_expand body))}
                      ; The LSP specifies that certain items should be preselected, ignoring their position in the suggestion list. This disables this.
                      :preselect cmp.PreselectMode.None
                      :sources [{:name :nvim_lsp}
                                {:name :luasnip}
                                {:name :orgmode}
                                {:name :nvim_lua}
                                {:name :neorg}
                                ; {:name :path}
                                {:name :latex_symbols}
                                {:name :omni}
                                {:name :buffer
                                 :option {:keyword_pattern "\\k\\+"}}]
                      :window {:completion (cmp.config.window.bordered)
                               :documentation (cmp.config.window.bordered)}
                      :formatting {:format (let [display-names {:nvim_lsp "[LSP]"
                                                                :luasnip "[Snp]"
                                                                :nvim_lua "[Lua]"
                                                                :latex_symbols "[LTX]"
                                                                :path "[Pth]"
                                                                :omni "[Omn]"
                                                                :calc "[Clc]"
                                                                :buffer "[Buf]"}]
                                             (fn [{:source {: name}} item]
                                               (->> name
                                                    (. display-names)
                                                    (#(or $1 "[???]"))
                                                    (tset item :menu))
                                               item))}}]
          (cmp.setup config)) ; Setup custom snippets in separate module to not duplicate them when ; reloading the config
        (setup :snippets) ; Find snippets defined by plugins
        (let [{:lazy_load load_from_vscode} (require :luasnip.loaders.from_vscode)
              {:lazy_load load_from_lua} (require :luasnip.loaders.from_lua)]
          (load_from_vscode {:exclude [:norg]}) ; (load_from_lua)
          )))

;; Coding related stuff

; Run current file, if applicable
(let [call #($1)
      ft-to-runner {:fennel #(vim.cmd.Fnlfile "%")
                    :python #(vim.api.nvim_command "w !python3")
                    :sh #(vim.api.nvim_command "w !bash")
                    :fish #(vim.api.nvim_command "w !fish")
                    :lua #(vim.api.nvim_command "lua dofile(vim.fn.expand('%'))")}]
  (nmap! :<leader>rr #(-?>> vim.o.filetype
                            (. ft-to-runner)
                            (call))))

(nmap! :<leader>mk ":make!<CR>")
(nmap! :<leader>mf ":make! flash<CR>")
(nmap! :<leader>mc ":make! clean<CR>")
(nmap! :<leader>mt ":make! test<CR>")
(nmap! :<leader>mb ":make! build<CR>")

(add! :neomake/neomake
      (fn []
        (nmap! :<leader>nm #(vim.cmd :Neomake))
        (nmap! :<leader>nc #(vim.cmd :NeomakeClean)) ; Appearance
        (let! neomake_error_sign {:text "➤"
                                  ; :texthl "NeomakeErrorSign"
                                  })
        (let! neomake_warning_sign {:text "➤"
                                    ; :texthl "NeomakeWarningSign"
                                    })
        (let! neomake_message_sign {:text "➤"
                                    ; :texthl "NeomakemessageSign"
                                    })
        (let! neomake_info_sign {:text "➤"
                                 ; :texthl "NeomakeInfoSign"
                                 }) ; Link Neomake highlight groups and relink them on colorscheme change
        (let [links {:NeomakeErrorSign :DiagnosticSignError
                     :NeomakeWarningSign :DiagnosticSignWarn
                     :NeomakeMessageSign :DiagnosticSignHint
                     :NeomakeInfoSign :DiagnosticSignInfo
                     :NeomakeVirtualtextError :DiagnosticError
                     :NeomakeVirtualtextWarning :DiagnosticWarn
                     :NeomakeVirtualtextMessage :DiagnosticHint
                     :NeomakeVirtualtextInfo :DiagnosticInfo}
              set-hl #(each [name link (pairs links)]
                        (vim.api.nvim_set_hl 0 name {: link}))
              autocmd! (augroup! :neomake_highlighting)]
          (set-hl)
          (autocmd! :ColorScheme "*" set-hl))))

(add! [:nvim-treesitter/nvim-treesitter
       ; "nvim-treesitter/playground"
       :nvim-treesitter/nvim-treesitter-textobjects]
      (fn [] ; Use treesitter-based folds
        (set! foldmethod :expr)
        (set! foldexpr "nvim_treesitter#foldexpr()")
        (setup :nvim-treesitter.configs
               {:highlight {:enable true}
                ; :indent {:enable true}
                ; :additional_vim_regex_highlighting [:fennel] ; Use with indent=true
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
                                     :swap_previous {"<leader>," "@parameter.inner"}}}})))

(add! [:neovim/nvim-lspconfig :mickael-menu/zk-nvim]
      #(let [lspconfig (require :lspconfig)
             extend (partial vim.tbl_extend :force)
             default-keymaps {[:n :gD] vim.lsp.buf.declaration
                              [:n :gt] vim.lsp.buf.type_definition
                              [:n :gd] vim.lsp.buf.definition
                              [:n :K] vim.lsp.buf.hover
                              [:n :gi] vim.lsp.buf.implementation
                              [:n :<C-k>] vim.lsp.buf.signature_help
                              [:n :<leader>rn] vim.lsp.buf.rename
                              [:n :<leader>ca] vim.lsp.buf.code_action
                              [:n :gr] vim.lsp.buf.references
                              [:n :gqq] vim.lsp.buf.format
                              [:v :gq] vim.lsp.buf.format
                              [:n :<leader>od] vim.diagnostic.open_float
                              [:n "[d"] vim.diagnostic.goto_prev
                              [:n "]d"] vim.diagnostic.goto_next}
            mk-on_attach (fn [?extra-keymaps]
                           (let [keymaps (extend default-keymaps (or ?extra-keymaps {}))]
                             (fn [client bufnr]
                                      ;; Always show signcolumn for that
                                      ;; buffer, so the code doesn't move
                                      ;; horizontally when the number of
                                      ;; diagnostics changes to or from zero.
                                      ;; TODO: We assume here that the code is
                                      ;; always opened in the current window,
                                      ;; not sure how true this.
                                      (tset vim.wo 0 :signcolumn :yes)
                                      ;; Create local keymaps
                                      (let [opts {:noremap true
                                                  :silent true
                                                  :buffer bufnr}]
                                        (each [[mode lhs] rhs (pairs keymaps)]
                                          (vim.keymap.set mode lhs rhs opts))))))
            mk-capabilities #(let [(ok mod) (pcall require
                                                   :cmp_nvim_lsp)]
                               (if ok (mod.default_capabilities)
                                   (vim.lsp.protocol.make_client_capabilitites)))
             lsp! (fn [name ?config ?extra-keymaps]
                    (let [capabilities (mk-capabilities)
                          on_attach (mk-on_attach ?extra-keymaps)
                          config (extend {: capabilities : on_attach}
                                         (or ?config {}))
                          {: setup} (. lspconfig name)]
                      (setup config)))]
         (add! :mrcjkb/rustaceanvim
               #(let [on_attach (mk-on_attach)]
                  (let! rustaceanvim {:server {: on_attach}})))
         (lsp! :clangd {} {[:n :<C-c>] #(vim.cmd.ClangdSwitchSourceHeader)})
         ;; {:name :lua_ls :config (require :lsp.configs.sumneko_lua) :keymaps {}}
         ;; (lsp! :bashls)
         ;; (lsp! :vimls)
         ;; (lsp! :fennel_ls)
         ;; (lsp! :fennel_language_server
         ;; {:settings { :fennel {:diagnostics {:globals [:vim]} :workspace {:library (vim.api.nvim_list_runtime_paths)}}}})
         (lsp! :pyright)
         (lsp! :cmake)
         ;; (lsp! :rust_analyzer)  ; Configured by rustaceans instead
         (lsp! :hls)
         ;; (lsp! :marksman)
         (lsp! :racket_langserver)
         (let [border :rounded
               cap-to-handler {:textDocument/hover vim.lsp.handlers.hover
                               :textDocument/signatureHelp vim.lsp.handlers.signature_help}]
           (vim.diagnostic.config {:float {: border}})
           (tset (require :lspconfig.ui.windows) :default_options {: border})
           (each [cap handler (pairs cap-to-handler)]
             (tset vim.lsp.handlers cap (vim.lsp.with handler {: border}))))
         (let [zk (require :zk)
               util (require :zk.util)
               create-and-insert-link #(let [loc (util.get_lsp_location_from_caret)
                                             title (vim.fn.input "Title: ")]
                                         (if (not= (length title) 0)
                                             (zk.new {: title
                                                      :edit false
                                                      :insertLinkAtLocation loc})))
               ; create-note #(let [title (vim.fn.input "Title: ")]
               ;                (if (not= (# title) 0)
               ;                  (zk.new {: title})))
               ; TODO reactivate the above or document snippet
               create-note #(zk.new)
               extra-keymaps {[:i :<C-h>] :<Esc>hcT|
                              [:i :<C-l>] :<Esc>2la
                              [:i :<C-y>] "<Esc>2hvT|uf]2la"
                              [:n :<localleader>nz] create-note
                              [:n :<localleader>no] #(zk.edit)
                              [:n :<localleader>nb] #(vim.cmd.ZkBacklinks)
                              [:i :<C-i>] "<C-o>:ZkInsertLink<CR>"
                              [:i :<C-j>] create-and-insert-link
                              [:i :<C-p>] #(spawn-capture-output :zk-screenshot
                                                                 nil
                                                                 (fn [code
                                                                      _
                                                                      stdout
                                                                      stderr]
                                                                   (if (= 0
                                                                          code)
                                                                       (put! (.. "![["
                                                                                 stdout
                                                                                 "]]")))))}]
           (zk.setup {:picker :telescope
                      :lsp {:config {:on_attach (mk-on_attach extra-keymaps)
                                     :capabilities (mk-capabilities)}}}))))

(add! "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
      #(let [{: setup : toggle} (require :lsp_lines)
             toggle-diags #(let [lines-on (toggle)]
                             (vim.diagnostic.config {:virtual_text (not lines-on)}))]
         (setup)
         (vim.diagnostic.config {:virtual_lines false}) ; (toggle-diags) ; Start with lines off
         (nmap! :<leader>tl toggle-diags)))

; (add! :lukas-reineke/indent-blankline.nvim
;       #(let [{: colors} (require :base16-colors)
;              {: setup} (require :ibl)
;              enable-at-start false] ; Have to setup highlight groups before setup
;          (vim.api.nvim_set_hl 0 :IblScope {:fg colors.dark_green :bold true})
;          (vim.api.nvim_set_hl 0 :IblIndent {:fg colors.bg2 :bold true})
;          (nmap! :<leader>ti vim.cmd.IBLToggle)
;          (setup {:enabled enable-at-start})))

(add! [:mfussenegger/nvim-dap
       :nvim-neotest/nvim-nio
       :rcarriga/nvim-dap-ui
       :theHamsta/nvim-dap-virtual-text
       :mfussenegger/nvim-dap-python]
      (fn [] ; Setup common keymaps
        (let [dap (require :dap)
              adapter-configs {:gdb {:type :executable
                                     :command :gdb
                                     :args [:--interpreter=dap
                                            :--eval-command
                                            "set print pretty on"]}}
              filetype-configs {:cpp [{:name "Launch C++"
                                       :type :gdb
                                       :request :launch
                                       :program :build/main}]}
              mappings {:<leader>db (. dap :toggle_breakpoint)
                        :<leader>dl (. dap :list_breakpoints)
                        :<leader>ds (. dap :step_into)
                        :<leader>dn (. dap :step_over)
                        :<leader>dr (. dap :continue)
                        :<leader>dt (. dap :terminate)
                        :<leader>dc (. dap :clear_breakpoints)
                        :<leader>do (. dap :repl :open)}
              autocmd! (augroup! :nvim-dap)
              prompt-path (fn [prompt cb]
                            (vim.ui.input {: prompt :default (vim.loop.cwd)}
                                          #(if $1 (cb $1))))]
          (each [name config (pairs adapter-configs)]
            (tset dap :adapters name config))
          (each [name config (pairs filetype-configs)]
            (tset dap :configurations name config))
          (each [lhs rhs (pairs mappings)]
            (nmap! lhs rhs))
          (setup :nvim-dap-virtual-text) ; (setup :dapui) ; Setup python debugging via dedicated extension
          (setup :dap-python :python))))

(add! :bR3iN/emanote.nvim
      #(let [{: start : stop} (require :emanote-live)
             emanote_url "http://localhost:8080"]
         (vim.api.nvim_create_user_command :EmanoteConnect
                                           (fn []
                                             (start {:port 8000 : emanote_url})
                                             (spawn :qutebrowser
                                                    {:args [:--target
                                                            :window
                                                            emanote_url]}))
                                           {})
         (vim.api.nvim_create_user_command :EmanoteDisconnect
                                           (partial stop 500) {})))

; Forked as plugin doesn't have an API for custom keybindings
(add! :bR3iN/jupynium.nvim #(setup :jupynium))

(add! [:folke/trouble.nvim]
      #(setup :trouble {:icons {:indent {:last "╰╴"}}}))

(nmap! :<leader>ot ":<C-u>Trouble diagnostics focus<CR>")
(nmap! :<leader>qt ":<C-u>Trouble diagnostics close<CR>")

(add! :b0o/incline.nvim
      #(let [{: colors} (require :base16-colors)
             diag-colors [colors.red colors.orange colors.green colors.blue]
             diag-icons ["" "" "" ""]
             sep {:guifg colors.bg0 1 " | "}]
         (setup :incline
                {:hide {:cursorline :focused_win}
                 ; :window {:padding 1 :margin {:horizontal 0}}
                 :highlight {:groups {:InclineNormal :NONE
                                      :InclineNormalNC :NONE}}
                 :render (fn [{: buf : focused}]
                           (let [bg (if focused
                                        colors.bg3
                                        colors.bg3)
                                 fg (if focused
                                        colors.fg1
                                        colors.fg1)
                                 get-opt #(. vim.bo buf $1)
                                 filename (let [name (-> buf
                                                         (vim.api.nvim_buf_get_name)
                                                         (vim.fn.fnamemodify ":t"))
                                                has-name (not= "" name)]
                                            {:guifg fg
                                             :gui (if focused :bold)
                                             1 (if has-name
                                                   name
                                                   "[No Name]")})
                                 diag-indicator (icollect [severity level (ipairs [:Error
                                                                                   :Warn
                                                                                   :Info
                                                                                   :Hint])]
                                                  (let [n (length (vim.diagnostic.get buf
                                                                                      {: severity}))]
                                                    (if (> n 0)
                                                        [{:guifg (. diag-colors
                                                                    severity)
                                                          :gui :bold
                                                          1 (. diag-icons
                                                               severity)
                                                          2 " "
                                                          3 n}
                                                         sep])))]
                             [{:guifg bg 1 ""}
                              {:guibg bg
                               1 diag-indicator
                               2 filename
                               3 (let [is-ro (or (get-opt :readonly)
                                                 (not (get-opt :modifiable)))]
                                   (if is-ro
                                       {:guifg colors.green 1 " "}
                                       ""))
                               4 (if (get-opt :modified)
                                     {:guifg colors.yellow 1 " "}
                                     "")}
                              {:guifg bg 1 ""}]))})))

(add! :lewis6991/gitsigns.nvim #(setup :gitsigns {:signcolumn false}))

(add! [:rebelot/heirline.nvim :SmiteshP/nvim-navic]
      #(let [{: colors} (require :base16-colors)
             {: lsp_attached : is_git_repo} (require :heirline.conditions)
             ;; Statusline components
             sep-right {:provider "" :hl {:fg colors.bg0 :bold true}}
             sep-left {:provider "" :hl {:fg colors.bg0 :bold true}}
             navic-sep "  "
             navic-sep-hl {:fg colors.bg0} ; Current mode
             macrorec-bg colors.bg3 ; Macro currently recording
             is-macrorec #(not= (vim.fn.reg_recording) "")
             macrorec {:provider #(.. " " (vim.fn.reg_recording))
                       ; macrorec {:provider #(.. "(" (vim.fn.reg_recording) ")")
                       :hl #{:fg colors.dark_red :bold true :bg macrorec-bg}
                       :update [:RecordingEnter :RecordingLeave]}
             vi_mode (let [get-color #(let [{: mode_colors : mode} $1]
                                        (. mode_colors mode))]
                       {:provider #(.. " " (. $1 :mode))
                        1 {:provider ""
                           :hl #{:fg (get-color $1)
                                 :reverse false
                                 :bg (if (is-macrorec) macrorec-bg nil)}}
                        :init #(let [{: mode_names} $1]
                                 (->> (vim.fn.mode 1)
                                      (. mode_names)
                                      (tset $1 :mode)))
                        :hl #{:reverse true :bold true :fg (get-color $1)}
                        :update {1 :RecordingEnter
                                 2 :RecordingLeave
                                 3 :ModeChanged
                                 ; :pattern "*:*"
                                 ; :callback #(vim.schedule_wrap #(vim.cmd :redrawstatus))
                                 }
                        :static (let [mode_map {:NORMAL {:color colors.dark_green
                                                         :modes [:n
                                                                 :niI
                                                                 :niR
                                                                 :niV]}
                                                :OP {:color colors.dark_green
                                                     :modes [:no
                                                             :nov
                                                             :noV
                                                             "no\022"]}
                                                :VISUAL {:color colors.base0D
                                                         :modes [:v :vs]}
                                                :LINES {:color colors.base0D
                                                        :modes [:V :Vs]}
                                                :BLOCK {:color colors.base0D
                                                        :modes ["\022"
                                                                "\022s"
                                                                "\019"]}
                                                :SELECT {:color colors.base09
                                                         :modes [:s :S]}
                                                :INSERT {:color colors.base08
                                                         :modes [:i :ic :ix]}
                                                :REPLACE {:color colors.base0E
                                                          :modes [:R :Rc :Rx]}
                                                :V-REPLACE {:color colors.base0E
                                                            :modes [:Rv
                                                                    :Rvc
                                                                    :Rvx]}
                                                :COMMAND {:color colors.dark_green
                                                          :modes [:c :cv :ce]}
                                                :ENTER {:color colors.base0C
                                                        :modes [:r]}
                                                :MORE {:color colors.base0C
                                                       :modes [:rm]}
                                                :CONFIRM {:color colors.base09
                                                          :modes [:r?]}
                                                :SHELL {:color colors.dark_green
                                                        :modes [" !"]}
                                                :TERM {:color colors.dark_green
                                                       :modes [:nt :t]}
                                                :NONE {:color colors.base0A
                                                       :modes [:null]}}]
                                  (var res {:mode_names {} :mode_colors {}})
                                  (each [name {: color : modes} (pairs mode_map)]
                                    (tset res :mode_colors name color)
                                    (each [_ mode (ipairs modes)]
                                      (tset res :mode_names mode name)))
                                  res)})
             lsps {:hl {:fg colors.dark_yellow}
                   1 {:provider ""}
                   2 {:flexible 2
                      1 [{:provider " ["}
                         {:provider #(table.concat (icollect [_ {: name} (pairs (vim.lsp.get_clients {:bufnr 0}))]
                                                     name)
                                                   " ")}
                         {:provider "]"}]
                      2 [{:provider " "}
                         {:provider #(length (vim.lsp.get_clients))}]}}
             ; Breadcrumbs
             navic-available (. (require :nvim-navic) :is_available)
             navic (let [{: get_location} (require :nvim-navic)
                         navic-get #(let [loc (get_location $...)]
                                      (if (not (empty? loc))
                                          (.. navic-sep loc)
                                          loc))]
                     {:hl navic-sep-hl
                      :update :CursorMoved
                      1 {:flexible 5
                         1 {:provider #(navic-get)}
                         2 {:provider #(navic-get {:depth_limit 1})}}})
             no-cmd #(= vim.o.cmdheight 0) ; Number of search results
             has-search-count #(not= vim.v.hlsearch 0)
             search-count {:init #(let [(ok search) (pcall vim.fn.searchcount)]
                                    (if (and ok search.total)
                                        (tset $1 :search search)))
                           :hl {:fg colors.dark_yellow :bold true}
                           :provider #(let [{:search {: current
                                                      : total
                                                      : maxcount}} $1]
                                        (string.format "[%d/%d]" current
                                                       (math.min total maxcount)))}
             cursor-pos [{:provider " %l" :hl {:fg colors.dark_blue}}
                         {:provider ":"}
                         {:provider "%c" :hl {:fg colors.dark_blue}}]]
         (set! cmdheight 0)
         (set! laststatus 3)
         (set! showcmdloc :statusline) ; Setup navic highlight groups
         (each [hl-name hl-opt (pairs {:NavicIconsArray {:fg colors.yellow}
                                       :NavicIconsBoolean {:fg colors.cyan
                                                           :bold true}
                                       :NavicIconsClass {:fg colors.cyan}
                                       :NavicIconsConstant {:fg colors.yellow}
                                       :NavicIconsConstructor {:fg colors.cyan}
                                       :NavicIconsEnum {:fg colors.cyan}
                                       :NavicIconsEnumMember {:fg colors.fg0}
                                       :NavicIconsEvent {:fg colors.fg0}
                                       :NavicIconsField {:fg colors.fg0
                                                         :italic true}
                                       :NavicIconsFile {:fg colors.green}
                                       :NavicIconsFunction {:fg colors.blue
                                                            :italic true}
                                       :NavicIconsInterface {:fg colors.cyan}
                                       :NavicIconsKey {:fg colors.cyan}
                                       :NavicIconsMethod {:fg colors.blue
                                                          :italic true}
                                       :NavicIconsModule {:fg colors.fg0
                                                          :italic true}
                                       :NavicIconsNamespace {:fg colors.fg0
                                                             :italic true}
                                       :NavicIconsNull {:fg colors.cyan}
                                       :NavicIconsNumber {:fg colors.magenta}
                                       :NavicIconsObject {:fg colors.cyan}
                                       :NavicIconsOperator {:fg colors.cyan}
                                       :NavicIconsPackage {:fg colors.fg0
                                                           :italic true}
                                       :NavicIconsProperty {:fg colors.fg0
                                                            :italic true}
                                       :NavicIconsString {:fg colors.green
                                                          :italic true}
                                       :NavicIconsStruct {:fg colors.cyan}
                                       :NavicIconsTypeParameter {:fg colors.blue}
                                       :NavicIconsVariable {:fg colors.fg0
                                                            :bold true}
                                       :NavicText {:fg colors.fg1}
                                       :NavicSeparator navic-sep-hl})]
           (tset hl-opt :bg colors.base02)
           (vim.api.nvim_set_hl 0 hl-name hl-opt)) ; Setup plugins we depend on
         (setup :nvim-navic
                {:highlight true :separator navic-sep :lsp {:auto_attach true}})
         ; Setup the statusline itself
         (setup :heirline
                {:statusline {:hl {:fg colors.fg0 :bg colors.bg2}
                              ; left side
                              1 [vi_mode
                                 {:condition #(and (is-macrorec) (no-cmd))
                                  1 {:hl {:bg macrorec-bg}
                                     ; TODO: factor more nicely, merge with vi_mode
                                     1 {:provider " "}
                                     2 macrorec
                                     3 {:provider ""}}
                                  2 {:provider "" :hl {:fg macrorec-bg}}}
                                 {:provider " "}
                                 {:condition lsp_attached
                                  1 lsps
                                  2 {:provider " "}}
                                 {:condition #(and (has-search-count) (no-cmd))
                                  1 search-count
                                  2 {:provider " "}}
                                 {:provider "%3.5(%S%)"
                                  :hl {:fg colors.dark_orange :bold true}}
                                 {:provider " "}]
                              2 {:provider "%="}
                              ; middle
                              3 [{:hl {:fg colors.fg0 :bold true}
                                  :flexible 3
                                  1 {:provider "%f"}
                                  2 {:provider #(let [name (-> vim.g.actual_curbuf
                                                               (tonumber)
                                                               (vim.api.nvim_buf_get_name)
                                                               (vim.fn.fnamemodify ":t"))]
                                                  (if (not= "" name) name
                                                      "[No Name]"))}}
                                 {:condition #(navic-available)
                                  1 [navic {:provider " "}]}]
                              4 {:provider "%="}
                              ; right side
                              5 [{:provider " "}
                                 {:condition is_git_repo
                                  :init #(tset $1 :status_dict
                                               vim.b.gitsigns_status_dict)
                                  1 [{:hl {:fg colors.dark_orange :bold true}
                                      1 {:provider "󰘬"}
                                      2 {:flexible 4
                                         1 {:provider (fn [self]
                                                        (.. " "
                                                            self.status_dict.head))}
                                         2 {:provider ""}}}
                                     {:flexible 1
                                      1 [{:provider " "}
                                         {:provider (fn [self]
                                                      (.. "+"
                                                          (or self.status_dict.added
                                                              0)))
                                          :hl {:fg colors.dark_green}}
                                         {:provider (fn [self]
                                                      (.. "-"
                                                          (or self.status_dict.removed
                                                              0)))
                                          :hl {:fg colors.dark_red}}
                                         {:provider (fn [self]
                                                      (.. "~"
                                                          (or self.status_dict.changed
                                                              0)))
                                          :hl {:fg colors.dark_yellow}}]
                                      2 {:provider ""}}
                                     {:provider " "}]}
                                 sep-right
                                 ; Line count
                                 {:provider " %L "}
                                 sep-right
                                 ; Cursor position
                                 cursor-pos
                                 ; Percentage in file
                                 {:hl {:fg colors.dark_green
                                       :bold true
                                       :reverse true}
                                  1 {:provider " " :hl {:reverse false}}
                                  2 {:provider "%p%% "}}]}})))

; Keep ftplugin logic inline here to not spread the config too much
(let [ftplugins {:fennel (fn []
                           (nmap! [:buffer] :gqq
                                  (.. ":<C-u>w<CR>:" "! fnlfmt --fix %<CR><CR>"))
                           (let [{: find_files} (require :telescope.builtin)
                                 {: cache-prefix} (require :hotpot.api.cache)]
                             (setl- iskeyword ".") ; Search in cache
                             (nmap! [:buffer :silent] :<leader>fc
                                    #(find_files {:cwd (cache-prefix)
                                                  :hidden true}))))
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
                          (imap! [:buffer] :<C-CR>
                                 #(action :org_mappings.meta_return)))
                        (vim.cmd "abbreviate -- - [ ]"))
                 :qf #(nmap! [:buffer] :q :<Cmd>cclose<CR>)
                 :rust (fn []
                         (nmap! [:buffer] :<leader>cr ":<C-u>Crun<CR>")
                         (nmap! [:buffer] :<leader>cb ":<C-u>make build<CR>")
                         (nmap! [:buffer] :<leader>ct ":<C-u>make test<CR>")
                         (nmap! [:buffer] :<leader>cl ":<C-u>make clippy<CR>")
                         (nmap! [:buffer] :<leader>rf ":<C-u>RustFmt<CR>")
                         (vmap! [:buffer] :<leader>rf ":RustfmtRange<CR>"))
                 :sh #(setl! shiftwidth 4)
                 :zsh #(setl! shiftwidth 4)}]
  (autocmd! :ftplugins
            (icollect [filetype callback (pairs ftplugins)]
              {:event :FileType :pattern filetype : callback})))

; Floating preview in quickfix window
(add! :kevinhwang91/nvim-bqf
      #(setup :bqf {:func_map {:fzffilter "" :open "<C-]>"}
              :preview {:winblend 0}
              }))


(vim.keymap.set ["" "!" :t :l] "<C-;>" "<C-]>" {:remap true})
