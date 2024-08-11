(import-macros {: set! : setl! : setg! : set+ : let! : with-cb} :utils.macros)
(local {: add!} (require :pkg))
(local {: nmap! : vmap! : tmap! : cmap! : imap! : xmap!
        : command! : augroup! : put!}
  (require :utils.nvim))
(local {: nil?} (require :utils))
(local {: spawn : spawn-capture-output} (require :utils.async))
(local {: mk-op!} (require :utils.operator))

(fn feed [keys]
  (vim.api.nvim_feedkeys
    (vim.api.nvim_replace_termcodes keys true true true) :m true))

(fn setup [mod ?opts]
  ; Loads module and calls its `setup` function
  (let [{: setup} (require mod)]
    (setup (or ?opts {}))))

; Manage bootstrapped packages
(add! "rktjmp/hotpot.nvim")
(add! "bR3iN/pkg.nvim")


;; General Options and Keymaps

; set leader keys
(let! mapleader " ")
(let! maplocalleader " ")

; Enable dialogs
(set! confirm)

; Needed for hl-CursorLineNr
(set! cursorline)

; Ignore case for lowercase searches
(set! ignorecase)
(set! smartcase) ;requires `ignorecase`

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
(set! wildmode [:longest:full :full])

; Undo behaviour
(set! undofile)
(set! undodir (.. (vim.fn.stdpath :data) :undo))

; Wrap behaviour
(set! breakindent)
(set! wrap)

; Default tab behaviour
(setg! shiftwidth 4)
(set! tabstop 4)
(set! expandtab)

; Start with folds expanded
(set! foldlevelstart 99)

; Correctly indent when pasting multiple lines in insert mode
(imap! "<C-r>" "<C-r><C-o>")

; Break insert mode; similar to <C-g>u but also "resets" <C-a>
; TODO: experimental
(imap! "<C-q>" "<Esc>a")

; Clear highlight search
(nmap! "<C-L>" ":<c-u>nohlsearch<CR><C-L>")

; Capitalize word in front of cursor
(imap! "<c-u>" "<Esc>viwUea")

; Select until end of line (like `C`, `D` and `Y`)
(nmap! "<leader>v" "vg_")

(add! "tpope/vim-repeat")
(add! "tpope/vim-commentary")
(add! "tpope/vim-surround"
      (fn []
        ; Find numbers with `:echo char2nr("B")`
        ; "B"
        (let! surround_66  "{\r}\1\1")))

; Misc. autocmds
(let [autocmd! (augroup! :init.lua)]
  ; Autoreload config files on save
  (autocmd! :BufWritePost (.. vim.env.HOME "/.{dotfiles,config}/nvim/*.{vim,lua,fnl}")
            #(dofile vim.env.MYVIMRC))

  ; Don't create undofiles for temporary files
  (autocmd! :BufWritePre "/tmp/*"
            #(setl! noundofile))

  ; Highlight on yank
  (autocmd! :TextYankPost "*"
            #(vim.highlight.on_yank {:higroup :IncSearch
                                     :timeout 150})))


;; Package management

(nmap! "<leader>pu" "<Plug>PkgUpdate")
(nmap! "<leader>pc" "<Plug>PkgClean")
(nmap! "<leader>pl" "<Plug>PkgList")


;; Appearance

(set! termguicolors)

; Custom color setup; load colorscheme description (name + base16 colors)
(add!
  "bR3iN/base16.nvim"
  #(let [hl-ext! (fn [name hl]
                   (->> {: name}
                        (vim.api.nvim_get_hl 0)
                        (vim.tbl_extend :keep hl)
                        (vim.api.nvim_set_hl 0 name)))
         {: colors : name} (require :base16-colors)
             {: darken} (require :base16.utils)]
     ; Decide if we use an external colorscheme or our own base16-based one
     (match name
       "Tokyonight Moon" (add! "folke/tokyonight.nvim"
                               #(do
                                  (setup :tokyonight {:transparent true})
                                  (vim.cmd.colorscheme "tokyonight-moon")))
       "Nord" (add! "shaunsingh/nord.nvim"
                    #(do
                       (let! nord_disable_background true)
                       (vim.cmd.colorscheme "nord")))
       "Gruvbox" (add! "ellisonleao/gruvbox.nvim"
                       #(do
                          (setup :gruvbox {:transparent_mode true})
                          (vim.cmd.colorscheme "gruvbox")))
       "Kanagawa" (add! "rebelot/kanagawa.nvim"
                        #(do
                           (setup :kanagawa {:transparent true})
                           (vim.cmd.colorscheme "kanagawa")))
       "Catppuccin" (add! "catppuccin/nvim"
                          #(do
                             (vim.cmd.colorscheme "catppuccin-mocha")
                             (vim.api.nvim_set_hl 0 :Normal {:fg colors.base05 :bg None})
                             (vim.api.nvim_set_hl 0 :NormalNC {:fg colors.base05 :bg None})))
       ; Fallback; derives colorscheme from base16 colors
       _ (do
            (let! base16_colors_lua :base16-colors)
            (vim.cmd.colorscheme :base16)))
     ; Highlights overrides and groups for local plugins
     (hl-ext! :WinSeparator {:fg colors.base02})
     ; (hl-ext! :SignColumn {:bg colors.base02})
     ; (hl-ext! :LineNrAbove {:bg colors.base01})
     ; (hl-ext! :LineNrBelow {:bg colors.base01})
     ; (hl-ext! :CursorLineNr {:bg (darken colors.base02 0.1)})
     (hl-ext! :CursorLine {:bg (darken colors.base02 0.1)})
     (hl-ext! :Comment {:fg colors.base03})
     (hl-ext! :CommentHighlighted {:fg colors.base0F})
     (vim.api.nvim_set_hl 0 :GitSignsCurrentLineBlame {:link :CommentHighlighted})
     (hl-ext! :TrailingWhitespace {:fg colors.base09 :bg colors.base09})
     ; Toggle the color of comments TODO: For some reason doesn't work with external nord colorscheme
     ; Load after setting `CommentHighlighted` above
     (setup :plugin.toggle-comments)
     (nmap! "<C-h>" ":ToggleComments<CR>")
     (imap! "<C-h>" "<C-o>:ToggleComments<CR>")))

(set! fillchars { :vert :| })

(set! conceallevel 2)
; (set! cmdheight 2)
(set! scrolloff 5)
(set! linebreak)

(require :plugin.highlight-trailing-whitespace)

; Highlights hex color codes with their color
(add! "NvChad/nvim-colorizer.lua"
      #(setup :colorizer {:user_default_options {:names false}}))


(add! "lukas-reineke/headlines.nvim" #(setup :headlines))

(add!
  "nvim-orgmode/orgmode"
  #(setup
     :orgmode
     {:org_agenda_files ["~/neorg/**/*.org"]
      :win_split_mode :edit
      :org_default_notes_files "~/neorg/notes.org"
      :mappings {:prefix "<leader>a"
                 :org {:org_open_at_point "<C-]>"}
                 :org_return_uses_meta_return true}}))

; (add! "lukas-reineke/indent-blankline.nvim"
;       (fn []
;         (setup :ibl)))


;; Navigation

(nmap! "<Tab>" "ge")
(vmap! "<Tab>" "ge")
(nmap! "<S-Tab>" "gE")
(vmap! "<S-Tab>" "gE")

; Sane `<Esc>` behaviour in terminal mode
(tmap! "<Esc>" "<C-\\><C-n>")
(tmap! "<C-v><Esc>" "<Esc>")

; Open main.fnl
(nmap! "<leader>ov" ":<C-u>edit ~/.config/nvim/fnl/main.fnl<CR>")

; Goto alternative/[p]revious file
(nmap! "<C-p>" "<C-^>")

; Buffer navigation
(nmap! "]b" ":<C-u>bnext<CR>")
(nmap! "[b" ":<C-u>bprev<CR>")

; Navigate quickfix and location lists
(nmap! "]q"         ":<C-u>cnext<CR>")
(nmap! "[q"         ":<C-u>cprev<CR>")
(nmap! "]Q"         ":<C-u>clast<CR>")
(nmap! "[Q"         ":<C-u>cfirst<CR>")
(nmap! "<leader>oq" ":<C-u>copen<CR>")
(nmap! "<leader>qq" ":<C-u>cclose<CR>")

(nmap! "]l"         ":<C-u>lnext<CR>")
(nmap! "[l"         ":<C-u>lprev<CR>")
(nmap! "]L"         ":<C-u>llast<CR>")
(nmap! "[L"         ":<C-u>lfirst<CR>")
(nmap! "<leader>ol" ":<C-u>lopen<CR>")
(nmap! "<leader>ql" ":<C-u>lclose<CR>")

; Floating preview in quickfix window
(add! "kevinhwang91/nvim-bqf"
       #(setup :bqf {:func_map {:fzffilter ""}}))

; Better folds
(add! ["kevinhwang91/nvim-ufo" "kevinhwang91/promise-async"]
      #(setup :ufo {:provider_selector #[:treesitter :indent]}))

; Open text in browser
(let! browser :qutebrowser)
(mk-op! :OpenInBrowser
       (let [open-in-browser #(spawn vim.g.browser {:args [$1]})]
         (fn [lines]
           (vim.tbl_map open-in-browser lines))))
(nmap! "gb" "<Plug>OpenInBrowser")
(vmap! "gb" "<Plug>OpenInBrowser")

; Write and quit
(nmap! "<leader>w"  ":<C-u>w<cr>")
(nmap! "<leader>qv" ":<C-u>quit<cr>")
; "sudo write"-trick via polkit agent
(nmap! "<leader>W" ":<C-u>w !pkexec tee % >/dev/null<CR>")

; Navigate history containing substring
(cmap! "<M-p>" #(feed "<Up>"))
(cmap! "<M-n>" #(feed "<Down>"))

(add! ["nvim-telescope/telescope.nvim"
       "nvim-lua/plenary.nvim"]
      (fn []
        (let [builtins (require :telescope.builtin)
              actions (require :telescope.actions)]
          ;Setup plugin
          (setup :telescope
                 {:defaults {:sorting_strategy :ascending
                             :scroll_strategy :limit
                             :layout_config {:prompt_position :top}
                             :layout_strategy :flex
                             :mappings {:i {"<C-j>" actions.preview_scrolling_down
                                            "<C-k>" actions.preview_scrolling_up
                                            "<C-l>" actions.preview_scrolling_right
                                            "<C-h>" actions.preview_scrolling_left
                                            "<C-q>" (+ actions.smart_send_to_qflist actions.open_qflist)
                                            "<C-x>" false
                                            "<C-d>" actions.results_scrolling_down
                                            "<C-u>" actions.results_scrolling_up
                                            "<C-s>" actions.select_horizontal}
                                        :n {"<C-j>" actions.preview_scrolling_down
                                            "<C-k>" actions.preview_scrolling_up
                                            "<C-l>" actions.preview_scrolling_right
                                            "<C-h>" actions.preview_scrolling_left
                                            "<C-q>" (+ actions.smart_send_to_qflist actions.open_qflist)
                                            "<C-x>" false
                                            "<C-d>" actions.results_scrolling_down
                                            "<C-u>" actions.results_scrolling_up
                                            "<C-s>" actions.select_horizontal}}}})
          ; Setup keymaps
          (let [pick (fn [action ?opts]
                       (let [picker (. builtins action)]
                         (picker ?opts)))]
            (each [[mode lhs] rhs
                   ; TODO: Try out <C-r><C-[fp(files)wa(words)l(line)]>
                   (pairs {[:n "<leader>ff"] #(pick :find_files {:follow true})
                           ; Resumes previous picker
                           [:n "<leader>f."] #(pick :resume)
                           [:n "<leader>b"]  #(pick :buffers {:sort_lastused true :sort_mru true})
                           [:n "<leader>fd"] #(pick :diagnostics)
                           [:n "<leader>fg"] #(pick :live_grep)
                           [:n "<leader>fl"] #(pick :live_grep {:grep_open_files true})
                           [:n "<leader>fL"] #(pick :lsp_workspace_symbols)})]
              (vim.keymap.set mode lhs rhs))))))

; Leap with s
(add! "ggandor/leap.nvim"
      (fn []
        (let [{: set_default_keymaps : setup} (require :leap)]
          (set_default_keymaps)
          (setup {:safe_labels {}}))))

; Smooth scrolling
(add! "karb94/neoscroll.nvim"
      (fn []
        (let [{: setup : scroll : zt : zz : zb} (require :neoscroll)
              get-height #(vim.api.nvim_win_get_height 0)]
          ; Disable default mappings
          (setup {:mappings {}})
          ; Add custom mappings
          (nmap! :<C-u> #(scroll (- vim.wo.scroll) true 150))
          (nmap! :<C-d> #(scroll vim.wo.scroll true 150))
          (nmap! :<C-b> #(scroll (- (get-height)) true 350))
          (nmap! :<C-f> #(scroll (get-height) true 350))
          (nmap! :zt #(zt 125))
          (nmap! :zz #(zz 125))
          (nmap! :zb #(zb 125)))))

; Split file explorer
(add! "stevearc/oil.nvim"
      #(let [{: setup : open} (require :oil)]
         (setup
           {:columns [] ; disables icons
            :use_default_keymaps false
            :keymaps {"g?"    :actions.show_help
                      "<C-]>" :actions.select
                      "<C-s>v" :actions.select_vsplit
                      "<C-s>s" :actions.select_split
                      "gp"    :actions.preview
                      "<C-p>" :actions.close
                      "gf"    :actions.refresh
                      "-"     :actions.parent
                      "_"     :actions.open_cwd
                      "`"     :actions.cd
                      "~"     :actions.tcd
                      "g."    :actions.toggle_hidden}})
         (nmap! "-" open)))

; Tmux interop
(add! "christoomey/vim-tmux-navigator"
      (fn []
        ; Disable default mappings
        (let! tmux_navigator_no_mappings 1)
        ; Set custom mappings
        (let [mk-lhs #(.. :<M- $1 :>)]
          (each [direction keys (pairs
                                  {:Left  [:h :left]
                                   :Right [:l :right]
                                   :Up    [:k :up]
                                   :Down  [:j :down]})]
            (each [_ key (ipairs keys)]
              (let [lhs (mk-lhs key)
                    rhs #(vim.cmd (.. :TmuxNavigate direction))]
                (each [_ mk-map (ipairs [nmap! tmap!])]
                  (mk-map lhs rhs))))))))


;; Filetype plugins

(add! "elkowar/yuck.vim")

; Treesitter indentation for fennel is messed up, so use this plugin for this
(add! "bakpakin/fennel.vim")

(add! "lervag/vimtex"
      (fn []
        (let! vimtex_format_enabled 1)
        (let! vimtex_quickfix_mode 0)
        (let! vimtex_view_method :zathura)
        (let! tex_flavor :latex)))

(add! ["nvim-neorg/neorg"
       "nvim-treesitter/nvim-treesitter"
       "nvim-lua/plenary.nvim"]
      (fn []
        ; Define global keybinds for opening Neorg workspaces
        (let [Neorg #(vim.cmd.Neorg {:args $1})
              cd #(vim.cmd.cd "%:h")
              bindings {"<leader>gp" #(do
                                        (Neorg [:workspace :projects])
                                        (cd))
                        "<leader>gj" #(do
                                        (Neorg [:journal :toc :update])
                                        (Neorg [:workspace :journal])
                                        (cd))
                        "<leader>gJ" #(do
                                        (Neorg [:journal :today])
                                        (cd))
                        "<leader>gn" #(do
                                        (Neorg [:workspace :notes])
                                        (cd))}
              running? #(->> :Neorg
                             (. (vim.api.nvim_get_commands {}))
                             (nil?)
                             (not))]
          ; Wrap bindings to lazily start Neorg before executing them
          (each [lhs func (pairs bindings)]
            (let [rhs (fn []
                        (when (not (running?))
                          (vim.cmd.NeorgStart))
                        (func))]
              (nmap! lhs rhs)))

          ; Callback to (re)configure Neorg-local keybindings
          (fn keybinds-hook [keybinds]
            (let [remap_key keybinds.remap_key
                  unmap     keybinds.unmap]
              (remap_key :norg         :n "<CR>" "<C-]>")
              (remap_key :toc-split    :n "<CR>" "<C-]>")
              (remap_key :gtd-displays :n "<CR>" "<C-]>")))

          ; Main config and setup
          (let [configs
                {:core.defaults {}
                 :core.concealer {}
                 :core.qol.toc {:close_after_use true}
                 :core.qol.todo_items {}
                 :core.export {}
                 :core.export.markdown {}
                 :core.journal {:strategy :nested
                                :workspace :journal}
                 :core.dirman {:workspaces {:notes "~/neorg/notes"
                                            :journal "~/neorg/journal"
                                            :projects "~/neorg/projects"}
                               :default :notes
                               ; Open last workspace on `nvim`; can be set to "default" for default workspace instead
                               :open_last_workspace false}
                 :core.completion {:engine :nvim-cmp}
                 :core.keybinds {:default_keybinds true
                                 :hook keybinds-hook}}]
            (setup
              :neorg
              {:load (vim.tbl_map #{:config $1} configs)})))))


;; Autocompletion and snippets

(add! ["hrsh7th/nvim-cmp"
       "hrsh7th/cmp-nvim-lsp"
       "hrsh7th/cmp-nvim-lua"
       "kdheepak/cmp-latex-symbols"
       "hrsh7th/cmp-path"
       "hrsh7th/cmp-omni"
       "hrsh7th/cmp-buffer"
       "hrsh7th/cmp-vsnip"]
      (fn []
        (let
          [cmp (require :cmp)
           jumpable (. vim.fn :vsnip#jumpable)
           feed #(-> $1
                     (vim.api.nvim_replace_termcodes true true true)
                     (vim.fn.feedkeys))
           config {:mapping
                   {:<Tab>  (fn [fallback]
                              (if
                                (cmp.visible) (cmp.select_next_item)
                                (= 1 (jumpable 1)) (feed "<Plug>(vsnip-jump-next)")
                                (fallback)))
                    :<S-Tab> (fn [fallback]
                               (if
                                 (cmp.visible) (cmp.select_prev_item)
                                 (= 1 (jumpable -1)) (feed "<Plug>(vsnip-jump-prev)")
                                 (cmp.complete)))
                    :<CR> (cmp.mapping.confirm {:select false})
                    :<C-e> (cmp.mapping.close)
                    :<C-Space> (cmp.mapping.complete)}

                   :snippet
                   {:expand (fn [{: body}]
                              ((. vim.fn :vsnip#anonymous) body))}

                   :sources
                   [{:name :nvim_lsp}
                    {:name :orgmode}
                    {:name :nvim_lua}
                    {:name :neorg}
                    {:name :vsnip}
                    {:name :path}
                    {:name :latex_symbols}
                    {:name :omni}
                    {:name :buffer
                     :option {:keyword_pattern "\\k\\+"}}]

                   :formatting
                   {:format (let [display-names
                                  {:nvim_lsp      "[LSP]"
                                   :nvim_lua      "[Lua]"
                                   :vsnip         "[Vsp]"
                                   :latex_symbols "[LTX]"
                                   :path          "[Pth]"
                                   :omni          "[Omn]"
                                   :calc          "[Clc]"
                                   :buffer        "[Buf]"}]
                              (fn [{:source {: name}} item]
                                (->> name
                                     (. display-names)
                                     (#(or $1 "[???]"))
                                     (tset item :menu))
                                item))}}]
          (cmp.setup config))))

(add! ["hrsh7th/vim-vsnip"
       ; Community maintained snippet collection
       ; FIXME: contains faulty norg snippets
       ; (add! "rafamadriz/friendly-snippets")
       ; Snippet integration with neovims builtin LSP client
       "hrsh7th/vim-vsnip-integ"]
      (fn []
        ; set snippet directory
        (let! vsnip_snippet_dir (.. (vim.fn.stdpath :config) :/vsnip))))

;; Coding related stuff

; Run current file; <Plug>RunFile bindings are defined in ftplugins where applicable
(nmap! "<leader>rr" "<Plug>RunFile")

(nmap! "<leader>mk" ":make!<CR>")
(nmap! "<leader>mf" ":make! flash<CR>")
(nmap! "<leader>mc" ":make! clean<CR>")
(nmap! "<leader>mt" ":make! test<CR>")
(nmap! "<leader>mb" ":make! build<CR>")

(add! "neomake/neomake"
      (fn []
        (nmap! "<leader>nm" #(vim.cmd :Neomake))
        (nmap! "<leader>nc" #(vim.cmd :NeomakeClean))

        ; Appearance
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
           })

        ; Link Neomake highlight groups and relink them on colorscheme change
        (let [links {:NeomakeErrorSign          :DiagnosticSignError
                     :NeomakeWarningSign        :DiagnosticSignWarn
                     :NeomakeMessageSign        :DiagnosticSignHint
                     :NeomakeInfoSign           :DiagnosticSignInfo
                     :NeomakeVirtualtextError   :DiagnosticError
                     :NeomakeVirtualtextWarning :DiagnosticWarn
                     :NeomakeVirtualtextMessage :DiagnosticHint
                     :NeomakeVirtualtextInfo    :DiagnosticInfo}
              set-hl #(each [name link (pairs links)]
                        (vim.api.nvim_set_hl 0 name {: link}))
              autocmd! (augroup! :neomake_highlighting)]
          (set-hl)
          (autocmd! :ColorScheme "*" set-hl))))

(add! ["nvim-treesitter/nvim-treesitter"
       ; "nvim-treesitter/playground"
       "nvim-treesitter/nvim-treesitter-textobjects"]
      (fn []
        ; Use treesitter-based folds
        (set! foldmethod :expr)
        (set! foldexpr "nvim_treesitter#foldexpr()")
        (setup
          :nvim-treesitter.configs
          {:highlight {:enable true}
           :indent {:enable false}
           ; :additional_vim_regex_highlighting [:fennel] ; Use with indent=true
           :textobjects {:select {:enable true
                                  :keymaps {"if" "@function.inner"
                                            "af" "@function.outer"
                                            "ic" "@call.inner"
                                            "ac" "@call.outer"
                                            "il" "@loop.inner"
                                            "al" "@loop.outer"
                                            "ik" "@conditional.inner"
                                            "ak" "@conditional.outer"}}
                         :swap {:enable true
                                :swap_next {"<leader>." "@parameter.inner"}
                                :swap_previous {"<leader>," "@parameter.inner"}}}})))

; (add! "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
;       #(fn []
;          (setup :lsp_lines)
;          ; Disable virtual_text since it's redundant due to `lsp_lines.nvim`
;          (vim.diagnostic.config {:virtual_text false})))

(add! ["neovim/nvim-lspconfig"
       "mickael-menu/zk-nvim"]
      (fn []
        (let [{: set-default-keymaps!
               : ls-setup!
               : mk-on_attach
               : mk-capabilities} (require :lsp)]
          (set-default-keymaps!
            {[:n "gD"]         vim.lsp.buf.declaration
             [:n "gd"]         vim.lsp.buf.definition
             [:n "K"]          vim.lsp.buf.hover
             [:n "gi"]         vim.lsp.buf.implementation
             [:n "<C-k>"]      vim.lsp.buf.signature_help
             [:n "<leader>D"]  vim.lsp.buf.type_definition
             [:n "<leader>rn"] vim.lsp.buf.rename
             [:n "<leader>ca"] vim.lsp.buf.code_action
             [:n "gr"]         vim.lsp.buf.references
             [:n "gqq"]        vim.lsp.buf.format
             [:v "gq"]         vim.lsp.buf.format
             [:n "<leader>od"] #(vim.diagnostic.open_float {:border :single})
             [:n "[d"]         #(vim.diagnostic.goto_prev {:float {:border :single}})
             [:n "]d"]         #(vim.diagnostic.goto_next {:float {:border :single}})})

          (ls-setup!
            :clangd {}
            {[:n "<C-c>"] #(vim.cmd.ClangdSwitchSourceHeader)})

          ; {:name :lua_ls
          ;  :config (require :lsp.configs.sumneko_lua)
          ;  :keymaps {}}

          (ls-setup! :bashls)
          ; (ls-setup! :vimls)
          ; (ls-setup! :fennel_ls)
          (ls-setup! :pyright)
          (ls-setup! :cmake)
          (ls-setup! :rust_analyzer)
          (ls-setup! :hls)
          (ls-setup! :racket_langserver)

          (let [zk (require :zk)
                util (require :zk.util)
                create-and-insert-link #(let [loc (util.get_lsp_location_from_caret)
                                              title (vim.fn.input "Title: ")]
                                          (if (not= (# title) 0)
                                            (zk.new {: title
                                                     :edit false
                                                     :insertLinkAtLocation loc})))
                create-note #(let [title (vim.fn.input "Title: ")]
                               (if (not= (# title) 0)
                                 (zk.new {: title})))
                extra-keymaps {[:i "<C-h>"] "<Esc>hcT|"
                               [:i "<C-l>"] "<Esc>2la"
                               [:i "<C-y>"] "<Esc>2hvT|uf]2la"
                               [:n "<localleader>nz"] create-note
                               [:n "<localleader>no"] #(zk.edit)
                               [:n "<localleader>nb"] #(vim.cmd.ZkBacklinks)
                               [:i "<C-o>"] "<Esc>:ZkInsertLink<CR>"
                               [:i "<C-j>"] create-and-insert-link
                               [:i "<C-p>"] #(spawn-capture-output
                                               :zk-screenshot nil
                                               (fn [code _ stdout stderr]
                                                 (if (= 0 code)
                                                   (put! (.. "![[" stdout "]]")))))}]
            (zk.setup {:picker :telescope
                       :lsp {:config {:on_attach (mk-on_attach extra-keymaps)
                                      :capabilities (mk-capabilities)}}})))))

(add! ["mfussenegger/nvim-dap"
       "mfussenegger/nvim-dap-python"]
      (fn []
        ; Setup common keymaps
        (let [dap (require :dap)
              adapter-configs {:gdb
                              {:type "executable"
                               :command "gdb"
                               :args ["-i" "dap"]}}
              filetype-configs {:cpp [{:name "Launch C++"
                                      :type "gdb"
                                      :request "launch"
                                      :program "build/main"}]}
              mappings {"<leader>db" (. dap :toggle_breakpoint)
                        "<leader>dl" (. dap :list_breakpoints)
                        "<leader>ds" (. dap :step_into)
                        "<leader>dn" (. dap :step_over)
                        "<leader>dr" (. dap :continue)
                        "<leader>dt" (. dap :terminate)
                        "<leader>dc" (. dap :clear_breakpoints)
                        "<leader>do" (. dap :repl :open)}
              autocmd! (augroup! :nvim-dap)
              prompt-path (fn [prompt cb]
                            (vim.ui.input
                              {: prompt :default (vim.loop.cwd)}
                              #(if $1 (cb $1))))]
          (each [name config (pairs adapter-configs)]
            (tset dap :adapters name config))
          (each [name config (pairs filetype-configs)]
            (tset dap :configurations name config))
          (each [lhs rhs (pairs mappings)]
            (nmap! lhs rhs))

          ; Setup python debugging via dedicated extension
          (setup :dap-python "python"))))

(add! "bR3iN/emanote.nvim")

; Forked as plugin doesn't have an API for custom keybindings
(add! "bR3iN/jupynium.nvim" #(setup :jupynium))

(add! ["folke/trouble.nvim"]
      #(setup :trouble {:icons false}))
(nmap! "<leader>ot" ":<C-u>Trouble<CR>")
(nmap! "<leader>qt" ":<C-u>TroubleClose<CR>")

(add! "b0o/incline.nvim"
      #(let [{: colors} (require :base16-colors)
             diag-colors [colors.red colors.orange colors.green colors.blue]
             diag-icons [ "" "" "" "" ]
             ; Extends first argument (table) with the tail of the arg list; incline
             ; mixes arrays with maps, which is otherwise a bit ugly in fennel.
             comp (fn [hl & comps]
                      (vim.tbl_extend :error comps hl))
             sep (comp {:guifg colors.bg0} " | " )]
         (setup
           :incline
           {:hide {:cursorline true}
            :render
            (fn [{: buf}]
              (let [get-opt #(. vim.bo buf $1)
                    filename (let [name (-> buf
                                            (vim.api.nvim_buf_get_name)
                                            (vim.fn.fnamemodify ":t"))
                                   has-name (not= "" name)]
                               (comp
                                 {:guifg colors.fg0 :gui :bold}
                                 (if has-name name "[No Name]")))
                    diag-indicator (icollect
                                     [severity level (ipairs [ :Error :Warn :Info :Hint ])]
                                     (let [n (length
                                               (vim.diagnostic.get buf {: severity}))]
                                       (if (> n 0)
                                         [(comp
                                            {:guifg (. diag-colors severity)}
                                            (. diag-icons severity) " " n)
                                          sep])))]
                (comp
                  {:guibg colors.bg2}
                  " " diag-indicator filename " "
                  (let [is-ro (or
                                (get-opt :readonly)
                                (not (get-opt :modifiable)))]
                    (if
                      is-ro (comp {:guifg colors.green} " ")
                      ""))
                  (if
                    (get-opt :modified) (comp {:guifg colors.yellow} " ")
                    ""))))})))

(add! "lewis6991/gitsigns.nvim"
      #(setup
         :gitsigns
         {:signcolumn false}))

(add! ["rebelot/heirline.nvim"
       "SmiteshP/nvim-navic"]
      #(let [{: colors} (require :base16-colors)
             {: lsp_attached : is_git_repo} (require :heirline.conditions)
             ;; Statusline components
             sep-right {:provider "" :hl {:fg colors.bg0 :bold true}}
             sep-left {:provider "" :hl {:fg colors.bg0 :bold true}}
             ; Current mode
             vi_mode (let [get-color #(let [{: mode_colors : mode} $1]
                                        (. mode_colors mode))]
                       {:provider #(.. " " (. $1 :mode) " ")
                        1 {:provider ""
                           :hl #{:fg (get-color $1) :reverse false}}
                        2 {:provider " "
                           :hl {:reverse false}}
                        :init #(let [{: mode_names} $1]
                                 (->> (vim.fn.mode 1)
                                      (. mode_names)
                                      (tset $1 :mode)))
                        :hl #{:reverse true :bold true
                              :fg (get-color $1)}
                        :update {1 :ModeChanged :pattern "*:*"
                                 :callback #(vim.schedule_wrap #(vim.cmd :redrawstatus))}
                        :static (let [mode_map {"NORMAL" {:color colors.base0B
                                                          :modes ["n" "niI" "niR" "niV"]}
                                                "OP" {:color colors.base0B
                                                      :modes ["no" "nov" "noV" "no\22"]}
                                                "VISUAL" {:color colors.base0D
                                                          :modes ["v" "vs"]}
                                                "LINES" {:color colors.base0D
                                                         :modes ["V" "Vs"]}
                                                "BLOCK" {:color colors.base0D
                                                         :modes ["\22" "\22s" "\19"]}
                                                "SELECT" {:color colors.base09
                                                          :modes ["s" "S"]}
                                                "INSERT" {:color colors.base08
                                                          :modes ["i" "ic" "ix"]}
                                                "REPLACE" {:color colors.base0E
                                                           :modes ["R" "Rc" "Rx"]}
                                                "V-REPLACE" {:color colors.base0E
                                                             :modes ["Rv" "Rvc" "Rvx"]}
                                                "COMMAND" {:color colors.base0B
                                                           :modes ["c" "cv" "ce"]}
                                                "ENTER" {:color colors.base0C
                                                         :modes ["r"]}
                                                "MORE" {:color colors.base0C
                                                        :modes ["rm"]}
                                                "CONFIRM" {:color colors.base09
                                                           :modes ["r?"]}
                                                "SHELL" {:color colors.base0B
                                                         :modes [" !"]}
                                                "TERM" {:color colors.base0B
                                                        :modes ["nt" "t"]}
                                                "NONE" {:color colors.base0A
                                                        :modes ["null"]}}]
                                  (var res {:mode_names {} :mode_colors {}})
                                  (each [name {: color : modes} (pairs mode_map)]
                                    (tset res :mode_colors name color)
                                    (each [_ mode (ipairs modes)]
                                      (tset res :mode_names mode name)))
                                  res)})
             lsps {:hl {:fg colors.yellow}
                   1 {:provider " ["}
                                    2 {:provider #(table.concat
                                                    (icollect [_ {: name}
                                                               (pairs (vim.lsp.get_active_clients {:bufnr 0}))]
                                                              name)
                                                    " ")}
                                    3 {:provider "]"}}
             ; Breadcrumbs
             navic-available (. (require :nvim-navic) :is_available)
             navic (let [{: get_location} (require :nvim-navic)]
                     {:hl {:bg colors.base02}
                      :update :CursorMoved
                      1 {:flexible 1
                         1 {:provider get_location}
                         2 {:provider #(get_location {:depth_limit 1})}}})
             no-cmd #(= vim.o.cmdheight 0)
             ; Number of search results
             has-search-count #(not= vim.v.hlsearch 0)
             search-count {:init #(let [(ok search) (pcall vim.fn.searchcount)]
                                    (if (and ok search.total) (tset $1 :search search)))
                           :provider #(let [{:search {: current : total : maxcount}} $1]
                                        (string.format "[%d/%d]" current (math.min total maxcount)))}
             ; Macro currently recording
             is-macrorec #(not= (vim.fn.reg_recording) "")
             macrorec {:provider #(.. "[" (vim.fn.reg_recording) "]")
                       :hl {:fg colors.orange :bold true}
                       :update [:RecordingEnter :RecordingLeave]}
             cursor-pos [{:provider " %l"
                          :hl {:fg colors.blue}}
                         {:provider ":"}
                         {:provider "%c"
                          :hl {:fg colors.blue}}]]
         (set! cmdheight 1)
         (set! laststatus 3)
         (set! showcmdloc :statusline)
         ; Setup navic highlight groups
         (each
           [hl-name hl-opt
            (pairs
              {:NavicIconsArray { :fg colors.yellow }
               :NavicIconsBoolean { :fg colors.cyan :bold true}
               :NavicIconsClass { :fg colors.cyan }
               :NavicIconsConstant { :fg colors.yellow }
               :NavicIconsConstructor { :fg colors.cyan }
               :NavicIconsEnum { :fg colors.cyan }
               :NavicIconsEnumMember { :fg colors.fg0 }
               :NavicIconsEvent { :fg colors.fg0 }
               :NavicIconsField { :fg colors.fg0 :italic true}
               :NavicIconsFile { :fg colors.green }
               :NavicIconsFunction { :fg colors.blue :italic true}
               :NavicIconsInterface { :fg colors.cyan }
               :NavicIconsKey { :fg colors.cyan }
               :NavicIconsMethod { :fg colo :italic true }
               :NavicIconsModule { :fg colors.fg0 :italic true }
               :NavicIconsNamespace { :fg colors.fg0 :italic true }
               :NavicIconsNull { :fg colors.cyan }
               :NavicIconsNumber { :fg colors.magenta }
               :NavicIconsObject { :fg colors.cyan }
               :NavicIconsOperator { :fg colors.cyan }
               :NavicIconsPackage { :fg colors.fg0 :italic true }
               :NavicIconsProperty { :fg colors.fg0 :italic true }
               :NavicIconsString { :fg colors.green :italic true }
               :NavicIconsStruct { :fg colors.cyan }
               :NavicIconsTypeParameter { :fg colors.blue }
               :NavicIconsVariable { :fg colors.fg0 :bold true }
               :NavicText { :fg colors.fg1 }
               :NavicSeparator { :fg colors.bg0 }})]
           (tset hl-opt :bg colors.base02)
           (vim.api.nvim_set_hl 0 hl-name hl-opt))
; Setup plugins we depend on
(setup
  :nvim-navic
  {:highlight true
   :separator "  "
   :lsp {:auto_attach true}})
; Setup the statusline itself
(setup
  :heirline
  {:statusline
   {:hl {:fg colors.fg0 :bg colors.bg2}
    ; left side
    1 [vi_mode
       {:condition lsp_attached
        1 {:provider " "}
        2 lsps}
       {:provider " %3.5(%S%) "
        :hl {:bold true}}]
    2 {:provider "%="}
    ; middle
    3 [{:hl {:fg colors.fg0 :bold true}
        :flexible 2
        1 {:provider "%f"}
        2 {:provider #(let [name (-> vim.g.actual_curbuf
                                     (tonumber)
                                     (vim.api.nvim_buf_get_name)
                                     (vim.fn.fnamemodify ":t"))]
                        (if (not= "" name) name "[No Name]"))}}
       {:condition #(navic-available)
        1 [{:provider " "}
           sep-left
           {:provider " "}
           navic
           {:provider " "}]}]
    4 {:provider "%="}
    ; right side
    5 [{:condition #(and (is-macrorec) (no-cmd))
        1 macrorec
        2 {:provider " "}}
       {:condition #(and (has-search-count) (no-cmd))
        1 search-count
        2 {:provider " "}}
       {:condition is_git_repo
        :init #(tset $1  :status_dict vim.b.gitsigns_status_dict)
        1 [sep-right
           {:provider (fn [self]
                        (.. "  " self.status_dict.head))
            :hl {:fg colors.orange :bold true}}
           {:provider " "}
           {:provider (fn [self]
                        (.. "+" (or self.status_dict.added 0)))
            :hl {:fg colors.green}}
           {:provider (fn [self]
                        (.. "-" (or self.status_dict.removed 0)))
            :hl {:fg colors.red}}
           {:provider (fn [self]
                        (.. "~" (or self.status_dict.changed 0)))
            :hl {:fg colors.yellow}}
           {:provider " "}]}
       sep-right
       ; Line count
       {:provider " %L "}
       sep-right
       ; Cursor position
       cursor-pos
       ; Percentage in file
       {:hl {:fg colors.green :bold true :reverse true}
        1 {:provider " " :hl {:reverse false}}
        2 {:provider " %p%% "}}]}})))
