(import-macros {: set! : setl! : setg! : set+ : let! } :utils.macros)
(local {: add!} (require :pkg))
(local {: nmap! : vmap! : tmap! : cmap! : imap!
        : command! : augroup! : put!}
  (require :utils.nvim))
(local {: nil?} (require :utils))
(local {: spawn : spawn-capture-output} (require :utils.async))
(local {: mk-op} (require :utils.operator))

;; General Options and Keymaps
; set leader keys
(let! mapleader " ")
(let! maplocalleader " ")

; set default browser
(let! browser :qutebrowser)

; enable dialogs
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

; Appearance
(set! termguicolors)
(vim.cmd.colorscheme :base16)
(set! fillchars { :vert :| })

(set! conceallevel 2)
(set! cmdheight 2)
(set! scrolloff 5)
(set! linebreak)

; Open text in browser
(mk-op :OpenInBrowser
       (let [open-in-browser #(spawn [vim.g.browser $1] {:args [$1]})]
         (fn [lines]
           (vim.tbl_map open-in-browser lines))))

(nmap! "gb" "<Plug>OpenInBrowser")
(vmap! "gb" "<Plug>OpenInBrowser")

; Correctly indent when pasting multiple lines in insert mode
(imap! "<C-r>" "<C-r><C-o>")

; select until end of line (like `C`, `D` and `Y`)
(nmap! "<leader>v" "vg_")

; Goto alternative/[p]revious file
(nmap! "<C-p>" "<C-^>")

; Toggle the color of comments
(nmap! "<C-h>" ":ToggleComments<CR>")
(imap! "<C-h>" "<C-o>:ToggleComments<CR>")

; clear highlight search
(nmap! "<C-L>" ":<c-u>nohlsearch<CR><C-L>")

(nmap! "<leader>mk" ":make!<CR>")

; Run current file; <Plug>RunFile bindings are defined in
; ftplugins where applicable
(nmap! "<leader>rr" "<Plug>RunFile")

; Sane `<Esc>` behaviour in terminal mode
(tmap! "<Esc>" "<C-\\><C-n>")
(tmap! "<C-v><Esc>" "<Esc>")

; Open terminal
(nmap! :<leader>ot "<Plug>OpenTerminal")

; Open main.fnl
(nmap! "<leader>ev" ":<C-u>edit ~/.config/nvim/fnl/main.fnl<CR>")

; Buffer navigation
(nmap! "]b" ":<C-u>bnext<CR>")
(nmap! "[b" ":<C-u>bprev<CR>")

; Navigate quickfix and location lists
(nmap! "]q"         ":<C-u>cnext<CR>")
(nmap! "[q"         ":<C-u>cprev<CR>")
(nmap! "]Q"         ":<C-u>clast<CR>")
(nmap! "[Q"         ":<C-u>cfirst<CR>")
(nmap! "<leader>oq" ":<C-u>copen<CR>")
(nmap! "<leader>cq" ":<C-u>cclose<CR>")

(nmap! "]l"         ":<C-u>lnext<CR>")
(nmap! "[l"         ":<C-u>lprev<CR>")
(nmap! "]L"         ":<C-u>llast<CR>")
(nmap! "[L"         ":<C-u>lfirst<CR>")
(nmap! "<leader>ol" ":<C-u>lopen<CR>")
(nmap! "<leader>cl" ":<C-u>lclose<CR>")

; Write and quit
(nmap! "<leader>w"  ":<C-u>w<cr>")
; "sudo write"-trick via polkit agent
(nmap! "<leader>sw" ":<C-u>w !pkexec tee % >/dev/null<CR>")
(nmap! "<leader>qq" ":<C-u>quit<cr>")

; Manage plugins
(nmap! "<leader>pu" "<Plug>PkgUpdate")
(nmap! "<leader>pc" "<Plug>PkgClean")
(nmap! "<leader>pl" "<Plug>PkgList")

; Navigate history containing substring
; FIXME: Buggy, doesn't update screen
(cmap! "<M-p>" "<Up>")
(cmap! "<M-n>" "<Down>")

; Capitalize word in front of cursor
(imap! "<c-u>" "<Esc>viwUea")

;; Load and configure plugins
(fn setup [mod ?opts]
  ; Loads module and calls its `setup` function
  (let [{: setup} (require mod)]
    (setup (or ?opts {}))))

(add! "bR3iN/pkg.nvim")


; LSP setup
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
                find-notes #(zk.edit)
                extra-keymaps {[:i "<C-h>"] "<Esc>hcT|"
                               [:i "<C-l>"] "<Esc>2la"
                               [:n "<localleader>nz"] create-note
                               [:n "<localleader>no"] find-notes
                               [:i "<C-j>"] create-and-insert-link
                               [:i "<C-p>"] #(spawn-capture-output
                                               :zk-screenshot nil
                                               (fn [code _ stdout stderr]
                                                 (if (= 0 code)
                                                   (put! (.. "![[" stdout "]]"))
                                                   (error stderr))))}]
            (zk.setup {:picker :telescope
                       :lsp {:config {:on_attach (mk-on_attach extra-keymaps)
                                      :capabilities (mk-capabilities)}}})))))
(add! "mfussenegger/nvim-dap"
      (fn []
        ; Setup common keymaps
        (let [dap (require :dap)
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
          (each [lhs rhs (pairs mappings)]
            (nmap! lhs rhs)))))

; (add! :Maan2003/lsp_lines.nvim
;        {:setup (fn []
;                  (setup :lsp_lines)
;                  ; Disable virtual_text since it's redundant due to `lsp_lines.nvim`
;                  (vim.diagnostic.config {:virtual_text false}))})


(add! "rktjmp/hotpot.nvim")
(add! "tpope/vim-repeat")
(add! "tpope/vim-commentary")
(add! "tpope/vim-surround"
      (fn []
        ; Find numbers with `:echo char2nr("B")`
        (let! surround_66  "{\r}\1\1")         ; "B"
        (let! surround_114 "{:\r:}")           ; "r"
        (let! surround_82  "{:\r:}[\1\1]")))  ; "R"

(add! ["nvim-telescope/telescope.nvim"
       "nvim-lua/plenary.nvim"]
      (fn []
        ;Setup plugin
        (setup :telescope {:defaults
                           {:sorting_strategy :ascending
                            :scroll_strategy :limit
                            :layout_config {:prompt_position :top}
                            :layout_strategy :flex}})
        ; Setup keymaps
        (let [builtins (require :telescope.builtin)
              pick (fn [action ?opts]
                     (let [picker (. builtins action)]
                       (picker ?opts)))]
          (each [[mode lhs] rhs
                 (pairs {[:n "<leader>ff"] #(pick :find_files)
                         [:n "<leader>fz"] #(pick :buffers)
                         [:n "<leader>f."] #(pick :resume)
                         [:n "<leader>b"]  #(pick :buffers)
                         [:n "<leader>fg"] #(pick :live_grep)
                         [:n "<leader>fl"] #(pick :live_grep {:grep_open_files true})
                         [:n "<leader>fL"] #(pick :lsp_workspace_symbols)})]
            (vim.keymap.set mode lhs rhs)))))

(add! "lervag/vimtex"
      (fn []
        (let! vimtex_format_enabled 1)
        (let! vimtex_quickfix_mode 0)
        (let! vimtex_view_method :zathura)
        (let! tex_flavor :latex)))

(add! ["hrsh7th/vim-vsnip"
       ; Community maintained snippet collection
       ; FIXME: contains faulty norg snippets
       ; (add! "rafamadriz/friendly-snippets")
       ; Snippet integration with neovims builtin LSP client
       "hrsh7th/vim-vsnip-integ"]
      (fn []
        ; set snippet directory
        (let! vsnip_snippet_dir (.. (vim.fn.stdpath :config) :/vsnip))))

(add! ["nvim-treesitter/nvim-treesitter"
       "nvim-treesitter/playground"
       "nvim-treesitter/nvim-treesitter-textobjects"]
      (fn []
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

; Treesitter indentation for fennel is messed up
(add! "bakpakin/fennel.vim")

(add! ["nvim-neorg/neorg"
       "nvim-lua/plenary.nvim"]
      (fn []
        ; Wrap bindings to lazily start Neorg before executing them
        (let [Neorg #(vim.cmd.Neorg {:args $1})
              cd #(vim.cmd.cd "%:h")
              bindings {"<leader>gc" #(Neorg [:gtd :capture])
                        "<leader>gv" #(Neorg [:gtd :views])
                        "<leader>ge" #(Neorg [:gtd :edit])
                        "<leader>go" #(do (Neorg [:workspace :gtd]) (cd))
                        "<leader>gz" #(do (Neorg [:workspace :zettelkasten]) (cd))
                        "<leader>gj" #(do (Neorg [:workspace :workspace1]) (cd))
                        "<leader>gr" #(do (Neorg [:workspace :references]) (cd))
                        "<leader>gn" #(do (Neorg [:workspace :notes]) (cd))}
              running? #(->> :Neorg
                             (. (vim.api.nvim_get_commands {}))
                             (nil?)
                             (not))]
          (each [lhs func (pairs bindings)]
            (let [rhs (fn []
                        (when (not (running?))
                          (vim.cmd.NeorgStart))
                        (func))]
              (nmap! lhs rhs)))

          ; Configures keybindings for navigating Neorg
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
                 :core.dirman {:workspaces {:notes "~/neorg/notes"
                                            :zettelkasten "~/neorg/zettelkasten"
                                            :workspace1 "~/neorg/workspace1"
                                            :references "~/neorg/references"
                                            :gtd "~/neorg/projects"}
                               :default :notes
                               ; Open last workspace on `nvim`; can be set to "default" for default workspace instead
                               :open_last_workspace false}
                 :core.completion {:engine :nvim-cmp}
                 ; :core.gtd.base {:workspace :gtd
                                   ;                 :default_lists {:inbox :index.norg
                                                                     ;                                 :someday :someday.norg}
                                   ;                 :custom_tag_completion true}
                 :core.keybinds {:default_keybinds true
                                 :hook keybinds-hook}}]
            (setup
              :neorg
              {:load (vim.tbl_map #{:config $1} configs)})))))


(add! "ggandor/leap.nvim"
      (fn []
        (let [{: set_default_keymaps : setup} (require :leap)]
          (set_default_keymaps)
          (setup {:safe_labels {}}))))

; Configure or look for alternative
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
           {:use_default_keymaps false
            :keymaps {"g?"    :actions.show_help
                      "<C-]>" :actions.select
                      "<C-v>" :actions.select_vsplit
                      "<C-s>" :actions.select_split
                      "gp"    :actions.preview
                      "<C-p>" :actions.close
                      "gf"    :actions.refresh
                      "-"     :actions.parent
                      "_"     :actions.open_cwd
                      "`"     :actions.cd
                      "~"     :actions.tcd
                      "g."    :actions.toggle_hidden}})
         (nmap! "-" open)))

; Highlights hex color codes with their color
(add! "norcalli/nvim-colorizer.lua"
      #(let [{: setup} (require :colorizer)]
         (setup)))

; Floating preview in quickfix window
(add! "kevinhwang91/nvim-bqf"
       #(setup :bqf {:func_map {:fzffilter ""}}))

;; Load local plugins
(require :plugin.toggle-comments)
(require :plugin.highlight-trailing-whitespace)

; Open hotpot-compiled fennel file; mapped in fennel ftplugin
(require :plugin.open-cache)

;; Misc. autocmds
(let [autocmd! (augroup! :init.lua)]
  ; Autoreload config files on save
  (autocmd! :BufWritePost (.. vim.env.HOME "/.{dotfiles,config}/nvim/*.{vim,lua,fnl}")
            #(dofile vim.env.MYVIMRC))

  ; Fold via marker in config files
  (autocmd! :BufRead (.. vim.env.HOME "/.{config,dotfiles}/*")
            #(setl! foldmethod :marker))

  ; Don't create undofiles for temporary files
  (autocmd! :BufWritePre "/tmp/*"
            #(setl! noundofile))

  ; Highlight on yank
  (autocmd! :TextYankPost "*"
            #(vim.highlight.on_yank {:higroup :IncSearch
                                     :timeout 150})))
