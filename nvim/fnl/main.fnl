;; Imports
(import-macros {: set! : setl! : setg! : set+ : let! } :utils.macros)
(local {:add2! add!} (require :pkg))
(local {: nmap! : vmap! : tmap! : cmap! : imap!
        : command! : augroup! : color!}
  (require :utils.nvim))
(local {: nil?} (require :utils))
(local {: spawn} (require :utils.async))

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

(set! cmdheight 2)
(set! scrolloff 5)

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
(setg! shiftwidth 4)  ; messes with fennel.vim
(set! tabstop 4)
(set! expandtab)

; Start with folds expanded
(set! foldlevelstart 99)

; let :find search subdirectories recursively
(set+ path :**)

; Load custom operators
(require :operators)
(nmap! "gp"   "<Plug>Print")
(vmap! "gp"   "<Plug>Print")

(imap! "<C-r>" "<C-r><C-o>")

; select until end of line (like `C`, `D`, etc.)
(nmap! "<leader>v" "vg_")

; Goto alternative file
; (nmap! "<C-c>" "<C-^>")
(nmap! "<C-p>" "<C-^>")

; Toggle the color of comments
(nmap! "<C-h>" ":ToggleComments<CR>")
(imap! "<C-h>" "<C-o>:ToggleComments<CR>")

; clear highlight search
(nmap! "<C-L>" ":<c-u>nohlsearch<CR><C-L>")

(nmap! "<leader>mk" ":make!<CR>")

; <Plug>RunFile bindings are defined in ftplugin/ where applicable to run the current file
(nmap! "<leader>rr" "<Plug>RunFile")

; Sane `<Esc>` behaviour in terminal mode
(tmap! "<Esc>" "<C-\\><C-n>")
(tmap! "<C-v><Esc>" "<Esc>")

; Open text in browser
(nmap! "gb" "<Plug>OpenInBrowser")
(vmap! "gb" "<Plug>OpenInBrowser")

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
(nmap! "<leader>sw" ":<C-u>w !pkexec tee % >/dev/null<CR>")  ;"sudo write" via polkit agent
(nmap! "<leader>qq" ":<C-u>quit<cr>")
(nmap! "<leader>QQ" ":<C-u>quitall<cr>")

; Manage plugins
(nmap! "<leader>pu" "<Plug>PkgUpdate")
(nmap! "<leader>pc" "<Plug>PkgClean")
(nmap! "<leader>pl" "<Plug>PkgList")

; Navigate history containing substring
(cmap! "<M-p>" "<up>")
(cmap! "<M-n>" "<down>")

; Capitalize word in front of cursor
(imap! "<c-u>" "<Esc>viwUea")

; Edit parent directory
(nmap! "-" ":edit %:p:h<CR>")

; Creates current directory
(command! "Mkdir" #(vim.fn.mkdir (vim.fn.expand "%:h") :p))

; Create tags asynchrounously
(command! "MakeTags" #(spawn ["ctags" "-R" "."]))

;; Appearance
(set! termguicolors)
(vim.cmd.colorscheme :base16)
(set! fillchars { :vert :| })

;; LSP setup
(add! "neovim/nvim-lspconfig"
       {:setup
        #(let [{: setup} (require :lsp)]
           (setup
             {:default-keymaps
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
               [:n "]d"]         #(vim.diagnostic.goto_next {:float {:border :single}})}

              :language-server
              [
               {:name :clangd
                :config {}
                :keymaps {[:n "<C-c>"] #(vim.cmd :ClangdSwitchSourceHeader)}}

               {:name :rls
                :config {:settings {:rust {:all_features true}}}
                :keymaps {}}

               {:name :sumneko_lua
                :config (require :lsp.configs.sumneko_lua)
                :keymaps {}}

               (->> [:bashls
                     :vimls
                     :pyright
                     :rust_analyzer
                     :hls
                     :racket_langserver]
                    (vim.tbl_map #{:name $1 :config {} :keymaps {}})
                    (unpack))
               ]}))})

(add! "mfussenegger/nvim-dap"
      {:setup (fn []
                ; Setup common keymaps
                (let [dap (require :dap)
                      mappings {
                                "<leader>db" (. dap :toggle_breakpoint)
                                "<leader>dl" (. dap :list_breakpoints)
                                "<leader>ds" (. dap :step_into)
                                "<leader>dn" (. dap :step_over)
                                "<leader>dr" (. dap :continue)
                                "<leader>dt" (. dap :terminate)
                                "<leader>dc" (. dap :clear_breakpoints)
                                "<leader>do" (. dap :repl :open)
                                }
                      autocmd! (augroup! :nvim-dap)
                      prompt-path (fn [prompt cb]
                                    (vim.ui.input
                                      {: prompt :default (vim.loop.cwd)}
                                      #(if $1 (cb $1))))]
                  (each [lhs rhs (pairs mappings)]
                    (nmap! lhs rhs))
                  ; Setup python debugging, `debugpy` needs to be
                  ; installed in project environment.
                  (add! "mfussenegger/nvim-dap-python"
                        {:setup (let [{: setup} (require :dap-python)]
                                  ; Takes python from PATH, so start Neovim in venv
                                  (setup :python))}
                        ; {:setup (let [{: setup} (require :dap-python)
                        ;               dap-setup #(prompt-path
                        ;                            "Choose venv: "
                        ;                            #(-> $1
                        ;                                 (.. :/bin/python)
                        ;                                 (setup)))]
                        ;           (nmap! "<leader>ds" dap-setup)
                        ;           )}
                        )))})

; (add! :Maan2003/lsp_lines.nvim
;        {:setup (fn []
;                   (let [{: setup} (require :lsp_lines)]
;                     (setup))
;                   ; Disable virtual_text since it's redundant due to `lsp_lines.nvim`
;                   (vim.diagnostic.config {:virtual_text false}))})


;; Load and configure plugins
(add! "rktjmp/hotpot.nvim")
(add! "tpope/vim-repeat")
(add! "tpope/vim-commentary")
(add! "tpope/vim-surround"
      {:setup #(let! surround_66 "{\r}\1\1")})  ; "B"

; Filetype plugins
(add! "georgewitteman/vim-fish")
(add! "jaawerth/fennel.vim")

(add! "ibhagwan/fzf-lua"
       {:setup
        (fn []
          ;Setup plugin
          (let [{: setup} (require :fzf-lua)]
            (setup {:files {:file_icons false}
                    :hl {:cursorline "CursorLineFzfLua"}}))
          ; Setup keymaps
          (each [[mode lhs] action
                 (pairs {[:n "<leader>ff"] :files
                         [:n "<leader>fl"] :lines
                         [:n "<leader>b"]  :buffers
                         [:n "<leader>fg"] :live_grep
                         [:n "<leader>fG"] :grep
                         [:v "<leader>fg"] :grep_visual
                         [:n "<leader>fd"] :lsp_declarations
                         [:n "<leader>fD"] :lsp_definitions})]
            (let [rhs #(vim.cmd.FzfLua {:args [action]})]
              (vim.keymap.set mode lhs rhs))))})

(add! "lervag/vimtex"
      {:setup
       (fn []
         (let! vimtex_format_enabled 1)
         (let! vimtex_quickfix_mode 0)
         (let! vimtex_view_method :zathura)
         (let! tex_flavor :latex))})

(add! "rust-lang/rust.vim"
      {:setup
       (fn []
         (let! rust_conceal 0)
         (let! rust_fold 1))})

(add! "hrsh7th/vim-vsnip"
       {:setup
        (fn []
          ; set snippet directory
          (let! vsnip_snippet_dir (.. (vim.fn.stdpath :config) :/vsnip))
          ; Community maintained snippet collection
          (add! "rafamadriz/friendly-snippets")
          ; Snippet integration (TODO: check if needed)
          (add! "hrsh7th/vim-vsnip-integ"))})

(add! "nvim-treesitter/nvim-treesitter"
      {:setup
       (fn []
         (add! "nvim-treesitter/nvim-treesitter-textobjects")
         (let [{: setup} (require :nvim-treesitter.configs)]
           (setup {:highlight {:enable true}
                   :indent {:enable false}
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
                                        :swap_previous {"<leader>," "@parameter.inner"}}}
                   :incremental_selection {:enable true
                                           :keymaps {; :init_selection "gnn"
                                                     :node_incremental "-"
                                                     :scope_incremental "g-"
                                                     :node_decremental "_"}}})))})


(add! "hrsh7th/nvim-cmp"
      {:setup
       (fn []
         (add! "hrsh7th/cmp-nvim-lsp")
         (add! "hrsh7th/cmp-nvim-lua")
         (add! "kdheepak/cmp-latex-symbols")
         (add! "hrsh7th/cmp-path")
         (add! "hrsh7th/cmp-buffer")
         (add! "hrsh7th/cmp-vsnip")
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
                     {:name :calc}
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
                                    :calc          "[Clc]"
                                    :buffer        "[Buf]"}]
                               (fn [{:source {: name}} item]
                                 (->> name
                                      (. display-names)
                                      (#(or $1 "[???]"))
                                      (tset item :menu))
                                 item))}}]
           (cmp.setup config)))})

(add! "neomake/neomake"
       {:setup
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
                 hl-set #(each [name link (pairs links)]
                           (vim.api.nvim_set_hl 0 name {: link}))
                 autocmd! (augroup! :neomake_highlighting)]
             (hl-set)
             (autocmd! :ColorScheme "*" hl-set)))})

(add! "christoomey/vim-tmux-navigator"
       {:setup (fn []
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
                           (mk-map lhs rhs)))))))})

; Used by `lir.nvim` and `neorg`
(add! "nvim-lua/plenary.nvim")

(add! "nvim-neorg/neorg"
      {:setup
       (fn []
         ; Wrap bindings to lazily start Neorg before executing them
         (let [Neorg #(vim.cmd.Neorg {:args $1})
               cd #(vim.cmd.cd "%:h")
               bindings {"<leader>gc" #(Neorg [:gtd :capture])
                         "<leader>gv" #(Neorg [:gtd :views])
                         "<leader>ge" #(Neorg [:gtd :edit])
                         "<leader>go" #(do (Neorg [:workspace :gtd]) (cd))
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
                (unmap :norg :n "<LocalLeader>tc")
                (unmap :norg :n "<LocalLeader>tv")
                (unmap :norg :n "<LocalLeader>te")

                (remap_key :norg         :n "<CR>" "<C-]>")
                (remap_key :toc-split    :n "<CR>" "<C-]>")
                (remap_key :gtd-displays :n "<CR>" "<C-]>")

                (remap_key :norg :n "gtu" "<LocalLeader>tu")
                (remap_key :norg :n "gtp" "<LocalLeader>tp")
                (remap_key :norg :n "gtd" "<LocalLeader>td")
                (remap_key :norg :n "gth" "<LocalLeader>th")
                (remap_key :norg :n "gtc" "<LocalLeader>tc")
                (remap_key :norg :n "gtr" "<LocalLeader>tr")
                (remap_key :norg :n "gti" "<LocalLeader>ti")))

            ; Main config and setup
            (let [configs
                  {:core.defaults {}
                   :core.norg.concealer {}
                   ; :core.norg.qol.toc {:close_split_on_jump true}
                   :core.norg.dirman {:workspaces {:notes "~/neorg/notes"
                                                   :gtd "~/neorg/tasks"}
                                      :default :notes
                                      ; Open last workspace on `nvim`; can be set to "default" for default workspace instead
                                      :open_last_workspace false}
                   :core.norg.completion {:engine :nvim-cmp}
                   ; :core.gtd.base {:workspace :gtd
                   ;                 :default_lists {:inbox :index.norg
                   ;                                 :someday :someday.norg}
                   ;                 :custom_tag_completion true}
                   :core.keybinds {:default_keybinds true
                                   :hook keybinds-hook}}

                  config {:load (vim.tbl_map #{:config $1} configs)}
                  {: setup} (require :neorg)]
              (setup config))))})


(add! "ggandor/leap.nvim"
       {:setup #(let [{: set_default_keymaps : setup} (require :leap)]
                  (set_default_keymaps)
                  (setup {:safe_labels {}}))})

(add! "tamago324/lir.nvim"
      {:setup
       #(let [mappings
              (let [actions (require :lir.actions)
                    bindings {"<C-]>" :edit
                              "<C-s>" :split
                              "<C-v>" :vsplit
                              "q" :quit
                              "-" :up
                              "." :toggle_show_hidden
                              "R" :rename
                              "N" :newfile
                              "K" :mkdir
                              "D" :delete}]
                (vim.tbl_map #(. actions $1) bindings))
              config {:show_hidden_files false
                      :hide_cursor true
                      : mappings}
              {: setup} (require :lir)]
          (setup config))})

(add! "norcalli/nvim-colorizer.lua"
       {:setup #(let [{: setup} (require :colorizer)]
                  (setup))})

;; Load local plugins
(require :plugin.toggle-comments)
(require :plugin.highlight-trailing-whitespace)
(command! :Rename (require :plugin.rename))

; Open hotpot-compiled fennel file
(require :plugin.open-cache)
(nmap! "ghoc" "<Plug>OpenCache")

;; Misc. autocmds
(let [autocmd! (augroup! :init.lua)]
  ; Autoload config files on save
  (autocmd! :BufWritePost (.. vim.env.HOME "/.{dotfiles,config}/nvim/*.{vim,lua,fnl}")
            #(dofile vim.env.MYVIMRC))

  ; Fold via marker in config files
  (autocmd! :BufRead (.. vim.env.HOME "/.{config,dotfiles}/*")
            #(setl! foldmethod :marker))

  ; Don't save undofiles for tempfiles
  (autocmd! :BufWritePre "/tmp/*"
            #(setl! noundofile))

  ; Highlight yanks
  (autocmd! :TextYankPost "*"
            #(vim.highlight.on_yank {:higroup :IncSearch
                                     :timeout 150})))
