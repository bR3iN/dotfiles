;; Imports
(import-macros {: set! : setl! : setg! : set+ : let! } :utils.macros)
(local {: add!} (require :pkg))
(local {: nmap! : vmap! : tmap! : cmap! : imap!
        : command! : augroup! : color!}
  (require :utils.nvim))
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

; select until end of line (like `C`, `D`, etc.)
(nmap! "<leader>v" "vg_")

; Goto alternative file
(nmap! "<C-c>" "<C-^>")

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

(nmap! "<C-[>" "<C-^>")
(nmap! "-" ":edit %:p:h<CR>")

; Creates current directory
(command! "Mkdir" #(vim.fn.mkdir (vim.fn.expand "%:h") :p))

; Create tags asynchrounously
(command! "MakeTags" #(spawn ["ctags" "-R" "."]))

;; LSP setup
(let [keymaps
      {"gD"         [:n :declaration]
       "gd"         [:n :definition]
       "K"          [:n :hover]
       "gi"         [:n :implementation]
       "<C-k>"      [:n :signature_help]
       "<leader>D"  [:n :type_definition]
       "<leader>rn" [:n :rename]
       "<leader>ca" [:n :code_action]
       "gr"         [:n :references]
       "<leader>od" [:n #(vim.diagnostic.open_float {:border :single})]
       "[d"         [:n #(vim.diagnostic.goto_prev {:float {:border :single}})]
       "]d"         [:n #(vim.diagnostic.goto_next {:float {:border :single}})]
       "gqq"        [:n :formatting]
       "gq"         [:n :range_formatting]}
      setup {:with-config [:clangd
                           :sumneko_lua
                           :rls]
             :with-defaults [:bashls
                             :vimls
                             :pyright
                             :hls
                             :racket_langserver]}]
  (add! "neovim/nvim-lspconfig"
        :setup {: keymaps : setup}))
(add! :Maan2003/lsp_lines.nvim :load-config)

;; Appearance
(require :base16)
(set! fillchars { :vert :| })

;; Load and configure plugins
(add! "rktjmp/hotpot.nvim")
(add! "tpope/vim-repeat")
(add! "tpope/vim-commentary")

(add! "georgewitteman/vim-fish")
(add! "jaawerth/fennel.vim")

(add! "ibhagwan/fzf-lua" :load-config)
(nmap! "<leader>ff" ":FzfLua files<CR>")
(nmap! "<leader>fl" ":FzfLua lines<CR>")
(nmap! "<leader>b"  ":FzfLua buffers<CR>")
(nmap! "<leader>fg" ":FzfLua live_grep<CR>")
(nmap! "<leader>fG" ":FzfLua grep<CR>")
(vmap! "<leader>fg" ":<C-u>FzfLua grep_visual<CR>")
(nmap! "<leader>fd" ":FzfLua lsp_declarations<CR>")
(nmap! "<leader>fD" ":FzfLua lsp_definitions<CR>")

(add! "tpope/vim-surround")
(let! surround_66 "{\r}\1\1")  ; "B"

(add! "lervag/vimtex" :load-config)
(add! "rust-lang/rust.vim" :load-config)

(add! "hrsh7th/nvim-cmp" :setup {:tab-role  :<Tab>
                                 :stab-role :<S-Tab>
                                 :confirm   :<CR>
                                 :cancel    :<C-e>})

(add! "hrsh7th/vim-vsnip" :load-config)
(add! "nvim-treesitter/nvim-treesitter" :load-config)

(add! "neomake/neomake" :load-config)
(nmap! "<leader>nm" ":<C-u>Neomake<CR>")
(nmap! "<leader>nc" ":<C-u>NeomakeClean<CR>")

(add! "christoomey/vim-tmux-navigator"
      :setup #(.. :<M- $1 :>))  ; Use Alt as modifier

(add! "nvim-lua/plenary.nvim")
(add! "nvim-neorg/neorg" :setup
      {"<leader>gc" "Neorg gtd capture"
       "<leader>gv" "Neorg gtd views"
       "<leader>ge" "Neorg gtd edit"
       "<leader>go" "Neorg workspace gtd"
       "<leader>gn" "Neorg workspace notes"
       "<leader>gr" "Neorg workspace references"
       "<leader>gd" "Neorg workspace docs"})

(add! "ggandor/leap.nvim" :load-config)

(add! "tamago324/lir.nvim" :load-config)

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
