(local {: reload
        : keymaps!
        : opts!
        : buf-opts!
        : buf-keymaps!
        : autocmd!
        : use!} (require :utils))

(local {: spawn} (require :utils.async))
(local {: mk-op!} (require :utils.operator))

;; <Plug> mappings have two uses here:
;; - Indirection so that we can define all global keymaps in a central place.
;; - Allowing filetypes to override them buffer-locally.
(keymaps! {:n {:<Plug>lsp#code-action vim.lsp.buf.code_action
               :<Plug>edit#format-file vim.lsp.buf.format}})

;; General Options and Keymaps

(opts! {;; TODO: experimental for fnlfmt-jumps
        ;; :lazyredraw false
        ;; Used for CursorHold autocmd
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

(autocmd! ;; Autoreload config files on save
          {:event :BufWritePost
           :pattern (.. vim.env.HOME "/.{dotfiles,config}/nvim/*.{vim,lua,fnl}")
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

;; Appearance

(set _G.border-type :single)

(opts! {:signcolumn :yes
        :termguicolors true
        ;; Remove redundant mode prompt in insert area
        :showmode true
        :cmdheight 1})

(opts! {;; Always display tab line
        :showtabline 1
        ;; Show statusline on all windows
        :laststatus 3
        :fillchars "vert:│,wbr: "
        :conceallevel 2
        :scrolloff 2
        :linebreak true})

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

;; Terminal setup
(use! "willothy/flatten.nvim" {:setup {:flatten {}}})

(autocmd! {:event :TermOpen
           :callback (fn [{:buf _}]
                       ;; (set vim.wo.number false)
                       ;; (set vim.wo.relativenumber false)
                       ;; FIXME: use when having separate picker
                       ;; (set vim.bo.buflisted false)
                       ;; Don't insert here as this doesn't have to be an active buffer
                       (buf-keymaps! {:n {;; "Forward" some keys directly in terminal mode
                                          "" (let [keys ["<CR>"
                                                         "<C-c>"
                                                         "<C-n>"
                                                         "<C-p>"
                                                         "q"]]
                                               (collect [_ key (ipairs keys)]
                                                 (values key (.. "i" key))))}}))}
          {:event [:BufWinEnter :BufEnter]
           :callback ;; Avoids triggering wrongly as startinsert will only happen after a sequence of commands and also catches more cases (for some reason).
           (vim.schedule_wrap #(when (= vim.bo.buftype :terminal)
                                           (vim.cmd.startinsert)))})

;; (use! "akinsho/toggleterm.nvim"
;;       {:setup {:toggleterm {:highlights {:StatusLine {:guibg colors.statusline}}
;;                             :direction :float
;;                             :shade_terminals false
;;                             :open_mapping "<c-j>"}}})

;; Discover and safely run .nvim.fnl files
(opts! {:exrc true})
(let [{: eval-string} (require :hotpot.api.eval)
      files (vim.fs.find ".nvim.fnl"
                         {:type :file
                          :upward true
                          :limit math.huge
                          :path (vim.uv.cwd)})]
  (each [_ file (ipairs files) &until (not vim.o.exrc)]
    (case (vim.secure.read file)
      nil nil
      content (eval-string content))))

;; Load keymaps
(reload :core.keymaps)
