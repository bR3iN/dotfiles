(local {: keymaps!} (require :utils))

;; set leader keys
(set vim.g.mapleader " ")
(set vim.g.maplocalleader " c")

;; (keymaps! {:n {"\\" ","}})

(fn feed [keys]
  (vim.api.nvim_feedkeys (vim.api.nvim_replace_termcodes keys true true true)
                         :n false))

;; Insert mode keymaps
(keymaps! {:i {;; Correctly indent when pasting multiple lines in insert mode
               :<C-r> {:desc "Paste (auto-indent)" :callback :<C-r><C-o>}
               ;; Capitalize word in front of cursor
               :<C-u> {:desc "Capitalize Word" :callback :<Esc>viwUea}}})

;; Clear highlight search
(keymaps! {:n {:<ESC> {:desc "Clear Search Highlight"
                       :callback ":<C-u>nohlsearch<CR><C-l>"}}})

;; Select until end of line (like `C`, `D` and `Y`)
(keymaps! {:n {:<leader>v {:desc "Select to End of Line" :callback :vg_}}})

;; Terminal mode keymaps
(keymaps! {:t {:<Esc> {:desc "Exit Terminal Mode" :callback "<C-\\><C-n>"}
               ["<C-v><C-[>" :<C-v><Esc>] {:desc "Send Escape to Terminal"
                                           :callback :<Esc>}}})

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

;; <leader>f prefix - Find things
(keymaps! {:n {:<leader>f {:f {:desc "Find Files"
                               :callback :<Plug>pick#files}
                           "/" {:desc "Grep Workspace"
                                :callback :<Plug>pick#grep-workspace}
                           :l {:desc "Grep Buffers"
                               :callback :<Plug>pick#grep-buffers}
                           :h {:desc "Help Tags"
                               :callback :<Plug>pick#help}}}
           :v {:<leader>f {:l {:desc "Grep Selection in Buffers"
                               :callback :<Plug>pick#grep-selection-buffers}}}})

;; <leader>l prefix - LSP things
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
                               :callback #(vim.lsp.buf.hover {:border _G.border-type})}
                           :n {:desc "Rename Symbol"
                               :callback vim.lsp.buf.rename}
                           :a {:desc "Code Action"
                               :callback :<Plug>lsp#code-action}
                           :l {:desc "Code Lenses"
                               :callback vim.lsp.codelens.run}
                           :f {:desc "Format File"
                               :callback :<Plug>edit#format-file}
                           ;; Picker symbol commands
                           :s {:desc "Document Symbols"
                               :callback :<Plug>pick#document-symbols}
                           :w {:desc "Workspace Symbols"
                               :callback :<Plug>pick#workspace-symbols}}}})

;; <leader>t prefix - Tabs
(let [keys (let [res {}]
             (for [i 1 9]
               (set (. res (tostring i)) i))
             res)]
  (keymaps! {:n {:<leader> {:t (collect [as_str _ (pairs keys)]
                                 (values as_str
                                         {:desc (.. "Focus Tab " as_str)
                                          :callback (.. as_str "gt")}))}}}))

;; UI toggles and cycles
(keymaps! {:n {:<leader>u {:h {:desc "Toggle Inlay Hints"
                                :callback #(vim.lsp.inlay_hint.enable (not (vim.lsp.inlay_hint.is_enabled)))}
                            :c {:desc "Toggle Colorizer"
                                :callback :<Plug>ui#toggle-colorizer}
                            :i {:desc "Toggle Indent Lines"
                                :callback :<Plug>ui#toggle-indent-lines}
                            :w {:desc "Toggle Line Wrap"
                                :callback #(set vim.wo.wrap
                                                (not vim.wo.wrap))}
                            :s {:desc "Toggle Spell Check"
                                :callback #(set vim.wo.spell
                                                (not vim.wo.spell))}
                            :n {:desc "Toggle Relative Numbers"
                                :callback #(set vim.wo.relativenumber
                                                (not vim.wo.relativenumber))}
                            ;; Diagnostic display cycle
                            :l {:desc "Cycle Diagnostic Display"
                                :callback :<Plug>ui#diag-cycle}
                            :L {:desc "Reset Diagnostic Display"
                                :callback :<Plug>ui#diag-reset}
                            :g {:desc "Toggle Git Diff"
                                :callback :<Plug>ui#toggle-git-diff}}}})

(do
  (var curr-idx 1)
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
                     (if (= curr-idx (length toggle-states))
                         1
                         (+ curr-idx 1)))
        set-state (fn [idx]
                    (set curr-idx idx)
                    (vim.diagnostic.config (. toggle-states idx)))]
    ;; Set initial state
    (set-state 1)
    (keymaps! {:n {:<Plug> {:ui#diag-cycle #(set-state (next-state))
                            :ui#diag-reset #(set-state 1)}}})))

;; Open things
(keymaps! {:n {:<leader>o {:q {:desc "Open Quickfix" :callback ":<C-u>copen<CR>"}
                           :l {:desc "Open Location List" :callback ":<C-u>lopen<CR>"}
                           :n {:desc "Open Notes"
                               :callback #(let [notes-dir (.. vim.env.HOME :/Notes)]
                                            (vim.cmd.cd notes-dir)
                                            (vim.cmd.edit :index.md))}
                           :a {:desc "Open Alternate File" :callback "<C-^>"}
                           :v {:desc "Open Vim Config Files"
                               :callback :<Plug>pick#config-files}
                           :V {:desc "Grep Vim Config"
                               :callback :<Plug>pick#config-grep}
                           :e {:desc "Buffer Errors"
                               :callback :<Plug>pick#buffer-errors}
                           :E {:desc "Workspace Errors"
                               :callback :<Plug>pick#workspace-errors}
                           :t {:desc "Open Diagnostics Window"
                               :callback :<Plug>win#open-diagnostics}
                           :g {:b {:desc "Open Git Blame"
                                   :callback :<Plug>win#open-git-blame}}
                           :d vim.diagnostic.open_float
                           :C ":e ~/.config/<CR>"}}})

;; Quit/Close things
(keymaps! {:n {:<leader>q {:V {:desc "Quit All" :callback ":<C-u>qall<CR>"}
                           :b {:desc "Delete Buffer" :callback ":bd<CR>"}
                           :w {:desc "Quit Window" :callback ":q<CR>"}
                           :q {:desc "Close Quickfix" :callback ":<C-u>cclose<CR>"}
                           :l {:desc "Close Location List" :callback ":<C-u>lclose<CR>"}
                           :t {:desc "Close Diagnostics Window"
                               :callback :<Plug>win#close-diagnostics}}}})

;; IO/External Operations
(keymaps! {:n {"<leader>e" {:w {:desc "Save File" :callback ":update<CR>"}
                            :l {:desc "Reload File from Disk" :callback ":e!<CR>"}
                            :a {:desc "Save All Files" :callback ":wall<CR>"}
                            :f {:desc "Write as Root"
                                :callback ":<C-u>w !pkexec tee % >/dev/null<CR>"}
                            :o {:desc "Open URL/file under cursor"
                                :callback #(vim.cmd (.. "!xdg-open "
                                                        (vim.fn.shellescape (vim.fn.expand "<cfile>"))))}
                            :y {:desc "Yank to Clipboard" :callback "\"+y"}
                            :p {:desc "Paste from Clipboard" :callback "\"+p"}
                            :P {:desc "Paste from Clipboard before cursor"
                                :callback "\"+P"}}}})

;; Misc
(keymaps! {:n {:<leader>x {:R {:desc "Reload Config"
                               :callback #(do
                                            (vim.print "Reloading config")
                                            (dofile vim.env.MYVIMRC))}}}
           :v {:<leader>x {:y {:desc "Yank to Clipboard" :callback "\"+y"}
                           :p {:desc "Paste from Clipboard" :callback "\"+p"}
                           :P {:desc "Paste from Clipboard before cursor"
                               :callback "\"+P"}}}})

;; Modify Git state
(keymaps! {:n {:<leader>g {:a {:desc "Stage Hunk"
                               :callback :<Plug>git#stage-hunk}
                           :h {:desc "Preview Hunk"
                               :callback :<Plug>ui#show-hunk-preview}
                           :RRR {:desc "Reset Hunk"
                                 :callback :<Plug>git#reset-hunk}}}})

;; Leader key commands (w/o prefix)
(keymaps! {:n {:<leader> {;; "." {:desc "Resume Picker"
                          ;;      :callback :<Plug>pick#resume}
                          "." {:desc "Swap Parameter Next"
                               :callback :<Plug>edit#swap-param-next}
                          "," {:desc "Swap Parameter Prev"
                               :callback :<Plug>edit#swap-param-prev}
                          :T {:desc "Pick Terminal"
                              :callback :<Plug>pick#terminal}
                          :b {:desc "Find Buffers"
                              :callback :<Plug>pick#buffers}
                          :d {:desc "Buffer Diagnostics"
                              :callback :<Plug>pick#buffer-diagnostics}
                          :D {:desc "Workspace Diagnostics"
                              :callback :<Plug>pick#workspace-diagnostics}
                          :G {:desc "Git Status"
                              :callback :<Plug>pick#git-status}
                          :v {:desc "Select to End of Line" :callback :vg_}
                          :w {:desc "Window Commands"
                              :callback "<C-w>"
                              :remap true}}}
           :v {:<leader> {"/" {:desc "Grep Selection in Workspace"
                               :callback :<Plug>pick#grep-selection-workspace}}}})

;; Window and tab management with number keys
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

;; ==============================================================================
;; NON-LEADER BINDINGS
;; ==============================================================================

;; Navigation commands
(keymaps! {[:n :v] {:s {:desc "Jump in Buffer"
                        :callback :<Plug>nav#jump-this-buffer}
                    :S {:desc "Jump to Other Buffer"
                        :callback :<Plug>nav#jump-other-buffer}
                    "]g" {:desc "Next Git Hunk" :callback :<Plug>nav#next-hunk}
                    "[g" {:desc "Previous Git Hunk"
                          :callback :<Plug>nav#prev-hunk}}
           :n {:<C-u> {:desc "Scroll Up" :callback :<Plug>nav#scroll-up}
               :<C-d> {:desc "Scroll Down" :callback :<Plug>nav#scroll-down}
               :<C-b> {:desc "Scroll Page Up"
                       :callback :<Plug>nav#scroll-page-up}
               :<C-f> {:desc "Scroll Page Down"
                       :callback :<Plug>nav#scroll-page-down}
               :zt {:desc "Scroll to Top" :callback :<Plug>nav#scroll-top}
               :zz {:desc "Scroll to Center"
                    :callback :<Plug>nav#scroll-center}
               :zb {:desc "Scroll to Bottom"
                    :callback :<Plug>nav#scroll-bottom}}})

;; Window commands
(keymaps! {:n {:- {:desc "Open File Explorer"
                   :callback :<Plug>win#open-file-explorer}
               :<C-h> {:desc "Toggle Comment Highlighting"
                       :callback :<Plug>ui#toggle-comments}
               "<C-w>" {"V" {:callback #(do
                                          (vim.cmd.vsplit)
                                          (vim.cmd.terminal {:args [:fish]}))}
                        "S" {:callback #(do
                                          (vim.cmd.split)
                                          (vim.cmd.terminal {:args [:fish]}))}}}})

;; Swap j <-> gj, k <-> gk
(keymaps! {[:n :v] {"<C-j>" "gj" "<C-k>" "gk"}})

;; Terminal keymaps
(keymaps! {:t {[:<Esc> "<C-[>"] {:desc "Exit Terminal Mode"
                                 :callback "<C-\\><C-n>"}}
           :n {;; Open terminal
               "_" {:desc "Open Terminal"
                    :callback (partial vim.cmd.terminal {:args [:fish]})}}})

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
