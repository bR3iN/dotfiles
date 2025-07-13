(local {: setup} (require :telescope))
(local builtins (require :telescope.builtin))
(local actions (require :telescope.actions))
; {: buffer_dir} (require :telescope.utils)
(local smart_qf_and_open (fn [bufnr]
                           (actions.smart_send_to_qflist bufnr) ; (actions.open_qflist bufnr)
                           (vim.cmd.cfirst)))

;; If in an oil buffer, use the buffer's directory as `cwd`
(local try-get-cwd #(if (= vim.o.filetype :oil)
                        (let [{: get_current_dir} (require :oil)]
                          (get_current_dir))
                        nil))

(local pick (fn [action ?opts]
              (let [picker (. builtins action)
                    opts (vim.tbl_extend :force {:cwd (try-get-cwd)}
                                         (or ?opts {}))]
                (picker opts))))

(local keymaps
       {:picker {:i {:<C-j> actions.preview_scrolling_down
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
                     :<C-s> actions.select_horizontal}}
        :global {[:n :<leader>ff] #(pick :find_files {:follow true})
                 ; Resumes previous picker
                 [:n :<leader>f.] #(pick :resume)
                 [:n :<leader>b] #(pick :buffers
                                        {:sort_lastused true :sort_mru true})
                 [:n :<leader>fd] #(pick :diagnostics)
                 [:n :<leader>fg] #(pick :live_grep)
                 [:n :<leader>fw] #(pick :grep_string)
                 [:v :<leader>fg] #(pick :grep_string)
                 [:n :<leader>fl] #(pick :live_grep {:grep_open_files true})
                 [:n :<leader>fL] #(pick :lsp_workspace_symbols)}})

(setup {:defaults {:sorting_strategy :ascending
                   :scroll_strategy :limit
                   :layout_config {:prompt_position :top}
                   :layout_strategy :flex
                   :mappings keymaps.picker}
        :pickers {:buffers {:mappings {:n {:<C-x> :delete_buffer}
                                       :i {:<C-x> :delete_buffer}}}}})

(each [[mode lhs] rhs (pairs keymaps.global)]
  (vim.keymap.set mode lhs rhs))
