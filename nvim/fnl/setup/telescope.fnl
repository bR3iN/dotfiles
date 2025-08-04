(local {: keymaps!} (require :utils))
(local {: setup : load_extension} (require :telescope))
(local {: buffer_dir} (require :telescope.utils))
(local builtins (require :telescope.builtin))
(local actions (require :telescope.actions))

(local smart_qf_and_open (fn [bufnr]
                           (actions.smart_send_to_qflist bufnr) ; (actions.open_qflist bufnr)
                           (vim.cmd.cfirst)))

;; If in an oil buffer, use the buffer's directory as `cwd`
(local oil-or-buffer-dir #(if (= vim.o.filetype :oil)
                              (let [{: get_current_dir} (require :oil)]
                                (get_current_dir))
                              (buffer_dir)))

(local try-get-cwd #(if (= vim.o.filetype :oil)
                        (let [{: get_current_dir} (require :oil)]
                          (get_current_dir))
                        nil))

(local pick (fn [action ?opts]
              (let [picker (. builtins action)
                    opts (or ?opts {})]
                (picker opts))))

(local keymaps {:picker {:i {"<C-j>" actions.preview_scrolling_down
                             "<C-k>" actions.preview_scrolling_up
                             "<C-l>" actions.preview_scrolling_right
                             "<C-h>" actions.preview_scrolling_left
                             "<C-q>" smart_qf_and_open
                             "<C-]>" actions.select_default
                             "<C-x>" actions.drop_all
                             "<C-a>" actions.select_all
                             "<C-d>" actions.results_scrolling_down
                             "<C-u>" actions.results_scrolling_up
                             "<C-s>" actions.select_horizontal
                             "<esc>" actions.close}
                         ;; :n {"<C-j>" actions.preview_scrolling_down
                         ;;     "<C-k>" actions.preview_scrolling_up
                         ;;     "<C-l>" actions.preview_scrolling_right
                         ;;     "<C-h>" actions.preview_scrolling_left
                         ;;     "<C-q>" smart_qf_and_open
                         ;;     "<C-]>" actions.select_default
                         ;;     "<C-x>" actions.drop_all
                         ;;     "<C-a>" actions.select_all
                         ;;     "<C-d>" actions.results_scrolling_down
                         ;;     "<C-u>" actions.results_scrolling_up
                         ;;     "<C-s>" actions.select_horizontal}
                         }})

(setup {:defaults {:sorting_strategy :ascending
                   :scroll_strategy :limit
                   :layout_config {:prompt_position :top}
                   :layout_strategy :flex
                   :mappings keymaps.picker}
        :extensions {:fzf {:fuzzy false
                           :override_generic_sorter true
                           :override_file_sorter true
                           :case_mode :smart_case}}
        :pickers {:buffers {:mappings {:n {:<C-x> :delete_buffer}
                                       :i {:<C-x> :delete_buffer}}}}})

(load_extension :fzf)

(keymaps! {:n {:<leader> {:f {:desc "Find Files"
                              :callback #(pick :find_files {:follow true})}
                          :F {:desc "Find Files (CWD)"
                              :callback #(pick :find_files
                                               {:cwd (oil-or-buffer-dir)
                                                :follow true})}
                          ;; Resumes previous picker
                          "'" {:desc "Resume Picker" :callback #(pick :resume)}
                          :b {:desc "Find Buffers"
                              :callback #(pick :buffers
                                               {:sort_lastused true
                                                :sort_mru true})}
                          :j {:desc "Jump List" :callback #(pick :jumplist)}
                          :G {:desc "Git Status" :callback #(pick :git_status)}
                          :d {:desc "Find Diagnostics"
                              :callback #(pick :diagnostics)}
                          :D {:desc "Workspace Diagnostics"
                              :callback #(pick :diagnostics {:workspace true})}
                          :/ {:desc "Live Grep"
                              :callback #(pick :live_grep {})}
                          :? {:desc "Live Grep (CWD)"
                              :callback #(pick :live_grep
                                               {:cwd (oil-or-buffer-dir)})}
                          ;; fw #(pick :grep_string)
                          ;; fg #(pick :grep_string)
                          ;; fl #(pick :live_grep {:grep_open_files true})
                          :s {:desc "Document Symbols"
                              :callback #(pick :lsp_document_symbols)}
                          :S {:desc "Workspace Symbols"
                              :callback #(pick :lsp_workspace_symbols)}
                          :h {:desc "Help Tags" :callback #(pick :help_tags)}
                          ;; :r {:desc "Git Branches" :callback #(pick :git_branches)}
                          }}})
