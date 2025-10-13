(local {: keymaps!} (require :utils))
(local {:setup setup_telescope : load_extension} (require :telescope))
(local {: buffer_dir} (require :telescope.utils))
(local builtins (require :telescope.builtin))
(local actions (require :telescope.actions))
(local sorters (require :telescope.sorters))

(local pickers (require :telescope.pickers))
(local finders (require :telescope.finders))
(local make_entry (require :telescope.make_entry))
(local conf (. (require :telescope.config) :values))

;; FIXME
(vim.system [:make]
            {:cwd (.. vim.env.HOME
                      "/.local/share/nvim/site/pack/core/opt/telescope-fzf-native.nvim/")})

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

(setup_telescope {:defaults {:sorting_strategy :ascending
                             :scroll_strategy :limit
                             :layout_config {:prompt_position :top}
                             :layout_strategy :flex
                             :mappings keymaps.picker}
                  :extensions {:fzf {:fuzzy false
                                     :override_generic_sorter true
                                     :override_file_sorter true
                                     :case_mode :smart_case}}
                  :pickers {:diagnostics {:sort_by :severity}
                            :buffers {:mappings {:n {:<C-x> :delete_buffer}
                                                 :i {:<C-x> :delete_buffer}}}}})

(load_extension :fzf)

;; Format expected by mmake_entry.gen_from_buffer
(fn bufnr->entry [bufnr]
  (let [flag ""]
    {: bufnr :info (. (vim.fn.getbufinfo bufnr) 1) : flag}))

(fn pick-bufs [bufnrs ?opts]
  (let [opts (or ?opts {})
        _ (set (. opts :bufnr_width)
               (-> bufnrs (_G.unpack) (math.max) (tostring) (length)))
        entry_maker (or opts.entry_maker (make_entry.gen_from_buffer opts))
        picker (pickers.new opts
                            {:prompt_title "terminal buffer"
                             :finder (finders.new_table {:results (vim.tbl_map bufnr->entry
                                                                               bufnrs)
                                                         : entry_maker})
                             :sorter (conf.generic_sorter opts)
                             :previewer (conf.grep_previewer opts)})]
    (picker:find)))

(fn term-buffers []
  (let [show-buffer? (fn [bufnr]
                       (and (vim.api.nvim_buf_is_loaded bufnr)
                            (= (. vim.bo bufnr :buftype) :terminal)))]
    (->> (vim.api.nvim_list_bufs)
         (vim.tbl_filter show-buffer?))))

(keymaps! {:n {:<leader> {";" {:desc "Resume Picker" :callback #(pick :resume)}
                          :T {:desc "Pick Terminal"
                              :callback #(-> (term-buffers)
                                             (pick-bufs))}
                          :i {:desc "Document Symbols"
                              :callback #(pick :lsp_document_symbols)}
                          :I {:desc "Workspace Symbols"
                              :callback #(pick :lsp_workspace_symbols)}
                          :b {:desc "Find Buffers"
                              :callback #(pick :buffers
                                               {:select_current true
                                                :sort_lastused true
                                                :sort_mru true})}
                          ;; (local conf (. (require :telescope.config) :values))
                          :d {:desc "Find Diagnostics"
                              :callback #(pick :diagnostics
                                               {:bufnr 0})}
                          :D {:desc "Workspace Diagnostics"
                              :callback #(pick :diagnostics)}
                          :o {
                              :e {:desc "Workspace Errors"
                                  :callback #(pick :diagnostics
                                                   {:bufnr 0
                                                    :severity vim.diagnostic.severity.ERROR
                                                    :prompt_title "Buffer Errors"})}
                              :E {:desc "Workspace Errors"
                                  :callback #(pick :diagnostics
                                                   {:severity vim.diagnostic.severity.ERROR
                                                    :prompt_title "Workspace Errors"})}}
                          :G {:desc "Git Status" :callback #(pick :git_status)}
                          :f {:f {:desc "Find Files"
                                  :callback #(pick :find_files
                                                   {:follow true
                                                    :cwd (try-get-cwd)})}
                              :/ {:desc "Live Grep"
                                  :callback #(pick :live_grep
                                                   {:cwd (try-get-cwd)})}
                              :l {:desc "Live Grep in Open Files"
                                  :callback #(pick :live_grep
                                                   {:grep_open_files true
                                                    :cwd (try-get-cwd)})}
                              :h {:desc "Help Tags"
                                  :callback #(pick :help_tags)}}}}
           :v {:<leader> {:/ {:desc "Live Grep for Selection"
                              :callback #(pick :grep_string {})}
                          :f {:l {:desc "Live Grep for Selection in Open Files"
                                  :callback #(pick :grep_string
                                                   {:grep_open_files true})}}}}})
