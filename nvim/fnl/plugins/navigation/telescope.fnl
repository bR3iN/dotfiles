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

;; Define <Plug> mappings for pickers
(keymaps! {:n {:<Plug>pick# {:resume #(pick :resume)
                             :terminal #(-> (term-buffers) (pick-bufs))
                             :document-symbols #(pick :lsp_document_symbols)
                             :workspace-symbols #(pick :lsp_workspace_symbols)
                             :buffers #(pick :buffers
                                             {:select_current true
                                              :sort_lastused true
                                              :sort_mru true})
                             :buffer-diagnostics #(pick :diagnostics {:bufnr 0})
                             :workspace-diagnostics #(pick :diagnostics)
                             :config-files #(pick :find_files
                                                  {:cwd (.. vim.env.HOME "/.config/nvim")
                                                   :follow true})
                             :config-grep #(pick :live_grep
                                                 {:cwd (.. vim.env.HOME "/.config/nvim")})
                             :buffer-errors #(pick :diagnostics
                                                   {:bufnr 0
                                                    :severity vim.diagnostic.severity.ERROR
                                                    :prompt_title "Buffer Errors"})
                             :workspace-errors #(pick :diagnostics
                                                      {:severity vim.diagnostic.severity.ERROR
                                                       :prompt_title "Workspace Errors"})
                             :git-status #(pick :git_status)
                             :files #(pick :find_files
                                           {:follow true
                                            :cwd (try-get-cwd)})
                             :grep-workspace #(pick :live_grep
                                                     {:cwd (try-get-cwd)})
                             :grep-buffers #(pick :live_grep
                                                   {:grep_open_files true
                                                    :cwd (try-get-cwd)})
                             :help #(pick :help_tags)}}
           :v {:<Plug>pick# {:grep-selection-workspace #(pick :grep_string {})
                             :grep-selection-buffers #(pick :grep_string
                                                            {:grep_open_files true})}}})