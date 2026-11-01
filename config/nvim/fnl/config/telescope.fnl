(local {: spawn-capture-output} (require :utils.async))

(local {: load_extension} (require :telescope))
(local pickers (require :telescope.pickers))
(local finders (require :telescope.finders))
(local make_entry (require :telescope.make_entry))
(local conf (. (require :telescope.config) :values))

(fn bufnr->entry [bufnr]
  ;; Format expected by make_entry.gen_from_buffer
  (let [flag ""
        info (. (vim.fn.getbufinfo bufnr) 1)]
    {: bufnr : info : flag}))

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

{:pick-term #(let [bufs (term-buffers)]
               (if (= (length bufs) 0)
                   (vim.notify "No terminal buffers")
                   (pick-bufs bufs)))
 :install-fzf-native #(let [path (-> [:telescope-fzf-native.nvim]
                                     (vim.pack.get)
                                     (. 1)
                                     (. :path))]
                        (let [{: code : stderr} (-> [:make]
                                                    (vim.system {:cwd path})
                                                    (: :wait))]
                          (if (not= 0 code)
                              (vim.print "Failed to compile fzf-native:" stderr))))}
