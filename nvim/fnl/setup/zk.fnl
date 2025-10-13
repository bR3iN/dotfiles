(local {: put! : keymaps!} (require :utils))
(local {: spawn-capture-output} (require :utils.async))

;; Setup zk
(let [zk (require :zk)
      util (require :zk.util)
      create-and-insert-link #(let [loc (util.get_lsp_location_from_caret)
                                    title (vim.fn.input "Title: ")]
                                (if (not= (length title) 0)
                                    (zk.new {: title
                                             :edit false
                                             :insertLinkAtLocation loc})))
      ; create-note #(let [title (vim.fn.input "Title: ")]
      ;                (if (not= (# title) 0)
      ;                  (zk.new {: title})))
      ; TODO reactivate the above or document snippet
      get-note #(vim.fn.bufname)
      open-results (fn [results]
                     (each [_ {: absPath} (ipairs results)]
                       (vim.cmd (.. "e " absPath))))
      open-backlinks #(zk.pick_notes {:linkTo [(get-note)]
                                      :maxDistance vim.v.count1
                                      :recursive true}
                                     {} open-results)
      open-links #(zk.pick_notes {:linkedBy [(get-note)]
                                  :maxDistance vim.v.count1
                                  :recursive true}
                                 {} open-results)
      create-note #(zk.new)
      extra-keymaps {:n {"<localleader>" {:c {:desc "Create new note"
                                              :callback create-note}
                                          :o {:desc "Edit note"
                                              :callback #(zk.edit)}
                                          :l {:desc "Show links"
                                              :callback open-links}
                                          :b {:desc "Show backlinks"
                                              :callback open-backlinks}}}
                     :i {:<C-h> {:desc "Move to start of link"
                                 :callback :<Esc>hcT|}
                         :<C-l> {:desc "Move past link" :callback :<Esc>2la}
                         :<C-y> {:desc "Select link text"
                                 :callback "<Esc>2hvT|uf]2la"}
                         :<C-i> {:desc "Insert link"
                                 :callback "<C-o>:ZkInsertLink<CR>"}
                         :<C-j> {:desc "Create and insert link"
                                 :callback create-and-insert-link}
                         :<C-p> {:desc "Insert screenshot"
                                 :callback #(spawn-capture-output :zk-screenshot
                                                                  nil
                                                                  (fn [code
                                                                       _signal
                                                                       stdout
                                                                       _stderr]
                                                                    (if (= 0
                                                                           code)
                                                                        (put! (.. "![["
                                                                                  stdout
                                                                                  "]]")))))}}}]
  (zk.setup {:picker :telescope
             :lsp {:config {:on_attach (fn [_client bufnr]
                                         (keymaps! extra-keymaps
                                                   {:buffer bufnr}))}}}))
