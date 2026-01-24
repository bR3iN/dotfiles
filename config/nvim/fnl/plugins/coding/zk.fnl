(local {: use!} (require :utils))
(local {: spawn} (require :utils.async))
(local {: put! : keymaps!} (require :utils))
(local {: spawn-capture-output} (require :utils.async))

;; Has weird spacing for inline math and don't work inside tmux
;; (use! :Thiago4532/mdmath.nvim
;;       {})

(use! :iamcco/markdown-preview.nvim
      {:init #(set vim.g.mkdp_combine_preview true)
       :config #((. vim.fn "mkdp#util#install"))
       :ft! {:markdown (fn [{: buf}]
                         (keymaps! {:<localleader> {:c {:desc "Preview in Browser"
                                                        :callback vim.cmd.MarkdownPreview}}}
                                   {:buffer buf}))}})

(use! :zk-org/zk-nvim
      {:config #(let [zk (require :zk)
                      util (require :zk.util)
                      create-and-insert-link (fn []
                                               (let [loc (util.get_lsp_location_from_caret)
                                                     title (vim.fn.input "Title: ")]
                                                 (when (not= title "")
                                                   (zk.new {: title
                                                            :edit false
                                                            :insertLinkAtLocation loc}))))
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
                      create-note #(let [title (vim.fn.input "Title: ")]
                                     (when (not= title "")
                                       (zk.new {: title})))
                      insert-screenshot #(spawn-capture-output :zk-screenshot
                                                               nil
                                                               (fn [code
                                                                    _signal
                                                                    stdout
                                                                    _stderr]
                                                                 (if (= 0 code)
                                                                     (put! (.. "![["
                                                                               stdout
                                                                               "]]")))))
                      on_attach (fn [_client bufnr]
                                  (keymaps! {:n {"<localleader>" {:n {:desc "Create new note"
                                                                      :callback create-note}
                                                                  ;; :N {:desc "Create new note and link it"
                                                                  ;;     :callback create-and-insert-link}
                                                                  :o {:desc "Edit note"
                                                                      :callback #(zk.edit)}
                                                                  :b {:desc "Edit open note"
                                                                      :callback ":<C-u>ZkBuffers<CR>"}
                                                                  :l {:desc "Show links"
                                                                      :callback open-links}
                                                                  :L {:desc "Show backlinks"
                                                                      :callback open-backlinks}}}
                                             :x {"<localleader>" {:n {:desc "Into note as title"
                                                                      :callback ":ZkNewFromTitleSelection<CR>"}
                                                                  :N {:desc "Into note as content"
                                                                      :callback ":ZkNewFromContentSelection<CR>"}}}
                                             :i {:<C-h> {:desc "Move to start of link"
                                                         :callback :<Esc>hcT|}
                                                 :<C-l> {:desc "Move past link"
                                                         :callback :<Esc>2la}
                                                 :<C-y> {:desc "Select link text"
                                                         :callback "<Esc>2hvT|uf]2la"}
                                                 :<C-i> {:desc "Insert link"
                                                         :callback "<C-o>:ZkInsertLink<CR>"}
                                                 :<C-j> {:desc "Create and insert link"
                                                         :callback create-and-insert-link}
                                                 :<C-p> {:desc "Insert screenshot"
                                                         :callback insert-screenshot}}}
                                            {:buffer bufnr}))]
                  (zk.setup {:picker :telescope :lsp {:config {: on_attach}}}))})

(use! :bR3iN/emanote.nvim {:command #(let [{: start : stop} (require :emanote-live)
                                           emanote_url "http://localhost:8080"]
                                       {:EmanoteConnect (fn []
                                                          (start {:port 8000
                                                                  : emanote_url})
                                                          (spawn :firefox
                                                                 {:args ["http://localhost:8000"]})
                                                          ;; (spawn :qutebrowser
                                                          ;;        {:args [:--target
                                                          ;;                :window
                                                          ;;                emanote_url]})
                                                          )
                                        :EmanoteDisconnect (partial stop 500)})
                           :ft {:markdown (fn [{:buf _buf}]
                                            ;; (vim.print buf)
                                            )}})
