(local {: use!} (require :utils))
(local {: spawn} (require :utils.async))

;; Filetype plugins
(use! :elkowar/yuck.vim)

;; Treesitter indentation for fennel is messed up, so use this plugin for this
(use! :jaawerth/fennel.vim)

(use! :lervag/vimtex {:config (fn []
                                (set vim.g.vimtex_format_enabled 1)
                                (set vim.g.vimtex_quickfix_mode 0)
                                (set vim.g.vimtex_view_method :zathura)
                                (set vim.g.tex_flavor :latex))})

;; Avoids "Unrecognized option: 'write-mode'" error.
(set vim.g.rustfmt_detect_version 1)

(use! [:mrcjkb/rustaceanvim]
      {:init #(let [lsp-config {:rust-analyzer {:cargo {:buildScripts {:enable true}}
                                                :procMacro {:enable true}
                                                :imports {;; Merge auto imports on the module level
                                                          :granularity {:group :module}
                                                          ;; Prefer relative auto import paths
                                                          :prefix :self}}}]
                (set vim.g.rustaceanvim
                     {:server {:default_settings lsp-config}}))})

(use! :bR3iN/emanote.nvim
      {:command #(let [{: start : stop} (require :emanote-live)
                       emanote_url "http://localhost:8080"]
                   {:EmanoteConnect (fn []
                                      (start {:port 8000 : emanote_url})
                                      (spawn :qutebrowser
                                             {:args [:--target
                                                     :window
                                                     emanote_url]}))
                    :EmanoteDisconnect (partial stop 500)})})

;; Forked as plugin doesn't have an API for custom keybindings
(use! :bR3iN/jupynium.nvim {:setup {:jupynium {}}})

;; Zk note-taking system
(local {: put! : keymaps!} (require :utils))
(local {: spawn-capture-output} (require :utils.async))

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
                      extra-keymaps {:n {"<localleader>" {:n {:desc "Create new note"
                                                              :callback create-note}
                                                          :o {:desc "Edit note"
                                                              :callback #(zk.edit)}
                                                          :l {:desc "Show links"
                                                              :callback open-links}
                                                          :b {:desc "Show backlinks"
                                                              :callback open-backlinks}}}
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
                                                                   {:buffer bufnr}))}}}))})
