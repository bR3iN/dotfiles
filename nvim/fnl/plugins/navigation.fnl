(local {: use! : reload} (require :utils))

;; FIXME: fzf-native needs `make` to be run, vim.pack.add currently does not support this
(use! [:nvim-telescope/telescope.nvim
       :nvim-telescope/telescope-fzf-native.nvim
       :nvim-lua/plenary.nvim] {:reload :plugins/navigation/telescope})

;; Leap with s
(use! :ggandor/leap.nvim
      {:setup {:leap {:safe_labels {}}}
       :keymaps {[:n :v] {:<Plug>nav#jump-this-buffer "<Plug>(leap)"
                          :<Plug>nav#jump-other-buffer "<Plug>(leap-from-window)"}}})

;; Smooth scrolling
(use! :karb94/neoscroll.nvim
      {;; Disable default mappings
       :setup {:neoscroll {:mappings {}}}
       :keymaps #(let [{: scroll : zt : zz : zb} (require :neoscroll)
                       get-height #(vim.api.nvim_win_get_height 0)]
                   {:n {:<Plug>nav#scroll-up #(scroll (- vim.wo.scroll) {:duration 100})
                        :<Plug>nav#scroll-down #(scroll vim.wo.scroll {:duration 100})
                        :<Plug>nav#scroll-page-up #(scroll (- (get-height)) {:duration 250})
                        :<Plug>nav#scroll-page-down #(scroll (get-height) {:duration 250})
                        :<Plug>nav#scroll-top #(zt {:half_win_duration 100})
                        :<Plug>nav#scroll-center #(zz {:half_win_duration 100})
                        :<Plug>nav#scroll-bottom #(zb {:half_win_duration 100})}})})

;; Split file explorer
(use! :stevearc/oil.nvim
      {:setup {:oil {:columns []
                     :use_default_keymaps false
                     :keymaps {:g? :actions.show_help
                               "<C-]>" :actions.select
                               "CR" :actions.select
                               :<C-s>v :actions.select_vsplit
                               :<C-s>s :actions.select_split
                               "gs" :actions.change_sort
                               :gp :actions.preview
                               :<C-p> :actions.close
                               :gf :actions.refresh
                               :- :actions.parent
                               :g- :actions.open_cwd
                               "`" :actions.cd
                               "~" :actions.tcd
                               :g. :actions.toggle_hidden
                               :<ESC> "<C-^>"}}}
       :keymaps #(let [{: open} (require :oil)]
                   {:n {:<Plug>win#open-file-explorer open}})})

;; TODO: Use fork until https://github.com/numToStr/Navigator.nvim/pull/35 is merged
;; (use! :Vinh-CHUC/Navigator.nvim
;;       {:setup {:Navigator nil}
;;        :keymaps {[:n :t] (let [mk-lhs #(.. :<M- $1 :>)
;;                            mk-rhs #(. vim.cmd (.. :Navigator $1))]
;;                        (collect [dir keys (pairs {:Left [:h :left]
;;                                                   :Right [:l :right]
;;                                                   :Up [:k :up]
;;                                                   :Down [:j :down]})]
;;                          (values (vim.tbl_map mk-lhs keys) (mk-rhs dir))))}})
