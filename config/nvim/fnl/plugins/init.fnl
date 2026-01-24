(local {: reload : use!} (require :utils))

(reload :plugins.editor)
(reload :plugins.navigation)
(reload :plugins.ui)
(reload :plugins.coding)

;; Depends on telescope and possibly more
(use! [:folke/snacks.nvim]
      {:setup {:snacks {:animate {:fps 120}
                        :bigfile {}
                        :image {}
                        :indent {:enabled false}
                        :words {}
                        ;; TODO:
                        :toggle {}
                        :scroll {:animate {:duration {:total 150}}}}}
       :config #(let [{: toggle} (require :snacks)]
                  (: (toggle.indent) :map :<leader>ui)
                  )
       :keymaps #(let [{:explorer {:open explorer} : toggle} (require :snacks)]
                   {:n {:<Plug> {:pick# {:explorer {:callback explorer}}
                                 ;; :ui# {:toggle-indent-lines toggle.indent}
                                 }}})})
