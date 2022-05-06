(local opt-tbl
  {:load {:core.defaults {}
          :core.norg.concealer {}
          :core.norg.dirman {:config {:workspaces {:notes "~/neorg"
                                                   :gtd "~/neorg/gtd"}
                                      ; :default :notes
                                      :autochdir true
                                      :autodetect true}}
          :core.norg.completion {:config {:engine :nvim-cmp}}
          :core.gtd.base {:config {:workspace :gtd
                                   :default_lists {:someday :someday.norg}}}
          :core.keybinds {:config {:default_keybinds true}}}})


(let [{: setup} (require :neorg)]
  (setup opt-tbl))
