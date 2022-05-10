(local {: nil?} (require :utils))
(local {: nmap!} (require :utils.nvim))

(fn running? []
  (->> :Neorg
       (. (vim.api.nvim_get_commands {}))
       (nil?)
       (not)))

(let [wrap (fn [func]
             (fn []
               (if (not (running?))
                 (vim.cmd :NeorgStart))
               (func)))]
  (nmap! "<Plug>NeorgGtdCapture" (wrap #(vim.cmd "Neorg gtd capture")))
  (nmap! "<Plug>NeorgGtdOpen"    (wrap #(vim.cmd "Neorg workspace gtd"))))

(fn keybinds-hook [keybinds]
  (let [remap_key keybinds.remap_key]
    (remap_key :norg :n "<LocalLeader>tc" "<LocalLeader>gc")
    (remap_key :norg :n "<LocalLeader>tv" "<LocalLeader>gv")
    (remap_key :norg :n "<LocalLeader>te" "<LocalLeader>ge")

    (remap_key :norg         :n "<CR>" "<C-]>")
    (remap_key :toc-split    :n "<CR>" "<C-]>")
    (remap_key :gtd-displays :n "<CR>" "<C-]>")

    (remap_key :norg :n "gtu" "<LocalLeader>tu")
    (remap_key :norg :n "gtp" "<LocalLeader>tp")
    (remap_key :norg :n "gtd" "<LocalLeader>td")
    (remap_key :norg :n "gth" "<LocalLeader>th")
    (remap_key :norg :n "gtc" "<LocalLeader>tc")
    (remap_key :norg :n "gtr" "<LocalLeader>tr")
    (remap_key :norg :n "gti" "<LocalLeader>ti")))

(local opt-tbl
  {:load {:core.defaults {}
          :core.norg.concealer {}
          :core.norg.dirman {:config {:workspaces {:notes "~/neorg"
                                                   :gtd "~/neorg/gtd"}
                                      :default :notes
                                      :autochdir true
                                      :open_last_workspace false}}
          :core.norg.completion {:config {:engine :nvim-cmp}}
          :core.gtd.base {:config {:workspace :gtd
                                   :default_lists {:someday :someday.norg}}}
          :core.keybinds {:config {:default_keybinds true
                                   :hook keybinds-hook}}}})


(let [{: setup} (require :neorg)]
  (setup opt-tbl))
