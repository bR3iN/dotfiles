(local {: nil? : string?} (require :utils))
(local {: nmap!} (require :utils.nvim))

(fn running? []
  ; Check if the command `Neorg` is defined
  (->> :Neorg
       (. (vim.api.nvim_get_commands {}))
       (nil?)
       (not)))

(fn keybinds-hook [keybinds]
  (let [remap_key keybinds.remap_key
        unmap     keybinds.unmap]
    (unmap :norg :n "<LocalLeader>tc")
    (unmap :norg :n "<LocalLeader>tv")
    (unmap :norg :n "<LocalLeader>te")

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
          :core.norg.qol.toc {:config {:close_split_on_jump true}}
          :core.norg.dirman {:config {:workspaces {:notes "~/neorg/notes"
                                                   :gtd "~/neorg/tasks"}
                                      :default :notes
                                      :autochdir true
                                      :open_last_workspace false}}
          :core.norg.completion {:config {:engine :nvim-cmp}}
          :core.gtd.base {:config {:workspace :gtd
                                   :default_lists {:inbox :index.norg
                                                   :someday :someday.norg}
                                   :custom_tag_completion true
                                   }}
          :core.keybinds {:config {:default_keybinds true
                                   :hook keybinds-hook}}}})

(fn rhs->func [rhs]
  (if (string? rhs)
    #(vim.cmd rhs)
    rhs))

(fn with-neorg-started [func]
  ; Starts Neorg if not running; then calls `func`
  (fn []
    (if (not (running?))
      (vim.cmd :NeorgStart))
    (func)))

{:setup (fn [keymaps]
          ; General setup
          (let [{: setup} (require :neorg)]
            (setup opt-tbl))
          ; Set keymaps
          (each [lhs rhs (pairs keymaps)]
            (let [wrapped-rhs (-> rhs
                                  (rhs->func)
                                  (with-neorg-started))]
              (nmap! lhs wrapped-rhs))))}
