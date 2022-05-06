(local {: create} (require :utils.operator))
(local {: spawn} (require :utils.async))

(create :Print #(print (vim.inspect $1)))
(create :OpenInBrowser
        (let [browser (or vim.g.browser :firefox)
              open-in-browser (fn [line]
                                (spawn [vim.g.browser line]))]
          (fn [lines]
            (vim.tbl_map open-in-browser lines))))
