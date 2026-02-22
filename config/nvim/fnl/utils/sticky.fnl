(local M {})

(fn stick1 [tbl]
  (let [char (vim.fn.nr2char (vim.fn.getchar))]
    (if (= "" char)
        (vim.print :exit)
        (let [val (. tbl char)]
          (if val
              (val)
              (vim.print :nope))
          (stick1 tbl)))))

(fn get-ns []
  (vim.api.nvim_create_namespace :sticky))

(fn M.stick! [tbl]
  (let [ns (get-ns)]
    (vim.on_key (fn [key typed]
                    (vim.print [key typed])
                  (if (= key "")
                      (do
                        (vim.print "exit")
                        (vim.on_key nil ns))
                      (. tbl typed)
                      (do ((. tbl typed))
                          true))
                  nil) ns)))

;; (M.stick! {:c #(vim.print :yes)})

M
