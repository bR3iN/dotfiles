(local M {})

(local zk (require :zk))
(local util (require :zk.util))
(local {: put!} (require :utils))
(local {: spawn-capture-output} (require :utils.async))

(fn M.create-and-insert-link []
  (let [loc (util.get_lsp_location_from_caret)
        title (vim.fn.input "Title: ")]
    (when (not= title "")
      (zk.new {: title :edit false :insertLinkAtLocation loc}))))

(fn M.get-note []
  (vim.fn.bufname))

(fn M.open-results [results]
  (each [_ {: absPath} (ipairs results)]
    (vim.cmd (.. "e " absPath))))

(fn M.open-backlinks []
  (zk.pick_notes {:linkTo [(M.get-note)]
                  :maxDistance vim.v.count1
                  :recursive true} {} M.open-results))

(fn M.open-links []
  (zk.pick_notes {:linkedBy [(M.get-note)]
                  :maxDistance vim.v.count1
                  :recursive true} {} M.open-results))

(fn M.create-note []
  (let [title (vim.fn.input "Title: ")]
    (when (not= title "")
      (zk.new {: title}))))

(fn M.capture-with [f]
  ;; Screenshot -> f -> insert text at cursor
  (spawn-capture-output :zk-screenshot nil
                        (fn [code _signal stdout _stderr]
                          (if (= 0 code)
                              (-> stdout f put!)))))

(fn M.insert-screenshot []
  (M.capture-with #(.. "![[" $1 "]]")))

M
