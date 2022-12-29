(local uv vim.loop)

(local SHELL :/usr/bin/bash)

(local M {})

(macro dispatch! [ls ...]
  (fn prepend [el ls]
    (table.insert ls 1 el)
    ls)
  (fn append [el ls]
    (table.insert ls el)
    ls)
  (let [func-wo-cb (table.remove ls)
        cb '(fn ,(prepend 'err# ls)
              (do
                (assert (not err#) err#)
                ,...))]
    (append cb func-wo-cb)))

(fn connect! [pipe buffer]
  (dispatch!
    [?data (pipe:read_start)]
    (-?>> ?data
          (table.insert buffer))))

(fn parse-cmd [[file & args]]
  (values file args))

(fn M.spawn-with-callback [cmd ?cb ?opts]
  (let [?cb (-?> ?cb (vim.schedule_wrap))
        (file args) (parse-cmd cmd)
        opts (doto (or ?opts {})
                   (tset :args args))]
    (uv.spawn file opts ?cb)))

; (fn M.spawn-with-lines-callback [cmd ?cb ?opt-tbl]
;     (let [stdout (uv.new_pipe)
;           stderr (uv.new_pipe)
;           stdout-buf {}
;           stderr-buf {}]
;         (connect! stdout stdout-buf)
;         (connect! stderr stderr-buf)

;         (let [(file args) (parse-cmd cmd)
;               opt-tb (vim.tbl_extend
;                          "force"
;                          (or ?opt-tbl {})
;                          {: args :stdio [nil stdout stderr]})
;               handle

;             ))


(fn M.spawn [cmd ?opt-tbl]
  (M.spawn-with-callback cmd nil ?opt-tbl))

(fn M.scan-dir [path cb]
  (let [entry-cb (vim.schedule_wrap
                   (fn [entry]
                     (cb entry.name entry.type)))]
    (dispatch!
      [dir (uv.fs_opendir path)]
      (fn iter []
        (dispatch!
          [entries (dir:readdir)]
          (if entries
            (do
              (vim.tbl_map entry-cb entries)
              (iter))
            (dir:closedir))))
      (iter))))

M
