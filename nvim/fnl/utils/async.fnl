(local uv vim.loop)

(local SHELL :/usr/bin/bash)

(local M {})

; Hide callbacks and error handling in low-level luvit calls.
; `args` are the output paramters of the call minus the error
; value as well the call itself minus the callback parameter.
; Example: `(dispatch!
;             [dir (uv.fs_opendir path)]
;             (use dir))`
; translates to
;          `(uv.fs_opendir
;             path
;             (fn [err dir]
;               (assert (not err) err)
;               (use dir)))`
(macro dispatch! [args ...]
  ; Split off the luvit call
  (let [call (table.remove args)
        ; Create the callback
        cb '(fn ,(doto args
                   ; Insert error paramter into function signature
                   (table.insert 1 'err#))
              (do
                ; Add error handling
                (assert (not err#) err#)
                ; Actual callback body
                ,...))]
    ; Insert callback into libuv call
    (doto call
      (table.insert cb))))

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
  (let [wrapped-cb (vim.schedule_wrap
                     (fn [entry]
                       (cb entry.name entry.type)))]
    (dispatch!
      [dir (uv.fs_opendir path)]
      (fn iter []
        (dispatch!
          [entries (dir:readdir)]
          (if entries
            (do
              (vim.tbl_map wrapped-cb entries)
              (iter))
            (dir:closedir))))
      (iter))))

M
