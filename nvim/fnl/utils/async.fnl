(local uv vim.loop)

(local SHELL :/usr/bin/bash)

(local M {})

; Hide callbacks and error handling in low-level luvit calls.
; `args` are the output paramters of the call minus the error
; value as well the call itself minus the callback parameter.
; Example: `(dispatch!
;             [dir (uv.fs_opendir path)]
;             (do-smth-with dir))`
; translates to
;          `(uv.fs_opendir
;             path
;             (fn [err dir]
;               (assert (not err) err)
;               (do-smth-with dir)))`
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

(fn M.spawn [cmd ?opts ?cb]
  (let [opts (or ?opts {})
        ?wrapped-cb (-?> ?cb (vim.schedule_wrap))]
    (uv.spawn cmd opts ?wrapped-cb)))

(fn M.spawn-capture-output [cmd ?opts cb]
  (var handle nil)
  (let [stdout (uv.new_pipe)
        stderr (uv.new_pipe)
        stdout-buf {}
        stderr-buf {}
        opts (doto (or ?opts {})
               (tset :stdio [nil stdout stderr]))
        cb (vim.schedule_wrap
             (fn [code signal]
               (stdout:close)
               (stderr:close)
               (handle:close)
               (let [stdout (table.concat stdout-buf)
                     stderr (table.concat stderr-buf)]
                 (cb code signal stdout stderr))))]
    ;; Order matters here
    (set handle (uv.spawn cmd opts (vim.schedule_wrap cb)))
    (connect! stdout stdout-buf)
    (connect! stderr stderr-buf)))

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
