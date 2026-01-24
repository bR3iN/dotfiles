;; fennel-ls: macro-file.
(local M {})

(fn exec [body]
  `(do
     (_G.unpack ,body)))

(fn with-cleanup-inner [cleanup body]
  `(case (#[$...] (pcall #,(exec body)))
     [true & res#] (do
                     ,cleanup
                     (_G.unpack res#))
     [false err#] (do
                    ,cleanup
                    (error err#))))

(fn M.with-cleanup [cleanup & body]
  (with-cleanup-inner cleanup body))

(fn M.with-saved [locs & body]
  (let [let-binds `[]
        cleanup `(do
                   )]
    (each [_ loc (ipairs locs)]
      (let [sym (gensym)]
        ;; Save current value
        (table.insert let-binds sym)
        (table.insert let-binds loc)
        ;; Restore saved value
        (table.insert cleanup `(set ,loc ,sym))))
    `(let ,let-binds
       ,(with-cleanup-inner cleanup body))))

(fn M.set-locally [binds & body]
  (when (not= 0 (% (length binds) 2))
    (error "local binds must contain an even number of forms"))
  (let [save-binds `[]
        sets []
        resets []]
    (for [i 1 (length binds) 2]
      (let [loc (. binds i)
            val (. binds (+ i 1))
            sym (gensym)]
        ;; Save current value
        (table.insert save-binds sym)
        (table.insert save-binds loc)
        ;; Override value
        (table.insert sets `(set ,loc ,val))
        ;; Restore saved value
        (table.insert resets `(set ,loc ,sym))))
    `(let ,save-binds
       ,(exec sets)
       ,(with-cleanup-inner (exec resets) body))))

(fn M.with-saved-view [& body]
  "Save view and redraw lazy while executing the body."
  `(let [view# (vim.fn.winsaveview)]
    ,(M.set-locally
      `[,(sym "vim.o.lazyredraw") true]
      (exec body)
      `(vim.fn.winrestview view#))))

; Execute code with a callback called at the end of the body, even if
; encountering an error.
(fn M.with-cb [[cb] ...]
  `(let [cb# ,cb]
     (case (#[$...] (pcall #(do
                              ,...)))
       [true & vs#] (do
                      (cb#)
                      (unpack vs#))
       [false err#] (do
                      (cb#)
                      (error err#)))))

M
