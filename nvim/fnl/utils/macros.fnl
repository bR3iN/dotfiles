;; fennel-ls: macro-file.

(local M {})

; Execute code with a callback called at the end of the block, even if
; encountering an error.
(fn M.with-cb [[cb] ...]
  `(let [cb# ,cb]
     (case (#[$...] (pcall #(do
                              ,...)))
       [true & vs#] (do
                      (cb#) (unpack vs#))
       [false err#] (do
                      (cb#) (error err#)))))

M
