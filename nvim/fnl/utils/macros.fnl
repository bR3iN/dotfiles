;; fennel-ls: macro-file.

(fn starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

(fn remove-prefix [str prefix]
  (string.sub str (+ 1 (length prefix))))

(local M {})

(fn mk-set [opt-tbl]
  (fn [name ?value]
    (let [name (tostring name)
          value (or ?value true)]
      (if (starts-with name :no)
          `(tset ,opt-tbl ,(remove-prefix name :no) ,(not value))
          `(tset ,opt-tbl ,name ,value)))))

(fn mk-set-with-method [opt-tbl method]
  (fn [name value]
    (let [opt-obj `(. ,opt-tbl ,(tostring name))]
      `(: ,opt-obj ,method ,(tostring value)))))

; Define all combinations of `set` `setl` and `setg` with a postfix in `!+^-`
(each [prefix opt-tbl (pairs {:set `vim.opt
                              :setl `vim.opt_local
                              :setg `vim.opt_global})]
  (tset M (.. prefix :!) (mk-set opt-tbl))
  (each [postfix method (pairs {:+ :append :^ :prepend :- :remove})]
    (tset M (.. prefix postfix) (mk-set-with-method opt-tbl method))))

(fn M.let! [name val]
  `(tset vim.g ,(tostring name) ,val))

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
