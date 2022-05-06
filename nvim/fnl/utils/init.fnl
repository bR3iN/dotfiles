(local M {})

(fn M.starts-with [str prefix]
  (= (string.sub str 1 (length prefix)) prefix))

(fn M.remove-prefix [str prefix]
  (string.sub str (+ 1 (length prefix))))

(fn M.contains [str substr]
  (not= (string.find str substr) nil))

; Exports `nil?`, `function?`, etc.
(let [mk-is-type (fn [tp]
                   (fn [el]
                     (= (type el)
                        tp)))]
  (each [_ tp (ipairs [:nil :function :string :table :number])]
    (let [macro-name (.. tp :?)]
      (tset M macro-name (mk-is-type tp)))))

(local func-tbl-name :__user_functions)
(tset _G func-tbl-name {})

(local generate-id
  (do (var id 0)
    (fn next-id []
      (set id (+ 1 id))
      (tostring id))
    next-id))


(fn M.register-func [func id]
  (tset (. _G func-tbl-name) id func))

; TODO: use a global `call_user_funcion` instead?
; Generate vimscript to call a previously registered function by id
(fn M.id->vimscript [id]
    (.. ":lua _G." func-tbl-name "[\"" id "\"]()<CR>"))

(fn M.func->str [func id]
  (let [str (M.id->vimscript id)]
    (M.register-func func id)
    str))

; If `rhs` is an anonymous function, substitute it with vimscript calling it
(fn M.rhs->str [rhs]
  (match (type rhs)
    :string rhs
    :function (let [id (generate-id)]
                (M.func->str rhs id))
    other (error (.. "Unsupported value for 'rhs': " other))))

M
