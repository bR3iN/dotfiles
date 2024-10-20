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
    (let [name (.. tp :?)]
      (tset M name (mk-is-type tp)))))

(fn M.empty? [str]
  (= (length str) 0))

M
