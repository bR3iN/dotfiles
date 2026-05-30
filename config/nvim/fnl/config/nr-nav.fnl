(local keys (let [res {}]
              (for [i 1 9]
                (set (. res (tostring i)) i))
              res))

(fn digit-keys [wrap-key make-entry]
  "For digits 1..9, builds {(wrap-key s) (make-entry s n)}."
  (collect [s n (pairs keys)]
    (values (wrap-key s) (make-entry s n))))

{: digit-keys}
