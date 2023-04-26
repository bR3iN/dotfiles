(import-macros {: with-cb} :utils.macros)

(set _G.__user_operators {})

(fn save [ls]
  (local cbs [])
  (each [_ v (ipairs ls)]
    (let [prefix (string.sub v 1 1)
          suffix (string.sub v 2)]
      (->> (match prefix
             "@" (let [tmp (vim.fn.getreg suffix)]
                   #(vim.fn.setreg suffix tmp))
             "&" (let [tmp (. vim.opt suffix)]
                   #(tset vim.opt suffix tmp))
             "'" (let [[tmpr tmpc] (vim.api.nvim_buf_get_mark 0 suffix)]
                   #(vim.api.nvim_buf_set_mark 0 suffix tmpr tmpc {}))
             _ (error (.. "Unexpected value: " v)))
           (table.insert cbs))))
  #(vim.tbl_map #($1) cbs))

(fn yank-to-unnamed [mode]
  (let [vselect (match mode
                  :char "`[v`]"
                  :line "`[V`]"
                  :block "`[`]"
                  (where (or :v :V "")) "gv"
                  _ (error (.. "Unknown mode: " mode)))
        cmd (.. "silent noautocmd keepjumps normal! " vselect "y")]
    (let [restore (save ["&clipboard" "&selection" "'<" "'>"])]
      (set vim.opt.clipboard {})
      (set vim.opt.selection :inclusive)
      (with-cb
        [restore]
        (vim.cmd cmd)))))

(fn get-text [mode]
  (let [restore (save ["@\""])]
    (with-cb
      [restore]
      (yank-to-unnamed mode)
      (. (vim.fn.getreginfo "\"") :regcontents))))


; (fn mode->marks [mode]
;   (match mode
;     (where (or :char :line :block)) (values "[" "]")
;     (where (or :v :V "")) (values "<" ">")
;     _ (error (.. "Unknown mode: " mode))))

; (fn doto-nth [list n func]
;   ; modify `list` in-place by applying `func` to its `n`th line and
;   ; substituting the result. Returns `list`.
;   (let [new-line (func (. list n))]
;     (tset list n new-line)
;     list))

; (fn trim-lines-char [lines coll colr]
;   (-> lines
;       (doto-nth 1 #(string.sub $1 coll))
;       (doto-nth (length lines) #(string.sub $1 1 colr))))

; (fn trim-lines-block [lines coll colr]
;   (vim.tbl_map #(string.sub $1 coll colr) lines))

; (fn get-text [mode]
;   (let [(lmark rmark) (mode->marks mode)]
;     (let [[row1 col1] (vim.api.nvim_buf_get_mark 0 lmark)
;           [row2 col2] (vim.api.nvim_buf_get_mark 0 rmark)]
;       (let [lines (vim.api.nvim_buf_get_lines 0 (- row1 1) row2 true)
;             trim-lines (match mode
;                          (where (or :char :v)) trim-lines-char
;                          (where (or :line :V)) #$1
;                          (where (or :block "")) trim-lines-block)]
;         (print row1 col1 row2 col2)
;         (trim-lines lines (+ col1 1) (+ col2 1))))))


(fn mk-op [name lines-cb]
  ; Make callback globally accessible
  (tset _G.__user_operators name
        (fn [mode]
          (-> mode
              (get-text)
              (lines-cb))
          nil))
  ; Create keymaps
  (let [opt-tbl {:noremap true :silent true}
        lhs (.. :<Plug> name)
        set-keymap (fn [mode rhs]
                     (vim.keymap.set mode lhs rhs opt-tbl))]
    (set-keymap
      :n (.. ":set operatorfunc=v:lua.__user_operators." name "<CR>g@"))
    (set-keymap
      :v (.. ":<C-u>call v:lua.__user_operators." name "(visualmode())<CR>"))))

{: mk-op}
