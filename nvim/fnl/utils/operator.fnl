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


(fn create [name lines-cb]
  ; Make callback globally accessible
  (let [wrapped-cb (fn [mode]
                     (-> mode
                         (get-text)
                         (lines-cb))
                     nil)]
    (tset _G.__user_operators name wrapped-cb))
  ; Create keymaps
  (let [opt-tbl {:noremap true :silent true}
        lhs (.. :<Plug> name)
        set-keymap (fn [mode rhs]
                     (vim.api.nvim_set_keymap
                       mode lhs rhs opt-tbl))]
    (set-keymap
      :n (.. ":set operatorfunc=v:lua.__user_operators." name "<CR>g@"))
    (set-keymap
      :v (.. ":<C-u>call v:lua.__user_operators." name "(visualmode())<CR>"))))

{: create}
