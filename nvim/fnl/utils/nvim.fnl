(local {: starts-with : remove-prefix : contains : nil? : string?} (require :utils))

(local M {})

(fn parse-map-options [list]
  (local opt-tbl { :noremap true :silent true }) ; Default options
  (each [_ opt (ipairs list)]
    (match opt
      :remap   (tset opt-tbl :noremap false)
      :verbose (tset opt-tbl :silent  false)
      _        (tset opt-tbl opt      true)))
  opt-tbl)

; Wrapper around `nvim_buf_set_keymap` that accepts options as a list. The
; `remap` option will be automatically added if `rhs` contains `<Plug>` or `<plug>`.
(fn M.set-keymap [mode lhs rhs ?opt-ls]
  (let [opt-tbl (parse-map-options
                  (or ?opt-ls []))]
    (if (and (string? rhs)
             (or (contains rhs :<Plug>)
                 (contains rhs :<plug>)))
      (tset opt-tbl :noremap false))
    (vim.keymap.set mode lhs rhs opt-tbl)))

; Define the `<mode>map!` functions, e.g. `nmap!`; they can be called as
; either `(nmap! lhs rhs)` or `(nmap! [:opt1 :opt2] lhs rhs)`
(each [_ mode (ipairs [:n :v :x :s :o :i :l :c :t])]
  (tset M (.. mode :map!)
        #(if (nil? $3)
           (M.set-keymap mode $1 $2)
           (M.set-keymap mode $2 $3 $1))))

(fn M.command! [lhs rhs ?opt-tbl]
  (let [opt-tbl (or ?opt-tbl {})]
    (vim.api.nvim_create_user_command lhs rhs opt-tbl)))

(set M.autocmd!
     (let [inner (fn [name clear cmds]
                   (let [group (vim.api.nvim_create_augroup name {: clear})]
                     (each [_ {: event & opt_tbl} (ipairs cmds)]
                       (vim.api.nvim_create_autocmd
                         event
                         (vim.tbl_extend :error opt_tbl {: group})))))]
       #(if (nil? $3)
          (inner $1 true $2)
          (inner $1 $2 $3))))

(fn M.augroup! [name ?clear]
  (let [id (let [clear (or ?clear true)]
             (vim.api.nvim_create_augroup name {: clear}))]
    (fn [event pattern callback ?opt-tbl]
      (let [set-cb (fn [tbl]
                     (match (type callback)
                       :string (tset tbl :command callback)
                       :function (tset tbl :callback callback)))
            opt-tbl (doto (or ?opt-tbl {})
                          (tset :group id)
                          (tset :pattern pattern)
                          (set-cb))]
        (vim.api.nvim_create_autocmd event opt-tbl)))))

(fn M.replace-termcodes [keys]
  (vim.api.nvim_replace_termcodes keys true false true))

(fn M.put! [text]
  (vim.api.nvim_put [text] "" false true))

M
