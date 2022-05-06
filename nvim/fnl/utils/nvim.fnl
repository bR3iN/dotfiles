(local {: starts-with : remove-prefix : contains : rhs->str : nil? : string?} (require :utils))

(local M {})

(fn parse-map-options [list]
  (local opt-tbl { :noremap true :silent true }) ; Default options
  (each [_ opt (ipairs list)]
    (match opt
      :remap   (tset opt-tbl :noremap false)
      :verbose (tset opt-tbl :silent  false)
      _        (tset opt-tbl opt      true)))
  opt-tbl)

; Wrapper around `nvim_buf_set_keymap` that accepts options as a list
; and accepts a string or an anonymous function as `rhs`. If `rhs` contains
; "<Plug>" or "<plug>", the mapping wil automatically be a "remap".
(fn M.set-keymap [mode lhs rhs ?options]
  (let [opt-tbl (parse-map-options
                  (or ?options []))
        set-keymap-api (if opt-tbl.buffer
                         (do
                           (set opt-tbl.buffer nil)
                           (partial vim.api.nvim_buf_set_keymap 0))
                         vim.api.nvim_set_keymap)]
    (if (and (string? rhs)
             (or (contains rhs :<Plug>)
                 (contains rhs :<plug>)))
      (tset opt-tbl :noremap false))
    (set-keymap-api mode lhs (rhs->str rhs) opt-tbl)))

; Define the `<mode>map!` macros, e.g. `nmap!`; they can be called as
; either `(nmap! lhs rhs)` or `(nmap! [:opt1 :opt2] lhs rhs)`
(each [_ mode (ipairs [:n :v :x :s :o :i :l :c :t])]
  (tset M (.. mode :map!)
        #(if (nil? $3)
           (M.set-keymap mode $1 $2)
           (M.set-keymap mode $2 $3 $1))))

(fn M.command! [lhs rhs]
  (vim.cmd (table.concat [:command! lhs (rhs->str rhs)] " ")))

(fn M.augroup! [name cmds]
  (vim.cmd (.. "augroup " name))
  (vim.cmd "autocmd!")
  (vim.api.nvim_exec (table.concat cmds "\n") false)
  (vim.cmd "augroup END"))

(fn M.color! [scheme]
  (if (not= vim.g.colors_name
            scheme)
    (vim.cmd (.. "colorscheme " scheme))))

(fn M.augroup! [name ?clear]
  (let [clear (or ?clear true)
        id (vim.api.nvim_create_augroup name {: clear})]
    (fn create-autocmd [event pattern callback ?opt-tbl]
      (let [set-cb (fn [tbl]
                     (match (type callback)
                       :string (tset tbl :command callback)
                       :function (tset tbl :callback callback)))
            opt-tbl (doto (or ?opt-tbl {})
                      (tset :group id)
                      (tset :pattern pattern)
                      (set-cb))]
        (vim.api.nvim_create_autocmd event opt-tbl)))
    create-autocmd))


; (augroup!
;     :init.lua
;     (autocmd :BufWritePost "*.py" "dosmth"))

M
