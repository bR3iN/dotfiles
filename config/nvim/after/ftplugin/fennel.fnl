(local {: buf-opts! : keymaps!} (require :utils))
(import-macros {: with-saved-view} :utils.macros)

(fn fnlfmt [input]
  (let [out (: (vim.system [:fnlfmt "-"] {:stdin input}) :wait)]
    (if (= out.code 0)
      (let [output (vim.split out.stdout "\n" {:plain true})]
        (when (= (. output (length output)) "")
          (table.remove output))
        output)
      (do
        (vim.notify out.stderr vim.log.levels.ERROR)
        nil))))

(fn _G._fnlfmt_formatexpr []
  (let [start (- vim.v.lnum 1)
        end (+ start vim.v.count)
        lines (vim.api.nvim_buf_get_lines 0 start end false)
        output (fnlfmt (table.concat lines "\n"))]
    (if output
      (do (vim.api.nvim_buf_set_lines 0 start end false output) 0)
      1)))

(buf-opts! {:commentstring ";; %s"
            :formatexpr "v:lua._fnlfmt_formatexpr()"
            ;; :formatprg "fnlfmt -"
            :iskeyword- "."})

(keymaps! {:opts {:buffer true}
           :n {:<Plug>edit#format-file #(with-saved-view (vim.cmd "normal! gggqG"))}})
