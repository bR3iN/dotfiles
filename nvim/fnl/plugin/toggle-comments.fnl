(local {: command!} (require :utils.nvim))

(var hidden true)

(fn toggle []
  (let [color-on "ctermfg=17 cterm=bold"
        color-off "ctermfg=8 cterm=NONE"]
    (set hidden (not hidden))
    (->> (if hidden
           color-off
           color-on)
         (.. ":hi Comment ")
         (vim.cmd))))

(command! :ToggleComments toggle)
