(local {: command!} (require :utils.nvim))

(var saved {:fg (. (require :flavours) 16)})

(fn replace-saved [new]
  (let [tmp saved]
    (set saved new)
    tmp))

(fn toggle []
  (let [new (replace-saved
              (vim.api.nvim_get_hl_by_name :Comment true))]
    (vim.api.nvim_set_hl 0 :Comment new)
    (vim.api.nvim_cmd {:cmd :redraw :bang true} {})))

(command! :ToggleComments toggle)
