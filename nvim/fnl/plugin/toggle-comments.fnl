(local {: nil?} (require :utils))
(local {: command!} (require :utils.nvim))

(fn get_hl [name]
  (vim.api.nvim_get_hl_by_name name true))

(fn set_hl [name val]
  (vim.api.nvim_set_hl 0 name val))

(var saved nil)

(fn replace-saved [new]
  ; Lazily load initial value so we don't get the wrong one
  ; if loaded before the colorscheme.
  ; TODO: Alternate between two named highlight groups instead?
  (when (nil? saved)
    (let [init (get_hl :NormalFloat)]
      (set saved init)))
  (let [tmp saved]
    (set saved new)
    tmp))

(fn toggle []
  (let [new (-> :Comment
                (get_hl)
                (replace-saved))]
    (set_hl :Comment new)
    (vim.cmd :redraw {:bang true})))

(command! :ToggleComments toggle)
