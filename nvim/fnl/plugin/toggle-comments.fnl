(local {: nil?} (require :utils))
(local {: command!} (require :utils.nvim))

(fn get_hl [name]
  (vim.api.nvim_get_hl_by_name name true))

(fn set_hl [name val]
  (vim.api.nvim_set_hl 0 name val))

(var saved nil)

; Replace saved value, returning previous value
(fn replace-saved [new]
  (when (nil? saved)
    (let [init (get_hl :CommentHighlighted)]
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


(fn setup []
  (command! :ToggleComments toggle)
  ; Lazily load initial value (i.e. the highlighted state) so we don't get the
  ; wrong one if loaded before the colorscheme.
  (let [init (get_hl :CommentHighlighted)]
    (set saved init)))

{: setup}
