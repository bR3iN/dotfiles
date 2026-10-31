(fn get_hl [name]
  (vim.api.nvim_get_hl 0 {: name}))

(fn set_hl [name val]
  (vim.api.nvim_set_hl 0 name val))

(var highlight-enabled false)

(local hls {})

(fn apply []
  (->> highlight-enabled
       (. hls)
       (set_hl :Comment)))

(fn toggle-state []
  (set highlight-enabled (not highlight-enabled)))

(fn on-colorscheme-change [_ev]
  (let [hl (get_hl :Comment)]
    (set (. hls false) hl))
  (apply))

(fn toggle []
  (toggle-state)
  (apply))

(fn setup [hl]
  (vim.api.nvim_create_user_command :ToggleComments toggle {})
  (set (. hls true) hl)
  ;; Get the :Comment hl group on the first `setup` call and on colorscheme changes.
  (when (not (. hls false))
    (set (. hls false) (get_hl :Comment)))
  (let [group (vim.api.nvim_create_augroup "user.toggle-comments" {:clear true})]
    (vim.api.nvim_create_autocmd :Colorscheme
                                 {: group :callback on-colorscheme-change})))

{: setup}
