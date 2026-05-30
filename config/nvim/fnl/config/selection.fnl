(local {: keymaps!} (require :utils))

(fn selection-forward? []
  (let [[cl cc] (vim.api.nvim_win_get_cursor 0)
        [bl bc] (vim.api.nvim_buf_get_mark 0 "<")]
    (and (= cl bl) (= cc bc))))

(fn ensure-forward [?forward]
  (let [cursor-is-ahead (selection-forward?)]
    (when (if (= ?forward false)
              cursor-is-ahead
              (not cursor-is-ahead))
      (vim.api.nvim_feedkeys "o" "x" false))))

(keymaps! {:v {:<Plug> {"(ensure-backward)" #(ensure-forward true)
                        "(ensure-forward)" #(ensure-forward)}}})
