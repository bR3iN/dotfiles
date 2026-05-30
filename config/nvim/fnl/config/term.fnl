(local {: keymaps! : autocmd! : get-cwd-override} (require :utils))

(fn setup-term [{: buf}]
  ;; (set vim.wo.number false)
  ;; (set vim.wo.relativenumber false)
  ;; FIXME: use when having separate picker
  ;; (set vim.bo.buflisted false)
  ;; Don't insert here as this doesn't have to be an active buffer
  (keymaps! {:opts {:buffer buf}
             :n {;; Go to shell prompts
                 "[s" {:desc "Previous Shell Prompt" :callback "[["}
                 "]s" {:desc "Next Shell Prompt" :callback "]]"}
                 ;; "Forward" some keys directly in terminal mode
                 "" (let [keys ["<CR>" "<C-c>" "<C-n>" "<C-p>"]]
                      (collect [_ key (ipairs keys)]
                        (values key (.. "i" key))))}}))

(fn teardown-term [{: buf}]
  (when (vim.api.nvim_buf_is_valid buf)
    ;; Autoclose terminal buffers on clean exit; `default-autocmd` does something similar for simple shell buffers, but always closes the window in the process.
    ;; We keep this functionality as things like `:Cargo` rely on the auto-closing behavior (which is why we check for buffer validity) above but reimplement it for other buffers in a more granular way, not necessarily deleting the window.
    (when (= 0 vim.v.event.status)
      (case (. vim.b buf :term_autoclose)
        :window (vim.api.nvim_buf_delete buf {:force true})
        :buffer (_G.Snacks.bufdelete.delete buf)
        nil nil
        other (error (.. "value unknown: " other))))))

(fn enter-term []
  (when (and (or (= vim.bo.buftype :terminal) (= vim.bo.buftype :prompt))
             (-?> (vim.api.nvim_get_mode)
                  (. :mode)
                  (vim.startswith :n))
             ;; NOTE: Don't do that in telescope prompts as this inserts a literal "A" in there
             (not= vim.bo.filetype :TelescopePrompt))
    (vim.cmd :startinsert!)))

(autocmd! {:event :TermOpen :callback setup-term}
          {:event :TermClose :callback teardown-term}
          {:event [:BufWinEnter :BufEnter]
           :callback ;; Avoids triggering wrongly as startinsert will only happen after a sequence of commands and also catches more cases (for some reason).
           (vim.schedule_wrap enter-term)})

(fn open-term [{: autoclose}]
  (let [cwd (get-cwd-override)
        buf (vim.api.nvim_create_buf false true)]
    (vim.api.nvim_set_current_buf buf)
    (tset vim.b buf :term_autoclose autoclose)
    (vim.fn.jobstart [:/usr/bin/fish] {:term true : cwd})))

{: open-term}
