(local {: autocmd! : buf-opts!} (require :utils))

(local config (.. vim.env.HOME "/.{dotfiles/config,config}"))

(fn exec-on-save [pattern cmd]
  {:event :BufWritePost
   : pattern
   :callback #(vim.system cmd {:text true}
                          (fn [res]
                            (if (not= 0 res.code)
                                (vim.schedule #(vim.notify (vim.inspect {: cmd
                                                                         : res})
                                                           vim.log.levels.ERROR)))))})

(autocmd! ;; Autoreload config files on save
          {:event :BufWritePost
           :pattern (.. config "/nvim/*.{vim,lua,fnl}")
           ;; Schedule is needed a otherwise highlight groups become weirdly mixed up.
           :callback #(vim.schedule #(dofile vim.env.MYVIMRC))}
          ;; Autotrust our own changes to .hotpot.fnl
          {:event :BufWritePre
           :pattern (.. config "/nvim/.hotpot.fnl")
           :callback (fn [{:buf bufnr}]
                       (vim.secure.trust {:action :allow : bufnr}))}
          ;; Don't create undofiles for temporary files
          {:event :BufWritePre
           :pattern :/tmp/*
           :callback #(buf-opts! {:undofile false})}
          {:event :BufWritePre
           :pattern "~/.crypt/*"
           :callback #(buf-opts! {:undofile false})}
          (exec-on-save (.. config "/waybar/*" ) [:killall :-SIGUSR2 :waybar]))

(when vim.env.TMUX
  ;; Autoreload tmux config on change
  (autocmd! (exec-on-save (.. config "/tmux/tmux.conf")
                          [:tmux
                           :source-file
                           (.. vim.env.HOME "/.config/tmux/tmux.conf")])))
