(local {: autocmd! : buf-opts!} (require :utils))

(autocmd! ;; Autoreload config files on save
          {:event :BufWritePost
           :pattern (.. vim.env.HOME
                        "/.{dotfiles/config,config}/nvim/*.{vim,lua,fnl}")
           ;; Schedule is needed a otherwise highlight groups become weirdly mixed up.
           :callback #(vim.schedule #(dofile vim.env.MYVIMRC))}
          ;; Autotrust our own changes to .hotpot.fnl
          {:event :BufWritePre
           :pattern (.. vim.env.HOME
                        "/.{dotfiles/config,config}/nvim/.hotpot.fnl")
           :callback (fn [{:buf bufnr}]
                       (vim.secure.trust {:action :allow : bufnr}))}
          ;; Autoreload tmux config on change
          (when vim.env.TMUX
            {:event :BufWritePost
             :pattern (.. vim.env.HOME
                          "/.{dotfiles/config,config}/tmux/tmux.conf")
             :callback #(let [cmd [:tmux
                                   :source-file
                                   (.. vim.env.HOME "/.config/tmux/tmux.conf")]]
                          (vim.system cmd {:text true}
                                      (fn [res]
                                        (if (not= 0 res.code)
                                            (vim.schedule #(vim.notify (vim.inspect {: cmd
                                                                                     : res})
                                                                       vim.log.levels.ERROR))))))})
          ;; Don't create undofiles for temporary files
          {:event :BufWritePre
           :pattern :/tmp/*
           :callback #(buf-opts! {:undofile false})}
          {:event :BufWritePre
           :pattern "~/.crypt/*"
           :callback #(buf-opts! {:undofile false})})
