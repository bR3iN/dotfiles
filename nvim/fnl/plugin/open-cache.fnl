(local {: nmap!} (require :utils.nvim))
(local {: cache-path-for-fnl-file} (require :hotpot.api.cache))

(fn open-cache []
  (->> :%:p
       (vim.fn.expand)
       (cache-path-for-fnl-file)
       (.. ":edit ")
       (vim.cmd)))

(nmap! "<Plug>OpenCache" #(open-cache))
