(if (= (vim.fn.executable :nvr) 1)
  (set vim.env.VISUAL "nvr -cc split --remote-wait +'set bufhidden=wipe'"))
