local url = 'https://github.com/rktjmp/hotpot.nvim'
local hotpot_path = vim.fn.stdpath('data') .. '/site/pack/pkgs/start/rktjmp_hotpot_nvim'

if vim.fn.isdirectory(hotpot_path) == 0 then
    print('Bootstrapping hotpot.nvim')
    vim.fn.system({'git', 'clone', url, hotpot_path})
    vim.cmd('packloadall!')
end

require('hotpot')
