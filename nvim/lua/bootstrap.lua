local pkg_path = vim.fn.stdpath('data') .. '/site/pack/pkgs/start'

function is_installed(mod)
    ok, _ = pcall(require, mod)
    return ok
end

function fetch_plugin(url, dirname)
    vim.fn.system({'git', 'clone', url, pkg_path .. dirname})
end

local fetch_hotpot = not is_installed('hotpot')

if fetch_hotpot then
    print('Bootstrapping hotpot.nvim')
    fetch_plugin('https://github.com/rktjmp/hotpot.nvim', "/tmp_hotpot")
end

local fetch_pkg = not is_installed('pkg')

if fetch_pkg then
    print('Bootstrapping pkg.nvim')
    fetch_plugin('https://github.com/bR3iN/pkg.nvim', "/tmp_pkg")
end

if fetch_hotpot or fetch_pkg then
    vim.cmd('packloadall!')
end

