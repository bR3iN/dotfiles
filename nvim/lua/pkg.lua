local async  = require'utils.async'
local uv     = vim.loop

local pkg_dir = os.getenv('HOME')..'/.local/share/nvim/site/pack/pkgs/start'
local pkgs    = {}

local M = {}

local function get_dirname(name)
    return vim.split(name, '/')[2]
end

local function get_path(name)
    return pkg_dir..'/'..get_dirname(name)
end

local function remove_file(filename)
    assert(filename ~= '', 'empty filename')
    local path = pkg_dir..'/'..filename

    async.spawn_with_callback({
        "rm", "-rf", path
    }, function(code)
        if code == 0 then
            print("Removed '"..filename.."'")
        end
    end)
end

local function update_repo(path, cb)
    async.spawn_with_callback({"git", "pull"}, cb, {cwd = path})
end

local function is_git_repo(path)
    return vim.fn.isdirectory(path..'/.git') ~= 0
end

M.add = function(name, cb)
    local path = get_path(name)

    if vim.fn.isdirectory(path) ~= 0 then
        if cb then cb() end
        table.insert(pkgs, name)
    else
        async.spawn_with_callback({
            "git", "clone", "https://github.com/"..name, path
        }, function(code)
            if code == 0 then
                if cb then cb() end
                table.insert(pkgs, name)
                print("Installed '"..name.."'")
            end
        end)
    end
end

M.init = function()
    vim.fn.mkdir(pkg_dir, 'p')
    pkgs = {}
    return M.add
end

M.update = function(name)
    print('Update plugins...')
    async.for_file_in(pkg_dir, function(filename)
        local path = pkg_dir..'/'..filename
        if is_git_repo(path) then
            update_repo(path)
        end
    end)
end

M.list = function()
    print("Plugins:")
    vim.tbl_map(function(name) print(" "..name) end, pkgs)
end

M.clean = function()
    local pkg_dirs = vim.tbl_map(get_dirname, pkgs)
    local function removable(filename)
        return not vim.tbl_contains(pkg_dirs, filename)
    end

    async.for_file_in(pkg_dir, function(filename, filetype)
        if filetype == 'directory' and removable(filename) then
        print(filename)
            remove_file(filename)
        end
    end)
end

return M
