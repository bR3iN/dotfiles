local uv = vim.loop

local SHELL = "bash"
local M = {}

local function parse_cmd(cmd)
    local args
    if type(cmd) == 'table' then
        args = cmd
        cmd = table.remove(args, 1)
    end
    return cmd, args
end

local function read_into(pipe, buffer)
    pipe:read_start(function(err, data)
        if data then
            table.insert(buffer, data)
        end
    end)
end

M.spawn_with_callback_linewise = function(cmd, cb, options)
    local cmd, args = parse_cmd(cmd)

    local stdout = uv.new_pipe()
    local stderr = uv.new_pipe()

    local stdout_buffer = {}
    local stderr_buffer = {}

    local options = vim.tbl_extend("force", options or {}, {
        args = args,
        stdio = {nil, stdout, stderr},
    })

    local handle
    local cb = vim.schedule_wrap(function(code, sig)
        stdout:close()
        stderr:close()
        handle:close()

        local stdout_string = table.concat(stdout_buffer)
        local stderr_string = table.concat(stderr_buffer)
        cb(code, sig, stdout_string, stderr_string)
    end)

    handle = uv.spawn(cmd, options, cb)
    read_into(stdout, stdout_buffer)
    read_into(stderr, stderr_buffer)
end

M.spawn_with_callback = function(cmd, cb, options)
    local cmd, args = parse_cmd(cmd)

    local options = options or {}
    options.args = args
    
    local cb = cb and vim.schedule_wrap(cb)
    uv.spawn(cmd, options, cb)
end

M.spawn = function(cmd, options)
    M.spawn_with_callback(cmd, nil, options)
end

for _, func in ipairs({
    "spawn",
    "spawn_with_callback",
})
do
    M[func.."_with_shell"] = function(cmd, ...)
        if type(cmd) ~= 'string' then
            error(func.."_with_shell (first argument): expected string, found "..type(cmd))
        end
        M[func]({SHELL, "-c", cmd}, ...)
    end
end

M.for_file_in = function(dir_name, cb, buf_nr)
    local function start_iter(err, dir)
        if dir then 
            local function iter_files()
                dir:readdir(function(err, entries)
                    if not entries then
                        dir:closedir()
                    else
                        vim.tbl_map(vim.schedule_wrap(function(entry)
                            cb(entry.name, entry.type)
                        end), entries)
                        return iter_files()
                    end
                end)
            end

            iter_files()
        else
            error(err)
        end
    end

    uv.fs_opendir(dir_name, start_iter, buf_nr)
end

return M
