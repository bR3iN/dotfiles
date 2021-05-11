local awful         = require("awful")
local beautiful     = require("beautiful")
local gears         = require("gears")
local wibox         = require("wibox")
local utils         = require("wibar.utils")

local widgets = {}
local default_font = 'Symbols Nerd Font '..beautiful.font_size 

local template = function(tbl) 
    local icon_font_size = tbl.font_size or beautiful.font_size
    local icon_font = tbl.icon_font or beautiful.nerd_font..' '..icon_font_size
    local widget = utils.statusbar_widget {
        layout = wibox.layout.fixed.horizontal, 
        {
            widget = wibox.widget.textbox,
            id = 'perc',
            text = "??%",
            font = tbl.text_font or beautiful.font,
        },
        utils.empty_space(5),
        {
            widget = wibox.widget.textbox,
            id = 'icon',
            text = '?',
            font = icon_font,
        },
    }
    widget:set_fg(tbl.color)
    function widget:set_icon(text)
        self:get_children_by_id('icon')[1].text = text
    end
    function widget:set_perc(perc)
        self:get_children_by_id('perc')[1].text = perc:gsub('\n', '')
    end
    return widget
end

local function create_system_widget(tbl)
    -- tbl : table with the following keys
    --   cmd  : shell command to call; returns percentage or output to be
    --          parsed with `parser`
    --   text : text to be displayed (optional)
    --   parser : computes percentage and displayed text from `cmd`s 
    --            stdout (optional)
    --   time
    --   font
    --   color
    local widget = template {
        icon_font = tbl.font,
        color = tbl.color,
        text_font = tbl.text_font,
        font_size = tbl.font_size,
    }
    if tbl.text then widget:set_icon(tbl.text) end
    local parser = tbl.parser or function(str) return str end
    function widget:update()
        local callback = function(stdout, stderr, exitreason, exitcode)
            if exitreason == 'exit' and exitcode == 0 then
                local perc, icon = parser(stdout)
                self:set_perc(perc)
                if icon then self:set_icon(icon) end
            end
        end
        awful.spawn.easy_async_with_shell(tbl.cmd, callback)
    end
    gears.timer {
        timeout = tbl.time or 1,
        callback = function() widget:update() end,
        autostart = true,
        call_now = true,
    }
    return widget
end

local function create_battery()
    local function icon_from_perc(perc, charging)
        local icons = { '', '', '', '', '', '', '', '', '', '' }
        if charging then return '' end
        for i=1,10 do
            if perc <= i*10 then
                return icons[i]
            end
        end
        return ""
    end
    widgets.battery = create_system_widget {
        cmd = os.getenv('HOME')..'/.local/scripts/battery.sh',
        parser = function(stdout) 
            local output = gears.string.split(stdout, ' ')
            local perc = output[1]
            local icon = icon_from_perc(tonumber(perc), output[2] == 'Charging\n')
            return perc:gsub('\n', '')..'%', icon
        end,
        time = 10,
        font_size = beautiful.font_size + 3,
    }
end

local function create_ram()
    widgets.ram = create_system_widget {
        text = '',
        cmd  = "free --mega | awk '/Mem/{ print $3\" MB\"  }'",
        time = 2,
    }
end

local function create_cpu()
    widgets.cpu = create_system_widget {
        text = '',
        cmd = "top -b -n1 | awk '/%Cpu/{printf \"%.2d%%\", 100-$8}'",
        time = 2,
    }
end

return {
    -- create widgets on demand but keep only one instance
    battery = function() 
        if not widgets.battery then create_battery() end
        return widgets.battery
    end,
    cpu = function()
        if not widgets.cpu then create_cpu() end
        return widgets.cpu
    end,
    ram = function() 
        if not widgets.ram then create_ram() end
        return widgets.ram
    end,
}
