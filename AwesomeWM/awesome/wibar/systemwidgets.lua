local awful         = require("awful")
local beautiful     = require("beautiful")
local gears         = require("gears")
local wibox         = require("wibox")
local utils         = require("wibar.utils")

local widgets = {}
local default_font = 'Symbols Nerd Font '..beautiful.font_size 

local template = function(tbl) 

    local widget = utils.statusbar_widget {
        layout = wibox.layout.fixed.horizontal, 
        --widget.text_widget,
        wibox.widget{},
        utils.empty_space(5),
        wibox.widget{},
        id = 'template',
        --widget.widget,
    }

    function widget:set_text_widget(text_widget)
        self:get_children_by_id('template')[1]:set(1, text_widget)
    end
    function widget:set_icon_widget(icon_widget)
        self:get_children_by_id('template')[1]:set(3, icon_widget)
    end
    return widget
end

local function create_system_widget(tbl)
    --   tbl    : table with the following keys
    --   cb     : function called on output of 'cmd'; if 
    --            a string is returned, set it as the new text
    --   cmd    : shell command to call; returns percentage or 
    --            output to be parsed by 'cb'
    --   icon_widget
    --   time
    --   color

    local widget = template()
    widget:set_fg(tbl.color or beautiful.widget_fg)

    local text_widget = wibox.widget {
        widget = wibox.widget.textbox,
        text = '??',
    }

    widget:set_icon_widget(tbl.icon_widget)
    widget:set_text_widget(text_widget)

    widget.update = function()
        local callback = function(stdout, stderr, exitreason, exitcode)
            if exitreason == 'exit' and exitcode == 0 then
                local text = tbl.cb and tbl.cb(stdout) or stdout
                text_widget.text = text:gsub("\n", "")
            end
        end
        awful.spawn.easy_async_with_shell(tbl.cmd, callback)
    end

    widget.timer = gears.timer {
        timeout = tbl.time or 1,
        callback = widget.update,
        call_now = true,
    }

    return widget
end

local function create_battery()

    local function icon_from_perc(perc, is_charging)
        local icons = { '', '', '', '', '', '', '', '', '', '', '' }
        if is_charging then return '' end
        for i=1,11 do
            if perc <= i*10 - 5 then
                return icons[i]
            end
        end
        return ''
    end

    local icon_widget = wibox.widget {
        widget = wibox.widget.textbox,
        text = '',
        font = beautiful.nerd_font..' '..beautiful.font_size
    }

    widgets.battery = create_system_widget {
        icon_widget = icon_widget,
        cmd = os.getenv('HOME')..'/.local/share/scripts/battery.sh',
        cb = function(stdout) 
            local output = gears.string.split(stdout, ' ')
            local perc = tonumber(output[1])
            local icon = icon_from_perc(perc, output[2] == 'Charging\n')
            icon_widget.text = icon
            return tostring(perc)..'%'
        end,
        time = 10,
    }

    widgets.battery.visible = false

    awful.spawn.easy_async_with_shell('test -d /sys/class/power_supply/BAT0 && test -f ~/.local/share/scripts/battery.sh',
    function(stdout, stderr, exitreason, exitcode)
        if exitcode == 0 then
            widgets.battery.timer:start()
            widgets.battery.visible = true
        end
    end)

end

local function create_ram()
    widgets.ram = create_system_widget {
        icon_widget = wibox.widget {
            widget = wibox.widget.textbox,
            text = '',
            font = beautiful.nerd_font..' '..beautiful.font_size,
        },
        cmd  = "free --mega | awk '/Mem/{ print $3\"MB\"  }'",
        time = 2,
    }
    widgets.ram.timer:start()
end

local function create_cpu()
    widgets.cpu = create_system_widget {
        icon_widget = wibox.widget {
            widget = wibox.widget.textbox,
            text = '',
            font = beautiful.nerd_font..' '..beautiful.font_size,
        },
        cmd = "top -b -n1 | awk '/%Cpu/{printf \"%.2d%%\", 100-$8}'",
        time = 2,
    }
    widgets.cpu.timer:start()
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
