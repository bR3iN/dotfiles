local awful         = require("awful")
local beautiful     = require("beautiful")
local gears         = require("gears")
local wibox         = require("wibox")
local utils         = require("wibar.utils")
local naughty       = require("naughty")

local widgets = {}

local template = function(tbl) 
    local widget = utils.statusbar_widget {
        layout = wibox.layout.fixed.horizontal, 
        wibox.widget{},
        utils.empty_space(5),
        wibox.widget{},
        id = 'template',
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
    --   tbl.cb  : function called on output of 'cmd'; if 
    --             a string is returned, set it as the new text
    --   tbl.cmd : shell command to call; returns percentage or 
    --             output to be parsed by 'cb'
    --   tbl.icon_widget
    --   tbl.time
    --   tbl.color

    local widget = template()
    widget:set_fg(tbl.color or beautiful.widget_fg)

    local text_widget = wibox.widget {
        widget = wibox.widget.textbox,
        text = '??',
    }

    widget:set_icon_widget(tbl.icon_widget)
    widget:set_text_widget(text_widget)

    function widget:update()
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
    local icon_widget = wibox.widget {
        widget = wibox.widget.textbox,
        text = '',
        font = beautiful.nerd_font..' '..beautiful.font_size
    }

    local text_widget = wibox.widget {
        widget = wibox.widget.textbox,
        text = '??%',
    }

    local widget = template()
    widget.visible = false
    widget.percentage = 0

    widget:set_icon_widget(icon_widget)
    widget:set_text_widget(text_widget)

    local icons = {'', '', '', '', '', '', '', '', '', '', ''}
    function widget:set_icon(on_battery)
        local new_icon
        if not on_battery then 
            new_icon = ''
        else
            for i=1,11 do
                if self.percentage <= i*10 - 5 then
                    new_icon = icons[i]
                    break
                end
            end
        end
        icon_widget.text = new_icon or ''
    end

    function widget:set_perc(perc)
        widget.percentage = perc
        text_widget.text = string.format("%2.2d%%", perc)
    end

    function widget:init(perc, on_battery)
        self.visible = true
        widget:set_perc(perc)
        widget:set_icon(on_battery)

        dbus.add_match('system', "sender='org.freedesktop.UPower', interface='org.freedesktop.DBus.Properties', member='PropertiesChanged'")
        dbus.connect_signal('org.freedesktop.DBus.Properties',
        function(metadata, interface, values)
            if utils.all {
                metadata.path == '/org/freedesktop/UPower/devices/battery_BAT0',
                interface == 'org.freedesktop.UPower.Device',
                values['Percentage'] ~= nil,
            } then widget:set_perc(values['Percentage'])

            elseif utils.all {
                metadata.path == '/org/freedesktop/UPower',
                interface == 'org.freedesktop.UPower',
                values['OnBattery'] ~= nil,
            } then widget:set_icon(values['OnBattery'])
        end
        end)
    end

    awful.spawn.easy_async_with_shell([[
    dbus-send --print-reply --system --dest=org.freedesktop.UPower \
    /org/freedesktop/UPower/devices/battery_BAT0 \
    org.freedesktop.DBus.Properties.Get \
    string:org.freedesktop.UPower.Device \
    string:Percentage \
    | awk 'NR==2 { print $3 }'
    dbus-send --print-reply --system --dest=org.freedesktop.UPower \
    /org/freedesktop/UPower \
    org.freedesktop.DBus.Properties.Get \
    string:org.freedesktop.UPower \
    string:OnBattery \
    | awk 'NR==2 { print $3 }'
    ]],
    function(stdout, stderr, exitreason, exitcode)
        if exitcode == 0 and exitreason == 'exit' then
            local lines = gears.string.split(stdout, '\n')
            local perc, on_battery = table.unpack(lines)
            widget:init(tonumber(perc), on_battery == 'true')
        end
    end)

    widgets.battery = widget
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


local function create_spotify()
    local widget = utils.statusbar_widget {
        widget = wibox.widget.textbox,
        id = 'text',
    }
    widget.text_widget = widget:get_children_by_id('text')[1]

    function widget:set_metadata(artist, title)
        self.text_widget.text = artist .. " - " .. title
    end

    function widget:update(event)
        if event then
            event = event:gsub('\n', '')
        else
            return
        end
        if event == 'load' then
            awful.spawn.easy_async_with_shell([[
            playerctl -p spotifyd metadata \
            | sed -n 's/.*\(artist\|title\)\s*/\1@/p' \
            ]], function(stdout, stderr, exitreason, exitcode)
                local split = gears.string.split
                if exitreason == 'exit' and exitcode == 0 then
                    local metadata = {}
                    local lines = split(stdout, '\n')
                    for _, line in pairs(lines) do
                        local key, value = table.unpack(split(line, '@'))
                        metadata[key] = value
                    end
                    self:set_metadata(metadata.artist, metadata.title)

                    self.playing = true
                    if not self.hidden then
                        self.visible = true
                    end
                end
            end)
        elseif event == 'stop' then
            self.playing = false
            self.visible = false
        end
    end

    widget:buttons(awful.button({ }, 1, function() widget:update() end))

    widget.visible = false
    widgets.spotify = widget
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
    spotify = function()
        if not widgets.spotify then create_spotify() end
        return widgets.spotify
    end,
}
