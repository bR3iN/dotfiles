local awful     = require("awful")
local beautiful = require("beautiful")
local gears     = require("gears")
local wibox     = require("wibox")
local utils     = require("wibar.utils")
local naughty   = require("naughty")

-- Screen independent widgets {{{
local powermenu = awful.menu{
    { "Log out", function() awesome.quit() end },
    { "Reboot", 'systemctl reboot' },
    { "Poweroff", 'systemctl poweroff' },
}
local clock = utils.statusbar_widget()
clock:setup {
    layout = wibox.layout.fixed.horizontal,
    buttons = awful.button({ }, 1, function() powermenu:show() end),
    {
        layout = utils.layout_separated(),
        wibox.widget.textclock('%d/%m'),
        wibox.widget.textclock('%H:%M'),
    },
}

local redshift = utils.toggle {
    text_on   = '',
    text_off  = '',
    cb_on    = 'redshift',
    cb_off   = 'killall redshift',
    init      = 'killall redshift',
}

--local caffeine_timer = gears.timer {
    --callback = function() awful.spawn('xset s reset') end,
    --timeout = 3,
--}

local caffeine = utils.toggle {
    text_off = '',
    text_on  = '',
    --cb_on  = function() caffeine_timer:start() end,
    --cb_off = function() caffeine_timer:stop()  end,
    cb_on  = 'xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/presentation-mode -s true',
    cb_off = 'xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/presentation-mode -s false',
    cb_init = function(self) self:toggle_off() end,
}

local keyboardlayout = utils.toggle {
    text_off = "",
    cb_on   = function() awesome.xkb_set_layout_group(1) end,
    cb_off  = function() awesome.xkb_set_layout_group(0) end,
    cb_init = function(self) self:toggle_off() end,
    reverse = true,
}

local nogaps = utils.toggle {
    text_on = '',
    text_off = '',
    cb_off = function(self)
        beautiful.useless_gap = self.gap
        if client.focus then client.focus:emit_signal("raised") end
    end,
    cb_on = function(self)
        beautiful.useless_gap = 0
        if client.focus then client.focus:emit_signal("raised") end
    end,
    cb_init = function(self)
        self.gap = beautiful.useless_gap
        self:toggle_on()
    end,
}

local tray = utils.statusbar_widget {
    layout = wibox.layout.fixed.horizontal,
    {
        layout = wibox.layout.fixed.horizontal,
        spacing = 3,
        keyboardlayout,
        redshift,
        nogaps,
        caffeine,
    },
    utils.empty_space(5),
    {
        widget = wibox.container.margin,
        top = 1,
        bottom = 1,
        wibox.widget.systray(),
    },
}
--}}}

spotify = require'wibar.systemwidgets'.spotify()
local function create_wibar(s)
    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
    awful.button({ }, 1, function () awful.layout.inc( 1) end),
    awful.button({ }, 3, function () awful.layout.inc(-1) end)
    ))

    s.taglist        = require('wibar.taglist').create(s)
    s.tasklist_tabs  = require('wibar.tasklist').new_tabs(s)
    s.tasklist_icons = require('wibar.tasklist').new_icons(s)

    -- Make global toggles accessible as screen properties (used in keybindings)
    s.redshift       = redshift
    s.keyboardlayout = keyboardlayout
    s.caffeine       = caffeine
    s.nogaps         = nogaps

    local lhs = wibox.widget {
        layout = wibox.layout.fixed.horizontal,
        s.taglist,
        spotify,
        s.mypromptbox,
    }

    local spacing = beautiful.widget_outer_margin - 8
    local rhs = wibox.widget {
        layout = wibox.layout.align.horizontal,
        nil,
        nil,
        {
            layout = wibox.layout.fixed.horizontal,
            spacing = spacing,
            {
                layout = wibox.layout.fixed.horizontal,
                spacing = spacing,
                require'wibar.systemwidgets'.cpu(),
                require'wibar.systemwidgets'.ram(),
                id = 'hide',
            },
            require'wibar.systemwidgets'.battery(),
            tray,
            clock,
            power_button,
        },
    }

    local middle_with_tabs  = wibox.widget {
        layout = wibox.layout.align.horizontal,
        utils.empty_space(beautiful.widget_outer_spacing + 3),
        s.tasklist_tabs,
        utils.empty_space(beautiful.widget_outer_spacing),
    }

    local middle_with_icons  = wibox.widget {
        layout = wibox.layout.align.horizontal,
        utils.empty_space(beautiful.widget_outer_spacing),
        s.tasklist_icons,
        utils.empty_space(beautiful.widget_outer_spacing),
    }

    s.wibar = awful.wibar {
        position = 'top',
        screen = s,
        bg = beautiful.bg_wibar,
    }

    s.wibar:setup {
        layout = wibox.layout.align.horizontal,
        expand = 'outside',
        lhs,
        middle_with_icons,
        rhs,
    }

    function s.wibar:set_tasklist(widget)
        self.widget.second = widget
    end

    function s.update_tasklist()
        local new_tasklist
        if awful.layout.get(s) == awful.layout.suit.max then
            new_tasklist = middle_with_tabs
            s.wibar.widget.expand = 'inside'
            rhs:get_children_by_id('hide')[1].visible = false
            spotify.hidden = true
            spotify.visible = false
        else
            new_tasklist = middle_with_icons
            s.wibar.widget.expand = 'outside'
            rhs:get_children_by_id('hide')[1].visible = true
            spotify.hidden = false
            if spotify.playing then
                spotify.visible = true
            end
        end
        s.wibar:set_tasklist(new_tasklist)
    end

    s:connect_signal("tag::history::update", s.update_tasklist)
    tag.connect_signal("property::layout", s.update_tasklist)
    client.connect_signal("tagged", s.update_tasklist)
    client.connect_signal("untagged", s.update_tasklist)
end

return { create_wibar = create_wibar }
