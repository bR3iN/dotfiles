local awful         = require("awful")
local beautiful     = require("beautiful")
local gears         = require("gears")
local wibox         = require("wibox")
local utils         = require("wibar.utils")

-- Screen independent widgets {{{
local clock = utils.statusbar_widget()
clock:setup {
    layout = wibox.layout.fixed.horizontal,
    {
        layout = utils.layout_separated(),
        wibox.widget.textclock('%d/%m'),
        wibox.widget.textclock('%H:%M'),
    },
}

local redshift = utils.toggle {
    text_on   = '',
    text_off  = '',
    cmd_on    = 'redshift -x && redshift',
    cmd_off   = 'killall redshift',
}

local caffeine_timer = gears.timer {
    callback = function() awful.spawn('xset s reset') end,
    timeout = 3,
}

local caffeine = utils.toggle {
    text_off = '',
    text_on  = '',
    func_on  = function() caffeine_timer:start() end,
    func_off = function() caffeine_timer:stop()  end,
}

local xkbmap_flags = os.getenv('XKBMAP_FLAGS') and " "..os.getenv('XKBMAP_FLAGS') or ""
local keyboardlayout = utils.toggle {
    text_off = "",
    cmd_on = 'setxkbmap -layout us -variant intl' .. xkbmap_flags,
    cmd_off = 'setxkbmap -layout us' .. xkbmap_flags,
    reverse = true,
}

local nogaps = utils.toggle {
    text_on = '',
    text_off = '',
    func_off = function(self)
        beautiful.useless_gap = self.gap
        if client.focus then client.focus:emit_signal("raised") end
    end,
    func_on = function(self)
        self.gap = self.gap or beautiful.useless_gap
        beautiful.useless_gap = 0
        if client.focus then client.focus:emit_signal("raised") end
    end,
}

local tray = utils.statusbar_widget {
    widget = utils.margins(0),
    {
        layout = wibox.layout.fixed.horizontal,
        spacing = 3,
        keyboardlayout,
        redshift,
        nogaps,
        caffeine,
        wibox.widget.systray(),
    },
}

local powermenu = awful.menu{
    { "Log out", function() awesome.quit() end },
    { "Reboot", 'reboot' },
    { "Poweroff", 'poweroff' },
}

local power_button = utils.statusbar_widget({
    buttons = awful.button({ }, 1, function() powermenu:show() end),
    widget = wibox.widget.textbox,
    font = beautiful.taglist_font,
    text = "",
}, -5)
power_button:get_children_by_id('background')[1].shape = function(cr, w, h) gears.shape.rounded_rect(cr, w, h, 4) end
-- }}}

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
        --opacity = 0.9,
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
        if s.clients and #s.clients >= 1 then
            if awful.layout.get(s) == awful.layout.suit.max then
                new_tasklist = middle_with_tabs
                s.wibar.widget.expand = 'inside'
                rhs:get_children_by_id('hide')[1].visible = false
            else
                new_tasklist = middle_with_icons
                s.wibar.widget.expand = 'outside'
                rhs:get_children_by_id('hide')[1].visible = true
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
