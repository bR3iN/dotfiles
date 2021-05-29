local awful         = require("awful")
local beautiful     = require("beautiful")
local gears         = require("gears")
local wibox         = require("wibox")
local utils         = require("wibar.utils")

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
    cmd_on    = 'redshift',
    cmd_off   = 'killall redshift',
    init      = 'killall redshift',
}

--local caffeine_timer = gears.timer {
    --callback = function() awful.spawn('xset s reset') end,
    --timeout = 3,
--}

local caffeine = utils.toggle {
    text_off = '',
    text_on  = '',
    --func_on  = function() caffeine_timer:start() end,
    --func_off = function() caffeine_timer:stop()  end,
    cmd_on  = 'xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/presentation-mode -s true',
    cmd_off = 'xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/presentation-mode -s false',
    init    = 'xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/presentation-mode -s false',
}

local keyboardlayout = utils.toggle {
    text_off = "",
    cmd_on = 'setxkbmap -layout us -variant intl',
    cmd_off = 'setxkbmap -layout us',
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
        --top = 1,
        --bottom = 1,
        wibox.widget.systray(),
    },
}

--local powermenu = awful.menu{
    --{ "Log out", function() awesome.quit() end },
    --{ "Reboot", 'systemctl reboot' },
    --{ "Poweroff", 'systemctl poweroff' },
--}

--local power_button = wibox.widget {
    --widget = wibox.container.background,
    --bg = beautiful.widget_bg,
    --{
        --widget = wibox.container.margin,
        --left = 10,
        --right = 10,
        --{
            --buttons = awful.button({ }, 1, function() powermenu:show() end),
            --widget = wibox.widget.textbox,
            --font = beautiful.taglist_font,
            --text = "",
        --},
    --},
--}

--local power_button = wibox.widget {
    --widget = wibox.container.margin,
    --left = 2,
    --{
        --widget     = wibox.container.background,
        --shape      = beautiful.taglist_widget_shape,
        ----shape      = gears.shape.circle,
        --bg         = beautiful.taglist_widget_bg,
        --fg         = beautiful.taglist_fg_empty,
        --shape_clip = true,
        --shape_border_width = beautiful.taglist_widget_border_width,
        --shape_border_color = beautiful.taglist_widget_border_color,
        --id = 'background',
        --{
            --layout = wibox.layout.fixed.horizontal,
            --{
                --widget = wibox.container.margin,
                --left = 6,
                --right = 2,
                --top = 2,
                --bottom = 2,
            --},
            --{
                --widget = wibox.container.margin,
                --id     = 'inner',
                ----left = 8,
                --right = 8,
                --top = 4,
                --bottom = 4,
                --{
                    --buttons = awful.button({ }, 1, function() powermenu:show() end),
                    --widget = wibox.widget.textbox,
                    --font = beautiful.nerd_font..' '..beautiful.font_size+1,
                    --text = "",
                --},
            --},
        --},
    --},
--}
--power_button:get_children_by_id('background')[1].shape = function(cr, w, h) gears.shape.rounded_rect(cr, w, h, 4) end
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
