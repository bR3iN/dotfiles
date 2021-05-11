local awful         = require("awful")
local beautiful     = require("beautiful")
local gears         = require("gears")
local wibox         = require("wibox")

local M = {}

M.statusbar_widget = function(tbl, correction)
    local margin = (correction or 0) + beautiful.widget_outer_margin
    local widget = wibox.widget {
        widget  = wibox.container.margin,
        margins = 1,
        {
            widget     = wibox.container.background,
            shape      = beautiful.widget_shape,
            bg         = beautiful.widget_bg,
            shape_clip = true,
            shape_border_width = 1,
            shape_border_color = beautiful.widget_border_color,
            id = 'background',
            {
                widget = wibox.container.margin,
                left   = margin,
                right  = margin,
                id     = 'inner',
                tbl,
            },
        },
    }
    function widget:set_fg (color)
        self:get_children_by_id('background')[1].fg = color
    end
    function widget:setup (...)
        self:get_children_by_id('inner')[1]:setup(...)
    end
    return widget
end

M.empty_space = function(width)
    return wibox.widget {
        widget = wibox.container.margin,
        left   = width,
    }
end

M.separator = wibox.widget {
    layout = wibox.layout.align.horizontal,
    expand = 'outside',
    nil,
    {
        color     = beautiful.widget_separator_color,
        shape     = gears.shape.circle,
        widget    = wibox.widget.separator,
        forced_width = 2,
    },
    nil,
}

M.layout_separated = function(correction)
    correction = correction or 0
    return wibox.widget {
        widget         = wibox.layout.fixed.horizontal,
        spacing        = beautiful.widget_inner_spacing + correction,
        spacing_widget = M.separator,
    }
end

M.toggle = function(tbl)
    tbl.text_on = tbl.text_on or tbl.text_off

    -- convert cmd strings into lua functions
    for _, state in ipairs{ 'on', 'off' } do
        if tbl['cmd_'..state] then
            tbl['func_'..state] = function()
                awful.spawn.with_shell(tbl['cmd_'..state])
            end
        end
    end

    local default_font = beautiful.nerd_font.." "..beautiful.font_size

    local button = wibox.widget {
        widget = wibox.container.margin,
        top = 2,
        bottom = 2,
        left = 1,
        right = 1,
        {
            widget = wibox.container.background,
            id = 'background',
            shape = beautiful.widget_shape,
            bg = tbl.bg or beautiful.widget_bg,
            fg = tbl.fg or beautiful.widget_fg,
            {
                widget = wibox.container.margin,
                left = 3,
                right = 3,
                {
                    widget = wibox.widget.textbox,
                    text = tbl.text_off,
                    id = 'text',
                    font = tbl.font or default_font,
                },
            },
        },
    }
    button.on = false

    local widget_text = button:get_children_by_id('text')[1]
    function button:set_text(text)
        widget_text.text = text
    end

    local widget_bg = button:get_children_by_id('background')[1]
    function button:set_bg(color)
        local current = self:get_bg()
        widget_bg.bg = color
        return current
    end
    function button:set_fg(color)
        local current = self:get_fg()
        widget_bg.fg = color
        return current
    end
    function button:get_bg()
        return widget_bg.bg
    end
    function button:get_fg()
        return widget_bg.fg
    end

    function button:toggle()
        if self.on then tbl:func_off() else tbl:func_on() end
        self:set_text(self.on and tbl.text_off or tbl.text_on)
        self.on = not self.on
        if tbl.reverse then
            self:set_fg(self:set_bg(self:get_fg()))
        end
    end

    function button:toggle_on()
        if not self.on then button:toggle() end
    end

    function button:toggle_off()
        if self.on then button:toggle() end
    end

    button:buttons(awful.button({ }, 1, function() button:toggle() end))

    if tbl.toggle_on_init then button:toggle() end

    return button
end

--M.inner_margin = function(tbl, correction)
    --local margin = (correction or 0) + beautiful.widget_inner_margin
    --local widget =  wibox.widget {
        --widget = wibox.container.margin,
        --left = margin,
        --right = margin,
        --tbl,
    --}
    --return widget
--end

return M
