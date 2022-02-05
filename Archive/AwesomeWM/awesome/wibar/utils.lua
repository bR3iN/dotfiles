local awful         = require("awful")
local beautiful     = require("beautiful")
local gears         = require("gears")
local wibox         = require("wibox")

local M = {}

M.statusbar_widget = function(tbl, correction)
    local margin = (correction or 0) + beautiful.widget_outer_margin
    local widget = wibox.widget {
        widget  = wibox.container.margin,
        margins = 2,
        {
            widget     = wibox.container.background,
            shape      = beautiful.widget_shape,
            bg         = beautiful.widget_bg,
            fg         = beautiful.widget_fg,
            shape_clip = true,
            shape_border_width = beautiful.widget_border_width,
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

    for _, arg in ipairs{ 'cb_on', 'cb_off', 'cb_init' } do
        -- convert callback cmds into into functions if neccessary
        if type(tbl[arg]) == 'string' then
            local cmd = tbl[arg]
            tbl[arg] = function() awful.spawn.with_shell(cmd) end
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
            shape = function(cr,w,h) gears.shape.rounded_rect(cr, w, h, 2) end,
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

    button.on          = false
    button.text_widget = button:get_children_by_id('text')[1]
    button.bg_widget   = button:get_children_by_id('background')[1]
    button.cb_on       = tbl.cb_on
    button.cb_off      = tbl.cb_off

    function button:set_text(text)
        self.text_widget.text = text
    end

    function button:get_bg()
        return self.bg_widget.bg
    end

    function button:get_fg()
        return self.bg_widget.fg
    end

    function button:set_bg(color)
        local current_bg = self:get_bg()
        self.bg_widget.bg = color
        return current_bg
    end

    function button:set_fg(color)
        local current_fg = self:get_fg()
        self.bg_widget.fg = color
        return current_fg
    end

    function button:toggle()
        if self.on then self:cb_off() else self:cb_on() end
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

    if tbl.cb_init then tbl.cb_init(button) end

    return button
end

M.margins = function(margin)
    return function(tbl)
        local widget =  wibox.widget {
            widget = wibox.container.margin,
            left = margin,
            right = margin,
            tbl,
        }
        return widget
    end
end

M.all = function(tbl)
    for _, v in pairs(tbl) do
        if not v then
            return false
        end
    end
    return true
end

M.any = function(tbl)
    for _, v in pairs(tbl) do
        if v then
            return true
        end
    end
    return false
end


return M
