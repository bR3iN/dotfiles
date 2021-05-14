local awful     = require("awful")
local beautiful = require("beautiful")
local gears     = require("gears")
local wibox     = require("wibox")
local utils     = require("wibar.utils")

local tasklist_buttons = gears.table.join(
awful.button({ }, 1, function (c)
    if c == client.focus then
        c.minimized = true
    else
        c:emit_signal(
        "request::activate",
        "tasklist",
        {raise = true}
        )
    end
end),
awful.button({ }, 3, function()
    awful.menu.client_list({ theme = { width = 250 } })
end),
awful.button({ }, 4, function ()
    awful.client.focus.byidx(1)
end),
awful.button({ }, 5, function ()
    awful.client.focus.byidx(-1)
end)
)

local tab_shape = function(cr,w,h) gears.shape.rounded_rect(cr,w,h,2) end

local default_icon = wibox.widget {
    widget = wibox.widget.imagebox,
    image = "/usr/share/icons/Pop/128x128/categories/org.gnome.Settings.png",
}

local tab_template = {
    widget = wibox.container.background,
    id = 'background_role',
    {
        layout = wibox.container.margin,
        left   = 5,
        right  = 10,
        {
            layout = wibox.layout.fixed.horizontal,
            {
                widget = wibox.container.margin,
                margins = 2,
                id = 'icon_wrapper',
                {
                        widget = awful.widget.clienticon,
                        id = 'clienticon',
                },
            },
            utils.empty_space(3),
            {
                widget = wibox.widget.textbox,
                id = 'text_role',
            },
        },
    },
    create_callback = function(self, c)
        self:get_children_by_id('clienticon')[1].client = c
        awful.spawn.easy_async_with_shell("sleep 0.2", function()
            if c and c.valid and not c.icon then
                self:get_children_by_id('icon_wrapper')[1].widget = default_icon
            end
        end)
    end,
}

local function new_tasklist_tabs(screen)
    local tasklist = awful.widget.tasklist {
        screen  = screen,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons,
        style = {
            shape = tab_shape,
        },
        layout = {
            layout = wibox.layout.flex.horizontal,
        },
        widget_template = tab_template,
    }

    return wibox.widget {
        layout = wibox.layout.align.horizontal,
        utils.empty_space(beautiful.widget_outer_margin),
        {
            widget     = wibox.container.background,
            shape      = tab_shape,
            bg         = beautiful.tasklist_bg_normal,
            shape_clip = true,
            tasklist,
        },
        utils.empty_space(beautiful.widget_outer_margin),
    }
end

local icon_template = {
    widget = wibox.container.background,
    id = 'background_role',
    {
        widget = wibox.container.margin,
        bottom = 3,
        top = 3,
        left = 5,
        right = 5,
        id = 'icon_wrapper',
        {
            widget = awful.widget.clienticon,
            id = 'clienticon',
        },
    },
    create_callback = function(self, c)
        self:get_children_by_id('clienticon')[1].client = c
        awful.spawn.easy_async_with_shell("sleep 0.2", function()
            if c and c.valid and not c.icon then
                self:get_children_by_id('icon_wrapper')[1].widget = default_icon
            end
        end)
    end,
}

local function new_tasklist_icons(screen)
    local tasklist = awful.widget.tasklist {
        screen  = screen,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons,
        layout = utils.layout_separated(10),
        widget_template = icon_template,
    }

    return wibox.widget {
        layout = wibox.layout.align.horizontal,
        expand = 'outside',
        nil,
        {
            widget = wibox.container.margin,
            margins = 1,
            {
                widget = wibox.container.background,
                shape_border_width = 1,
                shape_border_color = beautiful.widget_border_color,
                bg = beautiful.tasklist_bg_normal,
                shape = beautiful.tasklist_shape,
                shape_clip = true,
                tasklist,
            },
        },
        nil,
    }
end

return {
    new_icons = new_tasklist_icons,
    new_tabs  = new_tasklist_tabs,
}
