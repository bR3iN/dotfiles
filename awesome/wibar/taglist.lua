local awful     = require("awful")
local beautiful = require("beautiful")
local gears     = require("gears")
local wibox     = require("wibox")
local utils     = require("wibar.utils")

local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local count = 0 -- counts volatile tags throughout session
local add_volatile = function(screen)
    return function()
        count = count + 1
        return awful.tag.add(tostring(count), {
            volatile = true,
            screen = screen,
            layout = awful.layout.layouts[1],
        })
    end
end

local function taglist(screen)
    return awful.widget.taglist {
        screen  = screen,
        filter  = awful.widget.taglist.filter.all,
        buttons = taglist_buttons,
        layout = utils.layout_separated(2),
        widget_template = {
            widget = wibox.widget.textbox,
            id = 'text_role',
        },
    }
end

local layoutbox_buttons = gears.table.join(
awful.button({ }, 1, function () awful.layout.inc( 1) end),
awful.button({ }, 3, function () awful.layout.inc(-1) end)
)

local function create_taglist(screen)
    screen.add_volatile = add_volatile(screen)

    local layoutbox = awful.widget.layoutbox(screen)
    layoutbox:buttons(layoutbox_buttons)

    local widget = wibox.widget {
        layout = wibox.layout.fixed.horizontal,
        spacing = beautiful.widget_outer_spacing,
        utils.statusbar_widget({
            widget = wibox.container.margin,
            top = 4,
            bottom = 4,
            layoutbox,
        }, -1),
        utils.statusbar_widget({
            widget = taglist(screen)
        }, 1),
        utils.statusbar_widget {
            buttons = awful.button({ }, 1, screen.add_volatile),
            widget = wibox.widget.textbox,
            font = beautiful.taglist_font,
            text = "",
        },
    }
    return widget
end

return create_taglist
