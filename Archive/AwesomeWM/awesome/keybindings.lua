-- Imports {{{
local gears = require("gears")
local awful = require("awful")
local naughty = require("naughty")
-- }}}

-- Helper functions {{{
local globalkeys    = {}
local clientkeys    = {}
local clientbuttons = {}

local function add_key_g(key) gears.table.merge(globalkeys,    key) end
local function add_key_c(key) gears.table.merge(clientkeys,    key) end
local function add_key_m(key) gears.table.merge(clientbuttons, key) end
--}}}

-- Awesome stuff {{{
gears.table.map(add_key_g, {
    awful.key({ modkey,           }, "Escape", function() 
        awful.spawn.with_shell('sleep 0.5; xset dpms force off') 
    end, {description = "lock screen", group = "awesome"}),

    awful.key({ modkey            }, "c", function()
        awful.screen.focused().caffeine:toggle()
    end, {description = "toggle disable screensaver", group = "awesome"}),

    awful.key({ modkey            }, "g", function()
        awful.screen.focused().nogaps:toggle()
    end, {description = "toggle gaps", group = "awesome"}),

    awful.key({ modkey,           }, "i", function()
        awful.screen.focused().keyboardlayout:toggle()
    end, {description = "toggle keyboardlayout", group = "awesome"}),

    awful.key({ modkey,           }, "r", function()
        awful.screen.focused().redshift:toggle()
    end, {description = "toggle redshift", group = "awesome"}),

    awful.key({ modkey, "Control", "Shift" }, "q", awesome.quit,
    {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey, "Control", "Shift" }, "s", require'awful.hotkeys_popup'.show_help,
    {description="show help", group="awesome"}),

    awful.key({ modkey, "Control", "Shift" }, "e", function()
        awful.spawn(editor_cmd .. " " .. awesome.conffile, {
            tag = '漣',
            switch_to_tags = true,
        })
    end, {description = "edit config", group = "awesome"}),

    awful.key({ modkey, "Control" }, "r", awesome.restart,
    {description = "reload awesome", group = "awesome"}),

    --awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
    --{description = "run prompt", group = "awesome"}),

    --awful.key({ modkey }, "x",
    --function ()
    --awful.prompt.run {
    --prompt       = "Run Lua code: ",
    --textbox      = awful.screen.focused().mypromptbox.widget,
    --exe_callback = awful.util.eval,
    --history_path = awful.util.get_cache_dir() .. "/history_eval"
    --}
    --end,
    --{description = "lua execute prompt", group = "awesome"}),

    awful.key({ modkey            }, "s",
    function ()
        local s = awful.screen.focused()
        s.wibar.visible = not s.wibar.visible
    end, {description = "toggle statusbar", group = "awesome"}),
}) 
-- }}}

-- Navigation {{{
gears.table.map(add_key_g, {
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
    {description = "jump to urgent client", group = "navigation"}),

    awful.key({ modkey, "Control" }, "n", function()
        if client.focus then
            local t = awful.screen.focused().add_volatile()
            client.focus:move_to_tag(t)
            t:view_only()
        end
    end, {description = "move client to new tag", group = "navigation"}),

    awful.key({ modkey,           }, "Tab", awful.tag.history.restore,
    {description = "go back", group = "navigation"}),
})

local function add_navigation_keys(left, down, up, right) --{{{
    add_key_g(awful.key({ modkey,           }, down, function ()
        awful.client.focus.byidx( 1)
    end, {description = "focus next client by index", group = "navigation"}))

    add_key_g(awful.key({ modkey,           }, up, function ()
        awful.client.focus.byidx(-1)
    end, {description = "focus previous client by index", group = "navigation"}))

    add_key_g(awful.key({ modkey,           }, left,   awful.tag.viewprev,
    {description = "view previous tag", group = "navigation"}))

    add_key_g(awful.key({ modkey,           }, right,  awful.tag.viewnext,
    {description = "view next tag", group = "navigation"}))

    add_key_g(awful.key({ modkey, "Shift" }, left, function ()
        if client.focus then
            local index = client.focus.first_tag.index
            local tag   = client.focus.screen.tags[index - 1] or client.focus.screen.tags[#client.focus.screen.tags]
            if tag then
                client.focus:move_to_tag(tag)
                tag:view_only()
            end
        end
    end, {description = "move focused client to the previous tag", group = "navigation"}))

    add_key_g(awful.key({ modkey, "Shift" }, right,
        function ()
            if client.focus then
                local index = client.focus.first_tag.index
                local tag   = client.focus.screen.tags[index + 1] or client.focus.screen.tags[1]
                if tag then
                    client.focus:move_to_tag(tag)
                    tag:view_only()
                end
            end
        end,
        {description = "move focused client to the next tag", group = "navigation"}))

    add_key_g(awful.key({ modkey, "Control" }, down, function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "navigation"}))

    add_key_g(awful.key({ modkey, "Control" }, up, function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "navigation"}))

    add_key_g(awful.key({ modkey, "Shift"   }, down, function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "navigation"}))

    add_key_g(awful.key({ modkey, "Shift"   }, up, function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "navigation"}))
end --}}}

add_navigation_keys('h', 'j', 'k', 'l')
add_navigation_keys('Left', 'Down', 'Up', 'Right')
-- }}}

-- Launcher {{{
gears.table.map(add_key_g, {
    awful.key({ modkey            }, "f", function()
        awful.spawn('thunar')
    end, {description = "open files", group = "launcher"}),

    awful.key({ modkey            }, "d", function()
        awful.spawn('rofi-open')
    end, {description = "open document", group = "launcher"}),

    awful.key({ modkey            }, "/", function()
        local cmd = 'rofi -show combi '..(os.getenv('ROFI_FLAGS') or '')
        awful.spawn.with_shell(cmd)
    end, {description = "launch rofi", group = "launcher"}),

    awful.key({ modkey, "Control" }, "e", function ()
        awful.spawn('rofi-emoji')
    end, {description = "launch emoji picker", group = "launcher"}),

    awful.key({ modkey,           }, "b", function ()
        awful.spawn("qutebrowser")
    end, {description = "open qutebrowser", group = "launcher"}),

    awful.key({ modkey, "Control" }, "b", function ()
        awful.spawn.with_shell("rofi-ff")
    end, {description = "open firefox", group = "launcher"}),

    awful.key({ modkey,           }, "e", function()
        awful.spawn('thunderbird', { tag = "" })
    end, {description = "open e-mail client", group = "launcher"}),

    awful.key({ modkey,           }, "t", function ()
        awful.spawn.with_shell(terminal..' -e '..(fish_available and 'fish' or 'bash'))
    end, {description = "open a terminal", group = "launcher"}),

    --awful.key({ modkey, "Shift"   }, "t", function ()
        --awful.spawn(terminal .. " -e tmux")
    --end, {description = "open tmux", group = "launcher"}),

    --awful.key({ modkey, "Control" }, "t", function ()
        --awful.spawn('torbrowser-launcher')
    --end, {description = "open tor browser", group = "launcher"}),
})
-- }}}

-- Layout {{{
gears.table.map(add_key_g, {
    awful.key({ modkey            }, ".", function ()
        awful.tag.incmwfact(-0.05)
    end, {description = "increase master width factor", group = "layout"}),

    awful.key({ modkey            }, ",", function ()
        awful.tag.incmwfact( 0.05)
    end, {description = "decrease master width factor", group = "layout"}),

    awful.key({modkey, "Control"}, ".", function() 
        awful.client.incwfact(-0.1)
    end, {description = "decrease clients width factor", group = "layout"}),

    awful.key({modkey, "Control"}, ",", function() 
        awful.client.incwfact( 0.1)
    end, {description = "increase clients width factor", group = "layout"}),

    awful.key({ modkey,           }, "space", function()
        awful.layout.inc( 1)
    end, {description = "select next", group = "layout"}),

    awful.key({ modkey, "Shift"   }, "space", function()
        awful.layout.inc(-1)
    end, {description = "select previous", group = "layout"}),
})

local function add_layout_keys(left, down, up, right) --{{{
    add_key_g(awful.key({ modkey, "Control" }, down,     function ()
        awful.tag.incnmaster( 1, nil, true)
    end, {description = "increase the number of master clients", group = "layout"}))

    add_key_g(awful.key({ modkey, "Control" }, up,     function ()
        awful.tag.incnmaster(-1, nil, true)
    end, {description = "decrease the number of master clients", group = "layout"}))

    add_key_g(awful.key({ modkey, "Control" }, right,     function ()
        awful.tag.incncol( 1, nil, true)
    end, {description = "increase the number of columns", group = "layout"}))

    add_key_g(awful.key({ modkey, "Control" }, left,     function ()
        awful.tag.incncol(-1, nil, true)
    end, {description = "decrease the number of columns", group = "layout"}))
end --}}}

add_layout_keys('h', 'j', 'k', 'l')
add_layout_keys('Left', 'Down', 'Up', 'Right')
--}}}

-- Media keys {{{
--add_key_g(awful.key({ modkey }, "XF86MonBrightnessUp", function()
    --awful.spawn('kbbacklight_toggle on')
--end, {}))
--add_key_g(awful.key({ modkey }, "XF86MonBrightnessDown", function()
    --awful.spawn('kbbacklight_toggle off')
--end, {}))
--add_key_g(awful.key({ }, "XF86MonBrightnessDown", function()
--awful.spawn('brightnessctl set -n 3%-')
--end, {}))
--add_key_g(awful.key({ }, "XF86MonBrightnessUp", function()
--awful.spawn('brightnessctl set -n +3%')
--end, {}))
-- }}}

-- Clientkeys {{{
add_key_g(awful.key({ modkey, "Shift"   }, "n", function ()
    local c = awful.client.restore()
    -- Focus restored client
    if c then
        c:emit_signal(
        "request::activate", "key.unminimize", {raise = true}
        )
    end
end, {description = "restore minimized", group = "client"}))

gears.table.map(add_key_c, {

    awful.key({ modkey, "Control" }, "f", function (c)
        c.fullscreen = not c.fullscreen
        c:raise()
    end, {description = "toggle fullscreen", group = "client"}),

    awful.key({ modkey,           }, "q", function (c)
        c:kill()
    end, {description = "close", group = "client"}),

    awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle,
    {description = "toggle floating", group = "client"}),

    awful.key({ modkey, "Control" }, "Return", function (c)
        awful.client.setslave(c)
    end, {description = "move to slave", group = "client"}),

    awful.key({ modkey,           }, "Return", function (c)
        awful.client.setmaster(c)
    end, {description = "move to master", group = "client"}),

    awful.key({ modkey,           }, "o", function (c)
        c:move_to_screen()
    end, {description = "move to screen", group = "client"}),

    awful.key({ modkey, "Control" }, "s", function (c)
        c.sticky = not c.sticky
    end, {description = "toggle sticky", group = "client"}),

    awful.key({ modkey, "Control" }, "t", function (c)
        c.ontop = not c.ontop
    end, {description = "toggle keep on top", group = "client"}),

    awful.key({ modkey,           }, "n", function (c)
        c.minimized = true
    end , {description = "minimize", group = "client"}),

    awful.key({ modkey,           }, "m", function (c)
        c.maximized = not c.maximized
        c:raise()
    end, {description = "(un)maximize", group = "client"}),

    awful.key({ modkey, "Control" }, "m", function (c)
        c.maximized_vertical = not c.maximized_vertical
        c:raise()
    end, {description = "(un)maximize vertically", group = "client"}),

    awful.key({ modkey, "Control", "Shift" }, "m", function (c)
        c.maximized_horizontal = not c.maximized_horizontal
        c:raise()
    end, {description = "(un)maximize horizontally", group = "client"})
})
-- }}}

-- Numbers {{{
for i = 1, 9 do
    gears.table.map(add_key_g, {
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
        function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
                tag:view_only()
            end
        end, {description = "view tag #"..i, group = "tag"}),

        -- Toggle tag display.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
        function ()
            local screen = awful.screen.focused()
            local tag = screen.tags[i]
            if tag then
                awful.tag.viewtoggle(tag)
            end
        end, {description = "toggle tag #" .. i, group = "tag"}),

        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
        function ()
            if client.focus then
                local tag = client.focus.screen.tags[i]
                if tag then
                    client.focus:move_to_tag(tag)
                end
            end
        end, {description = "move focused client to tag #"..i, group = "tag"}),

        -- Toggle tag on focused client.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
        function ()
            if client.focus then
                local tag = client.focus.screen.tags[i]
                if tag then
                    client.focus:toggle_tag(tag)
                end
            end
        end, {description = "toggle focused client on tag #" .. i, group = "tag"}),
    })
end
--- }}}

-- Clientbuttons {{{
add_key_m(awful.button({ }, 1, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
end))
add_key_m(awful.button({ modkey }, 1, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.move(c)
end))
add_key_m(awful.button({ modkey }, 3, function (c)
    c:emit_signal("request::activate", "mouse_click", {raise = true})
    awful.mouse.client.resize(c)
end))
-- }}}

return {
    globalkeys    = globalkeys,
    clientkeys    = clientkeys,
    clientbuttons = clientbuttons,
}
