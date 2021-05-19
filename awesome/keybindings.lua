-- Imports {{{
local gears = require("gears")
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
-- }}}

globalkeys = gears.table.join(
    awful.key({ modkey,           }, "Escape", function() awful.spawn.with_shell('sleep 0.3; xset dpms force off') end,
              {description = "lock screen", group = "awesome"}),

    awful.key({ modkey }, "d", function()
        awful.spawn('rofi-open')
    end, {}),
    awful.key({ modkey }, "/", function()
        awful.spawn('rofi -show combi')
    end, {}),
    awful.key({ }, "XF86MonBrightnessDown", function()
        awful.spawn('brightnessctl set -n 3%-')
    end, {}),
    awful.key({ }, "XF86MonBrightnessUp", function()
        awful.spawn('brightnessctl set -n +3%')
    end, {}),

    awful.key({ modkey, "Control" }, "n", function()
        if client.focus then
            local t = awful.screen.focused().add_volatile()
            client.focus:move_to_tag(t)
            t:view_only()
        end
    end,      {description = "Move to new tag", group = "tag"}),
    -- Awesome stuff {{{
    awful.key({ modkey            }, "g", function()
        awful.screen.focused().nogaps:toggle()
    end,      {description = "toggle gaps", group = "awesome"}),
    --awful.key({ modkey, "Control" }, "s", function()
        --local s = awful.screen.focused()
        --if s.wibar.visible then
            --s.nogaps:toggle_on()
        --else
            --s.nogaps:toggle_off()
        --end
        --s.wibar.visible = not s.wibar.visible
    --end,      {description = "toggle gaps and statusbar", group = "awesome"}),
    awful.key({ modkey,           }, "i", function()
        awful.screen.focused().keyboardlayout:toggle()
    end,      {description = "toggle keyboardlayout", group = "awesome"}),
    awful.key({ modkey,           }, "r", function()
        awful.screen.focused().redshift:toggle()
    end,      {description = "toggle redshift", group = "awesome"}),
    awful.key({ modkey, "Control", "Shift" }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey, "Control", "Shift" }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),

    awful.key({ modkey, "Control", "Shift" }, "e", function () awful.spawn(editor_cmd .. " " .. awesome.conffile) end,
              {description = "edit config", group = "awesome"}),
    awful.key({ modkey, "Control" }, "e", function () awful.spawn('rofi-emoji') end,
              {description = "launch emoji picker", group = "awesome"}),

    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),

    --awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              --{description = "run prompt", group = "awesome"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),

    awful.key({ modkey            }, "s",
        function ()
            local s = awful.screen.focused()
            s.wibar.visible = not s.wibar.visible
        end,
        {description = "toggle statusbar", group = "awesome"}),
    -- }}}

    -- Navigation with vim keys {{{
    awful.key({ modkey,           }, "j", function () awful.client.focus.byidx( 1) end,
        {description = "focus next by index", group = "navigation"}),

    awful.key({ modkey,           }, "k", function () awful.client.focus.byidx(-1) end,
        {description = "focus previous by index", group = "navigation"}),

    awful.key({ modkey,           }, "h",   awful.tag.viewprev,
              {description = "view previous", group = "navigation"}),

    awful.key({ modkey,           }, "l",  awful.tag.viewnext,
              {description = "view next", group = "navigation"}),

    awful.key({ modkey, "Shift" }, "h",
        function ()
            if client.focus then
                local index = client.focus.first_tag.index
                local tag   = client.focus.screen.tags[index - 1] or client.focus.screen.tags[#client.focus.screen.tags]
                if tag then
                    client.focus:move_to_tag(tag)
                    tag:view_only()
                end
            end
        end,
        {description = "move focused client to the previous tag", group = "navigation"}),

    awful.key({ modkey, "Shift" }, "l",
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
        {description = "move focused client to the next tag", group = "navigation"}),

    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "navigation"}),

    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "navigation"}),

    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "navigation"}),

    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "navigation"}),
    -- }}}

    -- Navigation with arrow keys {{{
    awful.key({ modkey, "Shift"   }, "Down", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "navigation"}),

    awful.key({ modkey, "Shift"   }, "Up", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "navigation"}),

    awful.key({ modkey, "Control" }, "Down", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "navigation"}),

    awful.key({ modkey, "Control" }, "Up", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "navigation"}),

    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "navigation"}),

    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "navigation"}),

    awful.key({ modkey, "Shift" }, "Left",
        function ()
            if client.focus then
                local index = client.focus.first_tag.index
                local tag   = client.focus.screen.tags[index - 1] or client.focus.screen.tags[#client.focus.screen.tags]
                if tag then
                    client.focus:move_to_tag(tag)
                    tag:view_only()
                end
            end
        end,
        {description = "move focused client to the previous tag", group = "navigation"}),

    awful.key({ modkey, "Shift" }, "Right",
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
        {description = "move focused client to the next tag", group = "navigation"}),

    awful.key({ modkey,           }, "Down", function () awful.client.focus.byidx( 1) end,
        {description = "focus next by index", group = "navigation"}),

    awful.key({ modkey,           }, "Up", function () awful.client.focus.byidx(-1) end,
        {description = "focus previous by index", group = "navigation"}),
    -- }}}

    -- Clients {{{
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    awful.key({ modkey, "Shift"   }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                    c:emit_signal(
                        "request::activate", "key.unminimize", {raise = true}
                    )
                  end
              end,
              {description = "restore minimized", group = "client"}),
    -- }}}

    -- Launcher {{{
    awful.key({ modkey,           }, "b", function () awful.spawn("firefox") end,
              {description = "Start Firefox", group = "launcher"}),

    awful.key({ modkey, "Control" }, "b", function () awful.spawn.with_shell("rofi-ff") end,
              {description = "Choose firefox profile", group = "launcher"}),

    awful.key({ modkey,           }, "e", function () awful.spawn('thunderbird') end,
              {description = "open thunderbird", group = "launcher"}),

    awful.key({ modkey,           }, "t", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),

    awful.key({ modkey, "Shift"   }, "t", function () awful.spawn(terminal .. " -e tmux") end,
              {description = "open tmux", group = "launcher"}),
    -- }}}

    -- Layout {{{

    -- With vim keys {{{
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),

    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "j",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "k",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "l",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "h",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    -- }}}

    -- With arrow keys {{{
    awful.key({ modkey, "Control" }, "Right",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),

    awful.key({ modkey, "Control" }, "Left",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "Down",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "Up",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "Right",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),

    awful.key({ modkey, "Shift", "Control" }, "Left",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    -- }}}

    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),

    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"})
    -- }}}

    --awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              --{description = "show main menu", group = "awesome"}),

    --awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              --{description = "jump to urgent client", group = "client"}),

    -- Prompt
    -- Menubar
    --awful.key({ modkey }, "p", function() menubar.show() end,
              --{description = "show the menubar", group = "launcher"})
)

clientkeys = gears.table.join( -- {{{
    awful.key({ modkey, "Control" }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey,           }, "q",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey, "Control" }, "s",      function (c) c.sticky = not c.sticky          end,
              {description = "toggle sticky", group = "client"}),
    --awful.key({ modkey, "Control" }, "t",      function (c) c.ontop = not c.ontop            end,
              --{description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "(un)maximize", group = "client"}),
    awful.key({ modkey, "Control" }, "m",
        function (c)
            c.maximized_vertical = not c.maximized_vertical
            c:raise()
        end ,
        {description = "(un)maximize vertically", group = "client"}),
    awful.key({ modkey, "Control", "Shift" }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c:raise()
        end ,
        {description = "(un)maximize horizontally", group = "client"})
)
-- }}}

-- Numbers {{{
-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end
--- }}}

clientbuttons = gears.table.join( -- {{{
    awful.button({ }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
    end),
    awful.button({ modkey }, 1, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.move(c)
    end),
    awful.button({ modkey }, 3, function (c)
        c:emit_signal("request::activate", "mouse_click", {raise = true})
        awful.mouse.client.resize(c)
    end)
)
-- }}}

root.keys(globalkeys)
