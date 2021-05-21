local gears = require'gears'
local theme_assets = require("beautiful.theme_assets")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi
local wibox = require'wibox'

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

-- Inherit from default theme and source colors.
local default_theme = dofile(themes_path.."default/theme.lua")
local theme = gears.table.join(default_theme, require'colors')


theme.maximized_hide_border = true
theme.fullscreen_hide_border = true

theme.font_size     = 10
theme.font          = "Fira Medium "..tostring(theme.font_size)
theme.nerd_font     = "Symbols Nerd Font"

theme.bg_normal     = theme.color0
theme.bg_focus      = theme.foreground
theme.bg_urgent     = theme.color1
theme.bg_minimize   = theme.bg_normal
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = theme.foreground
theme.fg_focus      = theme.background_dark
theme.fg_urgent     = theme.foreground
theme.fg_minimize   = theme.foreground

theme.gap_single_client = false
theme.useless_gap   = dpi(20)

theme.border_width  = dpi(1)
theme.border_normal = theme.color0
theme.border_focus  = '#55B19B'

-- statusbar widgets {{{
theme.widget_shape = gears.shape.rounded_bar
theme.bg_wibar = theme.background_dark
--theme.widget_bg = theme.background
theme.widget_bg = theme.color8
theme.widget_fg = theme.color7
theme.widget_separator_color = theme.color7
theme.widget_border_color = theme.grey
theme.widget_border_width = 1
theme.widget_outer_margin = 10
theme.widget_inner_spacing = 18 -- should be even
theme.widget_outer_spacing = 3
-- }}}

-- tasklist {{{
theme.tasklist_bg_normal   = theme.widget_bg
theme.tasklist_fg_normal   = theme.color15
theme.tasklist_bg_focus    = theme.tasklist_fg_normal
theme.tasklist_fg_focus    = theme.tasklist_bg_normal
theme.tasklist_bg_minimize = theme.background_dark
theme.tasklist_separator_color = theme.widget_separator_color
theme.tasklist_widget_border_width = theme.widget_border_width
theme.tasklist_widget_border_color = theme.widget_border_color
theme.tasklist_widget_shape = function(cr, w, h) gears.shape.rounded_rect(cr, w, h, 4) end
theme.tasklist_shape = theme.tasklist_widget_shape
theme.tasklist_tab_shape = function(cr,w,h) gears.shape.rounded_rect(cr, w, h, 2) end
-- }}}

-- taglist {{{
theme.taglist_font        = theme.nerd_font.." "..tostring(theme.font_size)
theme.taglist_fg_empty    = theme.widget_fg
theme.taglist_fg_focus    = theme.color10
theme.taglist_fg_occupied = theme.color13
theme.taglist_fg_urgent   = theme.color9
theme.taglist_bg_focus    = theme.color8
theme.taglist_bg_volatile = theme.color8
theme.taglist_widget_shape = function(cr, w, h) gears.shape.rounded_rect(cr, w, h, 4) end
theme.taglist_widget_bg = theme.widget_bg
theme.taglist_widget_border_width = theme.widget_border_width
theme.taglist_widget_border_color = theme.grey
-- }}}

theme.icon_theme = "Pop"

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
