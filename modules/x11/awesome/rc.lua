-- Awesome WM configuration

local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local wibox = require("wibox")
local beautiful = require("beautiful")

-- Initialize theme
beautiful.init(gears.filesystem.get_themes_dir() .. "default/theme.lua")

-- Wallpaper
local wallpaper = "/home/eudoxia/.eudoxia.d/data/wallpaper/panther.jpg"

local function set_wallpaper(s)
    gears.wallpaper.maximized(wallpaper, s, true)
end

screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    set_wallpaper(s)

    -- Each screen has its own tag table (workspaces)
    awful.tag({ "α", "β", "γ", "δ", "ε" }, s, awful.layout.layouts[1])

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            awful.widget.taglist {
                screen = s,
                filter = awful.widget.taglist.filter.all,
            },
        },
        nil, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            wibox.widget.systray(),
            wibox.widget.textclock(),
        },
    }
end)

-- Layouts: tiling first (default)
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.floating,
    awful.layout.suit.max,
}

-- Modkey
local modkey = "Mod4"

-- Key bindings
local globalkeys = gears.table.join(
    -- s-Q: Quit awesome
    awful.key({ modkey, "Shift" }, "q", awesome.quit,
        { description = "quit awesome", group = "awesome" }),

    -- s-R: Restart awesome
    awful.key({ modkey, "Shift" }, "r", awesome.restart,
        { description = "restart awesome", group = "awesome" }),

    -- s-r: dmenu
    awful.key({ modkey }, "r", function() awful.spawn("dmenu_run") end,
        { description = "run dmenu", group = "launcher" }),

    -- C-s-w: Firefox
    awful.key({ modkey, "Control" }, "w", function() awful.spawn("firefox") end,
        { description = "open firefox", group = "launcher" }),

    -- C-s-c: Alacritty
    awful.key({ modkey, "Control" }, "c", function() awful.spawn("alacritty") end,
        { description = "open alacritty", group = "launcher" }),

    -- C-s-f: Thunar
    awful.key({ modkey, "Control" }, "f", function() awful.spawn("thunar") end,
        { description = "open thunar", group = "launcher" }),

    -- Workspace switching
    awful.key({ modkey }, "1", function() awful.tag.viewonly(awful.screen.focused().tags[1]) end),
    awful.key({ modkey }, "2", function() awful.tag.viewonly(awful.screen.focused().tags[2]) end),
    awful.key({ modkey }, "3", function() awful.tag.viewonly(awful.screen.focused().tags[3]) end),
    awful.key({ modkey }, "4", function() awful.tag.viewonly(awful.screen.focused().tags[4]) end),
    awful.key({ modkey }, "5", function() awful.tag.viewonly(awful.screen.focused().tags[5]) end),

    -- Focus navigation
    awful.key({ modkey }, "j", function() awful.client.focus.byidx(1) end,
        { description = "focus next window", group = "client" }),
    awful.key({ modkey }, "k", function() awful.client.focus.byidx(-1) end,
        { description = "focus previous window", group = "client" }),

    -- Layout manipulation
    awful.key({ modkey, "Shift" }, "j", function() awful.client.swap.byidx(1) end,
        { description = "swap with next client", group = "client" }),
    awful.key({ modkey, "Shift" }, "k", function() awful.client.swap.byidx(-1) end,
        { description = "swap with previous client", group = "client" }),

    awful.key({ modkey }, "l", function() awful.tag.incmwfact(0.05) end,
        { description = "increase master width", group = "layout" }),
    awful.key({ modkey }, "h", function() awful.tag.incmwfact(-0.05) end,
        { description = "decrease master width", group = "layout" }),

    awful.key({ modkey }, "space", function() awful.layout.inc(1) end,
        { description = "cycle layouts", group = "layout" })
)

-- Client keys
local clientkeys = gears.table.join(
    -- s-q: Kill window
    awful.key({ modkey }, "q", function(c) c:kill() end,
        { description = "close", group = "client" }),

    awful.key({ modkey }, "f", function(c)
        c.fullscreen = not c.fullscreen
        c:raise()
    end, { description = "toggle fullscreen", group = "client" }),

    awful.key({ modkey, "Shift" }, "t", awful.client.floating.toggle,
        { description = "toggle floating", group = "client" })
)

-- Set keys
root.keys(globalkeys)

-- Rules
awful.rules.rules = {
    -- All clients
    {
        rule = {},
        properties = {
            border_width = beautiful.border_width,
            border_color = beautiful.border_normal,
            focus = awful.client.focus.filter,
            raise = true,
            keys = clientkeys,
            screen = awful.screen.preferred,
            placement = awful.placement.no_overlap + awful.placement.no_offscreen,
        }
    },
}

-- Focus follows mouse
client.connect_signal("mouse::enter", function(c)
    c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

-- Gaps
beautiful.useless_gap = 10
