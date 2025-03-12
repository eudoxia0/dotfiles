local appBindings = {
    {
        app = "Firefox",
        key = "1",
        mods = { "alt" }
    },
    {
        app = "Todoist",
        key = "2",
        mods = { "alt" }
    },
    {
        app = "Calendar",
        key = "3",
        mods = { "alt" }
    },
    {
        app = "Obsidian",
        key = "4",
        mods = { "alt" }
    },
    {
        app = "Zed",
        key = "5",
        mods = { "alt" }
    }
}

for _, binding in ipairs(appBindings) do
    hs.hotkey.bind(binding.mods, binding.key, function()
        hs.application.launchOrFocus(binding.app)
    end)
end
