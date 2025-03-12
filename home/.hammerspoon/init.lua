local appBindings = {
    {
        app = "Todoist",
        key = "1",
        mods = { "alt" }
    },
    {
        app = "Firefox",
        key = "2",
        mods = { "alt" }
    },
    {
        app = "Ghostty",
        key = "3",
        mods = { "alt" }
    },
    {
        app = "Zed",
        key = "4",
        mods = { "alt" }
    }
}

for _, binding in ipairs(appBindings) do
    hs.hotkey.bind(binding.mods, binding.key, function()
        hs.application.launchOrFocus(binding.app)
    end)
end
