local wezterm = require "wezterm"

return {
    default_prog = {"powershell"},
    color_scheme = "GruvboxDarkHard",
    font = wezterm.font "JetBrainsMono NF",
    font_size = 13,
    tab_bar_at_bottom = true,
    keys = {
        { key = " ", mods = "SHIFT", action = "ShowLauncher" },
    },
}
