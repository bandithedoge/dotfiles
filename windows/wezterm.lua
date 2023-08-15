local wezterm = require "wezterm"

return {
    default_prog = { "powershell" },
    color_scheme = "GruvboxDarkHard",
    font = wezterm.font "JetBrainsMono NF",
    font_size = 13,
    tab_bar_at_bottom = true,
    keys = {
        { key = " ", mods = "SHIFT", action = "ShowLauncher" },
    },
    launch_menu = {
        {
            label = "MSYS2 (UCRT64)",
            args = { "C:\\msys64\\msys2_shell.cmd", "-defterm", "-here", "-no-start", "-ucrt64" },
        },
        {
            label = "MSYS2 (MINGW64)",
            args = { "C:\\msys64\\msys2_shell.cmd", "-defterm", "-here", "-no-start", "-mingw64" },
        },
    },
}
