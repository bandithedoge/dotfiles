#!/usr/bin/env lua

local actions = {
    ["Lock"] = {
        cmd = "loginctl lock-session $XDG_SESSION_ID",
        icon = "key-symbolic",
    },
    ["Reboot"] = {
        cmd = "systemctl reboot",
        icon = "feed-refresh-symbolic",
    },
    ["Shutdown"] = {
        cmd = "systemctl poweroff",
        icon = "system-devices-symbolic",
    },
}

if arg[1] == nil then
    for action, data in pairs(actions) do
        -- print("%s\0icon\x1f%s", action.cmd, action.icon)
        print(action .. "\0icon\x1f" .. data.icon)
    end
else
    os.execute(actions[arg[1]].cmd)
end
