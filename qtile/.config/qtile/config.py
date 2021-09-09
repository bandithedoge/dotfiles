from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.config import (Click, Drag, EzKey, Group, Key, KeyChord, Match,
                             Screen)
from libqtile.lazy import lazy

mod = "mod4"
terminal = "kitty"
gap = 5
font1 = "Roboto Condensed"
font2 = "FiraCode Nerd Font"

# variables {{{
colors = dict(
    bg="20222c",
    bg0="181a23",
    bg2="2c2f3d",
    fg="d9dceb",
    accent="4b74ad",
    accent0="40587a",
    accent1="8eb9f5",
    comment="747dab",
    red="eb585f",
    red0="8b2f33",
    red1="f28b90",
)

settings = dict(
    border_width=2,
    border_normal=colors["bg"],
    border_focus=colors["accent"],
    font_size=12,
)
# }}}

# keybindings {{{
keys = [
    # switch window focus
    EzKey("M-h", lazy.layout.left()),
    EzKey("M-l", lazy.layout.right()),
    EzKey("M-j", lazy.layout.down()),
    EzKey("M-k", lazy.layout.up()),
    # change window order
    EzKey("M-S-h", lazy.layout.shuffle_left()),
    EzKey("M-S-l", lazy.layout.shuffle_right()),
    EzKey("M-S-j", lazy.layout.shuffle_down()),
    EzKey("M-S-k", lazy.layout.suffle_up()),
    # change window size
    EzKey("M-C-h", lazy.layout.grow_left()),
    EzKey("M-C-l", lazy.layout.grow_right()),
    EzKey("M-C-j", lazy.layout.grow_down()),
    EzKey("M-C-k", lazy.layout.grow_up()),
    # modify layout
    EzKey("M-<Tab>", lazy.next_layout()),
    EzKey("M-w", lazy.window.kill()),
    EzKey("M-t", lazy.window.toggle_floating()),
    EzKey("M-f", lazy.window.toggle_fullscreen()),
    # danger
    EzKey("M-C-r", lazy.restart()),
    EzKey("M-C-q", lazy.shutdown()),
    # shortcuts
    EzKey("M-<Return>", lazy.spawn(terminal)),
    EzKey("M-<space>", lazy.spawn("rofi -show drun")),
    KeyChord([mod],
             "BackSpace", [
                 EzKey("<space>", lazy.spawn("rofi -show drun")),
                 EzKey("S-<space>", lazy.spawn("rofi -show run")),
                 EzKey("k", lazy.spawn("rofi-keepassxc -d keepass/pass.kdbx")),
                 EzKey("c", lazy.spawn("connman-gtk")),
                 EzKey("a", lazy.spawn("pavucontrol")),
                 EzKey("h", lazy.spawn("helvum")),
                 EzKey("p", lazy.spawn("carla")),
                 EzKey("f", lazy.spawn("thunar")),
                 EzKey("b", lazy.spawn("qutebrowser")),
                 EzKey("v", lazy.spawn("vscodium")),
                 EzKey("s", lazy.spawn("flameshot gui")),
                 EzKey("e", lazy.spawn("emacsclient -c -a emacs"))
             ],
             mode="Command")
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend([
        # mod1 + letter of group = switch to group
        Key([mod],
            i.name,
            lazy.group[i.name].toscreen(),
            desc="Switch to group {}".format(i.name)),

        # mod1 + shift + letter of group = switch to & move focused window to group
        Key([mod, "shift"],
            i.name,
            lazy.window.togroup(i.name, switch_group=True),
            desc="Switch to & move focused window to group {}".format(i.name)),
        # Or, use below if you prefer not to switch to that group.
        # # mod1 + shift + letter of group = move focused window to group
        # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
        #     desc="move focused window to group {}".format(i.name)),
    ])

mouse = [
    Drag([mod],
         "Button1",
         lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod],
         "Button3",
         lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]
# }}}

# layouts {{{
layouts = [
    layout.Columns(
        margin=gap,
        border_width=settings["border_width"],
        border_normal=settings["border_normal"],
        border_focus=settings["border_focus"],
        border_on_single=True,
        grow_amount=5,
    ),
    layout.Max(),
]
# }}}

# bar {{{
widget_defaults = dict(
    background=colors["bg"],
    foreground=colors["fg"],
    font=font1,
    fontsize=settings["font_size"],
)
extension_defaults = widget_defaults.copy()


def icon(icon):
    return widget.TextBox(text=icon,
                          font=font2,
                          fontsize=16,
                          foreground=colors["fg"])


screens = [
    Screen(
        top=bar.Bar(
            [
                widget.Chord(padding=20,
                             background=colors["accent0"],
                             foreground=colors["accent1"]),
                widget.GroupBox(disable_drag=True,
                                hide_unused=True,
                                rounded=False,
                                padding=1,
                                margin_x=2,
                                fontsize=14,
                                highlight_method="block",
                                active=colors["fg"],
                                this_current_screen_border=colors["accent0"],
                                block_highlight_text_color=colors["accent1"],
                                inactive=colors["comment"],
                                background=colors["bg0"],
                                urgent_alert_method="block",
                                urgent_border=colors["red0"],
                                urgent_text=colors["red1"]),
                widget.TaskList(
                    margin=2,
                    padding=2,
                    icon_size=20,
                    rounded=False,
                    highlight_method="block",
                    borderwidth=0,
                    border=colors["bg2"],
                    unfocused_border=colors["bg0"],
                    txt_floating="",
                    txt_maximized="",
                    txt_minimized="",
                ),
                widget.Systray(),
                icon("﨎"),
                widget.ThermalSensor(
                    foreground=colors["fg"],
                    foreground_alert=colors["red"]
                    ),
                icon("﬙"),
                widget.CPUGraph(
                    border_color=colors["accent"],
                    fill_color=colors["accent0"],
                    graph_color=colors["accent1"],
                ),
                widget.CPU(format="{load_percent}%"),
                icon(""),
                widget.MemoryGraph(
                    border_color=colors["accent"],
                    fill_color=colors["accent0"],
                    graph_color=colors["accent1"],
                ),
                widget.Memory(format="{MemUsed: .0f}{mm}", ),
                widget.Spacer(length=5),
                widget.Battery(show_short_text=False,
                               format="{char}",
                               update_interval=1,
                               font=font2,
                               fontsize=18,
                               padding=3,
                               charge_char="",
                               discharge_char="",
                               empty_char="",
                               full_char="",
                               unknown_char=""),
                widget.Battery(
                    format="{percent:2.0%}",
                    update_interval=1,
                ),
                icon("直"),
                widget.Wlan(
                    interface="wlan0",
                    format="{essid}",
                    max_chars=20,
                ),
                icon("墳"),
                widget.Volume(),
                icon(""),
                widget.Clock(format="%A %d %B %Y"),
                icon(""),
                widget.Clock(format="%H:%M:%S"),
            ],
            25,
            margin=[0, 0, gap, 0],
        ),
        bottom=bar.Gap(gap),
        left=bar.Gap(gap),
        right=bar.Gap(gap),
    ),
]
# }}}

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='confirmreset'),  # gitk
    Match(wm_class='makebranch'),  # gitk
    Match(wm_class='maketag'),  # gitk
    Match(wm_class='ssh-askpass'),  # ssh-askpass
    Match(title='branchdialog'),  # gitk
    Match(title='pinentry'),  # GPG key password entry
])
auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
