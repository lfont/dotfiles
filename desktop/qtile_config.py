from libqtile.config import Key, Screen, Group, Drag, Click, Match
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook

mod = "mod4"
alt = "mod1"

keys = [
    # Switch between windows in current stack pane
    Key(
        [mod], "k",
        lazy.layout.down()),
    Key(
        [mod], "j",
        lazy.layout.up()
    ),

    # Move windows up or down in current stack
    Key(
        [mod, "shift"], "k",
        lazy.layout.shuffle_down()
    ),
    Key(
        [mod, "shift"], "j",
        lazy.layout.shuffle_up()
    ),

    # Switch window focus to other pane(s) of stack
    Key(
        [mod], "space",
        lazy.layout.next()
    ),
    Key(
        [mod, "shift"], "space",
        lazy.layout.previous()
    ),

    # Toggle between different layouts as defined below
    Key(
        [mod], "Tab",
        lazy.nextlayout()
    ),

    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key(
        [mod, "control"], "Tab",
        lazy.layout.toggle_split()
    ),

    # Swap panes of split stack
    Key(
        [mod, "shift"], "Tab",
        lazy.layout.rotate()
    ),

    # Switch screen
    Key(
        [mod, alt], "h",
        lazy.to_screen(0)
    ),
    Key(
        [mod, alt], "l",
        lazy.to_screen(1)
    ),

    # Some apps shortcuts
    Key(
        [mod, "control"], "t",
        lazy.spawn("terminator")
    ),
    Key(
        [mod, "control"], "w",
        lazy.spawn("firefox-developer")
    ),
    Key(
        [mod, "control"], "m",
        lazy.spawn("emacs -T gnus -f gnus")
    ),
    Key(
        [mod, "control"], "c",
        lazy.spawn("emacs -T jabber -f jabber-display-roster -f jabber-connect-all")
    ),
    Key(
        [mod, "control"], "f",
        lazy.spawn("pcmanfm")
    ),
    Key(
        [mod, "control"], "p",
        lazy.spawn("keepass2")
    ),

    # Some WM shortcuts
    Key(
        [mod], "r",
        lazy.spawncmd()
    ),
    Key(
        [mod], "w",
        lazy.window.kill()
    ),
    Key(
        [mod], "x",
        lazy.spawn("~/.config/dmenu/dmenu-bind.sh")
    ),

    Key(
        [mod, "control"], "r",
        lazy.restart()
    ),
    Key(
        [mod, "control"], "l",
        lazy.spawn("xscreensaver-command -lock")
    ),

    Key(
        [mod, "control", "shift"], "q",
        lazy.shutdown()
    ),
    Key(
        [mod, "control", "shift"], "s",
        lazy.spawn("systemctl suspend")
    ),
    Key(
        [mod, "control", "shift"], "h",
        lazy.spawn("systemctl hibernate")
    ),
    Key(
        [mod, "control", "shift"], "r",
        lazy.spawn("systemctl reboot")
    ),
    Key(
        [mod, "control", "shift"], "o",
        lazy.spawn("systemctl halt")
    ),
]

groups = [
    Group(
        "a",
        matches=[Match(wm_class=['Firefox', 'Google-chrome'])]
    ),
    Group(
        "s",
        layout="stack",
        matches=[Match(wm_class=['emacs', 'Emacs24'], title=['gnus', 'jabber'])]
    ),
    Group("d"),
    Group(
        "f",
        matches=[Match(wm_class=['keepass2', 'KeePass2'])]
    ),
    Group("u"),
    Group("i"),
    Group("o"),
    Group("p"),
]

for i in groups:
    # mod1 + letter of group = switch to group
    keys.append(
        Key(
            [mod], i.name,
            lazy.group[i.name].toscreen()
        )
    )

    # mod1 + shift + letter of group = switch to & move focused window to group
    keys.append(
        Key(
            [mod, "shift"], i.name,
            lazy.window.togroup(i.name)
        )
    )

layouts = [
    layout.Max(),
    layout.Stack(
        num_stacks=2
    )
]

widget_defaults = dict(
    font='Inconsolata',
    fontsize=16,
    padding=2,
)

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    margin_x=2,
                    margin_y=2
                ),
                widget.Sep(),
                widget.CurrentLayout(),
                widget.Sep(),
                widget.Prompt(),
                widget.WindowTabs(),
                widget.CPUGraph(
                    border_width=1,
                    line_width=1,
                    width=26,
                    margin_x=2,
                    margin_y=2
                ),
                widget.MemoryGraph(
                    border_width=1,
                    line_width=1,
                    width=26,
                    margin_x=2,
                    margin_y=2
                ),
                widget.HDDBusyGraph(
                    border_width=1,
                    line_width=1,
                    width=26,
                    margin_x=2,
                    margin_y=2
                ),
                widget.NetGraph(
                    border_width=1,
                    line_width=1,
                    width=26,
                    margin_x=2,
                    margin_y=2
                ),
                widget.Systray(),
                widget.Clock(
                    format="%a %d %b %H:%M"
                ),
                widget.Battery(
                    format="{char}{percent:2.0%}",
                    charge_char="+",
                    discharge_char="-"
                ),
                widget.Volume()
            ],
            26,
        )
    ),
    Screen(
        top=bar.Bar(
            [
                widget.GroupBox(
                    margin_x=2,
                    margin_y=2
                ),
                widget.Sep(),
                widget.CurrentLayout(),
                widget.Sep(),
                widget.WindowTabs(),
            ],
            26,
        )
    ),
]

# Drag floating layouts.
mouse = [
    Drag(
        [mod], "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position()
    ),
    Drag(
        [mod], "Button3",
        lazy.window.set_size_floating(),
        start=lazy.window.get_size()
    ),
    Click(
        [mod], "Button2",
        lazy.window.bring_to_front()
    )
]

dgroups_key_binder = None
dgroups_app_rules = []
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
floating_layout = layout.Floating()
auto_fullscreen = True
wmname = "qtile"

@hook.subscribe.startup
def startup():
    import subprocess
    subprocess.Popen(["terminator"])
