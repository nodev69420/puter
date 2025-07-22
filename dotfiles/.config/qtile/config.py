from libqtile import bar, layout, qtile, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal

mod = "mod4"
terminal = guess_terminal()

solid_colour = '#285577'
border_colour = '#4c7899'

# solid_colour = '#b3570b'
# border_colour = '#68360c'

keys = [
    Key([mod], "h", lazy.layout.left(), desc="move focus to left"),
    Key([mod], "l", lazy.layout.right(), desc="move focus to right"),
    Key([mod], "j", lazy.layout.down(), desc="move focus down"),
    Key([mod], "k", lazy.layout.up(), desc="move focus up"),
    Key([mod], "space", lazy.layout.next(), desc="move window focus to other window"),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="move window up"),
    Key([mod, "control"], "h", lazy.layout.grow_left(), desc="grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(), desc="grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="reset all window sizes"),
    Key(
        [mod, "shift"],
        "return",
        lazy.layout.toggle_split(),
        desc="toggle between split and unsplit sides of stack",
    ),
    Key([mod], "return", lazy.spawn(terminal), desc="launch terminal"),
    Key([mod], "tab", lazy.next_layout(), desc="toggle between layouts"),
    Key([mod], "w", lazy.window.kill(), desc="kill focused window"),
    Key(
        [mod],
        "f",
        lazy.window.toggle_fullscreen(),
        desc="toggle fullscreen on the focused window",
    ),
    Key([mod], "t", lazy.window.toggle_floating(), desc="toggle floating on the focused window"),
    Key([mod], "r", lazy.reload_config(), desc="reload the config"),
    Key([mod, "shift"], "q", lazy.shutdown(), desc="shutdown qtile"),
    Key([mod], "p", lazy.spawn("rofi -show drun"), desc="rofi drun"),
    Key([mod], "m", lazy.spawn("rofi -show window"), desc="rofi window"),
]

for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )


groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc=f"Switch to group {i.name}",
            ),
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=False),
                desc=f"Switch to & move focused window to group {i.name}",
            ),
        ]
    )

layouts = [
    layout.Max(),
    layout.Columns(
        border_width=1,
        border_focus='#285577',
        border_normal='#222222',
    ),
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font="TerminessNerdFontMono",
    fontsize=14,
    padding=1,
)
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.Spacer(length=4),
                widget.WindowCount(
                    fmt='[{}]'
                ),
                widget.WindowTabs(
                    padding=8,
                    selected=('<i>', '</i>'),
                ),
                widget.Spacer(),
                widget.Clock(
                    format="%d-%m-%Y %a %H:%M:%S",
                    padding=8,
                ),
            ],
            24,
            background=solid_colour,
            border_color=border_colour,
            border_width=1,
        ),
        bottom=bar.Bar(
            [
                widget.CurrentLayoutIcon(),
                widget.GroupBox(highlight_method='text'),
                widget.Spacer(),

                widget.Image(filename="~/ass/circle.png", margin=4),
                widget.Memory(),
                widget.Spacer(length=8),
                widget.MemoryGraph(
                    border_width=1,
                ),

                widget.Spacer(length=8),

                widget.Image(filename="~/ass/circle.png", margin=4),
                widget.Spacer(length=8),
                widget.CPU(),
                widget.Spacer(length=8),
                widget.CPUGraph(
                    border_width=1,
                ),

                widget.Systray(),
                # widget.Clock(format="%d-%m-%Y %a %H:%M:%S"),
            ],
            24,
        ),
        wallpaper="~/puter/pape/pape3.jpg",
        wallpaper_mode="fill"
    ),
]

mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),
        Match(wm_class="makebranch"),
        Match(wm_class="maketag"),
        Match(wm_class="ssh-askpass"),
        Match(title="branchdialog"),
        Match(title="pinentry"),
    ],
    border_width=0,
)

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True
auto_minimize = True
wl_input_rules = None
wl_xcursor_theme = "miku-cursor-linux"
wl_xcursor_size = 24
wmname = "LG3D"
