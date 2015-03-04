#!/usr/bin/env fish

function start
    set -l prog $argv[1]
    if not test (pidof $prog)
        if test (which $prog)
            eval "$prog &"
        end
    end
end

## Gnome polkit
start /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1

## Start xfce power manager
start xfce4-power-manager

## Pulse Audio volume control
start pasystray

## Start Clipboard manager
start clipit

## Start blueman applet
start blueman-applet

## Start network manager applet
start nm-applet
