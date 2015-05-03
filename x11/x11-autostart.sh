#!/usr/bin/env bash

function start {
    prog="$1"
    if [ -z $(pidof "$prog") ]; then
        if [ ! -z $(which "$prog") ]; then
            eval "$prog &"
        fi
    fi
}

## Gnome polkit
start /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1

## Start xfce power manager
start xfce4-power-manager

## Start xfce volume manager
start xfce4-volumed

## Pulse Audio volume control
start pasystray

## Start Clipboard manager
start clipit

## Start blueman applet
start blueman-applet

## Start network manager applet
start nm-applet
