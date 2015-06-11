#!/usr/bin/env bash

function start {
    prog="$1"
    if [ -z $(pidof "$prog") ]; then
        if [ ! -z $(which "$prog") ]; then
            eval "$prog &"
        fi
    fi
}

## gnome polkit
start /usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1

## xfce power manager
start xfce4-power-manager

## xfce volume manager
start xfce4-volumed

## volume control
start pasystray

## clipboard manager
start clipit

## bluetooth applet
start blueman-applet

## network applet
start nm-applet
