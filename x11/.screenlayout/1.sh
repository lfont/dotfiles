#!/bin/bash

## HiDPI
## https://wiki.archlinux.org/index.php/HiDPI
## https://wiki.archlinux.org/index.php/xorg#Display_size_and_DPI
xrandr --output eDP-1 --auto --primary --dpi 192 \
       --output DP-1 --off \
       --output DP-2 --off \
       --output HDMI-1 --off \
       --output HDMI-2 --off

if [[ ! -z `xrandr --query | grep "DP-1-2 connected"` ]]
then
    xrandr --output DP-1-2 --off
fi
