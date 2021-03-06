#!/bin/bash

## HiDPI
## https://wiki.archlinux.org/index.php/HiDPI
## https://wiki.archlinux.org/index.php/xorg#Display_size_and_DPI
xrandr --output eDP-1 --auto --dpi 192 \
       --output DP-1-2 --auto --primary --panning 3840x2160+3840+0 --scale 2x2 --right-of eDP-1 \
       --output DP-1 --off \
       --output DP-2 --off \
       --output HDMI-1 --off \
       --output HDMI-2 --off
