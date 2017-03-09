#!/usr/bin/env bash

window=$(wmctrl -l | cut -d' ' -f 5- | dmenu "$@")
[ -n "$window" ] && wmctrl -F -a "$window"
