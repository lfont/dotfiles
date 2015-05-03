#!/usr/bin/env bash

set -e

backlight=$(cat /sys/class/leds/smc::kbd_backlight/brightness)
increment=15

function set_backlight() {
    value=$argv[1]
    echo $value > /sys/class/leds/smc::kbd_backlight/brightness
}

case "$1" in
    up)
        value=$(expr $backlight + $increment)
        if [ $value -gt 255 ]; then
            value=255
        fi
        set_backlight $value
        ;;
    down)
        value=$(expr $backlight - $increment)
        if [ $value -lt 0 ]; then
            value=0
        fi
        set_backlight $value
        ;;
    all)
        value=$backlight
        while [ $value -lt 255 ]; do
            value=$(expr $value + 1)
            if [ $value -gt 255 ]; then
                value=255
            fi
        done
        set_backlight $value
        ;;
    none)
        value=$backlight
        while [ $value -gt 0 ]; do
            value=$(expr $value - 1)
            if [ $value -lt 0 ]; then
                value=0
            fi
        done
        set_backlight $value
        ;;
    *)
        echo "Use: keyboard-light up|down|all|none"
        exit 1
esac

exit 0
