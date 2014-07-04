#!/usr/bin/env bash

BACKLIGHT=$(cat /sys/class/leds/smc::kbd_backlight/brightness)
INCREMENT=15

#if [ $UID -ne 0 ]; then
#    echo "Please run this program as superuser"
#    exit 1
#fi

case $1 in

    up)
        TOTAL=`expr $BACKLIGHT + $INCREMENT`
        if [ $TOTAL -gt "255" ]; then
            TOTAL=255
        fi
        echo $TOTAL > /sys/class/leds/smc::kbd_backlight/brightness
        ;;
    down)
        TOTAL=`expr $BACKLIGHT - $INCREMENT`
        if [ $TOTAL -lt "0" ]; then
            TOTAL=0
        fi
        echo $TOTAL > /sys/class/leds/smc::kbd_backlight/brightness
        ;;
    total)
    TEMP_VALUE=$BACKLIGHT
    while [ $TEMP_VALUE -lt "255" ]; do
        TEMP_VALUE=`expr $TEMP_VALUE + 1`
        if [ $TEMP_VALUE -gt "255" ]; then TEMP_VALUE=255; fi
        echo $TEMP_VALUE > /sys/class/leds/smc::kbd_backlight/brightness
    done
        ;;
    off)
    TEMP_VALUE=$BACKLIGHT
    while [ $TEMP_VALUE -gt "0" ]; do
        TEMP_VALUE=`expr $TEMP_VALUE - 1`
        if [ $TEMP_VALUE -lt "0" ]; then TEMP_VALUE=0; fi
        echo $TEMP_VALUE > /sys/class/leds/smc::kbd_backlight/brightness
    done
        ;;
    *)
        echo "Use: keyboard-light up|down|total|off"
        ;;
esac

