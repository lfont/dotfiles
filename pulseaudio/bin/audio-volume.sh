#!/usr/bin/env bash

set -e

card=0
increment=1

case "$1" in
    up)
        pactl set-sink-mute $card false && pactl set-sink-volume $card +"$increment"%
        #amixer -c $card sset Master,0 $increment%+
        ;;
    down)
        pactl set-sink-mute $card false && pactl set-sink-volume $card -"$increment"%
        #amixer -c $card sset Master,0 $increment%-
        ;;
    max)
        pactl set-sink-mute $card false && pactl set-sink-volume $card 100%
        #amixer -c $card sset Master,0 100%+
        ;;
    min)
        pactl set-sink-mute $card false && pactl set-sink-volume $card 0%
        #amixer -c $card sset Master,0 0%
        ;;
    toggle)
        pactl set-sink-mute $card toggle
        #amixer -c $card sset Master,0 toggle
        ;;
    status)
        echo $(pactl list sinks | grep "^\s*Volume" | sed 's/.* \([0-9]\+\)%.*/\1/')
        #echo $(amixer -c $card sget Master,0 | grep % | head -n 1 | sed 's/.*\[\([0-9]\+\)%\].*/\1/')
        ;;
    *)
        echo 'Use: audio-volume up|down|max|min|toggle|status'
        exit 1
esac

exit 0
