#!/usr/bin/env bash

set -e

card=1
increment=5

case "$1" in
    up)
        pactl set-sink-mute $card false && pactl set-sink-volume $card +"$increment"%
        ;;
    down)
        pactl set-sink-mute $card false && pactl -- set-sink-volume $card -"$increment"%
        ;;
    all)
        pactl set-sink-mute $card false && pactl set-sink-volume $card 100%
        ;;
    none)
        pactl set-sink-mute $card false && pactl set-sink-volume $card 0%
        ;;
    toggle)
        pactl set-sink-mute $card toggle
        ;;
    status)
        echo $(pactl list sinks | grep "^\s*Volume" | tail -n 1 \
               | grep -oP '\d+%' | head -n 1 | sed 's/%//')
        ;;
    *)
        echo 'Use: audio-volume up|down|all|none|toggle|status'
        exit 1
esac

exit 0
