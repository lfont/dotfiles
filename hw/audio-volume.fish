#!/usr/bin/env fish

set card 0
set increment 5

switch (echo $argv[1])
    case up
        pactl set-sink-mute $card false; and pactl set-sink-volume $card +{$increment}%
    case down
        pactl set-sink-mute $card false; and pactl -- set-sink-volume $card -{$increment}%
    case some
        pactl set-sink-mute $card false; and pactl set-sink-volume $card 100%
    case none
        pactl set-sink-mute $card false; and pactl set-sink-volume $card 0%
    case toggle
        pactl set-sink-mute $card toggle
    case '*'
        echo 'Use: audio-volume up|down|some|none|toggle'
end

