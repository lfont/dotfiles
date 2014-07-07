#!/usr/bin/env fish

set backlight (cat /sys/class/leds/smc::kbd_backlight/brightness)
set increment 15

function set_backlight
    set -l value $argv[1]
    sudo fish -c "echo "$value" > /sys/class/leds/smc::kbd_backlight/brightness"
end

switch (echo $argv[1])
    case up
        set value (expr $backlight + $increment)
        if test $value -gt 255
            set value 255
        end
        set_backlight $value
    case down
        set value (expr $backlight - $increment)
        if test $value -lt 0
            set value 0
        end
        set_backlight $value
    case some
        set value $backlight
        while test $value -lt 255
            set value (expr $value + 1)
            if test $value -gt 255
                set value 255
            end
        end
        set_backlight $value
    case none
        set value $backlight
        while test $value -gt 0
            set value (expr $value - 1)
            if test $value -lt 0
                set value 0
            end
        end
        set_backlight $value
    case '*'
        echo "Use: keyboard-light up|down|some|none"
end

