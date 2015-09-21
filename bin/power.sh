#!/usr/bin/env bash

set -e

CMD=$(basename $0)

case $CMD in
    halt)
        if [ $(which systemctl) &>/dev/null ]; then
            systemctl poweroff
        else
            dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
        fi
        ;;
    reboot)
        if [ $(which systemctl) &>/dev/null ]; then
            systemctl reboot
        else
            dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart
        fi
        ;;
    zzz)
        if [ $(which systemctl) &>/dev/null ]; then
            systemctl suspend
        else
            dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend && xlock
        fi
        ;;
    ZZZ)
        if [ $(which systemctl) &>/dev/null ]; then
            systemctl hibernate
        else
            dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate && xlock
        fi
        ;;
    *)
        echo 'power.sh should be aliased to halt|reboot|zzz|ZZZ'
        exit 1
esac

exit 0
