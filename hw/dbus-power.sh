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
            systemctl suspend && slock
        else
            dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend && slock
        fi
        ;;
    ZZZ)
        if [ $(which systemctl) &>/dev/null ]; then
            systemctl hibernate && slock
        else
            dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate && slock
        fi
        ;;
    *)
        echo 'dbus-power.sh should be aliased to halt|reboot|zzz|ZZZ'
        exit 1
esac

exit 0
